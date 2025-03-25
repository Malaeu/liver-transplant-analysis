# ============================================================================
# DATA PREPROCESSING FUNCTIONS
# ============================================================================
# Functions for loading, validating, and preprocessing the datasets
# ============================================================================

#' Load and validate datasets
#'
#' @param full_data_path Path to the full dataset CSV file
#' @param bca_data_path Path to the BCA dataset RDS file
#' @param validate Whether to perform validation checks on the data
#' @return A list containing two data frames: full_data and bca_data
load_datasets <- function(full_data_path = "imputed_data_full.csv",
                          bca_data_path = "wl_df_with_bca.rds",
                          validate = TRUE) {
  
  # Load full dataset
  message("Loading full dataset from: ", full_data_path)
  full_data <- read.csv(full_data_path, stringsAsFactors = FALSE)
  
  # Load BCA dataset
  message("Loading BCA dataset from: ", bca_data_path)
  bca_data <- readRDS(bca_data_path)
  
  # Perform validation if requested
  if (validate) {
    validation_results <- validate_data(full_data, bca_data)
    if (!validation_results$valid) {
      warning("Data validation found issues: \n", validation_results$summary)
      print(validation_results$issues)
    } else {
      message("Data validation passed successfully")
    }
  }
  
  # Return both datasets
  return(list(
    full_data = full_data,
    bca_data = bca_data
  ))
}

#' Validate datasets for quality and consistency
#'
#' @param full_data The full dataset data frame
#' @param bca_data The BCA dataset data frame
#' @param check_negative Whether to check for negative values in body composition variables
#' @param check_consistency Whether to check for consistency between datasets
#' @return A list containing validation results
validate_data <- function(full_data, bca_data, 
                          check_negative = TRUE, 
                          check_consistency = TRUE) {
  
  # Initialize issues data frame
  issues <- data.frame(
    dataset = character(),
    variable = character(),
    issue = character(),
    count = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Check for missing values in key variables
  key_vars_full <- c("etnr_id", "waitlist_time_days", "status", "lab_meld")
  for (var in key_vars_full) {
    if (var %in% names(full_data)) {
      missing_count <- sum(is.na(full_data[[var]]))
      if (missing_count > 0) {
        issues <- rbind(issues, data.frame(
          dataset = "full_data",
          variable = var,
          issue = "missing_values",
          count = missing_count,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      issues <- rbind(issues, data.frame(
        dataset = "full_data",
        variable = var,
        issue = "variable_missing",
        count = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Check for missing values in BCA variables
  bca_vars <- c("muscle", "sat", "vat", "imat", "eat", "pat", "tat")
  for (var in bca_vars) {
    if (var %in% names(bca_data)) {
      missing_count <- sum(is.na(bca_data[[var]]))
      if (missing_count > 0) {
        issues <- rbind(issues, data.frame(
          dataset = "bca_data",
          variable = var,
          issue = "missing_values",
          count = missing_count,
          stringsAsFactors = FALSE
        ))
      }
    } else {
      issues <- rbind(issues, data.frame(
        dataset = "bca_data",
        variable = var,
        issue = "variable_missing",
        count = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Check for negative values in body composition variables
  if (check_negative) {
    for (var in bca_vars) {
      if (var %in% names(bca_data)) {
        negative_count <- sum(bca_data[[var]] < 0, na.rm = TRUE)
        if (negative_count > 0) {
          issues <- rbind(issues, data.frame(
            dataset = "bca_data",
            variable = var,
            issue = "negative_values",
            count = negative_count,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Check for consistency between datasets
  if (check_consistency && "etnr_id" %in% names(full_data) && "etnr_id" %in% names(bca_data)) {
    # Check for BCA patients not in full dataset
    bca_not_in_full <- sum(!bca_data$etnr_id %in% full_data$etnr_id)
    if (bca_not_in_full > 0) {
      issues <- rbind(issues, data.frame(
        dataset = "bca_data",
        variable = "etnr_id",
        issue = "not_in_full_data",
        count = bca_not_in_full,
        stringsAsFactors = FALSE
      ))
    }
    
    # Check for inconsistent values in common variables
    common_vars <- intersect(names(full_data), names(bca_data))
    common_vars <- common_vars[!common_vars %in% c("etnr_id")]
    
    for (var in common_vars) {
      # Get patients in both datasets
      common_ids <- intersect(full_data$etnr_id, bca_data$etnr_id)
      
      # Subset data to common patients
      full_subset <- full_data[full_data$etnr_id %in% common_ids, ]
      bca_subset <- bca_data[bca_data$etnr_id %in% common_ids, ]
      
      # Ensure same order
      full_subset <- full_subset[order(full_subset$etnr_id), ]
      bca_subset <- bca_subset[order(bca_subset$etnr_id), ]
      
      # Check for inconsistencies
      if (is.numeric(full_subset[[var]]) && is.numeric(bca_subset[[var]])) {
        # For numeric variables, check if difference exceeds threshold
        diff_count <- sum(abs(full_subset[[var]] - bca_subset[[var]]) > 0.001, na.rm = TRUE)
        if (diff_count > 0) {
          issues <- rbind(issues, data.frame(
            dataset = "both",
            variable = var,
            issue = "inconsistent_values",
            count = diff_count,
            stringsAsFactors = FALSE
          ))
        }
      } else {
        # For non-numeric variables, check for exact matches
        match_count <- sum(full_subset[[var]] != bca_subset[[var]], na.rm = TRUE)
        if (match_count > 0) {
          issues <- rbind(issues, data.frame(
            dataset = "both",
            variable = var,
            issue = "inconsistent_values",
            count = match_count,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
  }
  
  # Determine if validation passed
  valid <- nrow(issues) == 0
  
  # Create summary
  summary <- paste0(
    "Data validation ", ifelse(valid, "passed", "failed"), ".\n",
    "Found ", nrow(issues), " issues.\n",
    "Full dataset: ", nrow(full_data), " patients, ", ncol(full_data), " variables.\n",
    "BCA dataset: ", nrow(bca_data), " patients, ", ncol(bca_data), " variables."
  )
  
  # Return results
  return(list(
    valid = valid,
    issues = issues,
    summary = summary
  ))
}

#' Preprocess data for analysis
#'
#' @param data The input data frame
#' @param impute_missing Whether to impute missing values
#' @param normalize_bca Whether to normalize body composition variables
#' @param create_derived_vars Whether to create derived variables
#' @return A preprocessed data frame
preprocess_data <- function(data, 
                            impute_missing = TRUE, 
                            normalize_bca = TRUE,
                            create_derived_vars = TRUE) {
  
  # Make a copy of the data
  processed_data <- data
  
  # Handle missing values
  if (impute_missing) {
    # Identify variables with missing values
    vars_with_na <- names(processed_data)[colSums(is.na(processed_data)) > 0]
    
    if (length(vars_with_na) > 0) {
      message("Imputing missing values for ", length(vars_with_na), " variables")
      
      # Simple imputation for demonstration
      # In a real scenario, consider using mice package for multiple imputation
      for (var in vars_with_na) {
        if (is.numeric(processed_data[[var]])) {
          # For numeric variables, impute with median
          if (!all(is.na(processed_data[[var]]))) {  # Check if there are non-NA values
            processed_data[[var]][is.na(processed_data[[var]])] <-
              median(processed_data[[var]], na.rm = TRUE)
          }
        } else {
          # For categorical variables, impute with mode
          if (!all(is.na(processed_data[[var]]))) {  # Check if there are non-NA values
            mode_table <- table(processed_data[[var]])
            if (length(mode_table) > 0) {
              mode_value <- names(sort(mode_table, decreasing = TRUE))[1]
              if (!is.null(mode_value) && length(mode_value) > 0) {
                processed_data[[var]][is.na(processed_data[[var]])] <- mode_value
              }
            }
          }
        }
      }
    }
  }
  
  # Normalize body composition variables
  if (normalize_bca) {
    bca_vars <- c("muscle", "sat", "vat", "imat", "eat", "pat", "tat")
    bca_vars <- bca_vars[bca_vars %in% names(processed_data)]
    
    if (length(bca_vars) > 0 && "cm" %in% names(processed_data)) {
      message("Normalizing body composition variables by height")
      
      # Convert height to meters
      processed_data$height_m <- processed_data$cm / 100
      
      # Normalize by height squared (similar to BMI calculation)
      for (var in bca_vars) {
        norm_var <- paste0(var, "_norm")
        processed_data[[norm_var]] <- processed_data[[var]] / (processed_data$height_m^2)
      }
    }
  }
  
  # Create derived variables
  if (create_derived_vars) {
    message("Creating derived variables")
    
    # Calculate muscle-to-fat ratio
    if (all(c("muscle", "tat") %in% names(processed_data))) {
      processed_data$muscle_fat_ratio <- processed_data$muscle / pmax(processed_data$tat, 0.1)
    }
    
    # Calculate visceral-to-subcutaneous fat ratio
    if (all(c("vat", "sat") %in% names(processed_data))) {
      processed_data$vat_sat_ratio <- processed_data$vat / pmax(processed_data$sat, 0.1)
    }
    
    # Calculate subcutaneous fat percentage
    if (all(c("sat", "tat") %in% names(processed_data))) {
      processed_data$sat_percentage <- processed_data$sat / pmax(processed_data$tat, 0.1) * 100
    }
    
    # Create age categories
    if ("age" %in% names(processed_data)) {
      processed_data$age_cat <- cut(
        processed_data$age,
        breaks = c(0, 40, 60, 100),
        labels = c("<40", "40-60", ">60"),
        include.lowest = TRUE
      )
    }
    
    # Create MELD categories
    if ("lab_meld" %in% names(processed_data)) {
      processed_data$meld_cat <- cut(
        processed_data$lab_meld,
        breaks = c(0, 15, 25, 40),
        labels = c("<15", "15-25", ">25"),
        include.lowest = TRUE
      )
    }
    
    # Create BMI categories
    if ("bmi" %in% names(processed_data)) {
      processed_data$bmi_cat <- cut(
        processed_data$bmi,
        breaks = c(0, 18.5, 25, 30, 100),
        labels = c("Underweight", "Normal", "Overweight", "Obese"),
        include.lowest = TRUE
      )
    }
  }
  
  return(processed_data)
}

#' Create analysis dataset by merging and preprocessing data
#'
#' @param full_data The full dataset data frame
#' @param bca_data The BCA dataset data frame
#' @param merge_by The column name to use for merging datasets
#' @param filter_criteria Optional expression for filtering the merged dataset
#' @param bca_vars Optional vector of BCA variables to include in the merge
#' @return A merged and preprocessed data frame ready for analysis
create_analysis_dataset <- function(full_data,
                                    bca_data,
                                    merge_by = "etnr_id",
                                    filter_criteria = NULL,
                                    bca_vars = NULL) {
  
  # Ensure merge column exists in both datasets
  if (!merge_by %in% names(full_data) || !merge_by %in% names(bca_data)) {
    stop("Merge column '", merge_by, "' not found in both datasets")
  }
  
  # If bca_vars is provided, use only those variables from bca_data
  if (!is.null(bca_vars)) {
    # Check if all specified variables exist in bca_data
    missing_vars <- bca_vars[!bca_vars %in% names(bca_data)]
    if (length(missing_vars) > 0) {
      warning("The following BCA variables were not found: ",
              paste(missing_vars, collapse = ", "))
      bca_vars <- bca_vars[bca_vars %in% names(bca_data)]
    }
    
    # Select only the merge column and specified BCA variables
    bca_data_subset <- bca_data[, c(merge_by, bca_vars)]
    message("Using ", length(bca_vars), " specified BCA variables for merge")
  } else {
    # Identify common columns (besides merge column)
    common_cols <- intersect(names(full_data), names(bca_data))
    common_cols <- common_cols[common_cols != merge_by]
    
    if (length(common_cols) > 0) {
      message("Found ", length(common_cols), " common columns besides merge column")
      message("Common columns will be taken from full_data")
      
      # Remove common columns from bca_data to avoid duplicates
      bca_data_subset <- bca_data[, !names(bca_data) %in% common_cols]
    } else {
      bca_data_subset <- bca_data
    }
  }
  
  # Merge datasets
  message("Merging datasets by '", merge_by, "'")
  merged_data <- merge(full_data, bca_data_subset, by = merge_by, all.x = TRUE, all.y = FALSE)
  
  message("Merged dataset has ", nrow(merged_data), " rows and ", ncol(merged_data), " columns")
  
  # Apply filter criteria if provided
  if (!is.null(filter_criteria)) {
    message("Applying filter criteria")
    filter_expr <- eval(filter_criteria)
    pre_filter_rows <- nrow(merged_data)
    merged_data <- merged_data[filter_expr, ]
    message("Filtered from ", pre_filter_rows, " to ", nrow(merged_data), " rows")
  }
  
  # Preprocess the merged dataset
  message("Preprocessing merged dataset")
  processed_data <- preprocess_data(
    data = merged_data,
    impute_missing = TRUE,
    normalize_bca = TRUE,
    create_derived_vars = TRUE
  )
  
  return(processed_data)
}

#' Split data into training and validation sets
#'
#' @param data The input data frame
#' @param prop Proportion of data for training
#' @param stratify_var Optional variable for stratified sampling
#' @param seed Random seed for reproducibility
#' @return A list containing training and validation data frames
split_data <- function(data, 
                       prop = 0.7, 
                       stratify_var = NULL, 
                       seed = 123) {
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # If stratification variable is provided
  if (!is.null(stratify_var) && stratify_var %in% names(data)) {
    # Get stratification variable
    strata <- data[[stratify_var]]
    
    # Create index for each stratum
    indices <- lapply(unique(strata), function(s) {
      # Get indices for this stratum
      stratum_indices <- which(strata == s)
      
      # Sample indices for training
      train_size <- floor(prop * length(stratum_indices))
      train_indices <- sample(stratum_indices, train_size)
      
      return(train_indices)
    })
    
    # Combine indices from all strata
    train_indices <- unlist(indices)
    
  } else {
    # Simple random sampling
    train_size <- floor(prop * nrow(data))
    train_indices <- sample(1:nrow(data), train_size)
  }
  
  # Create training and validation sets
  train_data <- data[train_indices, ]
  validation_data <- data[-train_indices, ]
  
  message("Data split into ", nrow(train_data), " training samples and ", 
          nrow(validation_data), " validation samples")
  
  return(list(
    train = train_data,
    validation = validation_data
  ))
}

#' Create cross-validation folds
#'
#' @param data The input data frame
#' @param k Number of folds
#' @param stratify_var Optional variable for stratified sampling
#' @param seed Random seed for reproducibility
#' @return A list of indices for each fold
create_folds <- function(data, 
                         k = 5, 
                         stratify_var = NULL, 
                         seed = 123) {
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Initialize list to store fold indices
  folds <- vector("list", k)
  
  # If stratification variable is provided
  if (!is.null(stratify_var) && stratify_var %in% names(data)) {
    # Get stratification variable
    strata <- data[[stratify_var]]
    
    # For each unique value in the stratification variable
    for (s in unique(strata)) {
      # Get indices for this stratum
      stratum_indices <- which(strata == s)
      
      # Shuffle indices
      shuffled_indices <- sample(stratum_indices)
      
      # Assign to folds
      fold_size <- floor(length(shuffled_indices) / k)
      remainder <- length(shuffled_indices) %% k
      
      start_idx <- 1
      for (i in 1:k) {
        # Calculate end index for this fold
        # Add one extra if there's remainder and we haven't used it all
        extra <- if (i <= remainder) 1 else 0
        end_idx <- start_idx + fold_size + extra - 1
        
        # Assign indices to fold
        fold_indices <- shuffled_indices[start_idx:end_idx]
        folds[[i]] <- c(folds[[i]], fold_indices)
        
        # Update start index for next fold
        start_idx <- end_idx + 1
      }
    }
  } else {
    # Simple random assignment to folds
    indices <- sample(1:nrow(data))
    fold_size <- floor(nrow(data) / k)
    remainder <- nrow(data) %% k
    
    start_idx <- 1
    for (i in 1:k) {
      # Calculate end index for this fold
      # Add one extra if there's remainder and we haven't used it all
      extra <- if (i <= remainder) 1 else 0
      end_idx <- start_idx + fold_size + extra - 1
      
      # Assign indices to fold
      folds[[i]] <- indices[start_idx:end_idx]
      
      # Update start index for next fold
      start_idx <- end_idx + 1
    }
  }
  
  message("Created ", k, " cross-validation folds")
  
  return(folds)
}

#' Bootstrap sample for confidence intervals
#'
#' @param data The input data frame
#' @param n_boot Number of bootstrap samples
#' @param seed Random seed for reproducibility
#' @return A list of bootstrap sample indices
bootstrap_sample <- function(data, 
                             n_boot = 1000, 
                             seed = 123) {
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Create bootstrap samples
  boot_samples <- lapply(1:n_boot, function(i) {
    sample(1:nrow(data), nrow(data), replace = TRUE)
  })
  
  message("Created ", n_boot, " bootstrap samples")
  
  return(boot_samples)
}