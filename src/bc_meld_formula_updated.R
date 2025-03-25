# ============================================================================
# BC-MELD FORMULA FUNCTIONS
# ============================================================================
# Functions for implementing and validating BC-MELD formulas
# ============================================================================

#' Calculate the original BC-MELD score
#'
#' @param data The input data frame
#' @param meld_var Name of the MELD score variable
#' @param muscle_var Name of the muscle area variable
#' @param vat_var Name of the visceral adipose tissue variable
#' @param sat_var Name of the subcutaneous adipose tissue variable
#' @param tat_var Name of the total adipose tissue variable
#' @return A data frame with the original BC-MELD score added
calculate_bc_meld_original <- function(data,
                                      meld_var = "lab_meld",
                                      muscle_var = "muscle",
                                      vat_var = "vat",
                                      sat_var = "sat",
                                      tat_var = "tat") {
  
  # Ensure all variables exist
  required_vars <- c(meld_var, muscle_var, vat_var, sat_var, tat_var)
  missing_vars <- required_vars[!required_vars %in% names(data)]
  
  if (length(missing_vars) > 0) {
    stop("Missing variables: ", paste(missing_vars, collapse = ", "))
  }
  
  # Create a copy of the data
  result_data <- data
  
  # Calculate BC-MELD score
  message("Calculating original BC-MELD score")
  result_data$bc_meld_original <- result_data[[meld_var]] + 
                                 (0.1 * result_data[[muscle_var]]) - 
                                 (0.05 * result_data[[vat_var]]) + 
                                 (0.15 * (1 - (result_data[[sat_var]] / result_data[[tat_var]])))
  
  # Handle potential division by zero or NA values
  result_data$bc_meld_original[is.na(result_data$bc_meld_original) | 
                              is.infinite(result_data$bc_meld_original)] <- result_data[[meld_var]]
  
  message("BC-MELD score calculated for ", sum(!is.na(result_data$bc_meld_original)), " patients")
  
  return(result_data)
}

#' Derive coefficients for an improved BC-MELD formula
#'
#' @param data The input data frame
#' @param time_var Name of the time variable
#' @param event_var Name of the event variable
#' @param meld_var Name of the MELD score variable
#' @param bca_vars Vector of body composition variable names
#' @param alpha Elastic net mixing parameter (0=ridge, 1=lasso)
#' @param nfolds Number of cross-validation folds
#' @return A named vector of coefficients
derive_bc_meld_coefficients <- function(data,
                                       time_var = "waitlist_time_days",
                                       event_var = "status",
                                       meld_var = "lab_meld",
                                       bca_vars = c("muscle", "sat", "vat", "imat", "eat", "pat", "tat"),
                                       alpha = 0.5,
                                       nfolds = 10) {
  
  # Ensure all variables exist
  required_vars <- c(time_var, event_var, meld_var)
  missing_vars <- required_vars[!required_vars %in% names(data)]
  
  if (length(missing_vars) > 0) {
    stop("Missing required variables: ", paste(missing_vars, collapse = ", "))
  }
  
  # Filter BCA variables to those that exist in the data
  bca_vars <- bca_vars[bca_vars %in% names(data)]
  
  if (length(bca_vars) == 0) {
    stop("None of the specified BCA variables exist in the data")
  }
  
  message("Deriving coefficients for improved BC-MELD formula")
  message("Using variables: ", paste(c(meld_var, bca_vars), collapse = ", "))
  
  # Create model matrix
  x <- model.matrix(~ ., data = data[, c(meld_var, bca_vars)])[, -1]  # Remove intercept
  
  # Create survival object
  y <- Surv(data[[time_var]], data[[event_var]])
  
  # Fit elastic net model with cross-validation
  set.seed(123)  # For reproducibility
  cv_fit <- cv.glmnet(
    x = x,
    y = y,
    family = "cox",
    alpha = alpha,
    nfolds = nfolds
  )
  
  # Get coefficients at optimal lambda
  coefs <- coef(cv_fit, s = "lambda.min")
  
  # Convert to named vector
  coef_vector <- as.vector(coefs)
  names(coef_vector) <- rownames(coefs)
  
  # Print coefficients
  message("Derived coefficients:")
  for (var in names(coef_vector)) {
    message("  ", var, ": ", sprintf("%.4f", coef_vector[var]))
  }
  
  return(coef_vector)
}

#' Calculate the improved BC-MELD score
#'
#' @param data The input data frame
#' @param coefficients Named vector of coefficients from derive_bc_meld_coefficients
#' @param meld_var Name of the MELD score variable
#' @return A data frame with the improved BC-MELD score added
calculate_bc_meld_improved <- function(data,
                                      coefficients,
                                      meld_var = "lab_meld") {
  
  # Ensure MELD variable exists
  if (!meld_var %in% names(data)) {
    stop("MELD variable not found in data")
  }
  
  # Create a copy of the data
  result_data <- data
  
  # Initialize improved BC-MELD with base MELD score
  result_data$bc_meld_improved <- result_data[[meld_var]]
  
  # Add contribution from each coefficient
  for (var in names(coefficients)) {
    if (var %in% names(result_data)) {
      result_data$bc_meld_improved <- result_data$bc_meld_improved + 
                                     (coefficients[var] * result_data[[var]])
    } else {
      warning("Variable ", var, " not found in data, skipping")
    }
  }
  
  message("Improved BC-MELD score calculated for ", sum(!is.na(result_data$bc_meld_improved)), " patients")
  
  return(result_data)
}

#' Compare the performance of different MELD formulas
#'
#' @param data The input data frame
#' @param formulas Vector of formula variable names to compare
#' @param time_var Name of the time variable
#' @param event_var Name of the event variable
#' @param times Vector of time points for evaluation
#' @return A data frame of performance metrics for each formula
compare_meld_formulas <- function(data,
                                 formulas = c("lab_meld", "bc_meld_original", "bc_meld_improved"),
                                 time_var = "waitlist_time_days",
                                 event_var = "status",
                                 times = c(90, 180, 365)) {
  
  # Ensure all formulas exist in the data
  missing_formulas <- formulas[!formulas %in% names(data)]
  
  if (length(missing_formulas) > 0) {
    stop("Missing formula variables: ", paste(missing_formulas, collapse = ", "))
  }
  
  # Ensure time and event variables exist
  if (!time_var %in% names(data) || !event_var %in% names(data)) {
    stop("Time or event variable not found in data")
  }
  
  # Create survival object
  surv_obj <- Surv(data[[time_var]], data[[event_var]])
  
  # Initialize results data frame
  results <- data.frame(
    Formula = character(),
    Metric = character(),
    Value = numeric(),
    Lower = numeric(),
    Upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Calculate metrics for each formula
  for (formula in formulas) {
    message("Evaluating formula: ", formula)
    
    # C-index (Harrell's concordance) - Verwende die aktuelle concordance-Funktion statt survConcordance
    tryCatch({
      # Verwende die neuere concordance-Funktion
      cindex <- concordance(surv_obj ~ data[[formula]])
      
      # Add C-index to results
      results <- rbind(results, data.frame(
        Formula = formula,
        Metric = "C-index",
        Value = cindex$concordance,
        Lower = cindex$concordance - 1.96 * sqrt(cindex$var),
        Upper = cindex$concordance + 1.96 * sqrt(cindex$var),
        stringsAsFactors = FALSE
      ))
    }, error = function(e) {
      # Fallback für den Fall eines Fehlers
      warning("Fehler bei der Berechnung des C-Index für ", formula, ": ", e$message)
      results <- rbind(results, data.frame(
        Formula = formula,
        Metric = "C-index",
        Value = NA,
        Lower = NA,
        Upper = NA,
        stringsAsFactors = FALSE
      ))
    })
    
    # Calculate Brier score for each time point
    for (t in times) {
      # Fit Cox model
      cox_fit <- coxph(surv_obj ~ data[[formula]])
      
      # Calculate Brier score
      if (requireNamespace("pec", quietly = TRUE)) {
        brier <- try({
          pec::pec(
            object = cox_fit,
            formula = surv_obj ~ 1,
            data = data,
            times = t,
            reference = FALSE
          )$AppErr$coxph
        }, silent = TRUE)
        
        if (!inherits(brier, "try-error")) {
          # Add Brier score to results
          results <- rbind(results, data.frame(
            Formula = formula,
            Metric = paste0("Brier score (", t, " days)"),
            Value = brier,
            Lower = NA,  # No confidence intervals for Brier score
            Upper = NA,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Calculate R-squared
    if (requireNamespace("rms", quietly = TRUE)) {
      tryCatch({
        # Fit Cox model using rms
        cox_fit_rms <- rms::cph(surv_obj ~ data[[formula]])
        
        # Verwende die korrekte Funktion für R²
        # In neueren rms-Versionen wird validate() statt rsquared() verwendet
        val <- rms::validate(cox_fit_rms, method="boot", B=50)
        rsq <- val["Dxy", "index.corrected"] / 2 + 0.5  # Approximation von R²
        
        # Add R-squared to results
        results <- rbind(results, data.frame(
          Formula = formula,
          Metric = "R-squared",
          Value = rsq,
          Lower = NA,  # No confidence intervals for R-squared
          Upper = NA,
          stringsAsFactors = FALSE
        ))
      }, error = function(e) {
        warning("Fehler bei der Berechnung des R-squared für ", formula, ": ", e$message)
        # Alternative: Berechne keine R²-Statistik
      })
    }
  }
  
  return(results)
}

#' Create a formatted table comparing MELD formulas
#'
#' @param comparison_results Results from compare_meld_formulas
#' @param caption Table caption
#' @param digits Number of decimal places
#' @return A formatted table object
create_meld_comparison_table <- function(comparison_results,
                                        caption = "Comparison of MELD Formulas",
                                        digits = 3) {
  
  # Reshape data for table
  table_data <- comparison_results %>%
    pivot_wider(
      id_cols = "Metric",
      names_from = "Formula",
      values_from = "Value"
    )
  
  # Create a flextable
  ft <- flextable(table_data)
  
  # Add header
  ft <- set_header_labels(ft, 
                          Metric = "Metric")
  
  # Add caption
  ft <- set_caption(ft, caption = caption)
  
  # Format numbers
  for (col in names(table_data)[-1]) {
    ft <- colformat_double(ft, j = col, digits = digits)
  }
  
  # Format
  ft <- theme_vanilla(ft)
  ft <- autofit(ft)
  
  return(ft)
}

#' Plot comparison of MELD formulas
#'
#' @param comparison_results Results from compare_meld_formulas
#' @param metric Metric to plot
#' @param title Plot title
#' @param color_palette Color palette for the plot
#' @return A ggplot object
plot_meld_comparison <- function(comparison_results,
                                metric = "C-index",
                                title = NULL,
                                color_palette = "jco") {
  
  # Filter data for the specified metric
  plot_data <- comparison_results[comparison_results$Metric == metric, ]
  
  if (nrow(plot_data) == 0) {
    stop("No data found for metric: ", metric)
  }
  
  # Set default title if not provided
  if (is.null(title)) {
    title <- paste("Comparison of MELD Formulas -", metric)
  }
  
  # Create plot
  p <- ggplot(plot_data, aes(x = reorder(Formula, Value), y = Value, fill = Formula)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2) +
    labs(
      title = title,
      x = NULL,
      y = metric
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Add value labels
  p <- p + geom_text(aes(label = sprintf("%.3f", Value)), vjust = -0.5)
  
  return(p)
}

#' Create a nomogram for the BC-MELD formula
#'
#' @param data The input data frame
#' @param coefficients Named vector of coefficients
#' @param time_var Name of the time variable
#' @param event_var Name of the event variable
#' @param meld_var Name of the MELD score variable
#' @param times Vector of time points for prediction
#' @return A nomogram object
create_bc_meld_nomogram <- function(data,
                                   coefficients,
                                   time_var = "waitlist_time_days",
                                   event_var = "status",
                                   meld_var = "lab_meld",
                                   times = c(90, 180, 365)) {
  
  # Ensure required packages are available
  if (!requireNamespace("rms", quietly = TRUE)) {
    stop("rms package is required for creating nomograms")
  }
  
  # Ensure all variables exist
  vars <- c(time_var, event_var, meld_var, names(coefficients))
  missing_vars <- vars[!vars %in% names(data)]
  
  if (length(missing_vars) > 0) {
    stop("Missing variables: ", paste(missing_vars, collapse = ", "))
  }
  
  message("Creating BC-MELD nomogram")
  
  # Set up rms
  dd <- rms::datadist(data)
  options(datadist = "dd")
  
  # Create formula
  formula_str <- paste0("Surv(", time_var, ", ", event_var, ") ~ ", 
                       paste(c(meld_var, names(coefficients)), collapse = " + "))
  
  # Fit Cox model
  cox_fit <- rms::cph(as.formula(formula_str), data = data, x = TRUE, y = TRUE, surv = TRUE)
  
  # Create nomogram
  nom <- rms::nomogram(
    cox_fit,
    fun = function(x) 1 - rms::survest(cox_fit, times = times, newdata = x)$surv,
    funlabel = paste("Risk at", paste(times, "days", collapse = ", ")),
    lp = FALSE
  )
  
  return(nom)
}

#' Plot survival curves stratified by BC-MELD score
#'
#' @param data The input data frame
#' @param formula_var Name of the BC-MELD formula variable
#' @param time_var Name of the time variable
#' @param event_var Name of the event variable
#' @param n_groups Number of groups to create
#' @param title Plot title
#' @return A ggsurvplot object
plot_survival_by_bc_meld <- function(data,
                                    formula_var,
                                    time_var = "waitlist_time_days",
                                    event_var = "status",
                                    n_groups = 3,
                                    title = NULL) {
  
  # Ensure variables exist
  if (!formula_var %in% names(data) || !time_var %in% names(data) || !event_var %in% names(data)) {
    stop("Required variables not found in data")
  }
  
  # Create groups based on BC-MELD score
  quantiles <- quantile(data[[formula_var]], probs = seq(0, 1, length.out = n_groups + 1), na.rm = TRUE)
  
  # Create group variable
  data$bc_meld_group <- cut(
    data[[formula_var]],
    breaks = quantiles,
    labels = paste("Group", 1:n_groups),
    include.lowest = TRUE
  )
  
  # Set default title if not provided
  if (is.null(title)) {
    title <- paste("Survival by", formula_var, "Groups")
  }
  
  # Fit Kaplan-Meier model
  km_fit <- survfit(as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ bc_meld_group")), 
                   data = data)
  
  # Create survival plot
  km_plot <- ggsurvplot(
    fit = km_fit,
    data = data,
    risk.table = TRUE,
    pval = TRUE,
    conf.int = TRUE,
    palette = "jco",
    xlab = "Time (days)",
    ylab = "Survival Probability",
    title = title,
    legend.title = "BC-MELD Group",
    risk.table.height = 0.25,
    risk.table.y.text = FALSE,
    tables.theme = theme_cleantable(),
    ggtheme = theme_minimal()
  )
  
  return(km_plot)
}
