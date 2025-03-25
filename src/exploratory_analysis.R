# ============================================================================
# EXPLORATORY DATA ANALYSIS FUNCTIONS
# ============================================================================
# Functions for descriptive statistics and visualizations
# ============================================================================

#' Generate descriptive statistics for the dataset
#'
#' @param data The input data frame
#' @param group_var Optional grouping variable for stratified statistics
#' @param vars Vector of variable names to include (NULL for all)
#' @param include_pvalues Whether to include p-values for group comparisons
#' @return A data frame of descriptive statistics
generate_descriptive_stats <- function(data,
                                       group_var = NULL,
                                       vars = NULL,
                                       include_pvalues = FALSE) {
  
  # If no variables specified, use all numeric and factor variables
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, function(x) is.numeric(x) || is.factor(x) || is.character(x))]
    # Exclude ID variables and other non-informative variables
    exclude_pattern <- "^(etnr_id|id|ID|Id)$"
    vars <- vars[!grepl(exclude_pattern, vars)]
  }
  
  # Ensure all variables exist in the data
  vars <- vars[vars %in% names(data)]
  
  if (length(vars) == 0) {
    stop("No valid variables found for descriptive statistics")
  }
  
  # Initialize results data frame
  stats_df <- data.frame(
    Variable = character(),
    Category = character(),
    Overall = character(),
    stringsAsFactors = FALSE
  )
  
  # Add group columns if group_var is provided
  if (!is.null(group_var) && group_var %in% names(data)) {
    group_levels <- unique(data[[group_var]])
    for (level in group_levels) {
      stats_df[[as.character(level)]] <- character()
    }
    
    if (include_pvalues) {
      stats_df$p_value <- character()
    }
  }
  
  # Calculate statistics for each variable
  for (var in vars) {
    # Skip if all values are NA
    if (all(is.na(data[[var]]))) {
      next
    }
    
    if (is.numeric(data[[var]])) {
      # Numeric variable
      
      # Overall statistics
      mean_val <- mean(data[[var]], na.rm = TRUE)
      sd_val <- sd(data[[var]], na.rm = TRUE)
      median_val <- median(data[[var]], na.rm = TRUE)
      q1_val <- quantile(data[[var]], 0.25, na.rm = TRUE)
      q3_val <- quantile(data[[var]], 0.75, na.rm = TRUE)
      min_val <- min(data[[var]], na.rm = TRUE)
      max_val <- max(data[[var]], na.rm = TRUE)
      n_missing <- sum(is.na(data[[var]]))
      
      # Add mean ± SD
      stats_df <- rbind(stats_df, data.frame(
        Variable = var,
        Category = "Mean ± SD",
        Overall = sprintf("%.1f ± %.1f", mean_val, sd_val),
        stringsAsFactors = FALSE
      ))
      
      # Add median [IQR]
      stats_df <- rbind(stats_df, data.frame(
        Variable = var,
        Category = "Median [IQR]",
        Overall = sprintf("%.1f [%.1f-%.1f]", median_val, q1_val, q3_val),
        stringsAsFactors = FALSE
      ))
      
      # Add range
      stats_df <- rbind(stats_df, data.frame(
        Variable = var,
        Category = "Range",
        Overall = sprintf("%.1f-%.1f", min_val, max_val),
        stringsAsFactors = FALSE
      ))
      
      # Add missing values
      if (n_missing > 0) {
        stats_df <- rbind(stats_df, data.frame(
          Variable = var,
          Category = "Missing",
          Overall = sprintf("%d (%.1f%%)", n_missing, n_missing/nrow(data)*100),
          stringsAsFactors = FALSE
        ))
      }
      
      # Group statistics if group_var is provided
      if (!is.null(group_var) && group_var %in% names(data)) {
        for (level in group_levels) {
          group_data <- data[data[[group_var]] == level, ]
          
          # Group statistics
          group_mean <- mean(group_data[[var]], na.rm = TRUE)
          group_sd <- sd(group_data[[var]], na.rm = TRUE)
          group_median <- median(group_data[[var]], na.rm = TRUE)
          group_q1 <- quantile(group_data[[var]], 0.25, na.rm = TRUE)
          group_q3 <- quantile(group_data[[var]], 0.75, na.rm = TRUE)
          group_min <- min(group_data[[var]], na.rm = TRUE)
          group_max <- max(group_data[[var]], na.rm = TRUE)
          group_missing <- sum(is.na(group_data[[var]]))
          
          # Update mean ± SD row
          stats_df[stats_df$Variable == var & stats_df$Category == "Mean ± SD", as.character(level)] <- 
            sprintf("%.1f ± %.1f", group_mean, group_sd)
          
          # Update median [IQR] row
          stats_df[stats_df$Variable == var & stats_df$Category == "Median [IQR]", as.character(level)] <- 
            sprintf("%.1f [%.1f-%.1f]", group_median, group_q1, group_q3)
          
          # Update range row
          stats_df[stats_df$Variable == var & stats_df$Category == "Range", as.character(level)] <- 
            sprintf("%.1f-%.1f", group_min, group_max)
          
          # Update missing row if it exists
          if (n_missing > 0) {
            stats_df[stats_df$Variable == var & stats_df$Category == "Missing", as.character(level)] <- 
              sprintf("%d (%.1f%%)", group_missing, group_missing/nrow(group_data)*100)
          }
        }
        
        # Add p-value if requested
        if (include_pvalues) {
          # Use t-test or Wilcoxon test depending on normality
          if (length(group_levels) == 2) {
            # For two groups
            g1 <- data[data[[group_var]] == group_levels[1], var]
            g2 <- data[data[[group_var]] == group_levels[2], var]
            
            # Check normality (simplified)
            is_normal <- shapiro.test(data[[var]])$p.value > 0.05
            
            if (is_normal) {
              # Use t-test
              p_val <- t.test(g1, g2)$p.value
            } else {
              # Use Wilcoxon test
              p_val <- wilcox.test(g1, g2)$p.value
            }
          } else {
            # For more than two groups, use ANOVA or Kruskal-Wallis
            is_normal <- shapiro.test(data[[var]])$p.value > 0.05
            
            if (is_normal) {
              # Use ANOVA
              p_val <- summary(aov(as.formula(paste(var, "~", group_var)), data = data))[[1]][["Pr(>F)"]][1]
            } else {
              # Use Kruskal-Wallis
              p_val <- kruskal.test(as.formula(paste(var, "~", group_var)), data = data)$p.value
            }
          }
          
          # Format p-value
          p_formatted <- if (p_val < 0.001) "<0.001" else sprintf("%.3f", p_val)
          
          # Update p-value column
          stats_df[stats_df$Variable == var & stats_df$Category == "Mean ± SD", "p_value"] <- p_formatted
          stats_df[stats_df$Variable == var & stats_df$Category == "Median [IQR]", "p_value"] <- ""
          stats_df[stats_df$Variable == var & stats_df$Category == "Range", "p_value"] <- ""
          if (n_missing > 0) {
            stats_df[stats_df$Variable == var & stats_df$Category == "Missing", "p_value"] <- ""
          }
        }
      }
    } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
      # Categorical variable
      
      # Convert to factor if character
      if (is.character(data[[var]])) {
        data[[var]] <- as.factor(data[[var]])
      }
      
      # Overall statistics
      table_var <- table(data[[var]], useNA = "ifany")
      n_missing <- sum(is.na(data[[var]]))
      
      # Add each level
      for (level in levels(data[[var]])) {
        count <- table_var[level]
        percent <- count / nrow(data) * 100
        
        stats_df <- rbind(stats_df, data.frame(
          Variable = var,
          Category = level,
          Overall = sprintf("%d (%.1f%%)", count, percent),
          stringsAsFactors = FALSE
        ))
      }
      
      # Add missing values
      if (n_missing > 0) {
        stats_df <- rbind(stats_df, data.frame(
          Variable = var,
          Category = "Missing",
          Overall = sprintf("%d (%.1f%%)", n_missing, n_missing/nrow(data)*100),
          stringsAsFactors = FALSE
        ))
      }
      
      # Group statistics if group_var is provided
      if (!is.null(group_var) && group_var %in% names(data)) {
        for (level in group_levels) {
          group_data <- data[data[[group_var]] == level, ]
          
          # Group statistics
          group_table <- table(group_data[[var]], useNA = "ifany")
          group_missing <- sum(is.na(group_data[[var]]))
          
          # Update each level row
          for (var_level in levels(data[[var]])) {
            group_count <- if (var_level %in% names(group_table)) group_table[var_level] else 0
            group_percent <- group_count / nrow(group_data) * 100
            
            stats_df[stats_df$Variable == var & stats_df$Category == var_level, as.character(level)] <- 
              sprintf("%d (%.1f%%)", group_count, group_percent)
          }
          
          # Update missing row if it exists
          if (n_missing > 0) {
            stats_df[stats_df$Variable == var & stats_df$Category == "Missing", as.character(level)] <- 
              sprintf("%d (%.1f%%)", group_missing, group_missing/nrow(group_data)*100)
          }
        }
        
        # Add p-value if requested
        if (include_pvalues) {
          # Use chi-square test
          formula <- as.formula(paste("~", var, "+", group_var))
          chi_test <- chisq.test(table(data[[var]], data[[group_var]]))
          p_val <- chi_test$p.value
          
          # Format p-value
          p_formatted <- if (p_val < 0.001) "<0.001" else sprintf("%.3f", p_val)
          
          # Update p-value column for first level only
          stats_df[stats_df$Variable == var & stats_df$Category == levels(data[[var]])[1], "p_value"] <- p_formatted
          
          # Clear p-value for other levels
          for (i in 2:length(levels(data[[var]]))) {
            stats_df[stats_df$Variable == var & stats_df$Category == levels(data[[var]])[i], "p_value"] <- ""
          }
          
          # Clear p-value for missing row if it exists
          if (n_missing > 0) {
            stats_df[stats_df$Variable == var & stats_df$Category == "Missing", "p_value"] <- ""
          }
        }
      }
    }
  }
  
  return(stats_df)
}

#' Create a formatted table of descriptive statistics
#'
#' @param stats_df Data frame of descriptive statistics
#' @param caption Table caption
#' @param digits Number of decimal places
#' @return A formatted table object
create_descriptive_table <- function(stats_df, 
                                     caption = "Descriptive Statistics", 
                                     digits = 1) {
  
  # Create a flextable
  ft <- flextable(stats_df)
  
  # Add header
  ft <- set_header_labels(ft, 
                          Variable = "Variable",
                          Category = "Category",
                          Overall = "Overall")
  
  # Add caption
  ft <- set_caption(ft, caption = caption)
  
  # Format
  ft <- theme_vanilla(ft)
  ft <- autofit(ft)
  
  return(ft)
}

#' Plot distributions of variables
#'
#' @param data The input data frame
#' @param vars Vector of variable names to plot
#' @param plot_type Type of plot ("histogram" or "density")
#' @param facet_var Optional variable for faceting
#' @param bins Number of bins for histograms
#' @return A ggplot object
plot_variable_distributions <- function(data,
                                        vars,
                                        plot_type = "histogram",
                                        facet_var = NULL,
                                        bins = 30) {
  
  # Ensure all variables exist in the data
  vars <- vars[vars %in% names(data)]
  
  if (length(vars) == 0) {
    stop("No valid variables found for plotting")
  }
  
  # Convert data to long format
  plot_data <- data %>%
    select(all_of(c(vars, facet_var))) %>%
    pivot_longer(cols = all_of(vars),
                 names_to = "Variable",
                 values_to = "Value")
  
  # Create plot
  p <- ggplot(plot_data, aes(x = Value))
  
  if (plot_type == "histogram") {
    p <- p + geom_histogram(aes(fill = Variable), bins = bins, alpha = 0.7)
  } else if (plot_type == "density") {
    p <- p + geom_density(aes(fill = Variable), alpha = 0.5)
  } else {
    stop("Invalid plot_type. Use 'histogram' or 'density'")
  }
  
  # Add facets if facet_var is provided
  if (!is.null(facet_var) && facet_var %in% names(data)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  } else {
    p <- p + facet_wrap(~ Variable, scales = "free")
  }
  
  # Add theme and labels
  p <- p + 
    labs(
      title = "Distribution of Variables",
      x = NULL,
      y = if (plot_type == "histogram") "Count" else "Density"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      strip.text = element_text(face = "bold")
    )
  
  return(p)
}

#' Create a correlation matrix heatmap
#'
#' @param data The input data frame
#' @param vars Vector of variable names to include (NULL for all numeric)
#' @param method Correlation method ("pearson", "spearman", or "kendall")
#' @param cluster Whether to cluster variables by similarity
#' @return A ggplot object
plot_correlation_matrix <- function(data,
                                    vars = NULL,
                                    method = "spearman",
                                    cluster = TRUE) {
  
  # If no variables specified, use all numeric variables
  if (is.null(vars)) {
    vars <- names(data)[sapply(data, is.numeric)]
  }
  
  # Ensure all variables exist in the data and are numeric
  vars <- vars[vars %in% names(data)]
  vars <- vars[sapply(data[vars], is.numeric)]
  
  if (length(vars) < 2) {
    stop("At least two numeric variables are required for correlation matrix")
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(data[vars], method = method, use = "pairwise.complete.obs")
  
  # Cluster if requested
  if (cluster) {
    hc <- hclust(as.dist(1 - abs(cor_matrix)))
    cor_matrix <- cor_matrix[hc$order, hc$order]
  }
  
  # Convert to long format for ggplot
  cor_data <- as.data.frame(as.table(cor_matrix))
  names(cor_data) <- c("Var1", "Var2", "Correlation")
  
  # Create plot
  p <- ggplot(cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1)
    ) +
    geom_text(aes(label = sprintf("%.2f", Correlation)), 
              color = ifelse(abs(cor_data$Correlation) > 0.7, "white", "black"),
              size = 3) +
    labs(
      title = paste("Correlation Matrix (", method, ")"),
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  return(p)
}

#' Create a bar chart of waitlist numbers by year
#'
#' @param data The input data frame
#' @param year_var Name of the year variable
#' @param fill_var Optional variable for fill color
#' @param show_counts Whether to display count labels on bars
#' @return A ggplot object
plot_waitlist_by_year <- function(data,
                                  year_var = "wl_year",
                                  fill_var = NULL,
                                  show_counts = TRUE) {
  
  # Ensure year variable exists
  if (!year_var %in% names(data)) {
    stop("Year variable not found in data")
  }
  
  # Create count data
  if (is.null(fill_var)) {
    # Simple counts by year
    count_data <- data %>%
      count(!!sym(year_var)) %>%
      mutate(!!sym(year_var) := as.factor(!!sym(year_var)))
    
    # Create plot
    p <- ggplot(count_data, aes(x = !!sym(year_var), y = n)) +
      geom_bar(stat = "identity", fill = "steelblue", width = 0.7)
    
  } else {
    # Counts by year and fill variable
    count_data <- data %>%
      count(!!sym(year_var), !!sym(fill_var)) %>%
      mutate(!!sym(year_var) := as.factor(!!sym(year_var)))
    
    # Create plot
    p <- ggplot(count_data, aes(x = !!sym(year_var), y = n, fill = !!sym(fill_var))) +
      geom_bar(stat = "identity", width = 0.7, position = "stack")
  }
  
  # Add count labels if requested
  if (show_counts) {
    if (is.null(fill_var)) {
      p <- p + geom_text(aes(label = n), vjust = -0.5)
    } else {
      p <- p + geom_text(aes(label = n), position = position_stack(vjust = 0.5), 
                         color = "white", size = 3)
    }
  }
  
  # Add theme and labels
  p <- p + 
    labs(
      title = "Number of Patients on Waitlist by Year",
      x = "Year",
      y = "Number of Patients",
      fill = if (!is.null(fill_var)) fill_var else NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = if (is.null(fill_var)) "none" else "right"
    )
  
  return(p)
}

#' Create a boxplot of variables by group
#'
#' @param data The input data frame
#' @param var Variable to plot
#' @param group_var Grouping variable
#' @param add_points Whether to add individual points
#' @param add_pvalue Whether to add p-value
#' @return A ggplot object
plot_boxplot_by_group <- function(data,
                                  var,
                                  group_var,
                                  add_points = TRUE,
                                  add_pvalue = TRUE) {
  
  # Ensure variables exist
  if (!var %in% names(data) || !group_var %in% names(data)) {
    stop("Variables not found in data")
  }
  
  # Create plot
  p <- ggplot(data, aes(x = !!sym(group_var), y = !!sym(var), fill = !!sym(group_var))) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA)
  
  # Add points if requested
  if (add_points) {
    p <- p + geom_jitter(width = 0.2, alpha = 0.5, size = 1)
  }
  
  # Add p-value if requested
  if (add_pvalue) {
    # Calculate p-value
    if (length(unique(data[[group_var]])) == 2) {
      # For two groups, use t-test or Wilcoxon test
      is_normal <- shapiro.test(data[[var]])$p.value > 0.05
      
      if (is_normal) {
        # Use t-test
        test_result <- t.test(as.formula(paste(var, "~", group_var)), data = data)
      } else {
        # Use Wilcoxon test
        test_result <- wilcox.test(as.formula(paste(var, "~", group_var)), data = data)
      }
      
      p_val <- test_result$p.value
    } else {
      # For more than two groups, use ANOVA or Kruskal-Wallis
      is_normal <- shapiro.test(data[[var]])$p.value > 0.05
      
      if (is_normal) {
        # Use ANOVA
        p_val <- summary(aov(as.formula(paste(var, "~", group_var)), data = data))[[1]][["Pr(>F)"]][1]
      } else {
        # Use Kruskal-Wallis
        p_val <- kruskal.test(as.formula(paste(var, "~", group_var)), data = data)$p.value
      }
    }
    
    # Format p-value
    p_formatted <- if (p_val < 0.001) "p < 0.001" else paste("p =", sprintf("%.3f", p_val))
    
    # Add p-value to plot
    p <- p + 
      annotate("text", x = Inf, y = Inf, label = p_formatted, 
               hjust = 1.1, vjust = 1.5, size = 4)
  }
  
  # Add theme and labels
  p <- p + 
    labs(
      title = paste(var, "by", group_var),
      x = group_var,
      y = var
    ) +
    theme_minimal() +
    theme(
      legend.position = "none"
    )
  
  return(p)
}

#' Create a scatter plot with regression line
#'
#' @param data The input data frame
#' @param x_var X-axis variable
#' @param y_var Y-axis variable
#' @param color_var Optional variable for color
#' @param add_regression Whether to add regression line
#' @param add_correlation Whether to add correlation coefficient
#' @return A ggplot object
plot_scatter <- function(data,
                         x_var,
                         y_var,
                         color_var = NULL,
                         add_regression = TRUE,
                         add_correlation = TRUE) {
  
  # Ensure variables exist
  if (!x_var %in% names(data) || !y_var %in% names(data)) {
    stop("Variables not found in data")
  }
  
  # Create plot
  if (is.null(color_var)) {
    p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
      geom_point(alpha = 0.7)
  } else {
    p <- ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var), color = !!sym(color_var))) +
      geom_point(alpha = 0.7)
  }
  
  # Add regression line if requested
  if (add_regression) {
    if (is.null(color_var)) {
      p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "blue")
    } else {
      p <- p + geom_smooth(method = "lm", formula = y ~ x, se = TRUE, aes(color = !!sym(color_var)))
    }
  }
  
  # Add correlation if requested
  if (add_correlation) {
    # Calculate correlation
    cor_val <- cor(data[[x_var]], data[[y_var]], use = "pairwise.complete.obs")
    cor_formatted <- sprintf("r = %.2f", cor_val)
    
    # Add to plot
    p <- p + 
      annotate("text", x = Inf, y = Inf, label = cor_formatted, 
               hjust = 1.1, vjust = 1.5, size = 4)
  }
  
  # Add theme and labels
  p <- p + 
    labs(
      title = paste(y_var, "vs", x_var),
      x = x_var,
      y = y_var,
      color = if (!is.null(color_var)) color_var else NULL
    ) +
    theme_minimal()
  
  return(p)
}
