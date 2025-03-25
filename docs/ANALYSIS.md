# Analysis Workflow Documentation

This document provides a detailed explanation of the survival analysis workflow implemented in this project.

## Table of Contents

- [Overview](#overview)
- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Kaplan-Meier Survival Analysis](#kaplan-meier-survival-analysis)
- [BC-MELD Formula Implementation](#bc-meld-formula-implementation)
- [Advanced Machine Learning Models](#advanced-machine-learning-models)
- [Model Validation](#model-validation)
- [Performance Metrics](#performance-metrics)
- [Visualization Techniques](#visualization-techniques)
- [Report Generation](#report-generation)

## Overview

The analysis workflow consists of several sequential steps:

1. Data loading and validation
2. Exploratory data analysis
3. Kaplan-Meier survival analysis
4. Implementation and validation of BC-MELD formula
5. Development of advanced machine learning models
6. Model validation and performance comparison
7. Report generation

Each step is implemented in separate R scripts located in the `src` directory.

## Exploratory Data Analysis

### Descriptive Statistics

The exploratory data analysis (EDA) includes:

- Summary statistics for demographic variables
- Distribution of MELD scores
- Distribution of body composition variables
- Correlation analysis between variables
- Temporal trends in waitlist characteristics

```r
# Example code for generating descriptive statistics
summary_stats <- function(data) {
  # Demographic summary
  demographics <- data %>%
    summarize(
      n = n(),
      age_mean = mean(age, na.rm = TRUE),
      age_sd = sd(age, na.rm = TRUE),
      male_percent = mean(sex == "M", na.rm = TRUE) * 100,
      bmi_mean = mean(bmi, na.rm = TRUE),
      bmi_sd = sd(bmi, na.rm = TRUE),
      meld_mean = mean(lab_meld, na.rm = TRUE),
      meld_sd = sd(lab_meld, na.rm = TRUE)
    )
  
  # Return formatted table
  return(demographics)
}
```

### Visualizations

Key visualizations in the EDA include:

- Histograms of continuous variables
- Bar charts of categorical variables
- Box plots of MELD scores by diagnosis groups
- Scatter plots of body composition variables
- Time series plots of waitlist numbers by year

```r
# Example code for waitlist numbers by year visualization
plot_waitlist_by_year <- function(data) {
  yearly_counts <- data %>%
    count(wl_year) %>%
    mutate(wl_year = as.factor(wl_year))
  
  ggplot(yearly_counts, aes(x = wl_year, y = n)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = n), vjust = -0.5) +
    labs(
      title = "Number of Patients on Waitlist by Year",
      x = "Year",
      y = "Number of Patients"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
```

## Kaplan-Meier Survival Analysis

### Overall Survival

The Kaplan-Meier method is used to estimate overall survival:

```r
# Example code for Kaplan-Meier analysis
km_overall <- function(data) {
  # Create survival object
  surv_obj <- Surv(time = data$waitlist_time_days, event = data$status)
  
  # Fit Kaplan-Meier model
  km_fit <- survfit(surv_obj ~ 1, data = data)
  
  # Return fit object
  return(km_fit)
}
```

### Stratified Analysis

Survival is also analyzed stratified by:

- Year of waitlist entry
- Diagnosis groups
- MELD score categories
- Age groups

```r
# Example code for stratified Kaplan-Meier analysis
km_stratified <- function(data, strata_var) {
  # Create formula
  formula <- as.formula(paste("Surv(waitlist_time_days, status) ~", strata_var))
  
  # Fit stratified model
  km_fit <- survfit(formula, data = data)
  
  # Return fit object
  return(km_fit)
}
```

### Visualization of Survival Curves

Publication-quality Kaplan-Meier curves are generated using the `survminer` package:

```r
# Example code for Kaplan-Meier curve visualization
plot_km_curve <- function(km_fit, title, risk_table = TRUE) {
  ggsurvplot(
    km_fit,
    data = NULL,
    risk.table = risk_table,
    pval = TRUE,
    conf.int = TRUE,
    palette = "jco",
    xlab = "Time (days)",
    ylab = "Survival Probability",
    title = title,
    legend.title = "Group",
    risk.table.height = 0.25,
    risk.table.y.text = FALSE,
    tables.theme = theme_cleantable()
  )
}
```

## BC-MELD Formula Implementation

### Original BC-MELD Formula

The original BC-MELD formula is implemented as a baseline:

```r
# Example code for original BC-MELD implementation
calculate_bc_meld_original <- function(data) {
  data %>%
    mutate(
      bc_meld_original = lab_meld + 
                         (0.1 * muscle) - 
                         (0.05 * vat) + 
                         (0.15 * (1 - (sat/tat)))
    )
}
```

### Derivation of Improved Formula

The improved BC-MELD formula is derived using:

1. Cox proportional hazards models to identify significant variables
2. Regularized regression (LASSO/Ridge) to select variables and estimate coefficients
3. Cross-validation to optimize regularization parameters

```r
# Example code for deriving improved BC-MELD formula
derive_bc_meld_improved <- function(data, alpha = 0.5) {
  # Create model matrix
  x <- model.matrix(~ lab_meld + muscle + sat + vat + imat + eat + pat + tat - 1, data = data)
  
  # Create survival object
  y <- Surv(data$waitlist_time_days, data$status)
  
  # Fit elastic net model (alpha=0.5 for elastic net, 0 for ridge, 1 for lasso)
  cv_fit <- cv.glmnet(x, y, family = "cox", alpha = alpha)
  
  # Get coefficients at optimal lambda
  coefs <- coef(cv_fit, s = "lambda.min")
  
  # Return coefficients
  return(coefs)
}
```

### Validation of Formula

The new formula is validated using:

- Cross-validation
- Comparison with original MELD and BC-MELD
- Performance metrics (Brier Score, C-Index, R²)

## Advanced Machine Learning Models

### Accelerated Oblique Random Survival Forests (AORSF)

AORSF models are implemented with hyperparameter tuning:

```r
# Example code for AORSF implementation
fit_aorsf <- function(data, n_trees = 500) {
  # Define formula
  formula <- Surv(waitlist_time_days, status) ~ 
             age + sex + bmi + lab_meld + 
             muscle + sat + vat + imat + eat + pat + tat
  
  # Fit AORSF model
  aorsf_fit <- aorsf(
    formula = formula,
    data = data,
    n_tree = n_trees,
    importance = "permute",
    n_split = 25,
    split_min_events = 5
  )
  
  # Return fitted model
  return(aorsf_fit)
}
```

### H2O Distributed Survival Analysis

H2O distributed computing is used for enhanced performance:

```r
# Example code for H2O implementation
fit_h2o_survival <- function(data) {
  # Initialize H2O
  h2o.init()
  
  # Convert data to H2O frame
  data_h2o <- as.h2o(data)
  
  # Define predictors and response
  predictors <- c("age", "sex", "bmi", "lab_meld", 
                 "muscle", "sat", "vat", "imat", "eat", "pat", "tat")
  response <- "waitlist_time_days"
  
  # Define survival parameters
  surv_params <- list(event_column = "status")
  
  # Train model
  model <- h2o.randomForest(
    x = predictors,
    y = response,
    training_frame = data_h2o,
    ntrees = 500,
    max_depth = 10,
    seed = 1234,
    stopping_rounds = 5,
    stopping_tolerance = 0.001,
    stopping_metric = "concordance",
    distribution = "cox",
    survival_params = surv_params
  )
  
  # Return model
  return(model)
}
```

### Ensemble Models

Ensemble models combine multiple approaches:

```r
# Example code for ensemble model
create_ensemble <- function(predictions_list, weights = NULL) {
  # If weights not provided, use equal weights
  if (is.null(weights)) {
    weights <- rep(1/length(predictions_list), length(predictions_list))
  }
  
  # Combine predictions with weights
  ensemble_pred <- mapply(function(pred, weight) pred * weight, 
                         predictions_list, weights) %>%
    Reduce(f = "+")
  
  return(ensemble_pred)
}
```

### Model Interpretation

SHAP values and variable importance plots are generated:

```r
# Example code for SHAP values calculation
calculate_shap <- function(model, data, n_samples = 100) {
  # Calculate SHAP values
  shap_values <- fastshap::explain(
    model,
    X = data,
    nsim = n_samples,
    pred_wrapper = function(model, newdata) predict(model, newdata)$predictions
  )
  
  return(shap_values)
}
```

## Model Validation

### Cross-Validation

Proper cross-validation is implemented:

```r
# Example code for nested cross-validation
nested_cv <- function(data, k_outer = 5, k_inner = 5) {
  # Create folds for outer CV
  set.seed(123)
  outer_folds <- createFolds(data$status, k = k_outer)
  
  # Initialize results storage
  results <- list()
  
  # Outer loop
  for (i in 1:k_outer) {
    # Split data
    train_data <- data[-outer_folds[[i]], ]
    test_data <- data[outer_folds[[i]], ]
    
    # Inner CV for parameter tuning
    inner_cv <- function(train_data, k = k_inner) {
      # Create folds for inner CV
      inner_folds <- createFolds(train_data$status, k = k)
      
      # Parameter grid
      param_grid <- expand.grid(
        n_trees = c(100, 300, 500),
        max_depth = c(3, 5, 10)
      )
      
      # Initialize performance storage
      cv_results <- matrix(NA, nrow = nrow(param_grid), ncol = k)
      
      # Loop through parameters
      for (j in 1:nrow(param_grid)) {
        # Loop through folds
        for (l in 1:k) {
          # Split data
          cv_train <- train_data[-inner_folds[[l]], ]
          cv_test <- train_data[inner_folds[[l]], ]
          
          # Train model with current parameters
          model <- fit_model(
            cv_train,
            n_trees = param_grid$n_trees[j],
            max_depth = param_grid$max_depth[j]
          )
          
          # Evaluate on test fold
          pred <- predict(model, cv_test)
          cv_results[j, l] <- calculate_cindex(cv_test, pred)
        }
      }
      
      # Find best parameters
      mean_performance <- rowMeans(cv_results)
      best_idx <- which.max(mean_performance)
      best_params <- param_grid[best_idx, ]
      
      return(best_params)
    }
    
    # Get best parameters
    best_params <- inner_cv(train_data)
    
    # Train final model with best parameters
    final_model <- fit_model(
      train_data,
      n_trees = best_params$n_trees,
      max_depth = best_params$max_depth
    )
    
    # Evaluate on test data
    pred <- predict(final_model, test_data)
    performance <- calculate_performance_metrics(test_data, pred)
    
    # Store results
    results[[i]] <- list(
      model = final_model,
      params = best_params,
      performance = performance
    )
  }
  
  return(results)
}
```

### Bootstrap Confidence Intervals

Bootstrap or Monte Carlo methods are used for confidence intervals:

```r
# Example code for bootstrap confidence intervals
bootstrap_ci <- function(data, model_func, n_boot = 1000, alpha = 0.05) {
  # Initialize storage
  boot_results <- numeric(n_boot)
  
  # Bootstrap loop
  for (i in 1:n_boot) {
    # Sample with replacement
    boot_idx <- sample(1:nrow(data), nrow(data), replace = TRUE)
    boot_data <- data[boot_idx, ]
    
    # Fit model
    boot_model <- model_func(boot_data)
    
    # Calculate performance metric
    boot_results[i] <- calculate_performance(boot_model, boot_data)
  }
  
  # Calculate confidence intervals
  ci_lower <- quantile(boot_results, alpha/2)
  ci_upper <- quantile(boot_results, 1 - alpha/2)
  
  return(list(
    mean = mean(boot_results),
    median = median(boot_results),
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}
```

## Performance Metrics

### Brier Score

The Brier Score measures prediction accuracy:

```r
# Example code for Brier Score calculation
calculate_brier_score <- function(data, predictions, times) {
  # Create survival object
  surv_obj <- Surv(data$waitlist_time_days, data$status)
  
  # Calculate Brier score
  brier <- pec::pec(
    object = list(model = predictions),
    formula = surv_obj ~ 1,
    data = data,
    times = times,
    reference = FALSE
  )
  
  # Return Brier score at specified times
  return(brier$AppErr$model)
}
```

### C-Index

The Concordance Index (C-Index) measures discrimination:

```r
# Example code for C-Index calculation
calculate_cindex <- function(data, predictions) {
  # Create survival object
  surv_obj <- Surv(data$waitlist_time_days, data$status)
  
  # Calculate C-Index
  cindex <- survcomp::concordance.index(
    x = predictions,
    surv.time = data$waitlist_time_days,
    surv.event = data$status
  )
  
  # Return C-Index and confidence interval
  return(list(
    cindex = cindex$c.index,
    lower = cindex$lower,
    upper = cindex$upper
  ))
}
```

### R²

R² measures explained variation:

```r
# Example code for R² calculation
calculate_rsquared <- function(data, predictions) {
  # Create survival object
  surv_obj <- Surv(data$waitlist_time_days, data$status)
  
  # Calculate R²
  rsq <- rms::rsquared(
    fit = predictions,
    newdata = data
  )
  
  # Return R²
  return(rsq)
}
```

## Visualization Techniques

### Publication-Quality Figures

All visualizations are created with publication-quality standards:

```r
# Example code for publication-quality theme
publication_theme <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "Arial", size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 11),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90")
    )
}
```

### Variable Importance Plots

Variable importance is visualized for model interpretation:

```r
# Example code for variable importance plot
plot_variable_importance <- function(importance_values) {
  # Convert to data frame
  imp_df <- data.frame(
    Variable = names(importance_values),
    Importance = as.numeric(importance_values)
  ) %>%
    arrange(desc(Importance))
  
  # Create plot
  ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Variable Importance",
      x = NULL,
      y = "Importance"
    ) +
    publication_theme()
}
```

## Report Generation

### HTML Report

A comprehensive HTML report is generated with:

```r
# Example code for HTML report generation
generate_html_report <- function() {
  rmarkdown::render(
    input = "src/report_template.Rmd",
    output_file = "Liver_Transplant_Survival_Analysis_Report.html",
    output_dir = "reports",
    params = list(
      data_path = "data/",
      results_path = "results/"
    )
  )
}
```

### Interactive Visualizations

Interactive visualizations are included in the report:

```r
# Example code for interactive survival curve
interactive_survival_curve <- function(km_fit) {
  # Convert to data frame
  surv_data <- surv_summary(km_fit)
  
  # Create plotly object
  plot_ly(
    data = surv_data,
    x = ~time,
    y = ~surv,
    color = ~strata,
    type = "scatter",
    mode = "lines",
    line = list(width = 2)
  ) %>%
    layout(
      title = "Interactive Survival Curve",
      xaxis = list(title = "Time (days)"),
      yaxis = list(title = "Survival Probability", range = c(0, 1)),
      hovermode = "closest"
    )
}
```

### Comparison Tables

Tables comparing model performance are included:

```r
# Example code for model comparison table
create_comparison_table <- function(models_list) {
  # Extract performance metrics
  comparison_df <- data.frame(
    Model = names(models_list),
    C_Index = sapply(models_list, function(x) x$performance$cindex),
    C_Index_Lower = sapply(models_list, function(x) x$performance$lower),
    C_Index_Upper = sapply(models_list, function(x) x$performance$upper),
    Brier_Score = sapply(models_list, function(x) x$performance$brier),
    R_Squared = sapply(models_list, function(x) x$performance$rsquared)
  )
  
  # Create formatted table
  DT::datatable(
    comparison_df,
    options = list(
      pageLength = 10,
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 1:5))
    ),
    rownames = FALSE,
    caption = "Comparison of Model Performance"
  ) %>%
    formatRound(columns = c("C_Index", "C_Index_Lower", "C_Index_Upper", 
                           "Brier_Score", "R_Squared"), digits = 3)
}
```

The complete analysis workflow is implemented in the R scripts located in the `src` directory. Each script is thoroughly documented with comments explaining the rationale behind methodological choices and interpretation of results.