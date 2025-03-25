# Model Documentation

This document provides detailed technical information about the survival prediction models implemented in this project.

## Table of Contents

- [Overview](#overview)
- [Traditional MELD Score](#traditional-meld-score)
- [BC-MELD Formula](#bc-meld-formula)
- [Cox Proportional Hazards Models](#cox-proportional-hazards-models)
- [Accelerated Oblique Random Survival Forests](#accelerated-oblique-random-survival-forests)
- [H2O Distributed Survival Analysis](#h2o-distributed-survival-analysis)
- [Ensemble Models](#ensemble-models)
- [Model Interpretation](#model-interpretation)
- [Implementation Details](#implementation-details)

## Overview

This project implements and compares several survival prediction models:

1. **Traditional MELD Score**: The standard Model for End-Stage Liver Disease score
2. **BC-MELD Formula**: Body composition enhanced MELD score
3. **Cox Proportional Hazards Models**: Classical survival regression models
4. **Accelerated Oblique Random Survival Forests (AORSF)**: Advanced machine learning for survival analysis
5. **H2O Distributed Survival Analysis**: Scalable machine learning models
6. **Ensemble Models**: Combinations of multiple model approaches

Each model is evaluated using rigorous validation techniques and performance metrics.

## Traditional MELD Score

### Formula

The traditional MELD score is calculated using the following formula:

```
MELD = 3.78 × ln(serum bilirubin [mg/dL]) + 11.2 × ln(INR) + 9.57 × ln(serum creatinine [mg/dL]) + 6.43
```

Where:
- Serum bilirubin measures liver function
- INR (International Normalized Ratio) measures blood clotting
- Serum creatinine measures kidney function

### Limitations

The traditional MELD score has several limitations:

- Does not account for frailty or sarcopenia
- Does not consider body composition
- May not fully capture disease severity in some patients
- Limited predictive accuracy for certain patient subgroups

### Implementation

In this project, we use the lab_meld variable from the dataset, which represents the laboratory MELD score calculated according to the standard formula.

## BC-MELD Formula

### Original Formula

The original BC-MELD formula incorporates body composition metrics:

```
BC-MELD = MELD + (β₁ × muscle) - (β₂ × visceral fat) + (β₃ × (1 - subcutaneous fat/total fat))
```

Where:
- muscle = skeletal muscle area at L3 (cm²)
- visceral fat = visceral adipose tissue area at L3 (cm²)
- subcutaneous fat = subcutaneous adipose tissue area at L3 (cm²)
- total fat = total adipose tissue area at L3 (cm²)

### Improved Formula

Our improved BC-MELD formula is derived using regularized regression:

```
BC-MELD_improved = MELD + (β₁ × muscle) + (β₂ × sat) + (β₃ × vat) + (β₄ × imat) + (β₅ × eat) + (β₆ × pat)
```

Where:
- sat = subcutaneous adipose tissue area
- vat = visceral adipose tissue area
- imat = intermuscular adipose tissue area
- eat = epicardial adipose tissue area
- pat = periaortic adipose tissue area

The coefficients (β values) are determined through regularized regression with cross-validation.

### Implementation

```r
# Implementation of improved BC-MELD formula
calculate_bc_meld_improved <- function(data, coefficients) {
  # Apply formula with derived coefficients
  data %>%
    mutate(
      bc_meld_improved = lab_meld +
                        (coefficients["muscle"] * muscle) +
                        (coefficients["sat"] * sat) +
                        (coefficients["vat"] * vat) +
                        (coefficients["imat"] * imat) +
                        (coefficients["eat"] * eat) +
                        (coefficients["pat"] * pat)
    )
}
```

## Cox Proportional Hazards Models

### Model Specification

Cox proportional hazards models are specified as:

```
h(t|X) = h₀(t) × exp(β₁X₁ + β₂X₂ + ... + βₚXₚ)
```

Where:
- h(t|X) is the hazard at time t for a patient with covariates X
- h₀(t) is the baseline hazard
- β are the regression coefficients
- X are the covariates (predictors)

### Variable Selection

Variables are selected using a combination of:

1. Clinical knowledge and prior research
2. Univariate analysis to identify significant predictors
3. Regularization techniques (LASSO/Ridge) to handle multicollinearity

### Proportional Hazards Assumption

The proportional hazards assumption is checked using:

```r
# Check proportional hazards assumption
check_ph_assumption <- function(cox_model) {
  # Test based on Schoenfeld residuals
  ph_test <- cox.zph(cox_model)
  
  # Plot Schoenfeld residuals
  plot(ph_test)
  
  return(ph_test)
}
```

### Implementation

```r
# Fit Cox proportional hazards model
fit_cox_model <- function(data, formula_str) {
  # Create formula object
  formula <- as.formula(formula_str)
  
  # Fit model
  cox_fit <- coxph(formula, data = data)
  
  # Return fitted model
  return(cox_fit)
}
```

## Accelerated Oblique Random Survival Forests

### Algorithm Overview

Accelerated Oblique Random Survival Forests (AORSF) extend traditional random survival forests by:

1. Using oblique splits (linear combinations of variables) instead of axis-parallel splits
2. Accelerating the training process through efficient computational techniques
3. Improving handling of non-linear relationships and interactions

### Hyperparameters

Key hyperparameters include:

| Parameter | Description | Default Value | Tuning Range |
|-----------|-------------|---------------|--------------|
| n_tree | Number of trees in the forest | 500 | 100-1000 |
| mtry | Number of variables to consider at each split | sqrt(p) | 1-p |
| n_split | Number of random splits to consider | 25 | 10-100 |
| split_min_events | Minimum number of events in a node to split | 5 | 1-20 |
| leaf_min_events | Minimum number of events in a leaf node | 1 | 1-10 |
| importance | Method for variable importance | "permute" | "permute", "anova" |

### Implementation

```r
# Fit AORSF model with hyperparameter tuning
fit_aorsf_tuned <- function(data, formula_str, param_grid = NULL) {
  # Create formula object
  formula <- as.formula(formula_str)
  
  # Default parameter grid if not provided
  if (is.null(param_grid)) {
    param_grid <- expand.grid(
      n_tree = c(100, 300, 500),
      mtry = c(3, 5, 7),
      n_split = c(10, 25, 50)
    )
  }
  
  # Initialize performance storage
  performance <- numeric(nrow(param_grid))
  
  # Cross-validation for parameter tuning
  for (i in 1:nrow(param_grid)) {
    # Create folds
    set.seed(123)
    folds <- createFolds(data$status, k = 5)
    
    # Initialize fold performance
    fold_perf <- numeric(5)
    
    # Cross-validation
    for (j in 1:5) {
      # Split data
      train_data <- data[-folds[[j]], ]
      test_data <- data[folds[[j]], ]
      
      # Fit model with current parameters
      model <- aorsf(
        formula = formula,
        data = train_data,
        n_tree = param_grid$n_tree[i],
        mtry = param_grid$mtry[i],
        n_split = param_grid$n_split[i]
      )
      
      # Predict on test data
      pred <- predict(model, test_data)
      
      # Calculate C-index
      fold_perf[j] <- calculate_cindex(test_data, pred)
    }
    
    # Average performance across folds
    performance[i] <- mean(fold_perf)
  }
  
  # Find best parameters
  best_idx <- which.max(performance)
  best_params <- param_grid[best_idx, ]
  
  # Fit final model with best parameters
  final_model <- aorsf(
    formula = formula,
    data = data,
    n_tree = best_params$n_tree,
    mtry = best_params$mtry,
    n_split = best_params$n_split,
    importance = "permute"
  )
  
  # Return model and best parameters
  return(list(
    model = final_model,
    best_params = best_params,
    best_performance = performance[best_idx]
  ))
}
```

## H2O Distributed Survival Analysis

### Algorithm Overview

H2O provides distributed implementations of survival analysis algorithms, including:

1. Distributed Random Survival Forests
2. Cox Proportional Hazards with regularization
3. Gradient Boosting Machines for survival

### Distributed Computing

H2O leverages distributed computing for:

- Parallel processing across multiple cores/machines
- Efficient handling of large datasets
- Faster hyperparameter tuning
- Memory-optimized data processing

### Hyperparameter Tuning

H2O's grid search capabilities are used for hyperparameter tuning:

```r
# H2O hyperparameter tuning
tune_h2o_survival <- function(data_h2o, predictors, response, event_column) {
  # Define hyperparameter grid
  hyper_grid <- list(
    ntrees = c(50, 100, 200),
    max_depth = c(3, 5, 10),
    sample_rate = c(0.7, 0.8, 0.9),
    col_sample_rate_per_tree = c(0.7, 0.8, 0.9)
  )
  
  # Define search criteria
  search_criteria <- list(
    strategy = "RandomDiscrete",
    max_models = 20,
    seed = 123
  )
  
  # Define survival parameters
  surv_params <- list(event_column = event_column)
  
  # Run grid search
  grid <- h2o.grid(
    algorithm = "randomForest",
    x = predictors,
    y = response,
    grid_id = "survival_grid",
    training_frame = data_h2o,
    hyper_params = hyper_grid,
    search_criteria = search_criteria,
    distribution = "cox",
    survival_params = surv_params
  )
  
  # Get best model
  best_model <- h2o.getModel(grid@model_ids[[1]])
  
  return(best_model)
}
```

### Implementation

```r
# Fit H2O survival model
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

## Ensemble Models

### Ensemble Approach

The ensemble approach combines predictions from multiple models:

```
ensemble_prediction = w₁ × prediction₁ + w₂ × prediction₂ + ... + wₙ × predictionₙ
```

Where:
- prediction₁, prediction₂, ..., predictionₙ are predictions from individual models
- w₁, w₂, ..., wₙ are weights assigned to each model

### Weight Optimization

Weights are optimized using:

1. Equal weighting (simple average)
2. Performance-based weighting (better models get higher weights)
3. Stacked regression (meta-model to learn optimal weights)

```r
# Optimize ensemble weights using stacked regression
optimize_weights <- function(predictions_list, true_outcomes) {
  # Create matrix of predictions
  pred_matrix <- do.call(cbind, predictions_list)
  
  # Fit meta-model
  meta_model <- glmnet(
    x = pred_matrix,
    y = true_outcomes,
    family = "cox",
    alpha = 0.5
  )
  
  # Get optimal weights
  weights <- coef(meta_model, s = "lambda.min")[-1]
  
  # Normalize weights to sum to 1
  weights <- weights / sum(weights)
  
  return(weights)
}
```

### Implementation

```r
# Create ensemble model
create_ensemble <- function(models_list, data, weights = NULL) {
  # Generate predictions from each model
  predictions_list <- lapply(models_list, function(model) {
    predict(model, data)
  })
  
  # If weights not provided, optimize them
  if (is.null(weights)) {
    weights <- optimize_weights(
      predictions_list,
      Surv(data$waitlist_time_days, data$status)
    )
  }
  
  # Combine predictions with weights
  ensemble_pred <- mapply(function(pred, weight) pred * weight, 
                         predictions_list, weights) %>%
    Reduce(f = "+")
  
  return(ensemble_pred)
}
```

## Model Interpretation

### SHAP Values

SHapley Additive exPlanations (SHAP) values are used to interpret model predictions:

```r
# Calculate SHAP values for AORSF model
calculate_aorsf_shap <- function(model, data, n_samples = 100) {
  # Calculate SHAP values
  shap_values <- fastshap::explain(
    object = model,
    X = data,
    nsim = n_samples,
    pred_wrapper = function(object, newdata) {
      predict(object, newdata)
    }
  )
  
  return(shap_values)
}
```

### Variable Importance

Variable importance is calculated for each model:

```r
# Extract variable importance from AORSF model
get_aorsf_importance <- function(model) {
  # Get variable importance
  importance <- orsf_vi_permute(model)
  
  return(importance)
}
```

### Partial Dependence Plots

Partial dependence plots show the relationship between predictors and outcomes:

```r
# Create partial dependence plot
create_pdp <- function(model, data, variable) {
  # Calculate partial dependence
  pd <- partial(
    model,
    pred.var = variable,
    train = data,
    grid.resolution = 20,
    ice = FALSE
  )
  
  # Create plot
  ggplot(pd, aes(x = get(variable), y = yhat)) +
    geom_line() +
    geom_smooth(method = "loess", se = TRUE) +
    labs(
      title = paste("Partial Dependence Plot for", variable),
      x = variable,
      y = "Predicted Risk"
    ) +
    theme_minimal()
}
```

## Implementation Details

### Code Organization

The model implementation is organized in the following files:

- `src/bc_meld_formula.R`: Implementation of BC-MELD formulas
- `src/cox_models.R`: Cox proportional hazards models
- `src/aorsf_models.R`: AORSF implementation
- `src/h2o_models.R`: H2O distributed survival analysis
- `src/ensemble_models.R`: Ensemble model implementation
- `src/model_interpretation.R`: Functions for model interpretation

### Computational Requirements

The computational requirements for each model are:

| Model | Memory | CPU | Training Time | Prediction Time |
|-------|--------|-----|---------------|----------------|
| BC-MELD | Low | Low | Fast | Very Fast |
| Cox PH | Low | Low | Fast | Fast |
| AORSF | Medium | Medium-High | Medium | Fast |
| H2O | High | High | Medium-Slow | Medium |
| Ensemble | High | High | Slow | Medium |

### Reproducibility

To ensure reproducibility:

1. Random seeds are set before model training
2. Cross-validation folds are created consistently
3. Hyperparameter tuning uses fixed search spaces
4. Model configurations are documented

```r
# Set random seed for reproducibility
set_reproducibility <- function() {
  set.seed(12345)
  if (requireNamespace("h2o", quietly = TRUE)) {
    h2o.init(seed = 12345)
  }
}
```

### Model Persistence

Models are saved for future use:

```r
# Save model to file
save_model <- function(model, filename) {
  # Create directory if it doesn't exist
  dir.create("models", showWarnings = FALSE)
  
  # Save model
  saveRDS(model, file = file.path("models", filename))
}

# Load model from file
load_model <- function(filename) {
  # Load model
  model <- readRDS(file = file.path("models", filename))
  
  return(model)
}
```

The complete model implementation details are available in the source code files in the `src` directory.