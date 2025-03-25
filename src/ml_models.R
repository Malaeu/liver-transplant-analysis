# ============================================================================
# MACHINE LEARNING MODELS FUNCTIONS
# ============================================================================
# Functions for aorsf, H2O, and ensemble models
# ============================================================================

#' Fit an Accelerated Oblique Random Survival Forest model
#'
#' @param data The input data frame
#' @param formula_str Model formula as a string
#' @param n_tree Number of trees in the forest
#' @param importance Method for variable importance
#' @param n_split Number of random splits to consider
#' @param split_min_events Minimum number of events in a node to split
#' @return An aorsf object
fit_aorsf_model <- function(data,
                           formula_str,
                           n_tree = 500,
                           importance = "permute",
                           n_split = 25,
                           split_min_events = 5) {
  
  # Ensure aorsf package is available
  if (!requireNamespace("aorsf", quietly = TRUE)) {
    stop("aorsf package is required for this function")
  }
  
  # Create formula object
  formula <- as.formula(formula_str)
  
  message("Fitting aorsf model with ", n_tree, " trees")
  
  # Fit aorsf model
  aorsf_fit <- aorsf::orsf(
    formula = formula,
    data = data,
    n_tree = n_tree,
    importance = importance,
    n_split = n_split,
    split_min_events = split_min_events
  )
  
  return(aorsf_fit)
}

#' Tune hyperparameters for an aorsf model
#'
#' @param data The input data frame
#' @param formula_str Model formula as a string
#' @param param_grid Data frame of parameter combinations to try
#' @param nfolds Number of cross-validation folds
#' @param seed Random seed for reproducibility
#' @return A list containing the best model, parameters, and results
tune_aorsf_model <- function(data,
                            formula_str,
                            param_grid = NULL,
                            nfolds = 5,
                            seed = 123) {
  
  # Ensure aorsf package is available
  if (!requireNamespace("aorsf", quietly = TRUE)) {
    stop("aorsf package is required for this function")
  }
  
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
  
  message("Tuning aorsf model with ", nrow(param_grid), " parameter combinations")
  message("Using ", nfolds, "-fold cross-validation")
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Create folds
  folds <- create_folds(data, k = nfolds, stratify_var = "status", seed = seed)
  
  # Initialize performance storage
  performance <- numeric(nrow(param_grid))
  
  # Cross-validation for parameter tuning
  for (i in 1:nrow(param_grid)) {
    message("Evaluating parameter combination ", i, "/", nrow(param_grid))
    
    # Initialize fold performance
    fold_perf <- numeric(nfolds)
    
    # Cross-validation
    for (j in 1:nfolds) {
      # Split data
      train_data <- data[-folds[[j]], ]
      test_data <- data[folds[[j]], ]
      
      # Fit model with current parameters
      model <- try({
        aorsf::orsf(
          formula = formula,
          data = train_data,
          n_tree = param_grid$n_tree[i],
          mtry = param_grid$mtry[i],
          n_split = param_grid$n_split[i]
        )
      }, silent = TRUE)
      
      if (!inherits(model, "try-error")) {
        # Predict on test data
        pred <- predict(model, test_data)
        
        # Calculate C-index
        surv_obj <- Surv(test_data$waitlist_time_days, test_data$status)
        cindex <- survConcordance(surv_obj ~ pred)$concordance
        
        fold_perf[j] <- cindex
      } else {
        fold_perf[j] <- NA
      }
    }
    
    # Average performance across folds
    performance[i] <- mean(fold_perf, na.rm = TRUE)
    
    message("  Mean C-index: ", sprintf("%.3f", performance[i]))
  }
  
  # Find best parameters
  best_idx <- which.max(performance)
  best_params <- param_grid[best_idx, ]
  
  message("Best parameters:")
  for (param in names(best_params)) {
    message("  ", param, ": ", best_params[[param]])
  }
  message("Best C-index: ", sprintf("%.3f", performance[best_idx]))
  
  # Fit final model with best parameters
  final_model <- aorsf::orsf(
    formula = formula,
    data = data,
    n_tree = best_params$n_tree,
    mtry = best_params$mtry,
    n_split = best_params$n_split,
    importance = "permute"
  )
  
  # Return model and best parameters
  return(list(
    best_model = final_model,
    best_params = best_params,
    best_performance = performance[best_idx],
    all_params = param_grid,
    all_performance = performance
  ))
}

#' Get variable importance from an aorsf model
#'
#' @param model An aorsf object
#' @return A named vector of variable importance values
get_variable_importance <- function(model) {
  
  # Ensure model is an aorsf model
  if (!inherits(model, "orsf")) {
    stop("Model must be an aorsf model")
  }
  
  # Get variable importance
  if ("importance" %in% names(model)) {
    # Direct access if available
    importance <- model$importance
  } else {
    # Calculate if not available
    importance <- aorsf::orsf_vi_permute(model)
  }
  
  # Normalize to 0-100 scale
  importance <- 100 * importance / max(importance)
  
  return(importance)
}

#' Fit an H2O distributed survival model
#'
#' @param data The input data frame
#' @param time_var Name of the time variable
#' @param event_var Name of the event variable
#' @param predictor_vars Vector of predictor variable names
#' @param algorithm H2O algorithm to use
#' @param ntrees Number of trees (for tree-based methods)
#' @param max_depth Maximum tree depth (for tree-based methods)
#' @return An H2O model object
fit_h2o_survival_model <- function(data,
                                  time_var = "waitlist_time_days",
                                  event_var = "status",
                                  predictor_vars = NULL,
                                  algorithm = "randomForest",
                                  ntrees = 500,
                                  max_depth = 10) {
  
  # Ensure H2O package is available
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("h2o package is required for this function")
  }
  
  # Ensure H2O is initialized
  if (!h2o::h2o.is_running()) {
    message("Initializing H2O")
    h2o::h2o.init(nthreads = -1)
  }
  
  # If no predictor variables specified, use all except time and event
  if (is.null(predictor_vars)) {
    predictor_vars <- setdiff(names(data), c(time_var, event_var))
  }
  
  # Ensure all variables exist
  missing_vars <- predictor_vars[!predictor_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Missing predictor variables: ", paste(missing_vars, collapse = ", "))
  }
  
  # Convert data to H2O frame
  message("Converting data to H2O frame")
  data_h2o <- h2o::as.h2o(data)
  
  # Define survival parameters
  surv_params <- list(event_column = event_var)
  
  message("Fitting H2O ", algorithm, " model")
  
  # Fit model based on algorithm
  if (algorithm == "randomForest") {
    model <- h2o::h2o.randomForest(
      x = predictor_vars,
      y = time_var,
      training_frame = data_h2o,
      ntrees = ntrees,
      max_depth = max_depth,
      seed = 1234,
      stopping_rounds = 5,
      stopping_tolerance = 0.001,
      stopping_metric = "concordance",
      distribution = "cox",
      survival_params = surv_params
    )
  } else if (algorithm == "gbm") {
    model <- h2o::h2o.gbm(
      x = predictor_vars,
      y = time_var,
      training_frame = data_h2o,
      ntrees = ntrees,
      max_depth = max_depth,
      seed = 1234,
      stopping_rounds = 5,
      stopping_tolerance = 0.001,
      stopping_metric = "concordance",
      distribution = "cox",
      survival_params = surv_params
    )
  } else if (algorithm == "xgboost") {
    model <- h2o::h2o.xgboost(
      x = predictor_vars,
      y = time_var,
      training_frame = data_h2o,
      ntrees = ntrees,
      max_depth = max_depth,
      seed = 1234,
      stopping_rounds = 5,
      stopping_tolerance = 0.001,
      stopping_metric = "concordance",
      distribution = "cox",
      survival_params = surv_params
    )
  } else {
    stop("Unsupported algorithm: ", algorithm)
  }
  
  return(model)
}

#' Tune hyperparameters for an H2O survival model
#'
#' @param data The input data frame
#' @param time_var Name of the time variable
#' @param event_var Name of the event variable
#' @param predictor_vars Vector of predictor variable names
#' @param algorithm H2O algorithm to use
#' @param hyper_params List of hyperparameter grids
#' @param max_models Maximum number of models to train
#' @return The best H2O model
tune_h2o_survival_model <- function(data,
                                   time_var = "waitlist_time_days",
                                   event_var = "status",
                                   predictor_vars = NULL,
                                   algorithm = "randomForest",
                                   hyper_params = NULL,
                                   max_models = 20) {
  
  # Ensure H2O package is available
  if (!requireNamespace("h2o", quietly = TRUE)) {
    stop("h2o package is required for this function")
  }
  
  # Ensure H2O is initialized
  if (!h2o::h2o.is_running()) {
    message("Initializing H2O")
    h2o::h2o.init(nthreads = -1)
  }
  
  # If no predictor variables specified, use all except time and event
  if (is.null(predictor_vars)) {
    predictor_vars <- setdiff(names(data), c(time_var, event_var))
  }
  
  # Default hyperparameter grid if not provided
  if (is.null(hyper_params)) {
    if (algorithm == "randomForest") {
      hyper_params <- list(
        ntrees = c(50, 100, 200, 500),
        max_depth = c(5, 10, 15, 20),
        sample_rate = c(0.7, 0.8, 0.9),
        col_sample_rate_per_tree = c(0.7, 0.8, 0.9)
      )
    } else if (algorithm == "gbm") {
      hyper_params <- list(
        ntrees = c(50, 100, 200, 500),
        max_depth = c(3, 5, 7, 9),
        learn_rate = c(0.01, 0.05, 0.1),
        sample_rate = c(0.7, 0.8, 0.9)
      )
    } else if (algorithm == "xgboost") {
      hyper_params <- list(
        ntrees = c(50, 100, 200, 500),
        max_depth = c(3, 5, 7, 9),
        learn_rate = c(0.01, 0.05, 0.1),
        sample_rate = c(0.7, 0.8, 0.9)
      )
    } else {
      stop("Unsupported algorithm: ", algorithm)
    }
  }
  
  # Convert data to H2O frame
  message("Converting data to H2O frame")
  data_h2o <- h2o::as.h2o(data)
  
  # Define survival parameters
  surv_params <- list(event_column = event_var)
  
  # Define search criteria
  search_criteria <- list(
    strategy = "RandomDiscrete",
    max_models = max_models,
    seed = 1234
  )
  
  message("Tuning H2O ", algorithm, " model with grid search")
  message("Maximum number of models: ", max_models)
  
  # Run grid search
  grid <- h2o::h2o.grid(
    algorithm = algorithm,
    x = predictor_vars,
    y = time_var,
    grid_id = paste0(algorithm, "_grid"),
    training_frame = data_h2o,
    hyper_params = hyper_params,
    search_criteria = search_criteria,
    distribution = "cox",
    survival_params = surv_params,
    seed = 1234,
    stopping_rounds = 5,
    stopping_tolerance = 0.001,
    stopping_metric = "concordance"
  )
  
  # Get grid results
  grid_results <- h2o::h2o.getGrid(
    grid_id = paste0(algorithm, "_grid"),
    sort_by = "concordance",
    decreasing = TRUE
  )
  
  # Print top models
  message("Top models:")
  print(grid_results@summary_table[1:min(3, nrow(grid_results@summary_table)), ])
  
  # Get best model
  best_model <- h2o::h2o.getModel(grid_results@model_ids[[1]])
  
  return(best_model)
}

#' Calculate SHAP values for model interpretation
#'
#' @param model A fitted model object
#' @param data The input data frame
#' @param n_samples Number of samples for SHAP approximation
#' @param model_type Type of model
#' @return A matrix of SHAP values
calculate_shap_values <- function(model,
                                 data,
                                 n_samples = 100,
                                 model_type = c("aorsf", "h2o", "cox")) {
  
  model_type <- match.arg(model_type)
  
  # Ensure fastshap package is available
  if (!requireNamespace("fastshap", quietly = TRUE)) {
    stop("fastshap package is required for this function")
  }
  
  message("Calculating SHAP values with ", n_samples, " samples")
  
  # Calculate SHAP values based on model type
  if (model_type == "aorsf") {
    # For aorsf models
    shap_values <- fastshap::explain(
      object = model,
      X = data,
      nsim = n_samples,
      pred_wrapper = function(object, newdata) {
        predict(object, newdata)
      }
    )
  } else if (model_type == "h2o") {
    # For H2O models
    # Convert data to H2O frame
    data_h2o <- h2o::as.h2o(data)
    
    # Get SHAP values
    shap_values <- h2o::h2o.predict_contributions(model, data_h2o)
    
    # Convert to R matrix
    shap_values <- as.matrix(shap_values)
  } else if (model_type == "cox") {
    # For Cox models
    shap_values <- fastshap::explain(
      object = model,
      X = data,
      nsim = n_samples,
      pred_wrapper = function(object, newdata) {
        predict(object, newdata = newdata, type = "risk")
      }
    )
  }
  
  return(shap_values)
}

#' Plot SHAP summary
#'
#' @param shap_values Matrix of SHAP values
#' @param data The input data frame
#' @param top_n Number of top variables to include
#' @param plot_type Type of plot ("bar", "beeswarm", or "violin")
#' @return A ggplot object
plot_shap_summary <- function(shap_values,
                             data,
                             top_n = 10,
                             plot_type = "bar") {
  
  # Calculate mean absolute SHAP values for each feature
  mean_shap <- colMeans(abs(shap_values))
  
  # Get top features
  top_features <- names(sort(mean_shap, decreasing = TRUE))[1:min(top_n, length(mean_shap))]
  
  if (plot_type == "bar") {
    # Create bar plot of feature importance
    shap_df <- data.frame(
      Feature = factor(names(mean_shap), levels = names(sort(mean_shap, decreasing = TRUE))),
      Importance = mean_shap
    )
    
    # Filter to top features
    shap_df <- shap_df[shap_df$Feature %in% top_features, ]
    
    # Create plot
    p <- ggplot(shap_df, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(
        title = "Feature Importance (Mean |SHAP|)",
        x = NULL,
        y = "Mean |SHAP Value|"
      ) +
      theme_minimal()
    
  } else if (plot_type == "beeswarm" || plot_type == "violin") {
    # Convert SHAP values to long format
    shap_long <- reshape2::melt(shap_values, varnames = c("Row", "Feature"), value.name = "SHAP")
    
    # Add feature values
    shap_long$Value <- NA
    for (feature in unique(shap_long$Feature)) {
      if (feature %in% names(data)) {
        shap_long$Value[shap_long$Feature == feature] <- data[[feature]][shap_long$Row]
      }
    }
    
    # Filter to top features
    shap_long <- shap_long[shap_long$Feature %in% top_features, ]
    
    # Create plot
    if (plot_type == "beeswarm") {
      # Beeswarm plot
      p <- ggplot(shap_long, aes(x = reorder(Feature, SHAP, FUN = function(x) mean(abs(x))), y = SHAP, color = Value)) +
        geom_jitter(width = 0.2, alpha = 0.7) +
        scale_color_viridis_c() +
        coord_flip() +
        labs(
          title = "SHAP Values",
          x = NULL,
          y = "SHAP Value",
          color = "Feature Value"
        ) +
        theme_minimal()
    } else {
      # Violin plot
      p <- ggplot(shap_long, aes(x = reorder(Feature, SHAP, FUN = function(x) mean(abs(x))), y = SHAP, fill = Feature)) +
        geom_violin(scale = "width", alpha = 0.7) +
        coord_flip() +
        labs(
          title = "SHAP Value Distribution",
          x = NULL,
          y = "SHAP Value"
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    }
  } else {
    stop("Invalid plot_type. Use 'bar', 'beeswarm', or 'violin'")
  }
  
  return(p)
}

#' Create an ensemble model from multiple survival models
#'
#' @param models_list List of fitted survival models
#' @param data The input data frame
#' @param weights Optional vector of model weights
#' @param optimize_weights Whether to optimize weights based on performance
#' @return An ensemble model object
create_ensemble_model <- function(models_list,
                                 data,
                                 weights = NULL,
                                 optimize_weights = TRUE) {
  
  # Ensure models_list is a named list
  if (is.null(names(models_list))) {
    stop("models_list must be a named list")
  }
  
  message("Creating ensemble model from ", length(models_list), " models")
  
  # Generate predictions from each model
  predictions_list <- list()
  
  for (model_name in names(models_list)) {
    model <- models_list[[model_name]]
    
    # Get predictions based on model type
    if (inherits(model, "orsf")) {
      predictions_list[[model_name]] <- predict(model, data)
    } else if (inherits(model, "H2OModel")) {
      data_h2o <- h2o::as.h2o(data)
      predictions_list[[model_name]] <- as.vector(h2o::h2o.predict(model, data_h2o)$predict)
    } else if (inherits(model, "coxph")) {
      predictions_list[[model_name]] <- predict(model, newdata = data, type = "risk")
    } else if (is.numeric(model)) {
      # For coefficient vectors (e.g., BC-MELD coefficients)
      # Apply coefficients to data
      pred <- data$lab_meld  # Start with base MELD
      for (var in names(model)) {
        if (var %in% names(data)) {
          pred <- pred + (model[var] * data[[var]])
        }
      }
      predictions_list[[model_name]] <- pred
    } else {
      stop("Unsupported model type for ", model_name)
    }
  }
  
  # Optimize weights if requested
  if (optimize_weights && is.null(weights)) {
    message("Optimizing ensemble weights")
    
    # Create matrix of predictions
    pred_matrix <- do.call(cbind, predictions_list)
    
    # Create survival object
    surv_obj <- Surv(data$waitlist_time_days, data$status)
    
    # Fit meta-model using Cox regression
    meta_model <- coxph(surv_obj ~ pred_matrix)
    
    # Extract coefficients
    weights <- coef(meta_model)
    
    # Normalize weights to sum to 1
    weights <- weights / sum(weights)
    
    message("Optimized weights:")
    for (i in 1:length(weights)) {
      message("  ", names(models_list)[i], ": ", sprintf("%.3f", weights[i]))
    }
  } else if (is.null(weights)) {
    # Equal weights if not provided and not optimized
    weights <- rep(1/length(models_list), length(models_list))
    names(weights) <- names(models_list)
  }
  
  # Create ensemble model object
  ensemble <- list(
    models = models_list,
    weights = weights,
    predictions = predictions_list
  )
  
  class(ensemble) <- "ensemble_survival_model"
  
  return(ensemble)
}

#' Predict using an ensemble model
#'
#' @param ensemble An ensemble model object
#' @param newdata New data for prediction
#' @return A vector of ensemble predictions
predict_ensemble <- function(ensemble, newdata) {
  
  # Ensure ensemble is an ensemble_survival_model
  if (!inherits(ensemble, "ensemble_survival_model")) {
    stop("ensemble must be an ensemble_survival_model object")
  }
  
  # Generate predictions from each model
  predictions_list <- list()
  
  for (model_name in names(ensemble$models)) {
    model <- ensemble$models[[model_name]]
    
    # Get predictions based on model type
    if (inherits(model, "orsf")) {
      predictions_list[[model_name]] <- predict(model, newdata)
    } else if (inherits(model, "H2OModel")) {
      newdata_h2o <- h2o::as.h2o(newdata)
      predictions_list[[model_name]] <- as.vector(h2o::h2o.predict(model, newdata_h2o)$predict)
    } else if (inherits(model, "coxph")) {
      predictions_list[[model_name]] <- predict(model, newdata = newdata, type = "risk")
    } else if (is.numeric(model)) {
      # For coefficient vectors (e.g., BC-MELD coefficients)
      # Apply coefficients to data
      pred <- newdata$lab_meld  # Start with base MELD
      for (var in names(model)) {
        if (var %in% names(newdata)) {
          pred <- pred + (model[var] * newdata[[var]])
        }
      }
      predictions_list[[model_name]] <- pred
    } else {
      stop("Unsupported model type for ", model_name)
    }
  }
  
  # Combine predictions with weights
  ensemble_pred <- mapply(function(pred, weight) pred * weight, 
                         predictions_list, ensemble$weights) %>%
    Reduce(f = "+")
  
  return(ensemble_pred)
}

#' Predict risk using a score
#'
#' @param score A vector of scores
#' @param model_type Type of score
#' @return A vector of risk predictions
predict_risk <- function(score, model_type = "score") {
  
  # For simple scores, just return the score
  # In a real implementation, this would convert scores to risk probabilities
  return(score)
}

#' Plot partial dependence for a variable
#'
#' @param model A fitted model object
#' @param data The input data frame
#' @param variable Variable name
#' @param grid_resolution Number of grid points
#' @param ice Whether to include individual conditional expectation curves
#' @param center Whether to center the curves
#' @return A ggplot object
plot_partial_dependence <- function(model,
                                   data,
                                   variable,
                                   grid_resolution = 20,
                                   ice = FALSE,
                                   center = TRUE) {
  
  # Ensure pdp package is available
  if (!requireNamespace("pdp", quietly = TRUE)) {
    stop("pdp package is required for this function")
  }
  
  # Ensure variable exists
  if (!variable %in% names(data)) {
    stop("Variable not found in data")
  }
  
  message("Calculating partial dependence for ", variable)
  
  # Define prediction function based on model type
  if (inherits(model, "orsf")) {
    pred_fun <- function(object, newdata) {
      predict(object, newdata)
    }
  } else if (inherits(model, "coxph")) {
    pred_fun <- function(object, newdata) {
      predict(object, newdata = newdata, type = "risk")
    }
  } else if (inherits(model, "ensemble_survival_model")) {
    pred_fun <- function(object, newdata) {
      predict_ensemble(object, newdata)
    }
  } else {
    stop("Unsupported model type")
  }
  
  # Calculate partial dependence
  pd <- pdp::partial(
    object = model,
    pred.var = variable,
    pred.fun = pred_fun,
    train = data,
    grid.resolution = grid_resolution,
    ice = ice,
    center = center
  )
  
  # Create plot
  p <- pdp::plotPartial(
    pd,
    rug = TRUE,
    train = data,
    smooth = TRUE,
    ylab = "Predicted Risk",
    main = paste("Partial Dependence Plot for", variable)
  )
  
  # Convert to ggplot
  p <- p + theme_minimal()
  
  return(p)
}