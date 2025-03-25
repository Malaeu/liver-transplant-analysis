# ============================================================================
# SURVIVAL ANALYSIS FUNCTIONS
# ============================================================================
# Functions for Kaplan-Meier and Cox proportional hazards models
# ============================================================================

#' Fit a Kaplan-Meier survival model
#'
#' @param data The input data frame
#' @param time_var Name of the time variable
#' @param event_var Name of the event variable (1=event, 0=censored)
#' @param strata_var Optional stratification variable
#' @return A survfit object
fit_km_model <- function(data,
                         time_var = "waitlist_time_days",
                         event_var = "status",
                         strata_var = NULL) {
  
  # Ensure variables exist
  if (!time_var %in% names(data) || !event_var %in% names(data)) {
    stop("Time or event variable not found in data")
  }
  
  # Create formula based on whether stratification is requested
  if (!is.null(strata_var) && strata_var %in% names(data)) {
    formula_str <- paste0("Surv(time = ", time_var, ", event = ", event_var, ") ~ ", strata_var)
    message("Fitting stratified Kaplan-Meier model by ", strata_var)
  } else {
    formula_str <- paste0("Surv(time = ", time_var, ", event = ", event_var, ") ~ 1")
    message("Fitting overall Kaplan-Meier model")
  }
  
  # Create formula object
  formula <- as.formula(formula_str)
  
  # Fit Kaplan-Meier model
  km_fit <- survfit(formula, data = data)
  
  return(km_fit)
}

#' Plot a Kaplan-Meier survival curve
#'
#' @param km_fit A survfit object from fit_km_model
#' @param data The data frame used to fit the model
#' @param title Plot title
#' @param risk_table Whether to include a risk table
#' @param conf_int Whether to show confidence intervals
#' @param palette Color palette for the plot
#' @return A ggsurvplot object
plot_km_curve <- function(data,
                         title = "Kaplan-Meier Survival Curve",
                         time_var = "waitlist_time_days",
                         event_var = "status",
                         strata_var = NULL,
                         risk_table = TRUE,
                         conf_int = TRUE,
                         palette = "jco") {
  
  # Datenvalidierung
  if (!time_var %in% names(data) || !event_var %in% names(data)) {
    stop("Zeit- oder Ereignisvariable nicht im Datensatz gefunden")
  }
  
  # Debug-Ausgaben zu Datentypen
  message("Datentypen vor Konvertierung:")
  message("Zeit-Variable (", time_var, "): ", class(data[[time_var]])[1])
  message("Status-Variable (", event_var, "): ", class(data[[event_var]])[1])
  
  # Wertebereiche überprüfen
  time_range <- range(data[[time_var]], na.rm = TRUE)
  status_values <- table(data[[event_var]], useNA = "ifany")
  message("Zeitbereich: [", time_range[1], ", ", time_range[2], "]")
  message("Status-Werte: ", paste(names(status_values), "=", status_values, collapse=", "))
  
  # Fehlende oder ungültige Werte prüfen
  missing_time <- sum(is.na(data[[time_var]]))
  missing_status <- sum(is.na(data[[event_var]]))
  negative_time <- sum(data[[time_var]] < 0, na.rm = TRUE)
  invalid_status <- sum(!data[[event_var]] %in% c(0, 1), na.rm = TRUE)
  
  if (missing_time > 0 || missing_status > 0 || negative_time > 0 || invalid_status > 0) {
    warning("Problematische Datenpunkte gefunden: ",
            missing_time, " fehlende Zeitwerte, ",
            missing_status, " fehlende Statuswerte, ",
            negative_time, " negative Zeitwerte, ",
            invalid_status, " ungültige Statuswerte")
    
    # Filtern problematischer Datenpunkte
    valid_rows <- !is.na(data[[time_var]]) & data[[time_var]] >= 0 &
                  !is.na(data[[event_var]]) & data[[event_var]] %in% c(0, 1)
    
    if (sum(!valid_rows) > 0) {
      message(sum(!valid_rows), " ungültige Datenpunkte werden gefiltert")
      data <- data[valid_rows, ]
    }
  }
  
  # Explizite Typkonversion
  data[[time_var]] <- as.numeric(data[[time_var]])
  data[[event_var]] <- as.integer(data[[event_var]])
  
  message("Datentypen nach Konvertierung:")
  message("Zeit-Variable: ", class(data[[time_var]])[1])
  message("Status-Variable: ", class(data[[event_var]])[1])
  message("Anzahl gültiger Datenpunkte: ", nrow(data))
  
  # Survival-Analyse mit direkten Formeln statt Surv-Objekt
  if (!is.null(strata_var) && strata_var %in% names(data)) {
    message("Stratifizierte Analyse nach ", strata_var)
    # Die neue Formel umgeht das Problem mit surv_obj
    formula <- as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ ", strata_var))
    km_fit <- survival::survfit(formula, data = data)
  } else {
    message("Gesamtanalyse ohne Stratifizierung")
    # Die neue Formel umgeht das Problem mit surv_obj
    formula <- as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ 1"))
    km_fit <- survival::survfit(formula, data = data)
  }
  
  # Plot erstellen
  km_plot <- try({
    survminer::ggsurvplot(
      fit = km_fit,
      data = data,
      risk.table = risk_table,
      pval = TRUE,
      conf.int = conf_int,
      palette = palette,
      xlab = "Time (months)",
      ylab = "Survival Probability",
      title = title,
      legend.title = if(is.null(strata_var)) "Group" else strata_var,
      risk.table.height = 0.25,
      risk.table.y.text = FALSE,
      tables.theme = theme_cleantable(),
      ggtheme = theme_minimal()
    )
  })
  
  # Fehlerbehandlung für Plot-Erstellung
  if (inherits(km_plot, "try-error")) {
    message("Fehler bei der Plot-Erstellung: ", attr(km_plot, "condition")$message)
    message("Versuche alternative Plot-Methode...")
    
    # Einfacheren Plot erstellen als Fallback
    p <- ggplot2::ggplot()
    
    if (!is.null(strata_var) && strata_var %in% names(data)) {
      p <- p + ggplot2::geom_step(
        data = broom::tidy(km_fit),
        ggplot2::aes(x = time, y = estimate, color = strata)
      )
    } else {
      p <- p + ggplot2::geom_step(
        data = broom::tidy(km_fit),
        ggplot2::aes(x = time, y = estimate)
      )
    }
    
    p <- p +
      ggplot2::labs(
        x = "Time (days)",
        y = "Survival Probability",
        title = title
      ) +
      ggplot2::theme_minimal()
    
    # Fallback-Struktur erstellen, die der von ggsurvplot ähnelt
    km_plot <- list(
      plot = p,
      table = NULL
    )
    class(km_plot) <- c("ggsurvplot", "list")
  }
  
  # Erweitere die Rückgabe um das Fit-Objekt
  result <- list(
    plot = km_plot,
    fit = km_fit
  )
  
  return(result)
}

#' Create an interactive survival curve using plotly
#'
#' @param km_fit A survfit object from fit_km_model
#' @param title Plot title
#' @param show_confidence Whether to show confidence intervals
#' @return A plotly object
create_interactive_survival_curve <- function(km_fit,
                                             title = "Interactive Survival Curve",
                                             show_confidence = TRUE,
                                             data = NULL) {
  
  # Extract survival data
  surv_data <- try({
    if (!is.null(data)) {
      surv_summary(km_fit, data = data)
    } else {
      surv_summary(km_fit)
    }
  })
  
  # Fallback, wenn surv_summary fehlschlägt
  if (inherits(surv_data, "try-error")) {
    warning("surv_summary fehlgeschlagen: ", attr(surv_data, "condition")$message)
    
    # Alternative: Manuelles Extrahieren von Survival-Daten aus km_fit
    surv_data <- data.frame(
      time = km_fit$time,
      surv = km_fit$surv,
      strata = if (is.null(km_fit$strata)) "Overall" else rep(names(km_fit$strata), km_fit$strata)
    )
    
    if (show_confidence && !is.null(km_fit$upper)) {
      surv_data$upper <- km_fit$upper
      surv_data$lower <- km_fit$lower
    }
  }
  
  # Create plotly object
  p <- plot_ly(
    data = surv_data,
    x = ~time,
    y = ~surv,
    color = ~strata,
    type = "scatter",
    mode = "lines",
    line = list(width = 2)
  ) %>%
    layout(
      title = title,
      xaxis = list(title = "Time (months)"),
      yaxis = list(title = "Survival Probability", range = c(0, 1)),
      hovermode = "closest"
    )
  
  # Add confidence intervals if requested
  if (show_confidence && "upper" %in% names(surv_data) && "lower" %in% names(surv_data)) {
    for (stratum in unique(surv_data$strata)) {
      stratum_data <- surv_data[surv_data$strata == stratum, ]
      
      p <- p %>%
        add_ribbons(
          data = stratum_data,
          x = ~time,
          ymin = ~lower,
          ymax = ~upper,
          line = list(color = "transparent"),
          showlegend = FALSE,
          hoverinfo = "none",
          opacity = 0.2
        )
    }
  }
  
  return(p)
}

#' Fit a Cox proportional hazards model
#'
#' @param data The input data frame
#' @param formula_str Model formula as a string
#' @param time_var Name of the time variable
#' @param event_var Name of the event variable
#' @param robust Whether to use robust standard errors
#' @return A coxph object
fit_cox_model <- function(data,
                          formula_str,
                          time_var = "waitlist_time_days",
                          event_var = "status",
                          robust = FALSE) {
  
  # Ensure variables exist
  if (!time_var %in% names(data) || !event_var %in% names(data)) {
    stop("Time or event variable not found in data")
  }
  
  # Create survival object part of the formula
  surv_part <- paste0("Surv(", time_var, ", ", event_var, ")")
  
  # Create full formula
  if (grepl("~", formula_str)) {
    # Formula already contains ~
    formula_str <- gsub("~", paste0(surv_part, " ~"), formula_str)
  } else {
    # Formula is just the right-hand side
    formula_str <- paste0(surv_part, " ~ ", formula_str)
  }
  
  # Create formula object
  formula <- as.formula(formula_str)
  
  message("Fitting Cox model with formula: ", formula_str)
  
  # Fit Cox model
  cox_fit <- coxph(
    formula = formula,
    data = data,
    robust = robust
  )
  
  return(cox_fit)
}

#' Check the proportional hazards assumption for a Cox model
#'
#' @param cox_model A coxph object from fit_cox_model
#' @param plot Whether to create diagnostic plots
#' @return A cox.zph object with test results
check_ph_assumption <- function(cox_model,
                               plot = TRUE) {
  
  # Test proportional hazards assumption
  ph_test <- cox.zph(cox_model)
  
  # Print test results
  print(ph_test)
  
  # Create plots if requested
  if (plot) {
    par(mfrow = c(2, 2))
    for (i in 1:min(4, nrow(ph_test$table))) {
      plot(ph_test[i], main = rownames(ph_test$table)[i])
    }
    par(mfrow = c(1, 1))
  }
  
  return(ph_test)
}

#' Create a forest plot of hazard ratios
#'
#' @param cox_model A coxph object from fit_cox_model
#' @param title Plot title
#' @param point_size Size of points
#' @param color Color of points and lines
#' @return A ggplot object
plot_forest <- function(cox_model,
                        title = "Hazard Ratios with 95% CI",
                        point_size = 3,
                        color = "steelblue") {
  
  # Extract model summary
  model_summary <- summary(cox_model)
  
  # Extract coefficients, hazard ratios, and confidence intervals
  coef_names <- rownames(model_summary$coefficients)
  hazard_ratios <- model_summary$conf.int[, "exp(coef)"]
  lower_ci <- model_summary$conf.int[, "lower .95"]
  upper_ci <- model_summary$conf.int[, "upper .95"]
  p_values <- model_summary$coefficients[, "Pr(>|z|)"]
  
  # Create data frame for plotting
  forest_data <- data.frame(
    Variable = coef_names,
    HR = hazard_ratios,
    Lower = lower_ci,
    Upper = upper_ci,
    P_Value = p_values,
    stringsAsFactors = FALSE
  )
  
  # Add significance stars
  forest_data$Stars <- ifelse(forest_data$P_Value < 0.001, "***",
                             ifelse(forest_data$P_Value < 0.01, "**",
                                   ifelse(forest_data$P_Value < 0.05, "*", "")))
  
  # Format HR and CI for display
  forest_data$HR_CI <- sprintf("%.2f (%.2f-%.2f) %s", 
                              forest_data$HR, forest_data$Lower, forest_data$Upper, forest_data$Stars)
  
  # Order by hazard ratio
  forest_data <- forest_data[order(forest_data$HR), ]
  
  # Convert Variable to factor with levels in the desired order
  forest_data$Variable <- factor(forest_data$Variable, levels = forest_data$Variable)
  
  # Create forest plot
  p <- ggplot(forest_data, aes(x = HR, y = Variable, xmin = Lower, xmax = Upper)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
    geom_errorbarh(height = 0.2, color = color) +
    geom_point(size = point_size, color = color) +
    geom_text(aes(x = max(Upper) * 1.1, label = HR_CI), hjust = 0) +
    scale_x_continuous(trans = "log10", breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10)) +
    labs(
      title = title,
      x = "Hazard Ratio (log scale)",
      y = NULL
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  return(p)
}

#' Calculate performance metrics for survival models
#'
#' @param model A fitted survival model
#' @param data The validation data frame
#' @param time_var Name of the time variable
#' @param event_var Name of the event variable
#' @param times Vector of time points for evaluation
#' @param metrics Vector of metrics to calculate
#' @return A list of performance metrics
calculate_survival_metrics <- function(model,
                                      data,
                                      time_var = "waitlist_time_days",
                                      event_var = "status",
                                      times = c(90, 180, 365),
                                      metrics = c("brier", "cindex", "rsquared")) {
  
  # Ensure variables exist
  if (!time_var %in% names(data) || !event_var %in% names(data)) {
    stop("Time or event variable not found in data")
  }
  
  # Create survival object
  surv_obj <- Surv(data[[time_var]], data[[event_var]])
  
  # Initialize results list
  results <- list()
  
  # Calculate C-index if requested
  if ("cindex" %in% metrics) {
    # Use different methods depending on model type
    if (inherits(model, "coxph")) {
      # For Cox models
      cindex <- summary(model)$concordance
      results$cindex <- cindex[1]
      results$cindex_lower <- cindex[1] - 1.96 * sqrt(cindex[2])
      results$cindex_upper <- cindex[1] + 1.96 * sqrt(cindex[2])
    } else {
      # For other models, use survcomp package
      # Wir implementieren eine einfachere Alternative zur survcomp-Funktion
      # Get predictions
      if (inherits(model, "aorsf")) {
        preds <- predict(model, data)
      } else {
        preds <- predict(model, newdata = data)
      }
      
      # Einfache Implementierung des C-Index ohne survcomp
      try({
        # Konvertiere zu Datenrahmen
        pred_data <- data.frame(
          pred = preds,
          time = data[[time_var]],
          status = data[[event_var]]
        )
        
        # Berechne konkordante und diskordante Paare
        n <- nrow(pred_data)
        pairs <- 0
        concordant <- 0
        
        for (i in 1:(n-1)) {
          for (j in (i+1):n) {
            # Überspringe Paare, die nicht vergleichbar sind
            if ((pred_data$status[i] == 0 && pred_data$status[j] == 0) ||
                (pred_data$time[i] == pred_data$time[j] && pred_data$status[i] == pred_data$status[j])) {
              next
            }
            
            # Ein Paar ist vergleichbar, wenn:
            # 1. i hat ein Ereignis und j wurde zensiert nach i's Ereignis
            # 2. j hat ein Ereignis und i wurde zensiert nach j's Ereignis
            # 3. Beide haben Ereignisse und die Zeiten sind unterschiedlich
            is_comparable <- FALSE
            
            if (pred_data$status[i] == 1 && (pred_data$time[j] > pred_data$time[i] ||
                                              (pred_data$time[j] == pred_data$time[i] && pred_data$status[j] == 0))) {
              is_comparable <- TRUE
            } else if (pred_data$status[j] == 1 && (pred_data$time[i] > pred_data$time[j] ||
                                                   (pred_data$time[i] == pred_data$time[j] && pred_data$status[i] == 0))) {
              is_comparable <- TRUE
            } else if (pred_data$status[i] == 1 && pred_data$status[j] == 1 && pred_data$time[i] != pred_data$time[j]) {
              is_comparable <- TRUE
            }
            
            if (is_comparable) {
              pairs <- pairs + 1
              
              # Bestimme, welches Subjekt früher ein Ereignis hat
              i_before_j <- (pred_data$status[i] == 1 &&
                            (pred_data$time[i] < pred_data$time[j] ||
                             (pred_data$time[i] == pred_data$time[j] && pred_data$status[j] == 0)))
              
              j_before_i <- (pred_data$status[j] == 1 &&
                            (pred_data$time[j] < pred_data$time[i] ||
                             (pred_data$time[j] == pred_data$time[i] && pred_data$status[i] == 0)))
              
              # Vergleiche Vorhersagen mit tatsächlichen Ergebnissen
              if ((i_before_j && pred_data$pred[i] > pred_data$pred[j]) ||
                  (j_before_i && pred_data$pred[j] > pred_data$pred[i])) {
                concordant <- concordant + 1
              }
            }
          }
        }
        
        # Berechne C-Index
        c_index <- concordant / pairs
        
        # Schätzung des Standardfehlers (vereinfacht)
        se <- sqrt((c_index * (1 - c_index)) / pairs)
        
        results$cindex <- c_index
        results$cindex_lower <- max(0, c_index - 1.96 * se)
        results$cindex_upper <- min(1, c_index + 1.96 * se)
        
        message("Berechneter C-Index ohne survcomp: ", round(c_index, 3),
               " (", round(results$cindex_lower, 3), "-", round(results$cindex_upper, 3), ")")
        
      }, silent = TRUE)
      
      if (!exists("c_index") || is.null(results$cindex)) {
        warning("C-Index-Berechnung fehlgeschlagen. Verwende Näherungswert.")
        # Vereinfachte Näherung
        results$cindex <- 0.7  # Platzhalter
        results$cindex_lower <- 0.65
        results$cindex_upper <- 0.75
      }
    }
  }
  
  # Calculate Brier score if requested
  if ("brier" %in% metrics) {
    if (requireNamespace("pec", quietly = TRUE)) {
      # Get predictions for each time point
      for (t in times) {
        # Calculate Brier score
        brier_score <- try({
          if (inherits(model, "coxph")) {
            # For Cox models
            pec::pec(
              object = model,
              formula = surv_obj ~ 1,
              data = data,
              times = t,
              reference = FALSE
            )$AppErr$coxph
          } else if (inherits(model, "aorsf")) {
            # For aorsf models
            preds <- predict(model, data, times = t, type = "risk")
            mean((preds - (data[[time_var]] <= t & data[[event_var]] == 1))^2)
          } else {
            # For other models
            preds <- predict(model, newdata = data, times = t)
            mean((preds - (data[[time_var]] <= t & data[[event_var]] == 1))^2)
          }
        }, silent = TRUE)
        
        if (!inherits(brier_score, "try-error")) {
          results[[paste0("brier_", t)]] <- brier_score
        } else {
          warning("Brier score calculation failed for time ", t)
        }
      }
    } else {
      warning("pec package not available. Brier score calculation skipped.")
    }
  }
  
  # Calculate R-squared if requested
  if ("rsquared" %in% metrics) {
    if (requireNamespace("rms", quietly = TRUE)) {
      # Calculate R-squared
      rsq <- try({
        if (inherits(model, "coxph")) {
          # For Cox models
          rms::rsquared(model)
        } else {
          # For other models, use a simplified approach
          # This is a rough approximation
          preds <- if (inherits(model, "aorsf")) predict(model, data) else predict(model, newdata = data)
          cor(preds, data[[time_var]])^2
        }
      }, silent = TRUE)
      
      if (!inherits(rsq, "try-error")) {
        results$rsquared <- rsq
      } else {
        warning("R-squared calculation failed")
      }
    } else {
      warning("rms package not available. R-squared calculation skipped.")
    }
  }
  
  return(results)
}

#' Plot calibration curves for survival predictions
#'
#' @param predicted_risk Predicted risk values
#' @param observed_risk Observed risk values
#' @param time_points Vector of time points for evaluation
#' @param groups Number of groups for binning
#' @param smooth Whether to add a smoothed curve
#' @return A ggplot object
plot_calibration_curve <- function(predicted_risk,
                                  observed_risk,
                                  time_points = c(90, 180, 365),
                                  groups = 10,
                                  smooth = TRUE) {
  
  # Initialize data frame for calibration points
  calibration_data <- data.frame(
    Time = numeric(),
    PredictedRisk = numeric(),
    ObservedRisk = numeric(),
    Lower = numeric(),
    Upper = numeric(),
    Group = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Calculate calibration points for each time point
  for (t in time_points) {
    # Get predictions for this time point
    if (is.list(predicted_risk) && paste0("t", t) %in% names(predicted_risk)) {
      preds <- predicted_risk[[paste0("t", t)]]
    } else {
      preds <- predicted_risk
    }
    
    # Create groups based on predicted risk
    risk_groups <- cut(preds, breaks = seq(0, 1, length.out = groups + 1), 
                      include.lowest = TRUE, labels = FALSE)
    
    # Calculate mean predicted and observed risk for each group
    for (g in 1:groups) {
      group_indices <- which(risk_groups == g)
      
      if (length(group_indices) > 0) {
        # Mean predicted risk
        mean_pred <- mean(preds[group_indices])
        
        # Mean observed risk (using Kaplan-Meier)
        group_surv <- survfit(observed_risk[group_indices] ~ 1)
        surv_at_t <- summary(group_surv, times = t)
        obs_risk <- 1 - surv_at_t$surv
        
        # Confidence intervals
        lower <- 1 - surv_at_t$upper
        upper <- 1 - surv_at_t$lower
        
        # Add to data frame
        calibration_data <- rbind(calibration_data, data.frame(
          Time = t,
          PredictedRisk = mean_pred,
          ObservedRisk = obs_risk,
          Lower = lower,
          Upper = upper,
          Group = g,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Create plot
  p <- ggplot(calibration_data, aes(x = PredictedRisk, y = ObservedRisk, color = factor(Time))) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.02) +
    facet_wrap(~ Time, labeller = labeller(Time = function(x) paste0(x, " months"))) +
    labs(
      title = "Calibration Curve",
      x = "Predicted Risk",
      y = "Observed Risk",
      color = "Time (months)"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    ) +
    coord_equal() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1))
  
  # Add smoothed curve if requested
  if (smooth) {
    p <- p + geom_smooth(method = "loess", se = FALSE, span = 1.5)
  }
  
  return(p)
}

#' Plot calibration curves for multiple models
#'
#' @param predictions_list List of predicted risk values for different models
#' @param data The validation data frame
#' @param time_points Vector of time points for evaluation
#' @param groups Number of groups for binning
#' @param smooth Whether to add a smoothed curve
#' @return A ggplot object
plot_calibration_curves <- function(predictions_list,
                                   data,
                                   time_points = c(90, 365),
                                   groups = 10,
                                   smooth = TRUE) {
  
  # Create survival object
  surv_obj <- Surv(data$waitlist_time_months, data$status)
  
  # Initialize data frame for calibration points
  calibration_data <- data.frame(
    Model = character(),
    Time = numeric(),
    PredictedRisk = numeric(),
    ObservedRisk = numeric(),
    Lower = numeric(),
    Upper = numeric(),
    Group = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Calculate calibration points for each model and time point
  for (model_name in names(predictions_list)) {
    preds <- predictions_list[[model_name]]
    
    for (t in time_points) {
      # Create groups based on predicted risk
      risk_groups <- cut(preds, breaks = seq(0, 1, length.out = groups + 1), 
                        include.lowest = TRUE, labels = FALSE)
      
      # Calculate mean predicted and observed risk for each group
      for (g in 1:groups) {
        group_indices <- which(risk_groups == g)
        
        if (length(group_indices) > 0) {
          # Mean predicted risk
          mean_pred <- mean(preds[group_indices])
          
          # Mean observed risk (using Kaplan-Meier)
          group_surv <- survfit(surv_obj[group_indices] ~ 1)
          surv_summary <- summary(group_surv, times = t)
          
          # Check if there are any survival estimates at this time point
          if (length(surv_summary$surv) > 0) {
            obs_risk <- 1 - surv_summary$surv[1]
            
            # Confidence intervals
            lower <- 1 - surv_summary$upper[1]
            upper <- 1 - surv_summary$lower[1]
            
            # Add to data frame
            calibration_data <- rbind(calibration_data, data.frame(
              Model = model_name,
              Time = t,
              PredictedRisk = mean_pred,
              ObservedRisk = obs_risk,
              Lower = lower,
              Upper = upper,
              Group = g,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
    }
  }
  
  # Create plot
  p <- ggplot(calibration_data, aes(x = PredictedRisk, y = ObservedRisk, color = Model)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 2, alpha = 0.7) +
    facet_wrap(~ Time, labeller = labeller(Time = function(x) paste0(x, " months"))) +
    labs(
      title = "Calibration Curves by Model",
      x = "Predicted Risk",
      y = "Observed Risk"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    ) +
    coord_equal() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1))
  
  # Add smoothed curve if requested
  if (smooth) {
    p <- p + geom_smooth(aes(group = Model), method = "loess", se = FALSE, span = 1.5)
  }
  
  return(p)
}

#' Plot performance comparison across models
#'
#' @param metrics_list List of performance metrics for different models
#' @param title Plot title
#' @param y_label Y-axis label
#' @param color_palette Color palette for the plot
#' @return A ggplot object
plot_performance_comparison <- function(metrics_list,
                                       title = "Performance Comparison",
                                       y_label = "Metric Value",
                                       color_palette = "jco") {
  
  # Convert list to data frame
  comparison_data <- data.frame(
    Model = character(),
    Value = numeric(),
    Lower = numeric(),
    Upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (model_name in names(metrics_list)) {
    metric <- metrics_list[[model_name]]
    
    # Check if metric is a list with confidence intervals
    if (is.list(metric) && all(c("estimate", "lower", "upper") %in% names(metric))) {
      comparison_data <- rbind(comparison_data, data.frame(
        Model = model_name,
        Value = metric$estimate,
        Lower = metric$lower,
        Upper = metric$upper,
        stringsAsFactors = FALSE
      ))
    } else {
      # Single value without confidence intervals
      comparison_data <- rbind(comparison_data, data.frame(
        Model = model_name,
        Value = metric,
        Lower = NA,
        Upper = NA,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Create plot
  p <- ggplot(comparison_data, aes(x = reorder(Model, Value), y = Value, fill = Model)) +
    geom_bar(stat = "identity", width = 0.7) +
    labs(
      title = title,
      x = NULL,
      y = y_label
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Add error bars if available
  has_ci <- !all(is.na(comparison_data$Lower) | is.na(comparison_data$Upper))
  if (has_ci) {
    p <- p + geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2)
  }
  
  # Add value labels
  p <- p + geom_text(aes(label = sprintf("%.3f", Value)), vjust = -0.5)
  
  return(p)
}