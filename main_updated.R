# 1. LOAD CLEAN DATASET --------------------------------------------------

# Bereinigten Analysedatensatz laden
message("Lade bereinigten Analysedatensatz...")
analysis_data <- readRDS("analysis_dataset_clean.rds")
message(paste0("Analysedatensatz geladen: ", nrow(analysis_data), " Zeilen, ", ncol(analysis_data), " Spalten"))

# Überprüfen der Variablen im Datensatz
message("Verfügbare Variablen im Analysedatensatz:")
print(names(analysis_data))

# Zusammenfassung des Datensatzes
message("Zusammenfassung des Analysedatensatzes:")
print(summary(analysis_data[, c("age", "sex", "bmi", "lab_meld", "status", "waitlist_time_months")]))

# ============================================================================
# LIVER TRANSPLANT WAITLIST SURVIVAL ANALYSIS
# ============================================================================
# Comprehensive survival analysis workflow for liver transplant waitlist data
# with body composition analysis and advanced machine learning models
# 
# Author: Data Science Team
# Date: 24.03.2025
# ============================================================================

# 1. SETUP AND DATA LOADING --------------------------------------------------

# Clear environment and set working directory
rm(list = ls())
options(scipen = 999)  # Turn off scientific notation

# Load required packages
source("src/setup.R")
source("src/data_preprocessing.R")
source("src/exploratory_analysis.R")
source("src/report_generation.R") # Fehlende Quelldatei für save_plots und andere Report-Funktionen
source("src/bc_meld_formula.R") # Fehlende Quelldatei für BC-MELD Funktionen
source("src/survival_analysis.R") # Fehlende Quelldatei für Überlebensanalyse-Funktionen
source("src/ml_models.R") # Fehlende Quelldatei für Machine Learning Modelle

# Load and validate datasets
data_list <- load_datasets(
  full_data_path = "imputed_data_full.csv",
  bca_data_path = "wl_df_with_bca.rds",
  validate = TRUE
)

full_data <- data_list$full_data
bca_data <- data_list$bca_data

# Rename columns in bca_data to match full_data for consistent joining
names(bca_data)[names(bca_data) == "etnr"] <- "etnr_id"
names(bca_data)[names(bca_data) == "date_of_birth"] <- "birth_date"
names(bca_data)[names(bca_data) == "weight"] <- "kg"
names(bca_data)[names(bca_data) == "height"] <- "cm"
names(bca_data)[names(bca_data) == "rh"] <- "rh_factor"
names(bca_data)[names(bca_data) == "primary_diagnosis"] <- "diagnosis_1"
names(bca_data)[names(bca_data) == "date_of_wl"] <- "waitlist_date"
names(bca_data)[names(bca_data) == "todesdatum"] <- "death_date"
names(bca_data)[names(bca_data) == "todesursache"] <- "death_cause"

# Define unique BCA variables to keep
bca_unique_vars <- c("bone", "muscle", "sat", "vat", "imat", "eat", "pat", "tat",
                     "date_of_ct", "acsn_nr", "age")

# Create analysis dataset
analysis_data <- create_analysis_dataset(
  full_data = full_data,
  bca_data = bca_data,
  merge_by = "etnr_id",
  filter_criteria = NULL, # Deaktiviere die Filterung vorerst
  bca_vars = bca_unique_vars
)

# Auf Daten nach 2005 filtern (für alle Analysen)
message("Filtere Datensatz für Analysen: nur Daten nach 2005")
analysis_data <- analysis_data[analysis_data$wl_year > 2005, ]
message("Datensatzgröße nach Filterung: ", nrow(analysis_data), " Patienten")

# Problematische Variablennamen bereinigen
message("Bereinige problematische Variablennamen")
problematic_vars <- c("abdominelle_opâ.s")
analysis_data <- analysis_data[, !names(analysis_data) %in% problematic_vars]

# Umrechnung der Wartezeit von Tagen in Monate
message("Berechne Wartezeit in Monaten statt Tagen")
# Durchschnittliche Monatslänge: 30.44 Tage
analysis_data$waitlist_time_months <- analysis_data$waitlist_time_days / 30.44
message("Neuer Zeitbereich (Monate): ", round(min(analysis_data$waitlist_time_months, na.rm=TRUE), 2),
        " bis ", round(max(analysis_data$waitlist_time_months, na.rm=TRUE), 2))

# Überprüfe auf NaN/NA-Werte in Schlüsselvariablen
key_vars <- c("waitlist_time_months", "status", "lab_meld", "muscle", "sat", "vat")
for (var in key_vars) {
  if (var %in% names(analysis_data)) {
    na_count <- sum(is.na(analysis_data[[var]]))
    if (na_count > 0) {
      message("WARNUNG: ", na_count, " fehlende Werte in Variable '", var, "'")
    }
  }
}

# Split data for training and validation
data_splits <- split_data(
  data = analysis_data,
  prop = 0.7,
  stratify_var = "status",
  seed = 12345
)

train_data <- data_splits$train
validation_data <- data_splits$validation

# 2. EXPLORATORY DATA ANALYSIS -----------------------------------------------

# Generate descriptive statistics
descriptive_stats <- generate_descriptive_stats(
  data = analysis_data,
  group_var = NULL, # Gruppierung vorerst deaktivieren
  vars = c("age", "sex", "bmi", "lab_meld", "muscle", "vat", "sat", "imat", "eat", "pat", "tat")
)

# Create demographic table
demographic_table <- create_descriptive_table(
  stats_df = descriptive_stats,
  caption = "Patient Demographics and Clinical Characteristics",
  digits = 1
)

# Visualize distributions of key variables
bca_distributions <- plot_variable_distributions(
  data = analysis_data,
  vars = c("muscle", "vat", "sat", "imat", "eat", "pat", "tat"),
  plot_type = "density",
  facet_var = "status"
)

# Create correlation matrix
correlation_plot <- plot_correlation_matrix(
  data = analysis_data,
  vars = c("age", "bmi", "lab_meld", "muscle", "vat", "sat", "imat", "eat", "pat", "tat"),
  method = "spearman",
  cluster = TRUE
)

# Plot waitlist numbers by year
waitlist_plot <- plot_waitlist_by_year(
  data = full_data,
  year_var = "wl_year",
  fill_var = "status",
  show_counts = TRUE
)

# Save EDA results
save_plots(
  plots_list = list(
    bca_distributions = bca_distributions,
    correlation_plot = correlation_plot,
    waitlist_plot = waitlist_plot
  ),
  base_path = "results/figures/",
  width = 10,
  height = 8,
  dpi = 300,
  formats = c("png", "pdf")
)

# 3. KAPLAN-MEIER SURVIVAL ANALYSIS ------------------------------------------

# Plot overall survival curve
km_overall_plot <- plot_km_curve(
  data = analysis_data,
  title = "Overall Waitlist Survival",
  time_var = "waitlist_time_months",  # Zeit in Monaten statt Tagen
  event_var = "status",
  risk_table = TRUE,
  conf_int = TRUE
)

# Stratified survival analysis by MELD score categories
analysis_data$meld_cat <- cut(
  analysis_data$lab_meld,
  breaks = c(0, 15, 25, 40),
  labels = c("MELD < 15", "MELD 15-25", "MELD > 25"),
  include.lowest = TRUE
)

# Plot survival curves by MELD category
km_meld_plot <- plot_km_curve(
  data = analysis_data,
  title = "Survival by MELD Score",
  time_var = "waitlist_time_months",  # Zeit in Monaten statt Tagen
  event_var = "status",
  strata_var = "meld_cat",
  risk_table = TRUE,
  conf_int = TRUE,
  palette = "jco"
)

# Plot survival curves by year
km_year_plot <- plot_km_curve(
  data = analysis_data,
  title = "Survival by Year of Waitlist Entry",
  time_var = "waitlist_time_months",  # Zeit in Monaten statt Tagen
  event_var = "status",
  strata_var = "wl_year",
  risk_table = TRUE,
  conf_int = TRUE,
  palette = "nejm"
)

# Create interactive survival curves
interactive_km <- create_interactive_survival_curve(
  km_fit = km_meld_plot$fit,
  title = "Interactive Survival by MELD Score (Months)",  # Aktualisierter Titel
  show_confidence = TRUE,
  data = analysis_data
)

# Save Kaplan-Meier plots
save_plots(
  plots_list = list(
    km_overall = km_overall_plot$plot,  # Angepasste Referenzierung
    km_meld = km_meld_plot$plot,        # Angepasste Referenzierung
    km_year = km_year_plot$plot         # Angepasste Referenzierung
  ),
  base_path = "results/figures/",
  width = 10,
  height = 8,
  dpi = 300,
  formats = c("png", "pdf")
)

# 4. BC-MELD FORMULA IMPLEMENTATION AND VALIDATION ---------------------------

# Berechne BC-MELD-Scores für den gesamten Analysedatensatz
analysis_data <- calculate_bc_meld_original(
  data = analysis_data,
  meld_var = "lab_meld",
  muscle_var = "muscle",
  vat_var = "vat",
  sat_var = "sat",
  tat_var = "tat"
)

# Derive coefficients for improved BC-MELD formula
bc_meld_coefficients <- derive_bc_meld_coefficients(
  data = train_data,
  time_var = "waitlist_time_months",  # Zeit in Monaten statt Tagen
  event_var = "status",
  meld_var = "lab_meld",
  bca_vars = c("muscle", "sat", "vat", "imat", "eat", "pat", "tat"),
  alpha = 0.5,
  nfolds = 10
)

# Calculate improved BC-MELD score
analysis_data <- calculate_bc_meld_improved(
  data = analysis_data,
  coefficients = bc_meld_coefficients,
  meld_var = "lab_meld"
)

# Aufteilung der Daten in Trainings- und Testdaten nach der BC-MELD-Berechnung
# So dass die Scores in beiden Datensätzen vorhanden sind
data_splits <- split_data(
  data = analysis_data,
  prop = 0.7,
  stratify_var = "status",
  seed = 12345
)

train_data <- data_splits$train
validation_data <- data_splits$validation

# Compare MELD formulas
meld_comparison <- compare_meld_formulas(
  data = validation_data,
  formulas = c("lab_meld", "bc_meld_original", "bc_meld_improved"),
  time_var = "waitlist_time_months",  # Zeit in Monaten statt Tagen
  event_var = "status",
  times = c(3, 6, 12)  # Zeit in Monaten: 3, 6, 12 Monate
)

# Create comparison table
meld_comparison_table <- create_model_comparison_table(
  models_list = list(
    "MELD" = "lab_meld",
    "BC-MELD Original" = "bc_meld_original",
    "BC-MELD Improved" = "bc_meld_improved"
  ),
  metrics = c("cindex", "brier", "rsquared"),
  times = c(3, 6, 12),  # Zeit in Monaten: 3, 6, 12 Monate
  data = validation_data
)

# Save BC-MELD results
save_results(
  results_list = list(
    bc_meld_coefficients = bc_meld_coefficients,
    meld_comparison = meld_comparison
  ),
  base_path = "results/",
  prefix = "bc_meld_",
  formats = c("rds", "csv")
)

# 5. ADVANCED ML SURVIVAL MODELS ---------------------------------------------

# Problematische Variablen identifizieren und entfernen
remove_problematic_vars <- function(data) {
  # Identifiziere Variablen mit nicht-standardmäßigen Namen
  problematic_vars <- c("abdominelle_opâ.s")
  
  # Entferne diese Variablen, wenn sie im Datensatz vorhanden sind
  vars_to_remove <- problematic_vars[problematic_vars %in% names(data)]
  if (length(vars_to_remove) > 0) {
    message("Entferne problematische Variablen: ", paste(vars_to_remove, collapse=", "))
    data <- data[, !names(data) %in% vars_to_remove]
  }
  
  return(data)
}

# Bereinige Daten für ML-Modelle
prepare_ml_data <- function(data) {
  # Entferne problematische Variablen
  data <- remove_problematic_vars(data)
  
  # Konvertiere Character-Variablen zu Faktoren
  char_vars <- sapply(data, is.character)
  if (any(char_vars)) {
    char_var_names <- names(data)[char_vars]
    message("Konvertiere die folgenden Character-Variablen zu Faktoren: ",
            paste(char_var_names, collapse=", "))
    
    for (var in char_var_names) {
      data[[var]] <- as.factor(data[[var]])
    }
  }
  
  # Überprüfe auf NaN oder NA-Werte in zentralen Variablen
  main_vars <- c("waitlist_time_months", "status", "age.x", "sex", "bmi",
                "lab_meld", "muscle", "sat", "vat", "imat", "eat", "pat", "tat")
  
  for (var in main_vars) {
    if (var %in% names(data)) {
      na_count <- sum(is.na(data[[var]]))
      if (na_count > 0) {
        message("WARNUNG: ", na_count, " fehlende Werte in Variable '", var, "'")
      }
    } else {
      message("WARNUNG: Variable '", var, "' nicht im Datensatz gefunden")
    }
  }
  
  # Zusätzliche Datenbereinigung für ML-Modelle
  # Entferne Zeilen mit NA in kritischen Variablen
  critical_vars <- c("waitlist_time_months", "status", "age.x", "sex", "bmi", "lab_meld")
  critical_vars <- critical_vars[critical_vars %in% names(data)]
  
  na_rows <- rowSums(is.na(data[, critical_vars, drop=FALSE])) > 0
  if (sum(na_rows) > 0) {
    message("Entferne ", sum(na_rows), " Zeilen mit fehlenden Werten in kritischen Variablen")
    data <- data[!na_rows, ]
  }
  
  # Ausgabe der Variablennamen zur Inspektion
  message("Variablen im Analysedatensatz (erste 20): ",
          paste(head(names(data), 20), collapse=", "))
  message("Gesamtanzahl Variablen: ", ncol(data))
  message("Anzahl Beobachtungen nach Bereinigung: ", nrow(data))
  
  return(data)
}

# Bereite Daten für ML-Modelle vor
train_data_clean <- prepare_ml_data(train_data)
validation_data_clean <- prepare_ml_data(validation_data)

# Fit AORSF model with hyperparameter tuning
aorsf_formula <- "Surv(waitlist_time_months, status) ~ age + factor(sex) + bmi + lab_meld"

# Prüfe, ob alle Formel-Variablen im Datensatz vorhanden sind
formula_vars <- c("waitlist_time_months", "status", "age.x", "sex", "bmi",
                 "lab_meld", "muscle", "sat", "vat", "imat", "eat", "pat", "tat")
missing_vars <- formula_vars[!formula_vars %in% names(train_data_clean)]

if (length(missing_vars) > 0) {
  message("FEHLER: Die folgenden Variablen aus der Formel fehlen im Datensatz: ",
          paste(missing_vars, collapse=", "))
  # Beende den ML-Teil, wenn Variablen fehlen
  message("ML-Analyse wird übersprungen wegen fehlenden Variablen")
} else {
  aorsf_tuned <- tune_aorsf_model(
    data = train_data_clean,
    formula_str = aorsf_formula,
    param_grid = expand.grid(
      n_tree = c(100, 300, 500),
      mtry = c(3, 5, 7),
      n_split = c(10, 25, 50)
    ),
    nfolds = 5,
    seed = 12345
  )
}

aorsf_model <- aorsf_tuned$best_model

# Get variable importance from AORSF model
aorsf_importance <- get_variable_importance(aorsf_model)

# Plot variable importance
vi_plot <- plot_variable_importance(
  importance_values = aorsf_importance,
  title = "Variable Importance from AORSF Model",
  top_n = 10,
  color_palette = "viridis"
)

# Calculate SHAP values for AORSF model
aorsf_shap <- calculate_shap_values(
  model = aorsf_model,
  data = validation_data,
  n_samples = 100,
  model_type = "aorsf"
)

# Plot SHAP summary
shap_plot <- plot_shap_summary(
  shap_values = aorsf_shap,
  data = validation_data,
  top_n = 10,
  plot_type = "beeswarm"
)

# Initialize H2O
h2o.init(nthreads = -1, max_mem_size = "8G")

# Fit H2O survival model
h2o_model <- fit_h2o_survival_model(
  data = train_data,
  time_var = "waitlist_time_months",
  event_var = "status",
  predictor_vars = c("age", "sex", "bmi", "lab_meld", "muscle", "sat", "vat", "imat", "eat", "pat", "tat"),
  algorithm = "randomForest",
  ntrees = 500,
  max_depth = 10
)

# Create ensemble model
models_list <- list(
  aorsf = aorsf_model,
  h2o = h2o_model,
  bc_meld = bc_meld_coefficients
)

ensemble_model <- create_ensemble_model(
  models_list = models_list,
  data = train_data,
  optimize_weights = TRUE
)

# Evaluate models on validation data
model_predictions <- list(
  meld = predict_risk(validation_data$lab_meld, model_type = "score"),
  bc_meld_original = predict_risk(validation_data$bc_meld_original, model_type = "score"),
  bc_meld_improved = predict_risk(validation_data$bc_meld_improved, model_type = "score"),
  aorsf = predict(aorsf_model, validation_data),
  h2o = h2o.predict(h2o_model, as.h2o(validation_data))$predict,
  ensemble = predict_ensemble(ensemble_model, validation_data)
)

# Calculate performance metrics for all models
performance_metrics <- lapply(model_predictions, function(pred) {
  calculate_survival_metrics(
    predictions = pred,
    data = validation_data,
    time_var = "waitlist_time_months",
    event_var = "status",
    times = c(3, 6, 12),  # Zeit in Monaten statt Tagen
    metrics = c("brier", "cindex", "rsquared")
  )
})

# Create comparison table
model_comparison_table <- create_model_comparison_table(
  models_list = performance_metrics,
  metrics = c("cindex", "brier", "rsquared"),
  times = c(3, 6, 12)  # Zeit in Monaten statt Tagen
)

# Plot C-index comparison
cindex_plot <- plot_performance_comparison(
  metrics_list = lapply(performance_metrics, function(x) x$cindex),
  title = "C-Index Comparison",
  y_label = "C-Index",
  color_palette = "jco"
)

# Plot Brier score comparison
brier_plot <- plot_performance_comparison(
  metrics_list = lapply(performance_metrics, function(x) x$brier_3),
  title = "3-Month Brier Score Comparison",
  y_label = "Brier Score (lower is better)",
  color_palette = "jco"
)

# Create calibration curves
calibration_plot <- plot_calibration_curves(
  predictions_list = model_predictions,
  data = validation_data,
  time_points = c(3, 12),  # Zeit in Monaten statt Tagen
  groups = 10,
  smooth = TRUE
)

# Save ML model results
save_results(
  results_list = list(
    aorsf_model = aorsf_model,
    h2o_model = h2o_model,
    ensemble_model = ensemble_model,
    performance_metrics = performance_metrics
  ),
  base_path = "results/",
  prefix = "ml_models_",
  formats = c("rds")
)

save_plots(
  plots_list = list(
    vi_plot = vi_plot,
    shap_plot = shap_plot,
    cindex_plot = cindex_plot,
    brier_plot = brier_plot,
    calibration_plot = calibration_plot
  ),
  base_path = "results/figures/",
  width = 10,
  height = 8,
  dpi = 300,
  formats = c("png", "pdf")
)

# Shutdown H2O
h2o.shutdown(prompt = FALSE)

# 6. REPORT GENERATION -------------------------------------------------------

# Generate comprehensive HTML report
report_path <- generate_html_report(
  template_path = "src/report_template.Rmd",
  output_file = "Liver_Transplant_Survival_Analysis_Report.html",
  output_dir = "reports",
  params = list(
    data_path = "./",
    results_path = "results/",
    figures_path = "results/figures/",
    models = models_list,
    performance = performance_metrics
  )
)

# Print completion message
cat("\nAnalysis complete. Results saved in 'results/' directory.\n")
cat("HTML report generated:", report_path, "\n")
