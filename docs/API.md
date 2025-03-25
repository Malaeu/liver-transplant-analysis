# API Reference

This document provides detailed documentation for all functions and classes implemented in the liver transplant waitlist survival analysis project.

## Table of Contents

- [Data Processing Functions](#data-processing-functions)
- [Exploratory Analysis Functions](#exploratory-analysis-functions)
- [Survival Analysis Functions](#survival-analysis-functions)
- [BC-MELD Formula Functions](#bc-meld-formula-functions)
- [Machine Learning Model Functions](#machine-learning-model-functions)
- [Visualization Functions](#visualization-functions)
- [Utility Functions](#utility-functions)
- [Report Generation Functions](#report-generation-functions)

## Data Processing Functions

### load_datasets

Loads and validates the datasets used in the analysis.

```r
load_datasets(
  full_data_path = "data/imputed_data_full.csv",
  bca_data_path = "data/wl_df_with_bca.rds",
  validate = TRUE
)
```

**Parameters:**
- `full_data_path`: Path to the full dataset CSV file
- `bca_data_path`: Path to the BCA dataset RDS file
- `validate`: Whether to perform validation checks on the data

**Returns:**
- A list containing two data frames:
  - `full_data`: The full dataset with MELD scores and survival data
  - `bca_data`: The subset with body composition analysis data

**Example:**
```r
datasets <- load_datasets()
full_data <- datasets$full_data
bca_data <- datasets$bca_data
```

### validate_data

Performs validation checks on the datasets.

```r
validate_data(
  full_data,
  bca_data,
  check_negative = TRUE,
  check_consistency = TRUE
)
```

**Parameters:**
- `full_data`: The full dataset data frame
- `bca_data`: The BCA dataset data frame
- `check_negative`: Whether to check for negative values in body composition variables
- `check_consistency`: Whether to check for consistency between datasets

**Returns:**
- A list containing validation results:
  - `valid`: Logical indicating whether the data passed all checks
  - `issues`: Data frame of identified issues
  - `summary`: Text summary of validation results

**Example:**
```r
validation <- validate_data(full_data, bca_data)
if (!validation$valid) {
  print(validation$issues)
}
```

### preprocess_data

Preprocesses the data for analysis.

```r
preprocess_data(
  data,
  impute_missing = TRUE,
  normalize_bca = TRUE,
  create_derived_vars = TRUE
)
```

**Parameters:**
- `data`: The input data frame
- `impute_missing`: Whether to impute missing values
- `normalize_bca`: Whether to normalize body composition variables
- `create_derived_vars`: Whether to create derived variables

**Returns:**
- A preprocessed data frame

**Example:**
```r
preprocessed_data <- preprocess_data(bca_data)
```

### create_analysis_dataset

Creates the final analysis dataset by merging and preprocessing data.

```r
create_analysis_dataset(
  full_data,
  bca_data,
  merge_by = "etnr_id",
  filter_criteria = NULL
)
```

**Parameters:**
- `full_data`: The full dataset data frame
- `bca_data`: The BCA dataset data frame
- `merge_by`: The column name to use for merging datasets
- `filter_criteria`: Optional expression for filtering the merged dataset

**Returns:**
- A merged and preprocessed data frame ready for analysis

**Example:**
```r
analysis_data <- create_analysis_dataset(
  full_data,
  bca_data,
  filter_criteria = quote(age >= 18 & !is.na(lab_meld))
)
```

## Exploratory Analysis Functions

### generate_descriptive_stats

Generates descriptive statistics for the dataset.

```r
generate_descriptive_stats(
  data,
  group_var = NULL,
  vars = NULL,
  include_pvalues = FALSE
)
```

**Parameters:**
- `data`: The input data frame
- `group_var`: Optional grouping variable for stratified statistics
- `vars`: Vector of variable names to include (NULL for all)
- `include_pvalues`: Whether to include p-values for group comparisons

**Returns:**
- A data frame of descriptive statistics

**Example:**
```r
stats <- generate_descriptive_stats(
  data = analysis_data,
  group_var = "status",
  vars = c("age", "sex", "bmi", "lab_meld", "muscle", "vat")
)
```

### plot_variable_distributions

Creates histograms or density plots for continuous variables.

```r
plot_variable_distributions(
  data,
  vars,
  plot_type = "histogram",
  facet_var = NULL,
  bins = 30
)
```

**Parameters:**
- `data`: The input data frame
- `vars`: Vector of variable names to plot
- `plot_type`: Type of plot ("histogram" or "density")
- `facet_var`: Optional variable for faceting
- `bins`: Number of bins for histograms

**Returns:**
- A ggplot object

**Example:**
```r
plot_variable_distributions(
  data = analysis_data,
  vars = c("lab_meld", "muscle", "vat", "sat"),
  plot_type = "density",
  facet_var = "status"
)
```

### plot_correlation_matrix

Creates a correlation matrix heatmap.

```r
plot_correlation_matrix(
  data,
  vars = NULL,
  method = "spearman",
  cluster = TRUE
)
```

**Parameters:**
- `data`: The input data frame
- `vars`: Vector of variable names to include (NULL for all numeric)
- `method`: Correlation method ("pearson", "spearman", or "kendall")
- `cluster`: Whether to cluster variables by similarity

**Returns:**
- A ggplot object

**Example:**
```r
plot_correlation_matrix(
  data = analysis_data,
  vars = c("age", "bmi", "lab_meld", "muscle", "vat", "sat"),
  method = "spearman"
)
```

### plot_waitlist_by_year

Creates a bar chart of waitlist numbers by year.

```r
plot_waitlist_by_year(
  data,
  year_var = "wl_year",
  fill_var = NULL,
  show_counts = TRUE
)
```

**Parameters:**
- `data`: The input data frame
- `year_var`: Name of the year variable
- `fill_var`: Optional variable for fill color
- `show_counts`: Whether to display count labels on bars

**Returns:**
- A ggplot object

**Example:**
```r
plot_waitlist_by_year(
  data = full_data,
  fill_var = "status"
)
```

## Survival Analysis Functions

### fit_km_model

Fits a Kaplan-Meier survival model.

```r
fit_km_model(
  data,
  time_var = "waitlist_time_days",
  event_var = "status",
  strata_var = NULL
)
```

**Parameters:**
- `data`: The input data frame
- `time_var`: Name of the time variable
- `event_var`: Name of the event variable (1=event, 0=censored)
- `strata_var`: Optional stratification variable

**Returns:**
- A survfit object

**Example:**
```r
km_fit <- fit_km_model(
  data = analysis_data,
  strata_var = "diagnosis_group"
)
```

### plot_km_curve

Creates a Kaplan-Meier survival curve plot.

```r
plot_km_curve(
  km_fit,
  data = NULL,
  title = "Kaplan-Meier Survival Curve",
  risk_table = TRUE,
  conf_int = TRUE,
  palette = "jco"
)
```

**Parameters:**
- `km_fit`: A survfit object from fit_km_model
- `data`: The data frame used to fit the model
- `title`: Plot title
- `risk_table`: Whether to include a risk table
- `conf_int`: Whether to show confidence intervals
- `palette`: Color palette for the plot

**Returns:**
- A ggsurvplot object

**Example:**
```r
plot_km_curve(
  km_fit = km_fit,
  data = analysis_data,
  title = "Survival by Diagnosis Group"
)
```

### fit_cox_model

Fits a Cox proportional hazards model.

```r
fit_cox_model(
  data,
  formula_str,
  time_var = "waitlist_time_days",
  event_var = "status",
  robust = FALSE
)
```

**Parameters:**
- `data`: The input data frame
- `formula_str`: Model formula as a string
- `time_var`: Name of the time variable
- `event_var`: Name of the event variable
- `robust`: Whether to use robust standard errors

**Returns:**
- A coxph object

**Example:**
```r
cox_fit <- fit_cox_model(
  data = analysis_data,
  formula_str = "Surv(waitlist_time_days, status) ~ age + sex + lab_meld + muscle + vat"
)
```

### check_ph_assumption

Checks the proportional hazards assumption for a Cox model.

```r
check_ph_assumption(
  cox_model,
  plot = TRUE
)
```

**Parameters:**
- `cox_model`: A coxph object from fit_cox_model
- `plot`: Whether to create diagnostic plots

**Returns:**
- A cox.zph object with test results

**Example:**
```r
ph_test <- check_ph_assumption(cox_fit)
print(ph_test)
```

### calculate_survival_metrics

Calculates performance metrics for survival models.

```r
calculate_survival_metrics(
  model,
  data,
  time_var = "waitlist_time_days",
  event_var = "status",
  times = c(90, 180, 365),
  metrics = c("brier", "cindex", "rsquared")
)
```

**Parameters:**
- `model`: A fitted survival model
- `data`: The validation data frame
- `time_var`: Name of the time variable
- `event_var`: Name of the event variable
- `times`: Vector of time points for evaluation
- `metrics`: Vector of metrics to calculate

**Returns:**
- A list of performance metrics

**Example:**
```r
metrics <- calculate_survival_metrics(
  model = cox_fit,
  data = validation_data,
  times = c(90, 180, 365)
)
```

## BC-MELD Formula Functions

### calculate_bc_meld_original

Calculates the original BC-MELD score.

```r
calculate_bc_meld_original(
  data,
  meld_var = "lab_meld",
  muscle_var = "muscle",
  vat_var = "vat",
  sat_var = "sat",
  tat_var = "tat"
)
```

**Parameters:**
- `data`: The input data frame
- `meld_var`: Name of the MELD score variable
- `muscle_var`: Name of the muscle area variable
- `vat_var`: Name of the visceral adipose tissue variable
- `sat_var`: Name of the subcutaneous adipose tissue variable
- `tat_var`: Name of the total adipose tissue variable

**Returns:**
- A data frame with the original BC-MELD score added

**Example:**
```r
data_with_bc_meld <- calculate_bc_meld_original(bca_data)
```

### derive_bc_meld_coefficients

Derives coefficients for an improved BC-MELD formula.

```r
derive_bc_meld_coefficients(
  data,
  time_var = "waitlist_time_days",
  event_var = "status",
  meld_var = "lab_meld",
  bca_vars = c("muscle", "sat", "vat", "imat", "eat", "pat", "tat"),
  alpha = 0.5,
  nfolds = 10
)
```

**Parameters:**
- `data`: The input data frame
- `time_var`: Name of the time variable
- `event_var`: Name of the event variable
- `meld_var`: Name of the MELD score variable
- `bca_vars`: Vector of body composition variable names
- `alpha`: Elastic net mixing parameter (0=ridge, 1=lasso)
- `nfolds`: Number of cross-validation folds

**Returns:**
- A named vector of coefficients

**Example:**
```r
coefficients <- derive_bc_meld_coefficients(
  data = analysis_data,
  alpha = 0.7
)
```

### calculate_bc_meld_improved

Calculates the improved BC-MELD score.

```r
calculate_bc_meld_improved(
  data,
  coefficients,
  meld_var = "lab_meld"
)
```

**Parameters:**
- `data`: The input data frame
- `coefficients`: Named vector of coefficients from derive_bc_meld_coefficients
- `meld_var`: Name of the MELD score variable

**Returns:**
- A data frame with the improved BC-MELD score added

**Example:**
```r
data_with_improved_bc_meld <- calculate_bc_meld_improved(
  data = analysis_data,
  coefficients = coefficients
)
```

### compare_meld_formulas

Compares the performance of different MELD formulas.

```r
compare_meld_formulas(
  data,
  formulas = c("lab_meld", "bc_meld_original", "bc_meld_improved"),
  time_var = "waitlist_time_days",
  event_var = "status",
  times = c(90, 180, 365)
)
```

**Parameters:**
- `data`: The input data frame
- `formulas`: Vector of formula variable names to compare
- `time_var`: Name of the time variable
- `event_var`: Name of the event variable
- `times`: Vector of time points for evaluation

**Returns:**
- A data frame of performance metrics for each formula

**Example:**
```r
comparison <- compare_meld_formulas(
  data = analysis_data,
  formulas = c("lab_meld", "bc_meld_original", "bc_meld_improved")
)
```

## Machine Learning Model Functions

### fit_aorsf_model

Fits an Accelerated Oblique Random Survival Forest model.

```r
fit_aorsf_model(
  data,
  formula_str,
  n_tree = 500,
  importance = "permute",
  n_split = 25,
  split_min_events = 5
)
```

**Parameters:**
- `data`: The input data frame
- `formula_str`: Model formula as a string
- `n_tree`: Number of trees in the forest
- `importance`: Method for variable importance
- `n_split`: Number of random splits to consider
- `split_min_events`: Minimum number of events in a node to split

**Returns:**
- An aorsf object

**Example:**
```r
aorsf_fit <- fit_aorsf_model(
  data = analysis_data,
  formula_str = "Surv(waitlist_time_days, status) ~ age + sex + lab_meld + muscle + vat + sat + imat"
)
```

### tune_aorsf_model

Performs hyperparameter tuning for an AORSF model.

```r
tune_aorsf_model(
  data,
  formula_str,
  param_grid = NULL,
  nfolds = 5,
  seed = 123
)
```

**Parameters:**
- `data`: The input data frame
- `formula_str`: Model formula as a string
- `param_grid`: Data frame of parameter combinations to try
- `nfolds`: Number of cross-validation folds
- `seed`: Random seed for reproducibility

**Returns:**
- A list containing:
  - `best_model`: The best AORSF model
  - `best_params`: The best hyperparameters
  - `results`: Performance results for all parameter combinations

**Example:**
```r
tuned_aorsf <- tune_aorsf_model(
  data = analysis_data,
  formula_str = "Surv(waitlist_time_days, status) ~ age + sex + lab_meld + muscle + vat + sat + imat"
)
```

### fit_h2o_survival_model

Fits an H2O distributed survival model.

```r
fit_h2o_survival_model(
  data,
  time_var = "waitlist_time_days",
  event_var = "status",
  predictor_vars = NULL,
  algorithm = "randomForest",
  ntrees = 500,
  max_depth = 10
)
```

**Parameters:**
- `data`: The input data frame
- `time_var`: Name of the time variable
- `event_var`: Name of the event variable
- `predictor_vars`: Vector of predictor variable names
- `algorithm`: H2O algorithm to use
- `ntrees`: Number of trees (for tree-based methods)
- `max_depth`: Maximum tree depth (for tree-based methods)

**Returns:**
- An H2O model object

**Example:**
```r
h2o.init()
h2o_model <- fit_h2o_survival_model(
  data = analysis_data,
  predictor_vars = c("age", "sex", "lab_meld", "muscle", "vat", "sat")
)
```

### tune_h2o_survival_model

Performs hyperparameter tuning for an H2O survival model.

```r
tune_h2o_survival_model(
  data,
  time_var = "waitlist_time_days",
  event_var = "status",
  predictor_vars = NULL,
  algorithm = "randomForest",
  hyper_params = NULL,
  max_models = 20
)
```

**Parameters:**
- `data`: The input data frame
- `time_var`: Name of the time variable
- `event_var`: Name of the event variable
- `predictor_vars`: Vector of predictor variable names
- `algorithm`: H2O algorithm to use
- `hyper_params`: List of hyperparameter grids
- `max_models`: Maximum number of models to train

**Returns:**
- The best H2O model

**Example:**
```r
tuned_h2o_model <- tune_h2o_survival_model(
  data = analysis_data,
  predictor_vars = c("age", "sex", "lab_meld", "muscle", "vat", "sat"),
  max_models = 30
)
```

### create_ensemble_model

Creates an ensemble model from multiple survival models.

```r
create_ensemble_model(
  models_list,
  data,
  weights = NULL,
  optimize_weights = TRUE
)
```

**Parameters:**
- `models_list`: List of fitted survival models
- `data`: The input data frame
- `weights`: Optional vector of model weights
- `optimize_weights`: Whether to optimize weights based on performance

**Returns:**
- An ensemble model object

**Example:**
```r
ensemble_model <- create_ensemble_model(
  models_list = list(
    cox = cox_fit,
    aorsf = aorsf_fit,
    h2o = h2o_model
  ),
  data = analysis_data
)
```

### calculate_shap_values

Calculates SHAP values for model interpretation.

```r
calculate_shap_values(
  model,
  data,
  n_samples = 100,
  model_type = c("aorsf", "h2o", "cox")
)
```

**Parameters:**
- `model`: A fitted model object
- `data`: The input data frame
- `n_samples`: Number of samples for SHAP approximation
- `model_type`: Type of model

**Returns:**
- A matrix of SHAP values

**Example:**
```r
shap_values <- calculate_shap_values(
  model = aorsf_fit,
  data = analysis_data,
  model_type = "aorsf"
)
```

## Visualization Functions

### plot_variable_importance

Creates a variable importance plot.

```r
plot_variable_importance(
  importance_values,
  title = "Variable Importance",
  top_n = NULL,
  color_palette = "viridis"
)
```

**Parameters:**
- `importance_values`: Named vector of variable importance values
- `title`: Plot title
- `top_n`: Number of top variables to include
- `color_palette`: Color palette for the plot

**Returns:**
- A ggplot object

**Example:**
```r
plot_variable_importance(
  importance_values = get_variable_importance(aorsf_fit),
  top_n = 10
)
```

### plot_shap_summary

Creates a SHAP summary plot.

```r
plot_shap_summary(
  shap_values,
  data,
  top_n = 10,
  plot_type = "bar"
)
```

**Parameters:**
- `shap_values`: Matrix of SHAP values
- `data`: The input data frame
- `top_n`: Number of top variables to include
- `plot_type`: Type of plot ("bar", "beeswarm", or "violin")

**Returns:**
- A ggplot object

**Example:**
```r
plot_shap_summary(
  shap_values = shap_values,
  data = analysis_data,
  plot_type = "beeswarm"
)
```

### plot_partial_dependence

Creates partial dependence plots for selected variables.

```r
plot_partial_dependence(
  model,
  data,
  vars,
  grid_resolution = 20,
  ice = FALSE,
  center = TRUE
)
```

**Parameters:**
- `model`: A fitted model object
- `data`: The input data frame
- `vars`: Vector of variable names to plot
- `grid_resolution`: Number of grid points
- `ice`: Whether to include individual conditional expectation curves
- `center`: Whether to center the curves

**Returns:**
- A list of ggplot objects

**Example:**
```r
pdp_plots <- plot_partial_dependence(
  model = aorsf_fit,
  data = analysis_data,
  vars = c("age", "lab_meld", "muscle")
)
```

### plot_calibration_curve

Creates a calibration curve for survival predictions.

```r
plot_calibration_curve(
  predicted_risk,
  observed_risk,
  time_points = c(90, 180, 365),
  groups = 10,
  smooth = TRUE
)
```

**Parameters:**
- `predicted_risk`: Predicted risk values
- `observed_risk`: Observed risk values
- `time_points`: Vector of time points for evaluation
- `groups`: Number of groups for binning
- `smooth`: Whether to add a smoothed curve

**Returns:**
- A ggplot object

**Example:**
```r
plot_calibration_curve(
  predicted_risk = predict(aorsf_fit, newdata = validation_data),
  observed_risk = Surv(validation_data$waitlist_time_days, validation_data$status),
  time_points = c(90, 180, 365)
)
```

### create_interactive_survival_curve

Creates an interactive survival curve using plotly.

```r
create_interactive_survival_curve(
  km_fit,
  title = "Interactive Survival Curve",
  show_confidence = TRUE
)
```

**Parameters:**
- `km_fit`: A survfit object from fit_km_model
- `title`: Plot title
- `show_confidence`: Whether to show confidence intervals

**Returns:**
- A plotly object

**Example:**
```r
interactive_curve <- create_interactive_survival_curve(
  km_fit = km_fit,
  title = "Interactive Survival by Diagnosis Group"
)
```

## Utility Functions

### split_data

Splits data into training and validation sets.

```r
split_data(
  data,
  prop = 0.7,
  stratify_var = NULL,
  seed = 123
)
```

**Parameters:**
- `data`: The input data frame
- `prop`: Proportion of data for training
- `stratify_var`: Optional variable for stratified sampling
- `seed`: Random seed for reproducibility

**Returns:**
- A list containing:
  - `train`: Training data frame
  - `validation`: Validation data frame

**Example:**
```r
splits <- split_data(
  data = analysis_data,
  prop = 0.8,
  stratify_var = "status"
)
```

### create_folds

Creates cross-validation folds.

```r
create_folds(
  data,
  k = 5,
  stratify_var = NULL,
  seed = 123
)
```

**Parameters:**
- `data`: The input data frame
- `k`: Number of folds
- `stratify_var`: Optional variable for stratified sampling
- `seed`: Random seed for reproducibility

**Returns:**
- A list of indices for each fold

**Example:**
```r
folds <- create_folds(
  data = analysis_data,
  k = 10,
  stratify_var = "status"
)
```

### bootstrap_sample

Creates bootstrap samples for confidence intervals.

```r
bootstrap_sample(
  data,
  n_boot = 1000,
  seed = 123
)
```

**Parameters:**
- `data`: The input data frame
- `n_boot`: Number of bootstrap samples
- `seed`: Random seed for reproducibility

**Returns:**
- A list of bootstrap sample indices

**Example:**
```r
boot_samples <- bootstrap_sample(
  data = analysis_data,
  n_boot = 500
)
```

### calculate_bootstrap_ci

Calculates bootstrap confidence intervals.

```r
calculate_bootstrap_ci(
  statistic_func,
  boot_samples,
  data,
  alpha = 0.05,
  method = "percentile"
)
```

**Parameters:**
- `statistic_func`: Function to calculate the statistic
- `boot_samples`: List of bootstrap sample indices
- `data`: The input data frame
- `alpha`: Significance level
- `method`: Method for confidence interval calculation

**Returns:**
- A list containing:
  - `estimate`: Point estimate
  - `lower`: Lower confidence bound
  - `upper`: Upper confidence bound

**Example:**
```r
ci <- calculate_bootstrap_ci(
  statistic_func = function(d) mean(d$lab_meld, na.rm = TRUE),
  boot_samples = boot_samples,
  data = analysis_data
)
```

## Report Generation Functions

### create_descriptive_table

Creates a formatted table of descriptive statistics.

```r
create_descriptive_table(
  stats_df,
  caption = "Descriptive Statistics",
  digits = 1
)
```

**Parameters:**
- `stats_df`: Data frame of descriptive statistics
- `caption`: Table caption
- `digits`: Number of decimal places

**Returns:**
- A formatted table object

**Example:**
```r
table <- create_descriptive_table(
  stats_df = generate_descriptive_stats(analysis_data),
  digits = 2
)
```

### create_model_comparison_table

Creates a formatted table comparing model performance.

```r
create_model_comparison_table(
  models_list,
  metrics = c("brier", "cindex", "rsquared"),
  times = c(90, 180, 365),
  data = NULL
)
```

**Parameters:**
- `models_list`: Named list of fitted models
- `metrics`: Vector of metrics to include
- `times`: Vector of time points for evaluation
- `data`: Optional validation data frame

**Returns:**
- A formatted table object

**Example:**
```r
comparison_table <- create_model_comparison_table(
  models_list = list(
    "MELD" = meld_model,
    "BC-MELD" = bc_meld_model,
    "AORSF" = aorsf_model
  ),
  data = validation_data
)
```

### generate_html_report

Generates a comprehensive HTML report.

```r
generate_html_report(
  template_path = "src/report_template.Rmd",
  output_file = "Liver_Transplant_Survival_Analysis_Report.html",
  output_dir = "reports",
  params = list()
)
```

**Parameters:**
- `template_path`: Path to the R Markdown template
- `output_file`: Name of the output HTML file
- `output_dir`: Directory for the output file
- `params`: List of parameters to pass to the template

**Returns:**
- Path to the generated HTML report

**Example:**
```r
report_path <- generate_html_report(
  params = list(
    data_path = "data/",
    results_path = "results/",
    models = models_list
  )
)
```

### save_results

Saves analysis results to files.

```r
save_results(
  results_list,
  base_path = "results/",
  prefix = "",
  formats = c("rds", "csv")
)
```

**Parameters:**
- `results_list`: Named list of results to save
- `base_path`: Base directory for saving files
- `prefix`: Prefix for file names
- `formats`: Vector of file formats to save

**Returns:**
- A list of saved file paths

**Example:**
```r
saved_files <- save_results(
  results_list = list(
    descriptive_stats = stats_df,
    model_comparison = comparison_df,
    coefficients = coefficients
  ),
  prefix = "analysis_"
)
```

### save_plots

Saves plots to files.

```r
save_plots(
  plots_list,
  base_path = "results/figures/",
  width = 8,
  height = 6,
  dpi = 300,
  formats = c("png", "pdf")
)
```

**Parameters:**
- `plots_list`: Named list of ggplot objects
- `base_path`: Base directory for saving files
- `width`: Plot width in inches
- `height`: Plot height in inches
- `dpi`: Resolution in dots per inch
- `formats`: Vector of file formats to save

**Returns:**
- A list of saved file paths

**Example:**
```r
saved_plots <- save_plots(
  plots_list = list(
    km_curve = km_plot,
    variable_importance = vi_plot,
    calibration = cal_plot
  ),
  width = 10,
  height = 8
)
```

All functions are thoroughly documented in the source code with additional details and examples.