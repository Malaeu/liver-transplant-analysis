# ============================================================================
# SETUP SCRIPT
# ============================================================================
# Loads required packages and sets up the environment for the analysis
# ============================================================================

# Create necessary directories if they don't exist
dir.create("results", showWarnings = FALSE)
dir.create("results/figures", showWarnings = FALSE)
dir.create("reports", showWarnings = FALSE)

# Function to install and load required packages
load_packages <- function() {
  # List of required packages
  packages <- c(
    # Core packages
    "tidyverse",    # Data manipulation and visualization
    "survival",     # Survival analysis
    "survminer",    # Survival curve visualization
    "glmnet",       # Regularized regression
    "rms",          # Regression modeling strategies
    "pec",          # Prediction error curves
    "timeROC",      # Time-dependent ROC curves
    "randomForestSRC", # Random survival forests
    "ggplot2",      # Visualization
    "viridis",      # Color palettes
    "knitr",        # Report generation
    "rmarkdown",    # R Markdown
    "DT",           # Interactive tables
    "plotly",       # Interactive plots
    "htmlwidgets",  # HTML widgets
    
    # Advanced packages
    # "aorsf" wird separat behandelt (siehe unten)
    "h2o",          # H2O distributed computing
    "fastshap",     # SHAP values for model interpretation
    
    # Utility packages
    "data.table",   # Fast data manipulation
    "mice",         # Multiple imputation
    "corrplot",     # Correlation plots
    "gridExtra",    # Arranging multiple plots
    "cowplot",      # Plot composition
    "scales",       # Scale functions for visualization
    "gtsummary",    # Summary tables
    "flextable"     # Formatted tables
  )
  
  # Check which packages are not installed
  not_installed <- packages[!packages %in% installed.packages()[,"Package"]]
  
  # Install missing packages
  if(length(not_installed) > 0) {
    message("Installing the following packages: ", paste(not_installed, collapse = ", "))
    install.packages(not_installed)
  }
  
  # Load all packages
  invisible(lapply(packages, library, character.only = TRUE))
  
  # Special handling for aorsf
  if(!"aorsf" %in% installed.packages()[,"Package"]) {
    message("Installing aorsf from GitHub")
    if(!require("remotes")) install.packages("remotes")
    remotes::install_github("ropensci/aorsf")
    library(aorsf)
  } else {
    # Wenn aorsf bereits installiert ist, lade es einfach
    library(aorsf)
  }
  
  message("All required packages loaded successfully")
}

# Set global options
set_global_options <- function() {
  # Set seed for reproducibility
  set.seed(12345)
  
  # Turn off scientific notation
  options(scipen = 999)
  
  # Set default theme for ggplot2
  theme_set(theme_minimal() +
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
              ))
  
  # Set knitr options
  knitr::opts_chunk$set(
    echo = FALSE,
    warning = FALSE,
    message = FALSE,
    fig.width = 10,
    fig.height = 8,
    dpi = 300
  )
  
  message("Global options set successfully")
}

# Load packages and set options
load_packages()
set_global_options()

# Print system information
cat("R version:", R.version.string, "\n")
cat("Working directory:", getwd(), "\n")
cat("Analysis date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")