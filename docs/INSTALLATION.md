# Installation Guide

This document provides detailed instructions for setting up the environment required to run the liver transplant waitlist survival analysis.

## Table of Contents

- [Prerequisites](#prerequisites)
- [R and RStudio Installation](#r-and-rstudio-installation)
- [Required Packages](#required-packages)
- [Project Setup](#project-setup)
- [Data Preparation](#data-preparation)
- [Troubleshooting](#troubleshooting)

## Prerequisites

- R (version 4.2.0 or higher)
- RStudio (recommended, version 2022.02 or higher)
- At least 8GB RAM (16GB recommended for H2O distributed computing)
- Approximately 2GB of free disk space

## R and RStudio Installation

### Installing R

1. Visit the [R Project website](https://www.r-project.org/)
2. Click on "CRAN" in the left sidebar
3. Select a mirror close to your location
4. Choose your operating system (Windows, macOS, or Linux)
5. Download and install the latest version of R

### Installing RStudio

1. Visit the [RStudio download page](https://www.rstudio.com/products/rstudio/download/#download)
2. Download the free RStudio Desktop version for your operating system
3. Install RStudio following the installation wizard

## Required Packages

The analysis requires several R packages. You can install them by running the following script:

```r
# Core packages
install.packages(c(
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
  "htmlwidgets"   # HTML widgets
))

# Advanced packages
install.packages(c(
  "AORSF",        # Accelerated Oblique Random Survival Forests
  "h2o",          # H2O distributed computing
  "fastshap"      # SHAP values for model interpretation
))

# If you encounter issues with AORSF installation, try:
if (!require("AORSF")) {
  install.packages("remotes")
  remotes::install_github("ropensci/AORSF")
}

# Initialize H2O
library(h2o)
h2o.init()
```

## Project Setup

1. Clone or download the project repository
2. Open RStudio and create a new project:
   - File > New Project > Existing Directory
   - Browse to the downloaded repository folder
   - Click "Create Project"

3. Set up the project directory structure (if not already present):

```r
# Create necessary directories
dir.create("data", showWarnings = FALSE)
dir.create("results", showWarnings = FALSE)
dir.create("reports", showWarnings = FALSE)
```

## Data Preparation

1. Place the dataset files in the `data` directory:
   - `imputed_data_full.csv`: Full dataset with MELD scores and survival data
   - `wl_df_with_bca.rds`: Subset with body composition analysis data

2. Run the data validation script to ensure data integrity:

```r
source("src/validate_data.R")
```

This script will check for:
- Missing values
- Negative values in body composition variables
- Inconsistencies between datasets
- Data type issues

## Troubleshooting

### Common Issues

#### H2O Initialization Problems

If you encounter issues initializing H2O:

```r
# Try with explicit memory allocation
h2o.init(max_mem_size = "4g")

# If port conflicts occur
h2o.init(port = 54321)
```

#### Memory Issues with Large Datasets

For systems with limited memory:

```r
# Set R memory limit (adjust as needed)
memory.limit(size = 8000)  # Windows only

# For Unix/Mac systems, start R with increased memory
# In terminal: R --max-mem-size=8G
```

#### Package Installation Errors

If you encounter package installation errors:

1. Ensure your R version is up to date
2. Install package dependencies manually
3. For Linux users, install system dependencies:
   ```bash
   sudo apt-get install libssl-dev libcurl4-openssl-dev libxml2-dev
   ```

### Getting Help

If you encounter persistent issues:

1. Check the [project issues page](https://github.com/username/liver-transplant-analysis/issues)
2. Create a new issue with detailed information about your problem
3. Contact the project maintainer at: example@email.com