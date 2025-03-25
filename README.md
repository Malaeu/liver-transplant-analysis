# Liver Transplant Waitlist Survival Analysis

## Overview

This project implements a comprehensive survival analysis workflow for liver transplant waitlist data, with a focus on improving survival prediction using body composition analysis (BCA) and advanced machine learning models.

The analysis aims to develop and validate an improved BC-MELD formula that outperforms the traditional MELD score in predicting waitlist mortality. It also implements advanced machine learning survival models, including Accelerated Oblique Random Survival Forests (AORSF) and H2O distributed computing.

## Features

- **Data Validation**: Comprehensive checks for data quality and consistency
- **Exploratory Data Analysis**: Descriptive statistics and visualizations of patient characteristics
- **Kaplan-Meier Survival Analysis**: Overall and stratified survival analysis with publication-quality visualizations
- **BC-MELD Formula**: Implementation and validation of the original and improved BC-MELD formulas
- **Advanced ML Models**: AORSF, H2O distributed computing, and ensemble models
- **Model Interpretation**: SHAP values and variable importance for model interpretation
- **Report Generation**: Comprehensive HTML report with embedded visualizations

## Project Structure

```
.
├── main.R                  # Main script that runs the entire workflow
├── README.md               # Project documentation
├── data/                   # Data directory (not included in repository)
│   ├── imputed_data_full.csv  # Full dataset with MELD scores and survival data
│   └── wl_df_with_bca.rds     # Subset with body composition analysis data
├── src/                    # Source code
│   ├── setup.R                # Environment setup and package loading
│   ├── data_preprocessing.R   # Data loading, validation, and preprocessing
│   ├── exploratory_analysis.R # Descriptive statistics and visualizations
│   ├── survival_analysis.R    # Kaplan-Meier and Cox proportional hazards models
│   ├── bc_meld_formula.R      # BC-MELD formula implementation and validation
│   ├── ml_models.R            # AORSF, H2O, and ensemble models
│   ├── report_generation.R    # HTML report generation
│   └── report_template.Rmd    # R Markdown template for the report
├── results/                # Results directory (created by the scripts)
│   ├── figures/               # Saved visualizations
│   └── interactive/           # Interactive visualizations
└── reports/                # Generated reports
    └── Liver_Transplant_Survival_Analysis_Report.html  # Final HTML report
```

## Requirements

- R (version 4.2.0 or higher)
- Required R packages:
  - Core: tidyverse, survival, survminer, glmnet, rms
  - Visualization: ggplot2, plotly, viridis
  - Machine Learning: AORSF, h2o
  - Report Generation: rmarkdown, knitr, DT, htmlwidgets

## Installation

1. Clone the repository:
   ```
   git clone https://github.com/username/liver-transplant-analysis.git
   cd liver-transplant-analysis
   ```

2. Install required packages:
   ```R
   source("src/setup.R")
   ```

3. Place the datasets in the `data` directory:
   - `imputed_data_full.csv`: Full dataset with MELD scores and survival data
   - `wl_df_with_bca.rds`: Subset with body composition analysis data

## Usage

Run the entire workflow:

```R
source("main.R")
```

This will:
1. Load and validate the datasets
2. Perform exploratory data analysis
3. Conduct Kaplan-Meier survival analysis
4. Implement and validate BC-MELD formulas
5. Train and evaluate advanced machine learning models
6. Generate a comprehensive HTML report

## Results

The analysis produces several outputs:

- **Descriptive Statistics**: Tables of patient demographics and clinical characteristics
- **Survival Curves**: Kaplan-Meier curves for overall and stratified survival
- **BC-MELD Formula**: Coefficients and performance metrics for the improved formula
- **Model Performance**: Comparison of all models using C-index, Brier score, and R²
- **Variable Importance**: Identification of key predictors of waitlist mortality
- **HTML Report**: Comprehensive report with embedded visualizations and clinical interpretation

## Documentation

Detailed documentation is available in the `docs` directory:

- [ANALYSIS.md](docs/ANALYSIS.md): Detailed explanation of the analysis workflow
- [API.md](docs/API.md): Function and class documentation
- [DATA.md](docs/DATA.md): Description of datasets and variables
- [MODELS.md](docs/MODELS.md): Technical details of implemented models
- [RESULTS.md](docs/RESULTS.md): Summary of findings and visualizations

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- The original BC-MELD formula was developed by [Reference]
- The AORSF implementation is based on the [AORSF package](https://github.com/ropensci/AORSF)
- The H2O distributed computing framework is provided by [H2O.ai](https://www.h2o.ai/)