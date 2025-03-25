# Dataset Documentation

This document provides detailed information about the datasets used in the liver transplant waitlist survival analysis project.

## Table of Contents

- [Overview](#overview)
- [Dataset Files](#dataset-files)
- [Variable Descriptions](#variable-descriptions)
- [Body Composition Variables](#body-composition-variables)
- [Outcome Variables](#outcome-variables)
- [Data Quality Considerations](#data-quality-considerations)
- [Data Preprocessing Steps](#data-preprocessing-steps)

## Overview

The analysis utilizes two primary datasets:

1. **Full Dataset** (~1200 patients): Contains MELD scores and survival data
   - File: `imputed_data_full.csv`
   - Includes demographic information, clinical variables, and survival outcomes

2. **BCA Subset** (~600 patients): Contains additional body composition analysis data
   - File: `wl_df_with_bca.rds`
   - Includes body composition variables derived from imaging studies

## Dataset Files

### imputed_data_full.csv

This file contains data for approximately 1,200 patients on the liver transplant waitlist. It includes:

- Demographic information
- Clinical variables
- MELD scores
- Survival outcomes
- Waitlist information

The data has been imputed to handle missing values, ensuring completeness for analysis.

### wl_df_with_bca.rds

This R data file contains a subset of approximately 600 patients who had body composition analysis (BCA) performed. It includes all variables from the full dataset plus additional body composition metrics derived from imaging studies.

## Variable Descriptions

### Demographic Variables

| Variable | Description | Type | Example Values |
|----------|-------------|------|---------------|
| `etnr_id` | Unique patient identifier | Integer | 132818, 137656 |
| `age` | Patient age in years | Integer | 44, 60, 39 |
| `sex` | Patient gender | Character | "M", "W" |
| `birth_date` | Date of birth | Date | 1964-06-10 |
| `nationality` | Patient nationality | Character | "deutsch", "albanisch" |
| `Race` | Patient race | Character | "Caucasian" |
| `RaceGroup` | Grouped race categories | Character | "Caucasian" |

### Clinical Variables

| Variable | Description | Type | Example Values |
|----------|-------------|------|---------------|
| `blood_type` | Blood type | Character | "A", "B", "AB", "0" |
| `rh_factor` | Rhesus factor | Character | "Pos", "Neg" |
| `kg` | Weight in kilograms | Numeric | 77, 101, 93 |
| `cm` | Height in centimeters | Integer | 167, 181, 162 |
| `bmi` | Body Mass Index | Numeric | 28, 31, 22 |
| `diagnosis_1` | Primary diagnosis | Character | "cirrhosis", "cancers" |
| `diagnosis_2` | Secondary diagnosis | Character | "Virus BD related cirrhosis" |
| `diagnosis_3` | Tertiary diagnosis | Character | "Re Tx" |
| `lab_meld` | Laboratory MELD score | Integer | 18, 23, 12 |
| `exc_meld` | Exception MELD score | Integer | 22, 0 |
| `ped_meld` | Pediatric MELD score | Integer | 0 |

### Waitlist Variables

| Variable | Description | Type | Example Values |
|----------|-------------|------|---------------|
| `waitlist_date` | Date of waitlist entry | Date | 2009-05-11 |
| `wl_year` | Year of waitlist entry | Integer | 2009, 2010 |
| `wait_days` | Days on waitlist | Integer | 5401, 167, 801 |
| `wait_month` | Months on waitlist | Integer | 180, 6, 27 |
| `urgency` | Urgency status | Character | "t", "nt" |
| `status` | Patient status (0=alive, 1=deceased) | Integer | 0, 1 |
| `ltx` | Liver transplant received (0=no, 1=yes) | Integer | 0, 1 |
| `relist` | Relisting status | Character | "X", "" |

## Body Composition Variables

The following body composition variables are available in the `wl_df_with_bca.rds` dataset:

| Variable | Description | Unit | Clinical Significance |
|----------|-------------|------|----------------------|
| `bone` | Bone tissue area | cm² | Indicator of skeletal integrity |
| `muscle` | Skeletal muscle area | cm² | Marker of muscle mass, related to frailty |
| `sat` | Subcutaneous adipose tissue area | cm² | Peripheral fat storage |
| `vat` | Visceral adipose tissue area | cm² | Metabolically active fat associated with inflammation |
| `imat` | Intermuscular adipose tissue area | cm² | Fat infiltration within muscle, marker of muscle quality |
| `eat` | Epicardial adipose tissue area | cm² | Fat around the heart, associated with cardiovascular risk |
| `pat` | Periaortic adipose tissue area | cm² | Fat around the aorta, associated with vascular risk |
| `tat` | Total adipose tissue area | cm² | Sum of all fat compartments |

These variables are derived from cross-sectional imaging (CT or MRI) at the level of the third lumbar vertebra (L3), which is the standard location for body composition analysis in clinical research.

## Outcome Variables

| Variable | Description | Type | Example Values |
|----------|-------------|------|---------------|
| `death_date` | Date of death (if applicable) | Date | 2011-06-07 |
| `death_cause` | Cause of death | Character | "MOF/Sepsis", "unbekannte Todesursachen" |
| `letzter_kontakt` | Date of last contact | Date | 2024-02-23 |
| `event_date` | Date of event (death or last contact) | Date | 2011-06-07, 2024-02-23 |
| `waitlist_time_days` | Time on waitlist in days | Integer | 167, 5401 |
| `waitlist_time_months` | Time on waitlist in months | Integer | 5, 177 |

## Data Quality Considerations

### Missing Values

- The full dataset has been imputed to handle missing values
- The BCA subset may contain some missing values in body composition variables
- Missing values should be handled appropriately during analysis

### Potential Biases

- Selection bias: The BCA subset represents patients who underwent imaging studies
- Survival bias: Longer-surviving patients may be overrepresented in the dataset
- Temporal bias: Clinical practices and waitlist management changed over the study period

### Data Limitations

- Body composition data is cross-sectional and does not capture changes over time
- Cause of death information may be incomplete or imprecise
- MELD scores may have been calculated using different formulas over time

## Data Preprocessing Steps

The following preprocessing steps are recommended before analysis:

1. **Data Validation**
   - Check for negative values in body composition variables
   - Verify consistency between datasets
   - Ensure date variables are properly formatted

2. **Feature Engineering**
   - Calculate derived variables (e.g., muscle-to-fat ratio)
   - Normalize body composition variables by height or weight as appropriate
   - Create categorical variables for stratified analysis

3. **Handling Missing Values**
   - Use multiple imputation for missing body composition variables
   - Perform sensitivity analysis with complete cases
   - Document imputation methods and assumptions

4. **Outcome Definition**
   - Define clear endpoints for survival analysis
   - Create time-to-event variables
   - Handle competing risks (death vs. transplantation)

The preprocessing code is available in `src/data_preprocessing.R`.