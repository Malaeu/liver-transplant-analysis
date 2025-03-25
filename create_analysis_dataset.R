# create_analysis_dataset.R
# Skript zur Erstellung eines sauberen Analysedatensatzes

library(tidyverse)
library(lubridate)

# 1. Daten laden
load_clean_data <- function(full_data_path, bca_data_path) {
  message("Lade Datensätze...")
  
  # Hauptdatensatz laden
  full_data <- read.csv(full_data_path, stringsAsFactors = FALSE)
  message(paste0("Hauptdatensatz geladen: ", nrow(full_data), " Zeilen, ", ncol(full_data), " Spalten"))
  
  # BCA-Datensatz laden
  bca_data <- readRDS(bca_data_path)
  message(paste0("BCA-Datensatz geladen: ", nrow(bca_data), " Zeilen, ", ncol(bca_data), " Spalten"))
  
  return(list(full_data = full_data, bca_data = bca_data))
}

# 2. Datensätze zusammenführen
merge_datasets <- function(full_data, bca_data) {
  message("Führe Datensätze zusammen...")
  
  # Gemeinsame Spalten identifizieren
  common_cols <- intersect(names(full_data), names(bca_data))
  message(paste0("Gemeinsame Spalten: ", paste(common_cols, collapse = ", ")))
  
  # Prüfen, ob etnr_id in beiden Datensätzen vorhanden ist
  if ("etnr_id" %in% names(full_data) && "etnr" %in% names(bca_data)) {
    message("Verwende 'etnr_id' und 'etnr' für den Join")
    # Umbenennen der ID-Spalte im BCA-Datensatz
    bca_data <- bca_data %>% rename(etnr_id = etnr)
    join_col <- "etnr_id"
  } else if ("etnr_id" %in% common_cols) {
    message("Verwende 'etnr_id' für den Join")
    join_col <- "etnr_id"
  } else {
    # Wenn keine eindeutige ID vorhanden ist, verwenden wir die erste gemeinsame Spalte
    join_col <- common_cols[1]
    message(paste0("Keine eindeutige ID gefunden. Verwende '", join_col, "' für den Join"))
  }
  
  # Datensätze zusammenführen
  merged_data <- full_data %>%
    left_join(bca_data, by = join_col, suffix = c(".full", ".bca"))
  
  message(paste0("Zusammengeführter Datensatz: ", nrow(merged_data), " Zeilen, ", ncol(merged_data), " Spalten"))
  
  return(merged_data)
}

# 3. Variablen auswählen
select_variables <- function(data) {
  message("Wähle relevante Variablen aus...")
  
  # Demografische Variablen
  demographic_vars <- c(
    # Alter und Geschlecht
    "age.full", "age.bca", "sex.full", "sex.bca",
    # Körpermaße
    "bmi.full", "bmi.bca", "kg", "weight", "cm", "height"
  )
  
  # Klinische Variablen
  clinical_vars <- c(
    # MELD-Scores
    "lab_meld.full", "lab_meld.bca", "exc_meld.full", "exc_meld.bca", 
    # Diagnosen
    "diagnosis_1", "diagnosis_2", "diagnosis_3", "primary_diagnosis",
    "diagnosis_clean", "diagnosis_final", "diagnosis_grouped", "diagnosis_group", "diagnosis_subgroup",
    # Klinische Faktoren
    "has_ascites", "has_varices", "has_encephalopathy", "has_splenomegaly"
  )
  
  # BCA-Variablen
  bca_vars <- c(
    "muscle", "sat", "vat", "imat", "eat", "pat", "tat"
  )
  
  # Outcome-Variablen
  outcome_vars <- c(
    "waitlist_time_days", "waitlist_time_months", "status", "surv_time", "surv_status"
  )
  
  # Alle relevanten Variablen
  all_vars <- c(demographic_vars, clinical_vars, bca_vars, outcome_vars)
  
  # Vorhandene Variablen identifizieren
  available_vars <- all_vars[all_vars %in% names(data)]
  missing_vars <- all_vars[!all_vars %in% names(data)]
  
  message(paste0("Verfügbare Variablen: ", length(available_vars), " von ", length(all_vars)))
  if (length(missing_vars) > 0) {
    message(paste0("Fehlende Variablen: ", paste(missing_vars, collapse = ", ")))
  }
  
  # Datensatz auf ausgewählte Variablen reduzieren
  selected_data <- data %>% select(all_of(available_vars))
  
  message(paste0("Reduzierter Datensatz: ", nrow(selected_data), " Zeilen, ", ncol(selected_data), " Spalten"))
  
  return(selected_data)
}

# 4. Variablen bereinigen und vereinheitlichen
clean_variables <- function(data) {
  message("Bereinige Variablen...")
  
  # Kopie des Datensatzes erstellen
  clean_data <- data
  
  # Alter vereinheitlichen
  if (all(c("age.full", "age.bca") %in% names(clean_data))) {
    message("Vereinheitliche Alter-Variable")
    clean_data <- clean_data %>%
      mutate(age = coalesce(age.full, age.bca)) %>%
      select(-age.full, -age.bca)
  } else if ("age.full" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(age = age.full)
  } else if ("age.bca" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(age = age.bca)
  }
  
  # Geschlecht vereinheitlichen
  if (all(c("sex.full", "sex.bca") %in% names(clean_data))) {
    message("Vereinheitliche Geschlecht-Variable")
    # Prüfen, ob sex.bca ein Faktor ist
    if ("factor" %in% class(clean_data$sex.bca)) {
      # Wenn ja, in Character umwandeln
      clean_data$sex.bca <- as.character(clean_data$sex.bca)
    }
    
    clean_data <- clean_data %>%
      mutate(sex = coalesce(sex.full, sex.bca)) %>%
      select(-sex.full, -sex.bca)
  } else if ("sex.full" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(sex = sex.full)
  } else if ("sex.bca" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(sex = sex.bca)
  }
  
  # BMI vereinheitlichen
  if (all(c("bmi.full", "bmi.bca") %in% names(clean_data))) {
    message("Vereinheitliche BMI-Variable")
    clean_data <- clean_data %>%
      mutate(bmi = coalesce(bmi.full, bmi.bca)) %>%
      select(-bmi.full, -bmi.bca)
  } else if ("bmi.full" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(bmi = bmi.full)
  } else if ("bmi.bca" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(bmi = bmi.bca)
  }
  
  # Gewicht vereinheitlichen
  if (all(c("kg", "weight") %in% names(clean_data))) {
    message("Vereinheitliche Gewicht-Variable")
    # Überprüfen und konvertieren der Typen
    if (is.character(clean_data$weight)) {
      message("Konvertiere 'weight' von character zu numeric")
      clean_data$weight <- as.numeric(clean_data$weight)
    }
    
    clean_data <- clean_data %>%
      mutate(weight_kg = coalesce(kg, weight)) %>%
      select(-kg, -weight)
  } else if ("kg" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(weight_kg = kg)
  } else if ("weight" %in% names(clean_data)) {
    # Sicherstellen, dass weight numerisch ist
    if (is.character(clean_data$weight)) {
      clean_data$weight <- as.numeric(clean_data$weight)
    }
    clean_data <- clean_data %>%
      rename(weight_kg = weight)
  }
  
  # Größe vereinheitlichen
  if (all(c("cm", "height") %in% names(clean_data))) {
    message("Vereinheitliche Größe-Variable")
    clean_data <- clean_data %>%
      mutate(height_cm = coalesce(cm, height)) %>%
      select(-cm, -height)
  } else if ("cm" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(height_cm = cm)
  } else if ("height" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(height_cm = height)
  }
  
  # MELD-Score vereinheitlichen
  if (all(c("lab_meld.full", "lab_meld.bca") %in% names(clean_data))) {
    message("Vereinheitliche MELD-Score-Variable")
    clean_data <- clean_data %>%
      mutate(lab_meld = coalesce(lab_meld.full, lab_meld.bca)) %>%
      select(-lab_meld.full, -lab_meld.bca)
  } else if ("lab_meld.full" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(lab_meld = lab_meld.full)
  } else if ("lab_meld.bca" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(lab_meld = lab_meld.bca)
  }
  
  # Exception MELD-Score vereinheitlichen
  if (all(c("exc_meld.full", "exc_meld.bca") %in% names(clean_data))) {
    message("Vereinheitliche Exception MELD-Score-Variable")
    clean_data <- clean_data %>%
      mutate(exc_meld = coalesce(exc_meld.full, exc_meld.bca)) %>%
      select(-exc_meld.full, -exc_meld.bca)
  } else if ("exc_meld.full" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(exc_meld = exc_meld.full)
  } else if ("exc_meld.bca" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(exc_meld = exc_meld.bca)
  }
  
  # Diagnose vereinheitlichen
  if ("diagnosis_final" %in% names(clean_data)) {
    message("Verwende 'diagnosis_final' als Hauptdiagnose")
    clean_data <- clean_data %>%
      rename(diagnosis = diagnosis_final)
  } else if ("diagnosis_clean" %in% names(clean_data)) {
    clean_data <- clean_data %>%
      rename(diagnosis = diagnosis_clean)
  } else if (all(c("diagnosis_1", "primary_diagnosis") %in% names(clean_data))) {
    clean_data <- clean_data %>%
      mutate(diagnosis = coalesce(diagnosis_1, primary_diagnosis))
  }
  
  # Character-Variablen in Faktoren umwandeln
  char_vars <- names(clean_data)[sapply(clean_data, is.character)]
  message(paste0("Wandle ", length(char_vars), " Character-Variablen in Faktoren um"))
  
  for (var in char_vars) {
    clean_data[[var]] <- as.factor(clean_data[[var]])
  }
  
  return(clean_data)
}

# 5. Fehlende Werte behandeln
handle_missing_values <- function(data) {
  message("Behandle fehlende Werte...")
  
  # Anzahl fehlender Werte pro Variable
  na_counts <- sapply(data, function(x) sum(is.na(x)))
  na_percent <- round(na_counts / nrow(data) * 100, 2)
  
  na_df <- data.frame(
    Variable = names(na_counts),
    Fehlende_Werte = na_counts,
    Prozent_Fehlend = na_percent,
    stringsAsFactors = FALSE
  )
  
  # Sortieren nach Anzahl fehlender Werte (absteigend)
  na_df <- na_df[order(-na_df$Fehlende_Werte), ]
  
  # Variablen mit fehlenden Werten anzeigen
  vars_with_na <- na_df[na_df$Fehlende_Werte > 0, ]
  
  if (nrow(vars_with_na) > 0) {
    message("\nVariablen mit fehlenden Werten:")
    print(vars_with_na)
    
    # Variablen mit kritischem Anteil fehlender Werte (> 50%)
    critical_vars <- vars_with_na$Variable[vars_with_na$Prozent_Fehlend > 50]
    if (length(critical_vars) > 0) {
      message("\nVariablen mit kritischem Anteil fehlender Werte (> 50%):")
      print(critical_vars)
      
      # Entfernen von Variablen mit zu vielen fehlenden Werten
      message("Entferne Variablen mit > 50% fehlenden Werten")
      data <- data %>% select(-all_of(critical_vars))
    }
    
    # Variablen mit moderatem Anteil fehlender Werte (20-50%)
    moderate_vars <- vars_with_na$Variable[vars_with_na$Prozent_Fehlend > 20 & vars_with_na$Prozent_Fehlend <= 50]
    if (length(moderate_vars) > 0) {
      message("\nVariablen mit moderatem Anteil fehlender Werte (20-50%):")
      print(moderate_vars)
      
      # Hier könnte eine komplexere Imputation implementiert werden
      # Für dieses Beispiel verwenden wir einfache Imputation
      message("Führe einfache Imputation für Variablen mit 20-50% fehlenden Werten durch")
      
      for (var in moderate_vars) {
        if (is.numeric(data[[var]])) {
          # Für numerische Variablen: Median
          data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm = TRUE)
        } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
          # Für kategoriale Variablen: Modus
          mode_val <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
          data[[var]][is.na(data[[var]])] <- mode_val
        }
      }
    }
    
    # Variablen mit geringem Anteil fehlender Werte (< 20%)
    minor_vars <- vars_with_na$Variable[vars_with_na$Prozent_Fehlend <= 20 & vars_with_na$Prozent_Fehlend > 0]
    if (length(minor_vars) > 0) {
      message("\nVariablen mit geringem Anteil fehlender Werte (< 20%):")
      print(minor_vars)
      
      # Einfache Imputation für Variablen mit wenigen fehlenden Werten
      message("Führe einfache Imputation für Variablen mit < 20% fehlenden Werten durch")
      
      for (var in minor_vars) {
        if (is.numeric(data[[var]])) {
          # Für numerische Variablen: Median
          data[[var]][is.na(data[[var]])] <- median(data[[var]], na.rm = TRUE)
        } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
          # Für kategoriale Variablen: Modus
          mode_val <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
          data[[var]][is.na(data[[var]])] <- mode_val
        }
      }
    }
  } else {
    message("Keine fehlenden Werte im Datensatz gefunden.")
  }
  
  # Überprüfen, ob noch fehlende Werte vorhanden sind
  na_counts_after <- sapply(data, function(x) sum(is.na(x)))
  total_na_after <- sum(na_counts_after)
  
  if (total_na_after > 0) {
    message(paste0("WARNUNG: Nach der Imputation sind noch ", total_na_after, " fehlende Werte vorhanden."))
    message("Entferne Zeilen mit fehlenden Werten in kritischen Variablen")
    
    # Kritische Variablen für die Überlebensanalyse
    critical_analysis_vars <- c("waitlist_time_months", "status", "lab_meld")
    available_critical_vars <- critical_analysis_vars[critical_analysis_vars %in% names(data)]
    
    if (length(available_critical_vars) > 0) {
      # Entfernen von Zeilen mit fehlenden Werten in kritischen Variablen
      data <- data %>% drop_na(all_of(available_critical_vars))
    }
  }
  
  message(paste0("Datensatz nach Behandlung fehlender Werte: ", nrow(data), " Zeilen, ", ncol(data), " Spalten"))
  
  return(data)
}

# 6. Ausreißer behandeln
handle_outliers <- function(data) {
  message("Behandle Ausreißer in numerischen Variablen...")
  
  # Numerische Variablen identifizieren
  num_vars <- names(data)[sapply(data, is.numeric)]
  
  # Ausreißer identifizieren und behandeln
  for (var in num_vars) {
    # Überprüfen, ob die Variable noch im Datensatz ist
    if (!(var %in% names(data))) next
    
    # Überprüfen, ob die Variable konstant ist
    if (length(unique(data[[var]])) <= 1) {
      message(paste0("Variable '", var, "' hat nur einen Wert. Überspringe Ausreißerbehandlung."))
      next
    }
    
    # Ausreißer mit IQR-Methode identifizieren
    q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
    q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    outliers <- data[[var]] < lower_bound | data[[var]] > upper_bound
    outliers_count <- sum(outliers, na.rm = TRUE)
    outliers_percent <- round(outliers_count / sum(!is.na(data[[var]])) * 100, 2)
    
    if (outliers_count > 0) {
      message(paste0("Variable '", var, "': ", outliers_count, " Ausreißer (", outliers_percent, "%)"))
      
      # Winsorisierung: Begrenzung auf 1. und 99. Perzentil
      p01 <- quantile(data[[var]], 0.01, na.rm = TRUE)
      p99 <- quantile(data[[var]], 0.99, na.rm = TRUE)
      
      data[[var]] <- pmin(pmax(data[[var]], p01), p99)
    }
  }
  
  return(data)
}

# 7. Neue Variablen erstellen
create_derived_variables <- function(data) {
  message("Erstelle abgeleitete Variablen...")
  
  # BMI-Kategorien erstellen (falls BMI vorhanden)
  if ("bmi" %in% names(data)) {
    message("Erstelle BMI-Kategorien")
    data <- data %>%
      mutate(bmi_category = case_when(
        bmi < 18.5 ~ "Untergewicht",
        bmi >= 18.5 & bmi < 25 ~ "Normalgewicht",
        bmi >= 25 & bmi < 30 ~ "Übergewicht",
        bmi >= 30 ~ "Adipositas",
        TRUE ~ NA_character_
      ))
  }
  
  # MELD-Kategorien erstellen (falls MELD-Score vorhanden)
  if ("lab_meld" %in% names(data)) {
    message("Erstelle MELD-Kategorien")
    data <- data %>%
      mutate(meld_category = case_when(
        lab_meld < 10 ~ "MELD < 10",
        lab_meld >= 10 & lab_meld < 20 ~ "MELD 10-19",
        lab_meld >= 20 & lab_meld < 30 ~ "MELD 20-29",
        lab_meld >= 30 ~ "MELD ≥ 30",
        TRUE ~ NA_character_
      ))
  }
  
  # Alter-Kategorien erstellen (falls Alter vorhanden)
  if ("age" %in% names(data)) {
    message("Erstelle Alter-Kategorien")
    data <- data %>%
      mutate(age_category = case_when(
        age < 30 ~ "< 30",
        age >= 30 & age < 40 ~ "30-39",
        age >= 40 & age < 50 ~ "40-49",
        age >= 50 & age < 60 ~ "50-59",
        age >= 60 & age < 70 ~ "60-69",
        age >= 70 ~ "≥ 70",
        TRUE ~ NA_character_
      ))
  }
  
  # Geschlecht standardisieren (falls vorhanden)
  if ("sex" %in% names(data)) {
    message("Standardisiere Geschlecht-Variable")
    data <- data %>%
      mutate(sex_std = case_when(
        toupper(as.character(sex)) %in% c("M", "MALE", "MÄNNLICH") ~ "M",
        toupper(as.character(sex)) %in% c("F", "W", "FEMALE", "WEIBLICH") ~ "F",
        TRUE ~ NA_character_
      ))
  }
  
  # Normalisierung der BCA-Variablen nach Körpergröße (falls vorhanden)
  bca_vars <- c("muscle", "sat", "vat", "imat", "eat", "pat", "tat")
  available_bca_vars <- bca_vars[bca_vars %in% names(data)]
  
  if (length(available_bca_vars) > 0 && "height_cm" %in% names(data)) {
    message("Normalisiere BCA-Variablen nach Körpergröße")
    
    for (var in available_bca_vars) {
      # Normalisierung: Wert / (Größe in m)²
      norm_var_name <- paste0(var, "_norm")
      data[[norm_var_name]] <- data[[var]] / ((data$height_cm / 100)^2)
    }
  }
  
  return(data)
}

# 8. Hauptfunktion
create_analysis_dataset <- function(full_data_path, bca_data_path, output_path) {
  # Daten laden
  datasets <- load_clean_data(full_data_path, bca_data_path)
  full_data <- datasets$full_data
  bca_data <- datasets$bca_data
  
  # Datensätze zusammenführen
  merged_data <- merge_datasets(full_data, bca_data)
  
  # Variablen auswählen
  selected_data <- select_variables(merged_data)
  
  # Variablen bereinigen und vereinheitlichen
  clean_data <- clean_variables(selected_data)
  
  # Fehlende Werte behandeln
  imputed_data <- handle_missing_values(clean_data)
  
  # Ausreißer behandeln
  outlier_handled_data <- handle_outliers(imputed_data)
  
  # Neue Variablen erstellen
  final_data <- create_derived_variables(outlier_handled_data)
  
  # Datensatz speichern
  saveRDS(final_data, output_path)
  message(paste0("Analysedatensatz gespeichert unter: ", output_path))
  message(paste0("Finaler Datensatz: ", nrow(final_data), " Zeilen, ", ncol(final_data), " Spalten"))
  
  return(final_data)
}

# Ausführung
if (!interactive()) {
  # Pfade zu den Datensätzen
  full_data_path <- "imputed_data_full.csv"
  bca_data_path <- "wl_df_with_bca.rds"
  output_path <- "analysis_dataset_clean.rds"
  
  # Analysedatensatz erstellen
  analysis_data <- create_analysis_dataset(full_data_path, bca_data_path, output_path)
}