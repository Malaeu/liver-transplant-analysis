# explore_data.R
# Skript zur Exploration der Datensätze und Erstellung eines Berichts

library(tidyverse)
library(skimr)
library(knitr)
library(lubridate)

# 1. DATEN LADEN UND GRUNDLEGENDE INFORMATIONEN
load_and_explore_data <- function(full_data_path, bca_data_path) {
  message("1. DATEN LADEN UND GRUNDLEGENDE INFORMATIONEN")
  
  # Hauptdatensatz laden
  full_data <- read.csv(full_data_path, stringsAsFactors = FALSE)
  message(paste0("Hauptdatensatz geladen: ", nrow(full_data), " Zeilen, ", ncol(full_data), " Spalten"))
  
  # BCA-Datensatz laden
  bca_data <- readRDS(bca_data_path)
  message(paste0("BCA-Datensatz geladen: ", nrow(bca_data), " Zeilen, ", ncol(bca_data), " Spalten"))
  
  # Grundlegende Informationen zum Hauptdatensatz
  message("\nGrundlegende Informationen zum Hauptdatensatz:")
  message("Variablennamen:")
  print(names(full_data))
  
  message("\nVariablentypen:")
  print(sapply(full_data, class))
  
  # Grundlegende Informationen zum BCA-Datensatz
  message("\nGrundlegende Informationen zum BCA-Datensatz:")
  message("Variablennamen:")
  print(names(bca_data))
  
  message("\nVariablentypen:")
  print(sapply(bca_data, class))
  
  return(list(full_data = full_data, bca_data = bca_data))
}

# 2. VARIABLENSTRUKTUR ANALYSIEREN
analyze_variable_structure <- function(data, dataset_name) {
  message(paste0("\n2. VARIABLENSTRUKTUR ANALYSIEREN - ", dataset_name))
  
  # Erste 5 Zeilen jeder Variable anzeigen
  message("\nErste 5 Zeilen jeder Variable:")
  for (col in names(data)[1:min(10, length(names(data)))]) {  # Begrenzen auf die ersten 10 Variablen
    message(paste0("Variable: ", col))
    message(paste0("Typ: ", class(data[[col]])))
    message(paste0("Erste 5 Werte: ", paste(head(data[[col]], 5), collapse = ", ")))
    message("")
  }
  
  if (length(names(data)) > 10) {
    message(paste0("... und ", length(names(data)) - 10, " weitere Variablen"))
  }
  
  # Variablen nach Typ gruppieren
  var_types <- sapply(data, class)
  
  # Numerische Variablen
  num_vars <- names(var_types[var_types %in% c("numeric", "integer")])
  message(paste0("\nNumerische Variablen (", length(num_vars), "):"))
  message(paste(head(num_vars, 20), collapse = ", "))
  if (length(num_vars) > 20) {
    message("...")
  }
  
  # Kategoriale Variablen (Character und Faktor)
  cat_vars <- names(var_types[var_types %in% c("character", "factor")])
  message(paste0("\nKategoriale Variablen (", length(cat_vars), "):"))
  message(paste(head(cat_vars, 20), collapse = ", "))
  if (length(cat_vars) > 20) {
    message("...")
  }
  
  # Datumsvariablen (basierend auf Namensmuster)
  date_patterns <- c("date", "datum", "day", "month", "year", "zeit", "time")
  potential_date_vars <- names(data)[grepl(paste(date_patterns, collapse = "|"), names(data), ignore.case = TRUE)]
  message(paste0("\nPotenzielle Datumsvariablen (", length(potential_date_vars), "):"))
  if (length(potential_date_vars) > 0) {
    message(paste(potential_date_vars, collapse = ", "))
  } else {
    message("Keine potenziellen Datumsvariablen gefunden.")
  }
  
  # Identifikatoren (basierend auf Namensmuster)
  id_patterns <- c("id", "nr", "code", "key", "etnr")
  potential_id_vars <- names(data)[grepl(paste(id_patterns, collapse = "|"), names(data), ignore.case = TRUE)]
  message(paste0("\nPotenzielle ID-Variablen (", length(potential_id_vars), "):"))
  if (length(potential_id_vars) > 0) {
    message(paste(potential_id_vars, collapse = ", "))
  } else {
    message("Keine potenziellen ID-Variablen gefunden.")
  }
  
  # Detaillierte Analyse kategorialer Variablen
  if (length(cat_vars) > 0) {
    message("\nDetaillierte Analyse kategorialer Variablen:")
    
    for (var in head(cat_vars, 10)) {  # Begrenzen auf die ersten 10 kategorialen Variablen
      unique_values <- unique(data[[var]])
      n_unique <- length(unique_values)
      
      message(paste0("\nVariable: ", var))
      message(paste0("Anzahl unterschiedlicher Werte: ", n_unique))
      
      if (n_unique <= 20) {
        # Häufigkeitstabelle für Variablen mit wenigen Werten
        value_counts <- table(data[[var]], useNA = "ifany")
        value_counts_sorted <- sort(value_counts, decreasing = TRUE)
        print(value_counts_sorted)
      } else {
        # Nur die häufigsten Werte für Variablen mit vielen Werten
        value_counts <- table(data[[var]], useNA = "ifany")
        value_counts_sorted <- sort(value_counts, decreasing = TRUE)
        message("Top 10 häufigste Werte:")
        print(head(value_counts_sorted, 10))
        message(paste0("... und ", n_unique - 10, " weitere Werte"))
      }
    }
    
    if (length(cat_vars) > 10) {
      message(paste0("\n... und ", length(cat_vars) - 10, " weitere kategoriale Variablen"))
    }
  }
  
  # Detaillierte Analyse numerischer Variablen
  if (length(num_vars) > 0) {
    message("\nDetaillierte Analyse numerischer Variablen:")
    
    # Zusammenfassende Statistiken für alle numerischen Variablen
    num_summary <- data %>%
      select(all_of(num_vars)) %>%
      summary()
    
    print(num_summary)
  }
  
  return(list(
    numeric_vars = num_vars,
    categorical_vars = cat_vars,
    date_vars = potential_date_vars,
    id_vars = potential_id_vars
  ))
}

# 3. FEHLENDE WERTE ANALYSIEREN
analyze_missing_values <- function(data, dataset_name) {
  message(paste0("\n3. FEHLENDE WERTE ANALYSIEREN - ", dataset_name))
  
  # Anzahl und Prozentsatz fehlender Werte pro Variable
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
    
    message(paste0("\nGesamtzahl der Variablen mit fehlenden Werten: ", nrow(vars_with_na)))
    message(paste0("Gesamtzahl der fehlenden Werte: ", sum(na_counts)))
    message(paste0("Durchschnittlicher Prozentsatz fehlender Werte: ", 
                  round(mean(vars_with_na$Prozent_Fehlend), 2), "%"))
    
    # Variablen mit kritischem Anteil fehlender Werte (> 50%)
    critical_vars <- vars_with_na[vars_with_na$Prozent_Fehlend > 50, ]
    if (nrow(critical_vars) > 0) {
      message("\nVariablen mit kritischem Anteil fehlender Werte (> 50%):")
      print(critical_vars)
    }
  } else {
    message("\nKeine fehlenden Werte im Datensatz gefunden.")
  }
  
  return(na_df)
}

# 4. KATEGORIALE VARIABLEN ANALYSIEREN
analyze_categorical_variables <- function(data, dataset_name, cat_vars) {
  message(paste0("\n4. KATEGORIALE VARIABLEN ANALYSIEREN - ", dataset_name))
  
  if (length(cat_vars) == 0) {
    message("\nKeine kategorialen Variablen im Datensatz gefunden.")
    return(NULL)
  }
  
  cat_var_summary <- list()
  
  for (var in cat_vars) {
    unique_values <- unique(data[[var]])
    n_unique <- length(unique_values)
    
    message(paste0("\nVariable: ", var))
    message(paste0("Anzahl unterschiedlicher Werte: ", n_unique))
    
    # Häufigkeitstabelle
    value_counts <- table(data[[var]], useNA = "ifany")
    value_counts_sorted <- sort(value_counts, decreasing = TRUE)
    
    if (n_unique <= 20) {
      print(value_counts_sorted)
    } else {
      message("Top 10 häufigste Werte:")
      print(head(value_counts_sorted, 10))
      message(paste0("... und ", n_unique - 10, " weitere Werte"))
      
      message("\nWARNUNG: Mehr als 20 unterschiedliche Werte. Gruppierung empfohlen.")
      
      # Vorschlag zur Gruppierung
      if (n_unique > 100) {
        message("Empfehlung: Gruppiere in 5-10 Kategorien oder verwende als ID-Variable")
      } else if (n_unique > 50) {
        message("Empfehlung: Gruppiere in Top-10-Kategorien + 'Sonstige'")
      } else {
        message("Empfehlung: Gruppiere ähnliche Kategorien oder seltenere Werte zu 'Sonstige'")
      }
    }
    
    # Prüfen auf Inkonsistenzen und Tippfehler
    if (n_unique <= 100) {  # Nur für Variablen mit überschaubarer Anzahl an Werten
      # Einfache Prüfung auf ähnliche Strings
      if (is.character(data[[var]])) {
        values_lower <- tolower(unique_values)
        potential_duplicates <- list()
        
        for (i in 1:(length(values_lower) - 1)) {
          for (j in (i + 1):length(values_lower)) {
            # Levenshtein-Distanz oder einfacher Vergleich
            if (values_lower[i] != values_lower[j] && 
                (startsWith(values_lower[i], values_lower[j]) || 
                 startsWith(values_lower[j], values_lower[i]))) {
              potential_duplicates[[length(potential_duplicates) + 1]] <- 
                c(as.character(unique_values[i]), as.character(unique_values[j]))
            }
          }
        }
        
        if (length(potential_duplicates) > 0) {
          message("\nMögliche Inkonsistenzen/Tippfehler gefunden:")
          for (pair in potential_duplicates) {
            message(paste0("  - '", pair[1], "' und '", pair[2], "'"))
          }
        }
      }
    }
    
    # Speichern der Zusammenfassung
    cat_var_summary[[var]] <- list(
      n_unique = n_unique,
      value_counts = value_counts_sorted
    )
  }
  
  return(cat_var_summary)
}

# 5. NUMERISCHE VARIABLEN ANALYSIEREN
analyze_numeric_variables <- function(data, dataset_name, num_vars) {
  message(paste0("\n5. NUMERISCHE VARIABLEN ANALYSIEREN - ", dataset_name))
  
  if (length(num_vars) == 0) {
    message("\nKeine numerischen Variablen im Datensatz gefunden.")
    return(NULL)
  }
  
  num_var_summary <- list()
  
  for (var in num_vars) {
    message(paste0("\nVariable: ", var))
    
    # Basisstatistik
    var_data <- data[[var]]
    var_data_clean <- var_data[!is.na(var_data)]
    
    if (length(var_data_clean) == 0) {
      message("WARNUNG: Alle Werte sind NA.")
      next
    }
    
    stats <- summary(var_data_clean)
    print(stats)
    
    # Zusätzliche Statistiken
    sd_val <- sd(var_data_clean)
    message(paste0("Standardabweichung: ", round(sd_val, 4)))
    
    # Prüfung auf Ausreißer
    q1 <- quantile(var_data_clean, 0.25)
    q3 <- quantile(var_data_clean, 0.75)
    iqr <- q3 - q1
    lower_bound <- q1 - 1.5 * iqr
    upper_bound <- q3 + 1.5 * iqr
    
    outliers <- var_data_clean[var_data_clean < lower_bound | var_data_clean > upper_bound]
    outliers_count <- length(outliers)
    outliers_percent <- round(outliers_count / length(var_data_clean) * 100, 2)
    
    message(paste0("Ausreißer (IQR-Methode): ", outliers_count, " (", outliers_percent, "%)"))
    message(paste0("Untere Grenze: ", round(lower_bound, 4)))
    message(paste0("Obere Grenze: ", round(upper_bound, 4)))
    
    if (outliers_count > 0) {
      message("Beispiele für Ausreißer:")
      print(head(sort(outliers), 5))
      
      if (outliers_count > 5) {
        message(paste0("... und ", outliers_count - 5, " weitere"))
      }
    }
    
    # Prüfung auf Schiefe und Notwendigkeit einer Transformation
    skewness <- mean((var_data_clean - mean(var_data_clean))^3) / (sd_val^3)
    message(paste0("Schiefe: ", round(skewness, 4)))
    
    if (abs(skewness) > 1) {
      message("WARNUNG: Stark schiefe Verteilung. Transformation empfohlen.")
      
      if (skewness > 1 && min(var_data_clean, na.rm = TRUE) >= 0) {
        message("Empfehlung: Logarithmische Transformation (log(x + 1))")
      } else if (skewness < -1) {
        message("Empfehlung: Quadratische Transformation (x^2)")
      }
    }
    
    # Speichern der Zusammenfassung
    num_var_summary[[var]] <- list(
      stats = stats,
      sd = sd_val,
      outliers_count = outliers_count,
      outliers_percent = outliers_percent,
      skewness = skewness
    )
  }
  
  return(num_var_summary)
}

# 6. DATUMSVARIABLEN ANALYSIEREN
analyze_date_variables <- function(data, dataset_name, date_vars) {
  message(paste0("\n6. DATUMSVARIABLEN ANALYSIEREN - ", dataset_name))
  
  if (length(date_vars) == 0) {
    message("\nKeine potenziellen Datumsvariablen im Datensatz gefunden.")
    return(NULL)
  }
  
  date_var_summary <- list()
  
  for (var in date_vars) {
    message(paste0("\nVariable: ", var))
    message(paste0("Typ: ", class(data[[var]])))
    
    # Beispielwerte
    example_values <- head(unique(data[[var]]), 5)
    message(paste0("Beispielwerte: ", paste(example_values, collapse = ", ")))
    
    # Versuchen, Datumsvariablen zu erkennen und zu konvertieren
    if (is.character(data[[var]])) {
      # Versuchen, verschiedene Datumsformate zu erkennen
      date_formats <- c(
        "%Y-%m-%d", "%d-%m-%Y", "%m/%d/%Y", "%d/%m/%Y", 
        "%Y.%m.%d", "%d.%m.%Y", "%m.%d.%Y"
      )
      
      converted <- FALSE
      
      for (format in date_formats) {
        tryCatch({
          test_date <- as.Date(example_values[1], format = format)
          if (!is.na(test_date)) {
            message(paste0("Erkanntes Datumsformat: ", format))
            converted <- TRUE
            break
          }
        }, error = function(e) {
          # Ignorieren und nächstes Format probieren
        })
      }
      
      if (!converted) {
        message("WARNUNG: Konnte kein Datumsformat erkennen.")
      }
    } else if (inherits(data[[var]], "Date")) {
      # Bereits ein Datum
      message("Variable ist bereits im Datumsformat.")
      
      # Zeitspanne analysieren
      date_range <- range(data[[var]], na.rm = TRUE)
      message(paste0("Zeitspanne: ", date_range[1], " bis ", date_range[2]))
      
      # Prüfen auf fehlende Werte
      na_count <- sum(is.na(data[[var]]))
      message(paste0("Fehlende Werte: ", na_count, " (", 
                    round(na_count / nrow(data) * 100, 2), "%)"))
    }
    
    # Speichern der Zusammenfassung
    date_var_summary[[var]] <- list(
      class = class(data[[var]]),
      examples = example_values
    )
  }
  
  return(date_var_summary)
}

# 7. EMPFEHLUNGEN ZUR VORVERARBEITUNG
generate_preprocessing_recommendations <- function(full_data, bca_data, full_analysis, bca_analysis) {
  message("\n7. EMPFEHLUNGEN ZUR VORVERARBEITUNG")
  
  # Variablen, die beibehalten werden sollten
  message("\nEmpfohlene Variablen für die Analyse:")
  
  # Demografische Variablen
  demographic_patterns <- c("age", "sex", "gender", "bmi", "height", "weight")
  demographic_vars <- c(
    grep(paste(demographic_patterns, collapse = "|"), names(full_data), value = TRUE, ignore.case = TRUE),
    grep(paste(demographic_patterns, collapse = "|"), names(bca_data), value = TRUE, ignore.case = TRUE)
  )
  demographic_vars <- unique(demographic_vars)
  
  message("\nDEMOGRAFISCHE VARIABLEN:")
  message(paste(demographic_vars, collapse = ", "))
  
  # Klinische Variablen
  clinical_patterns <- c("meld", "lab", "score", "diagnosis", "clinical", "medical")
  clinical_vars <- c(
    grep(paste(clinical_patterns, collapse = "|"), names(full_data), value = TRUE, ignore.case = TRUE),
    grep(paste(clinical_patterns, collapse = "|"), names(bca_data), value = TRUE, ignore.case = TRUE)
  )
  clinical_vars <- unique(clinical_vars)
  
  message("\nKLINISCHE VARIABLEN:")
  message(paste(clinical_vars, collapse = ", "))
  
  # BCA-Variablen
  bca_patterns <- c("muscle", "sat", "vat", "imat", "eat", "pat", "tat", "bca")
  bca_vars <- c(
    grep(paste(bca_patterns, collapse = "|"), names(full_data), value = TRUE, ignore.case = TRUE),
    grep(paste(bca_patterns, collapse = "|"), names(bca_data), value = TRUE, ignore.case = TRUE)
  )
  bca_vars <- unique(bca_vars)
  
  message("\nBCA-VARIABLEN:")
  message(paste(bca_vars, collapse = ", "))
  
  # Outcome-Variablen
  outcome_patterns <- c("waitlist", "time", "status", "survival", "death", "transplant", "outcome")
  outcome_vars <- c(
    grep(paste(outcome_patterns, collapse = "|"), names(full_data), value = TRUE, ignore.case = TRUE),
    grep(paste(outcome_patterns, collapse = "|"), names(bca_data), value = TRUE, ignore.case = TRUE)
  )
  outcome_vars <- unique(outcome_vars)
  
  message("\nOUTCOME-VARIABLEN:")
  message(paste(outcome_vars, collapse = ", "))
  
  # Variablen, die ausgeschlossen werden sollten
  exclude_patterns <- c("id", "name", "vorname", "birth", "date", "datum", "studie", "etnr")
  exclude_vars <- c(
    grep(paste(exclude_patterns, collapse = "|"), names(full_data), value = TRUE, ignore.case = TRUE),
    grep(paste(exclude_patterns, collapse = "|"), names(bca_data), value = TRUE, ignore.case = TRUE)
  )
  exclude_vars <- unique(exclude_vars)
  
  message("\nVARIABLEN ZUM AUSSCHLIESSEN:")
  message(paste(exclude_vars, collapse = ", "))
  
  # Strategie für fehlende Werte
  message("\nStrategie für fehlende Werte:")
  
  # Fehlende Werte im Hauptdatensatz
  full_na <- full_analysis$missing_values
  full_na_critical <- full_na[full_na$Prozent_Fehlend > 20, ]
  
  if (nrow(full_na_critical) > 0) {
    message("\nKritische Variablen im Hauptdatensatz (> 20% fehlende Werte):")
    print(full_na_critical)
    
    message("Empfehlungen:")
    message("1. Variablen mit > 50% fehlenden Werten: Ausschließen")
    message("2. Variablen mit 20-50% fehlenden Werten: Multiple Imputation oder Modellbasierte Imputation")
  }
  
  # Fehlende Werte im BCA-Datensatz
  bca_na <- bca_analysis$missing_values
  bca_na_critical <- bca_na[bca_na$Prozent_Fehlend > 20, ]
  
  if (nrow(bca_na_critical) > 0) {
    message("\nKritische Variablen im BCA-Datensatz (> 20% fehlende Werte):")
    print(bca_na_critical)
    
    message("Empfehlungen:")
    message("1. Variablen mit > 50% fehlenden Werten: Ausschließen")
    message("2. Variablen mit 20-50% fehlenden Werten: Multiple Imputation oder Modellbasierte Imputation")
  }
  
  # Strategie für kategoriale Variablen
  message("\nStrategie für kategoriale Variablen:")
  
  # Kategoriale Variablen mit vielen Levels
  cat_vars_full <- full_analysis$variable_structure$categorical_vars
  cat_vars_bca <- bca_analysis$variable_structure$categorical_vars
  
  message("\nKategoriale Variablen, die in Faktoren umgewandelt werden sollten:")
  
  for (var in cat_vars_full) {
    n_unique <- length(unique(full_data[[var]]))
    if (n_unique <= 20) {
      message(paste0("- ", var, " (", n_unique, " Levels)"))
    } else if (n_unique <= 50) {
      message(paste0("- ", var, " (", n_unique, " Levels) - Gruppierung empfohlen"))
    }
  }
  
  for (var in cat_vars_bca) {
    n_unique <- length(unique(bca_data[[var]]))
    if (n_unique <= 20) {
      message(paste0("- ", var, " (", n_unique, " Levels)"))
    } else if (n_unique <= 50) {
      message(paste0("- ", var, " (", n_unique, " Levels) - Gruppierung empfohlen"))
    }
  }
  
  # Strategie für numerische Variablen
  message("\nStrategie für numerische Variablen:")
  
  # Numerische Variablen mit Ausreißern
  num_vars_full <- full_analysis$variable_structure$numeric_vars
  num_vars_bca <- bca_analysis$variable_structure$numeric_vars
  
  message("\nNumerische Variablen mit potenziellen Ausreißern:")
  
  for (var in num_vars_full) {
    if (!all(is.na(full_data[[var]]))) {
      var_data <- full_data[[var]][!is.na(full_data[[var]])]
      q1 <- quantile(var_data, 0.25)
      q3 <- quantile(var_data, 0.75)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      
      outliers_count <- sum(var_data < lower_bound | var_data > upper_bound)
      outliers_percent <- round(outliers_count / length(var_data) * 100, 2)
      
      if (outliers_percent > 5) {
        message(paste0("- ", var, " (", outliers_percent, "% Ausreißer)"))
      }
    }
  }
  
  for (var in num_vars_bca) {
    if (!all(is.na(bca_data[[var]]))) {
      var_data <- bca_data[[var]][!is.na(bca_data[[var]])]
      q1 <- quantile(var_data, 0.25)
      q3 <- quantile(var_data, 0.75)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      
      outliers_count <- sum(var_data < lower_bound | var_data > upper_bound)
      outliers_percent <- round(outliers_count / length(var_data) * 100, 2)
      
      if (outliers_percent > 5) {
        message(paste0("- ", var, " (", outliers_percent, "% Ausreißer)"))
      }
    }
  }
  
  message("\nEmpfehlungen für Ausreißer:")
  message("1. Winsorisierung: Begrenzung auf 1. und 99. Perzentil")
  message("2. Logarithmische Transformation für stark rechtsschiefe Verteilungen")
  message("3. Robuste Skalierung mit Median und IQR statt Mittelwert und Standardabweichung")
  
  # Zusammenfassung der Empfehlungen
  message("\nZUSAMMENFASSUNG DER EMPFEHLUNGEN:")
  message("1. Datensätze über gemeinsame ID-Variable zusammenführen")
  message("2. Unnötige Variablen entfernen (IDs, Namen, redundante Variablen)")
  message("3. Kategoriale Variablen in Faktoren umwandeln, ggf. Levels gruppieren")
  message("4. Fehlende Werte durch geeignete Imputationsmethoden ersetzen")
  message("5. Ausreißer behandeln (Winsorisierung oder Transformation)")
  message("6. Datumsvariablen in korrektes Format umwandeln")
  message("7. Neue abgeleitete Variablen erstellen (z.B. Alter aus Geburtsdatum)")
  
  return(list(
    keep_vars = list(
      demographic = demographic_vars,
      clinical = clinical_vars,
      bca = bca_vars,
      outcome = outcome_vars
    ),
    exclude_vars = exclude_vars
  ))
}

# 8. FINALEN DATENSATZ ERSTELLEN
create_final_dataset <- function(full_data, bca_data, recommendations) {
  message("\n8. FINALEN DATENSATZ ERSTELLEN")
  
  # Identifizieren der gemeinsamen Spalten für den Join
  common_cols <- intersect(names(full_data), names(bca_data))
  message("\nGemeinsame Spalten in beiden Datensätzen:")
  print(common_cols)
  
  # Wähle die erste gemeinsame Spalte für den Join
  if (length(common_cols) == 0) {
    message("FEHLER: Keine gemeinsamen Spalten für den Join gefunden.")
    return(NULL)
  }
  
  join_col <- common_cols[1]
  message(paste0("\nVerwende Spalte '", join_col, "' für den Join."))
  
