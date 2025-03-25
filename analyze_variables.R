# analyze_variables.R
# Skript zur detaillierten Analyse der Variablen in den Datensätzen

library(tidyverse)
library(skimr)  # Für zusammenfassende Statistiken
library(knitr)  # Für Tabellendarstellung
library(lubridate)

# Funktion zum Laden der Datensätze
load_datasets <- function(full_data_path, bca_data_path) {
  message("Lade Datensätze...")
  
  # Hauptdatensatz laden
  full_data <- read.csv(full_data_path, stringsAsFactors = FALSE)
  message(paste0("Hauptdatensatz geladen: ", nrow(full_data), " Zeilen, ", ncol(full_data), " Spalten"))
  
  # BCA-Datensatz laden
  bca_data <- readRDS(bca_data_path)
  message(paste0("BCA-Datensatz geladen: ", nrow(bca_data), " Zeilen, ", ncol(bca_data), " Spalten"))
  
  # Datensätze zurückgeben
  return(list(full_data = full_data, bca_data = bca_data))
}

# Funktion zur Analyse der Variablentypen
analyze_variable_types <- function(data, dataset_name) {
  message(paste0("Analysiere Variablentypen im ", dataset_name, "..."))
  
  # Variablentypen ermitteln
  var_types <- sapply(data, function(x) class(x)[1])  # Nur die erste Klasse nehmen
  var_types_df <- data.frame(
    Variable = names(var_types),
    Typ = var_types,
    stringsAsFactors = FALSE
  )
  
  # Zusammenfassung der Variablentypen
  type_summary <- table(var_types)
  message("Zusammenfassung der Variablentypen:")
  print(type_summary)
  
  # Detaillierte Auflistung nach Typ
  message("Variablen nach Typ:")
  for (type in unique(var_types)) {
    vars_of_type <- names(var_types[var_types == type])
    message(paste0("  ", type, " (", length(vars_of_type), "):"))
    message(paste0("    ", paste(head(vars_of_type, 10), collapse = ", "), 
                  ifelse(length(vars_of_type) > 10, "...", "")))
  }
  
  return(var_types_df)
}

# Funktion zur Analyse fehlender Werte
analyze_missing_values <- function(data, dataset_name) {
  message(paste0("Analysiere fehlende Werte im ", dataset_name, "..."))
  
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
    message(paste0("Variablen mit fehlenden Werten (Top 20):"))
    print(head(vars_with_na, 20))
    message(paste0("Insgesamt ", nrow(vars_with_na), " Variablen mit fehlenden Werten"))
  } else {
    message("Keine fehlenden Werte im Datensatz gefunden.")
  }
  
  return(na_df)
}

# Funktion zur Analyse von Character-Variablen
analyze_character_variables <- function(data, dataset_name) {
  message(paste0("Analysiere Character-Variablen im ", dataset_name, "..."))
  
  # Character-Variablen identifizieren
  char_vars <- names(data)[sapply(data, is.character)]
  
  if (length(char_vars) > 0) {
    message(paste0("Gefundene Character-Variablen (", length(char_vars), "):"))
    
    # Für jede Character-Variable
    char_var_summary <- list()
    
    for (var in char_vars) {
      # Anzahl unterschiedlicher Werte
      unique_values <- unique(data[[var]])
      n_unique <- length(unique_values)
      
      # Häufigkeitsverteilung
      value_counts <- table(data[[var]], useNA = "ifany")
      value_counts_sorted <- sort(value_counts, decreasing = TRUE)
      
      # Top-5 häufigste Werte
      top_values <- head(value_counts_sorted, 5)
      top_values_str <- paste(names(top_values), " (", top_values, ")", sep = "", collapse = ", ")
      
      # Zusammenfassung speichern
      char_var_summary[[var]] <- list(
        variable = var,
        n_unique = n_unique,
        top_values = top_values,
        top_values_str = top_values_str
      )
      
      # Ausgabe
      message(paste0("  ", var, ": ", n_unique, " unterschiedliche Werte"))
      message(paste0("    Top-5 häufigste Werte: ", top_values_str))
    }
    
    return(char_var_summary)
  } else {
    message("Keine Character-Variablen im Datensatz gefunden.")
    return(NULL)
  }
}

# Funktion zur Analyse numerischer Variablen
analyze_numeric_variables <- function(data, dataset_name) {
  message(paste0("Analysiere numerische Variablen im ", dataset_name, "..."))
  
  # Numerische Variablen identifizieren
  num_vars <- names(data)[sapply(data, is.numeric)]
  
  if (length(num_vars) > 0) {
    message(paste0("Gefundene numerische Variablen (", length(num_vars), "):"))
    
    # Zusammenfassende Statistiken für numerische Variablen
    num_summary <- data %>%
      select(all_of(num_vars)) %>%
      skim() %>%
      as.data.frame()
    
    # Ausgabe der wichtigsten Statistiken für die ersten 20 Variablen
    num_stats <- data.frame(
      Variable = num_vars,
      Min = sapply(data[num_vars], min, na.rm = TRUE),
      Max = sapply(data[num_vars], max, na.rm = TRUE),
      Mittelwert = sapply(data[num_vars], mean, na.rm = TRUE),
      Median = sapply(data[num_vars], median, na.rm = TRUE),
      SD = sapply(data[num_vars], sd, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
    
    message("Zusammenfassende Statistiken (erste 20 Variablen):")
    print(head(num_stats, 20))
    
    return(num_stats)
  } else {
    message("Keine numerischen Variablen im Datensatz gefunden.")
    return(NULL)
  }
}

# Funktion zur Analyse von Faktor-Variablen
analyze_factor_variables <- function(data, dataset_name) {
  message(paste0("Analysiere Faktor-Variablen im ", dataset_name, "..."))
  
  # Faktor-Variablen identifizieren
  factor_vars <- names(data)[sapply(data, is.factor)]
  
  if (length(factor_vars) > 0) {
    message(paste0("Gefundene Faktor-Variablen (", length(factor_vars), "):"))
    
    # Für jede Faktor-Variable
    factor_var_summary <- list()
    
    for (var in factor_vars) {
      # Anzahl der Levels
      levels_count <- length(levels(data[[var]]))
      
      # Häufigkeitsverteilung
      value_counts <- table(data[[var]], useNA = "ifany")
      
      # Ausgabe
      message(paste0("  ", var, ": ", levels_count, " Levels"))
      message(paste0("    Verteilung: ", paste(names(value_counts), " (", value_counts, ")", sep = "", collapse = ", ")))
      
      # Zusammenfassung speichern
      factor_var_summary[[var]] <- list(
        variable = var,
        levels_count = levels_count,
        value_counts = value_counts
      )
    }
    
    return(factor_var_summary)
  } else {
    message("Keine Faktor-Variablen im Datensatz gefunden.")
    return(NULL)
  }
}

# Funktion zur Analyse von Datumsvariablen
analyze_date_variables <- function(data, dataset_name) {
  message(paste0("Analysiere potenzielle Datumsvariablen im ", dataset_name, "..."))
  
  # Potenzielle Datumsvariablen identifizieren (basierend auf Variablennamen)
  date_var_patterns <- c("date", "datum", "day", "month", "year", "zeit", "time")
  potential_date_vars <- names(data)[grepl(paste(date_var_patterns, collapse = "|"), names(data), ignore.case = TRUE)]
  
  if (length(potential_date_vars) > 0) {
    message(paste0("Potenzielle Datumsvariablen (", length(potential_date_vars), "):"))
    
    # Für jede potenzielle Datumsvariable
    date_var_summary <- list()
    
    for (var in potential_date_vars) {
      # Variablentyp
      var_type <- class(data[[var]])
      
      # Beispielwerte
      example_values <- head(unique(data[[var]]), 5)
      example_str <- paste(example_values, collapse = ", ")
      
      # Ausgabe
      message(paste0("  ", var, " (", var_type, ")"))
      message(paste0("    Beispielwerte: ", example_str))
      
      # Zusammenfassung speichern
      date_var_summary[[var]] <- list(
        variable = var,
        type = var_type,
        examples = example_values
      )
    }
    
    return(date_var_summary)
  } else {
    message("Keine potenziellen Datumsvariablen im Datensatz gefunden.")
    return(NULL)
  }
}

# Funktion zur Analyse der Korrelation zwischen numerischen Variablen
analyze_correlations <- function(data, dataset_name) {
  message(paste0("Analysiere Korrelationen zwischen numerischen Variablen im ", dataset_name, "..."))
  
  # Numerische Variablen identifizieren
  num_vars <- names(data)[sapply(data, is.numeric)]
  
  if (length(num_vars) > 0) {
    # Korrelationsmatrix berechnen
    # Wir beschränken uns auf die ersten 20 numerischen Variablen, um die Ausgabe übersichtlich zu halten
    num_vars_subset <- head(num_vars, 20)
    
    # Korrelationsmatrix berechnen (mit Ausnahmebehandlung)
    tryCatch({
      cor_matrix <- cor(data[, num_vars_subset], use = "pairwise.complete.obs")
      
      # Stark korrelierte Variablenpaare identifizieren (|r| > 0.7)
      cor_df <- as.data.frame(as.table(cor_matrix))
      names(cor_df) <- c("Variable1", "Variable2", "Korrelation")
      
      # Duplikate und Selbstkorrelationen entfernen
      cor_df <- cor_df[cor_df$Variable1 != cor_df$Variable2, ]
      cor_df <- cor_df[!duplicated(t(apply(cor_df[, 1:2], 1, sort))), ]
      
      # Nach Korrelationsstärke sortieren
      cor_df$AbsKorrelation <- abs(cor_df$Korrelation)
      cor_df <- cor_df[order(-cor_df$AbsKorrelation), ]
      
      # Stark korrelierte Variablenpaare anzeigen
      strong_cor <- cor_df[cor_df$AbsKorrelation > 0.7, ]
      if (nrow(strong_cor) > 0) {
        message("Stark korrelierte Variablenpaare (|r| > 0.7):")
        print(head(strong_cor[, c("Variable1", "Variable2", "Korrelation")], 20))
      } else {
        message("Keine stark korrelierten Variablenpaare gefunden.")
      }
      
      return(cor_df)
    }, error = function(e) {
      message(paste0("Fehler bei der Berechnung der Korrelationsmatrix: ", e$message))
      return(NULL)
    })
  } else {
    message("Keine numerischen Variablen für Korrelationsanalyse gefunden.")
    return(NULL)
  }
}

# Funktion zur Analyse der Variablen für die Überlebensanalyse
analyze_survival_variables <- function(data, dataset_name, time_var, event_var) {
  message(paste0("Analysiere Überlebensvariablen im ", dataset_name, "..."))
  
  # Überprüfen, ob die Variablen existieren
  if (!time_var %in% names(data)) {
    message(paste0("FEHLER: Zeitvariable '", time_var, "' nicht im Datensatz gefunden."))
    return(NULL)
  }
  
  if (!event_var %in% names(data)) {
    message(paste0("FEHLER: Ereignisvariable '", event_var, "' nicht im Datensatz gefunden."))
    return(NULL)
  }
  
  # Analyse der Zeitvariable
  time_data <- data[[time_var]]
  time_summary <- summary(time_data)
  message(paste0("Zeitvariable '", time_var, "':"))
  message(paste0("  Zusammenfassung: ", paste(names(time_summary), time_summary, sep = " = ", collapse = ", ")))
  message(paste0("  Anzahl fehlender Werte: ", sum(is.na(time_data))))
  
  # Analyse der Ereignisvariable
  event_data <- data[[event_var]]
  event_table <- table(event_data, useNA = "ifany")
  message(paste0("Ereignisvariable '", event_var, "':"))
  message(paste0("  Verteilung: ", paste(names(event_table), " (", event_table, ")", sep = "", collapse = ", ")))
  message(paste0("  Anzahl fehlender Werte: ", sum(is.na(event_data))))
  
  # Zusammenfassung zurückgeben
  return(list(
    time_var = time_var,
    time_summary = time_summary,
    time_missing = sum(is.na(time_data)),
    event_var = event_var,
    event_table = event_table,
    event_missing = sum(is.na(event_data))
  ))
}

# Funktion zur Erstellung einer Empfehlung für relevante Variablen
recommend_variables <- function(full_data, bca_data, merged_data) {
  # Wenn merged_data NULL ist, verwenden wir nur full_data
  if (is.null(merged_data)) {
    message("Verwende nur den Hauptdatensatz für die Empfehlung.")
    merged_data <- full_data
  }
  
  message("Erstelle Empfehlung für relevante Variablen...")
  
  # Kategorien von Variablen
  categories <- list(
    demographic = c("age", "sex", "bmi", "height", "weight"),
    clinical = c("meld", "lab", "score", "diagnosis"),
    bca = c("muscle", "sat", "vat", "imat", "eat", "pat", "tat"),
    outcome = c("waitlist", "time", "status", "survival", "death", "transplant"),
    exclude = c("id", "name", "vorname", "birth", "date", "datum", "studie", "etnr")
  )
  
  # Funktion zur Kategorisierung von Variablen
  categorize_variable <- function(var_name) {
    var_lower <- tolower(var_name)
    
    for (cat_name in names(categories)) {
      patterns <- categories[[cat_name]]
      if (any(sapply(patterns, function(p) grepl(p, var_lower)))) {
        return(cat_name)
      }
    }
    
    return("other")
  }
  
  # Variablen aus dem zusammengeführten Datensatz kategorisieren
  merged_vars <- names(merged_data)
  var_categories <- sapply(merged_vars, categorize_variable)
  
  # Variablen nach Kategorie gruppieren
  vars_by_category <- split(merged_vars, var_categories)
  
  # Empfohlene Variablen
  recommended <- list(
    demographic = vars_by_category$demographic,
    clinical = vars_by_category$clinical,
    bca = vars_by_category$bca,
    outcome = vars_by_category$outcome
  )
  
  # Ausgabe der empfohlenen Variablen
  message("Empfohlene Variablen für die Analyse:")
  
  for (cat in names(recommended)) {
    if (length(recommended[[cat]]) > 0) {
      message(paste0("  ", toupper(cat), " (", length(recommended[[cat]]), "):"))
      message(paste0("    ", paste(recommended[[cat]], collapse = ", ")))
    }
  }
  
  # Variablen, die ausgeschlossen werden sollten
  excluded <- vars_by_category$exclude
  if (length(excluded) > 0) {
    message(paste0("  AUSZUSCHLIESSEN (", length(excluded), "):"))
    message(paste0("    ", paste(excluded, collapse = ", ")))
  }
  
  # Sonstige Variablen
  other <- vars_by_category$other
  if (length(other) > 0) {
    message(paste0("  SONSTIGE (", length(other), "):"))
    message(paste0("    ", paste(head(other, 20), collapse = ", "), 
                  ifelse(length(other) > 20, "...", "")))
  }
  
  return(list(
    recommended = recommended,
    excluded = excluded,
    other = other
  ))
}

# Hauptfunktion zur Analyse der Datensätze
analyze_datasets <- function(full_data_path, bca_data_path) {
  # Datensätze laden
  datasets <- load_datasets(full_data_path, bca_data_path)
  full_data <- datasets$full_data
  bca_data <- datasets$bca_data
  
  # Überprüfen der Spaltennamen in beiden Datensätzen
  message("Spaltennamen im Hauptdatensatz:")
  print(head(names(full_data), 20))
  message("...")
  
  message("Spaltennamen im BCA-Datensatz:")
  print(head(names(bca_data), 20))
  message("...")
  
  # Identifizieren der gemeinsamen Spalten für den Join
  common_cols <- intersect(names(full_data), names(bca_data))
  message("Gemeinsame Spalten in beiden Datensätzen:")
  print(common_cols)
  
  # Wenn keine gemeinsamen Spalten gefunden wurden, analysieren wir die Datensätze einzeln
  if (length(common_cols) == 0) {
    message("WARNUNG: Keine gemeinsamen Spalten für den Join gefunden. Analysiere Datensätze einzeln.")
    merged_data <- full_data  # Verwende nur den Hauptdatensatz für die weitere Analyse
  } else {
    # Wähle die erste gemeinsame Spalte für den Join
    join_col <- common_cols[1]
    message(paste0("Verwende Spalte '", join_col, "' für den Join."))
    
    # Zusammenführen der Datensätze
    merged_data <- merge(full_data, bca_data, by = join_col, all = TRUE)
    message(paste0("Zusammengeführter Datensatz: ", nrow(merged_data), " Zeilen, ", ncol(merged_data), " Spalten"))
  }
  
  # Analyse der Variablentypen
  full_types <- analyze_variable_types(full_data, "Hauptdatensatz")
  bca_types <- analyze_variable_types(bca_data, "BCA-Datensatz")
  merged_types <- analyze_variable_types(merged_data, "Zusammengeführter Datensatz")
  
  # Analyse fehlender Werte
  full_na <- analyze_missing_values(full_data, "Hauptdatensatz")
  bca_na <- analyze_missing_values(bca_data, "BCA-Datensatz")
  merged_na <- analyze_missing_values(merged_data, "Zusammengeführter Datensatz")
  
  # Analyse von Character-Variablen
  full_char <- analyze_character_variables(full_data, "Hauptdatensatz")
  bca_char <- analyze_character_variables(bca_data, "BCA-Datensatz")
  merged_char <- analyze_character_variables(merged_data, "Zusammengeführter Datensatz")
  
  # Analyse numerischer Variablen
  full_num <- analyze_numeric_variables(full_data, "Hauptdatensatz")
  bca_num <- analyze_numeric_variables(bca_data, "BCA-Datensatz")
  merged_num <- analyze_numeric_variables(merged_data, "Zusammengeführter Datensatz")
  
  # Analyse von Datumsvariablen
  full_dates <- analyze_date_variables(full_data, "Hauptdatensatz")
  bca_dates <- analyze_date_variables(bca_data, "BCA-Datensatz")
  merged_dates <- analyze_date_variables(merged_data, "Zusammengeführter Datensatz")
  
  # Analyse der Korrelationen
  merged_cor <- analyze_correlations(merged_data, "Zusammengeführter Datensatz")
  
  # Analyse der Überlebensvariablen (mit Fehlerbehandlung)
  survival_vars <- tryCatch({
    analyze_survival_variables(merged_data, "Zusammengeführter Datensatz", 
                              "waitlist_time_months", "status")
  }, error = function(e) {
    message(paste0("Fehler bei der Analyse der Überlebensvariablen: ", e$message))
    return(NULL)
  })
  
  # Empfehlung für relevante Variablen
  var_recommendations <- recommend_variables(full_data, bca_data, merged_data)
  
  # Ergebnisse zurückgeben
  return(list(
    datasets = datasets,
    merged_data = merged_data,
    variable_types = list(full = full_types, bca = bca_types, merged = merged_types),
    missing_values = list(full = full_na, bca = bca_na, merged = merged_na),
    character_vars = list(full = full_char, bca = bca_char, merged = merged_char),
    numeric_vars = list(full = full_num, bca = bca_num, merged = merged_num),
    date_vars = list(full = full_dates, bca = bca_dates, merged = merged_dates),
    correlations = merged_cor,
    survival_analysis = survival_vars,
    recommendations = var_recommendations
  ))
}

# Funktion zum Speichern der Analyseergebnisse
save_analysis_results <- function(results, output_path) {
  # Ergebnisse als RDS-Datei speichern
  saveRDS(results, output_path)
  message(paste0("Analyseergebnisse gespeichert unter: ", output_path))
}

# Ausführung
if (!interactive()) {
  # Pfade zu den Datensätzen
  full_data_path <- "imputed_data_full.csv"
  bca_data_path <- "wl_df_with_bca.rds"
  output_path <- "variable_analysis_results.rds"
  
  # Analyse durchführen
  results <- analyze_datasets(full_data_path, bca_data_path)
  
  # Ergebnisse speichern
  save_analysis_results(results, output_path)
}