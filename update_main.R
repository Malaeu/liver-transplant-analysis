# update_main.R
# Skript zur Aktualisierung der main.R-Datei, um den bereinigten Datensatz zu verwenden

library(tidyverse)

# Funktion zum Aktualisieren der main.R-Datei
update_main_script <- function(main_path, output_path) {
  message("Lese main.R...")
  
  # Datei einlesen
  main_lines <- readLines(main_path)
  
  # Neue Zeilen für das Laden des bereinigten Datensatzes
  new_load_section <- c(
    "# 1. LOAD CLEAN DATASET --------------------------------------------------",
    "",
    "# Bereinigten Analysedatensatz laden",
    "message(\"Lade bereinigten Analysedatensatz...\")",
    "analysis_data <- readRDS(\"analysis_dataset_clean.rds\")",
    "message(paste0(\"Analysedatensatz geladen: \", nrow(analysis_data), \" Zeilen, \", ncol(analysis_data), \" Spalten\"))",
    "",
    "# Überprüfen der Variablen im Datensatz",
    "message(\"Verfügbare Variablen im Analysedatensatz:\")",
    "print(names(analysis_data))",
    "",
    "# Zusammenfassung des Datensatzes",
    "message(\"Zusammenfassung des Analysedatensatzes:\")",
    "print(summary(analysis_data[, c(\"age\", \"sex\", \"bmi\", \"lab_meld\", \"status\", \"waitlist_time_months\")]))",
    ""
  )
  
  # Finde den Abschnitt, der ersetzt werden soll (Datenladen und Vorverarbeitung)
  start_pattern <- "# 1. LOAD AND PREPROCESS DATA"
  end_pattern <- "# 2. EXPLORATORY DATA ANALYSIS"
  
  start_idx <- grep(start_pattern, main_lines)[1]
  end_idx <- grep(end_pattern, main_lines)[1] - 1
  
  if (is.na(start_idx) || is.na(end_idx)) {
    message("WARNUNG: Konnte die Abschnitte zum Ersetzen nicht finden. Füge neuen Code am Anfang ein.")
    # Füge am Anfang ein
    updated_lines <- c(new_load_section, main_lines)
  } else {
    # Ersetze den Abschnitt
    updated_lines <- c(
      main_lines[1:(start_idx-1)],
      new_load_section,
      main_lines[end_idx:length(main_lines)]
    )
  }
  
  # Aktualisiere die Variablennamen in den Analyseabschnitten
  # Ersetze alle Vorkommen von "analysis_data$lab_meld" durch "analysis_data$lab_meld"
  # (falls die Variablennamen sich geändert haben)
  updated_lines <- gsub("analysis_data\\$lab_meld\\.x", "analysis_data\\$lab_meld", updated_lines)
  updated_lines <- gsub("analysis_data\\$lab_meld\\.y", "analysis_data\\$lab_meld", updated_lines)
  
  # Ersetze alle Vorkommen von "analysis_data$bmi.x" durch "analysis_data$bmi"
  updated_lines <- gsub("analysis_data\\$bmi\\.x", "analysis_data\\$bmi", updated_lines)
  updated_lines <- gsub("analysis_data\\$bmi\\.y", "analysis_data\\$bmi", updated_lines)
  
  # Ersetze alle Vorkommen von "analysis_data$sex.x" durch "analysis_data$sex"
  updated_lines <- gsub("analysis_data\\$sex\\.x", "analysis_data\\$sex", updated_lines)
  updated_lines <- gsub("analysis_data\\$sex\\.y", "analysis_data\\$sex", updated_lines)
  
  # Ersetze alle Vorkommen von "analysis_data$age.x" durch "analysis_data$age"
  updated_lines <- gsub("analysis_data\\$age\\.x", "analysis_data\\$age", updated_lines)
  updated_lines <- gsub("analysis_data\\$age\\.y", "analysis_data\\$age", updated_lines)
  
  # Aktualisiere die Formel für die Überlebensanalyse
  # Suche nach der Zeile mit "aorsf_formula <-"
  aorsf_idx <- grep("aorsf_formula <-", updated_lines)
  
  if (length(aorsf_idx) > 0) {
    # Ersetze die Formel
    updated_lines[aorsf_idx] <- "aorsf_formula <- \"Surv(waitlist_time_months, status) ~ age + factor(sex) + bmi + lab_meld\""
  }
  
  # Schreibe die aktualisierte Datei
  writeLines(updated_lines, output_path)
  message(paste0("Aktualisierte main.R gespeichert unter: ", output_path))
}

# Funktion zum Aktualisieren der bc_meld_formula.R-Datei
update_bc_meld_script <- function(bc_meld_path, output_path) {
  message("Lese bc_meld_formula.R...")
  
  # Datei einlesen
  bc_meld_lines <- readLines(bc_meld_path)
  
  # Aktualisiere die Variablennamen
  updated_lines <- gsub("data\\$lab_meld\\.x", "data\\$lab_meld", bc_meld_lines)
  updated_lines <- gsub("data\\$lab_meld\\.y", "data\\$lab_meld", updated_lines)
  
  # Schreibe die aktualisierte Datei
  writeLines(updated_lines, output_path)
  message(paste0("Aktualisierte bc_meld_formula.R gespeichert unter: ", output_path))
}

# Ausführung
if (!interactive()) {
  # Pfade zu den Dateien
  main_path <- "main.R"
  bc_meld_path <- "src/bc_meld_formula.R"
  
  # Ausgabepfade
  main_output_path <- "main_updated.R"
  bc_meld_output_path <- "src/bc_meld_formula_updated.R"
  
  # Aktualisiere die Dateien
  update_main_script(main_path, main_output_path)
  update_bc_meld_script(bc_meld_path, bc_meld_output_path)
}