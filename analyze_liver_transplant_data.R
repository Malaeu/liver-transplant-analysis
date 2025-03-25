# ============================================================================
# ANALYSE VON LEBERTRANSPLANTATIONSDATEN MIT KÖRPERZUSAMMENSETZUNGSANALYSE
# ============================================================================
# Dieses Skript analysiert Daten von Patienten auf der Warteliste für 
# Lebertransplantationen, mit besonderem Fokus auf die Körperzusammensetzung
# und deren Einfluss auf das Überleben.
# ============================================================================

# Bibliotheken laden
library(tidyverse)
library(survival)
library(survminer)
library(skimr)
library(knitr)
library(lubridate)

# 1. DATEN LADEN UND GRUNDLEGENDE INFORMATIONEN
# ============================================================================

# Hauptdatensatz laden
full_data <- read.csv("imputed_data_full.csv", stringsAsFactors = FALSE)
message(paste0("Hauptdatensatz geladen: ", nrow(full_data), " Zeilen, ", ncol(full_data), " Spalten"))

# BCA-Datensatz laden
bca_data <- readRDS("wl_df_with_bca.rds")
message(paste0("BCA-Datensatz geladen: ", nrow(bca_data), " Zeilen, ", ncol(bca_data), " Spalten"))

# 2. VARIABLEN IDENTIFIZIEREN UND KATEGORISIEREN
# ============================================================================

# Demografische Variablen
demographic_vars <- c(
  "age", "sex", "kg", "cm", "bmi"
)

# Klinische Variablen
clinical_vars <- c(
  "lab_meld", "exc_meld", "diagnosis_group", "diagnosis_subgroup",
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

# Variablen zum Ausschließen
exclude_vars <- c(
  "etnr_id", "etnr", "lebensfallnr", "name", "vorname", "acsn_nr", "tx.nr",
  "birth_date", "date_of_birth", "relisting_date", "re.list_datum", "death_date", 
  "last_contact", "letzter_kontakt", "date_of_ltx", "date_of_ct",
  "diagnosis_1", "diagnosis_2", "diagnosis_3", "diagnose", "diagnosis",
  "relisting_diagnosis", "diagnose_relistung", "diagnosis_to_clean"
)

# 3. DATENSÄTZE ZUSAMMENFÜHREN
# ============================================================================

# Umbenennen der ID-Spalte im BCA-Datensatz für konsistentes Joining
if ("etnr" %in% names(bca_data) && "etnr_id" %in% names(full_data)) {
  bca_data <- bca_data %>% rename(etnr_id = etnr)
  message("BCA-Datensatz: 'etnr' in 'etnr_id' umbenannt für konsistentes Joining")
}

# Identifizieren der gemeinsamen Spalten für den Join
common_cols <- intersect(names(full_data), names(bca_data))
message(paste0("Gemeinsame Spalten in beiden Datensätzen: ", length(common_cols)))

# Wähle die erste gemeinsame Spalte für den Join
join_col <- "etnr_id"
message(paste0("Verwende Spalte '", join_col, "' für den Join"))

# Datensätze zusammenführen
merged_data <- full_data %>%
  left_join(bca_data %>% select(etnr_id, all_of(bca_vars)), by = join_col)

message(paste0("Zusammengeführter Datensatz: ", nrow(merged_data), " Zeilen, ", ncol(merged_data), " Spalten"))

# 4. VARIABLEN AUSWÄHLEN UND BEREINIGEN
# ============================================================================

# Relevante Variablen auswählen
selected_vars <- c(
  join_col,
  demographic_vars,
  clinical_vars,
  bca_vars,
  outcome_vars
)

# Verfügbare Variablen identifizieren
available_vars <- selected_vars[selected_vars %in% names(merged_data)]
missing_vars <- selected_vars[!selected_vars %in% names(merged_data)]

message(paste0("Verfügbare Variablen: ", length(available_vars), " von ", length(selected_vars)))
if (length(missing_vars) > 0) {
  message(paste0("Fehlende Variablen: ", paste(missing_vars, collapse = ", ")))
}

# Datensatz auf ausgewählte Variablen reduzieren
analysis_data <- merged_data %>% select(all_of(available_vars))

message(paste0("Reduzierter Datensatz: ", nrow(analysis_data), " Zeilen, ", ncol(analysis_data), " Spalten"))

# 5. FEHLENDE WERTE ANALYSIEREN
# ============================================================================

# Anzahl und Prozentsatz fehlender Werte pro Variable
na_counts <- sapply(analysis_data, function(x) sum(is.na(x)))
na_percent <- round(na_counts / nrow(analysis_data) * 100, 2)

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
  }
  
  # Variablen mit moderatem Anteil fehlender Werte (20-50%)
  moderate_vars <- vars_with_na$Variable[vars_with_na$Prozent_Fehlend > 20 & vars_with_na$Prozent_Fehlend <= 50]
  if (length(moderate_vars) > 0) {
    message("\nVariablen mit moderatem Anteil fehlender Werte (20-50%):")
    print(moderate_vars)
  }
} else {
  message("\nKeine fehlenden Werte im Datensatz gefunden.")
}

# 6. ZWEI ANALYSEDATENSÄTZE ERSTELLEN
# ============================================================================

# 6.1 Vollständiger Datensatz (mit fehlenden BCA-Werten)
# Dieser Datensatz enthält alle Patienten, auch die ohne BCA-Daten
full_analysis_data <- analysis_data

# 6.2 BCA-Datensatz (nur Patienten mit BCA-Daten)
# Dieser Datensatz enthält nur Patienten mit vollständigen BCA-Daten
bca_analysis_data <- analysis_data %>%
  filter(!is.na(muscle) & !is.na(sat) & !is.na(vat) & 
         !is.na(imat) & !is.na(eat) & !is.na(pat) & !is.na(tat))

message(paste0("\nVollständiger Analysedatensatz: ", nrow(full_analysis_data), " Patienten"))
message(paste0("BCA-Analysedatensatz: ", nrow(bca_analysis_data), " Patienten"))

# 7. KATEGORIALE VARIABLEN ERSTELLEN
# ============================================================================

# 7.1 Vollständiger Datensatz
# BMI-Kategorien
if ("bmi" %in% names(full_analysis_data)) {
  full_analysis_data <- full_analysis_data %>%
    mutate(bmi_category = case_when(
      bmi < 18.5 ~ "Untergewicht",
      bmi >= 18.5 & bmi < 25 ~ "Normalgewicht",
      bmi >= 25 & bmi < 30 ~ "Übergewicht",
      bmi >= 30 ~ "Adipositas",
      TRUE ~ NA_character_
    ))
}

# MELD-Kategorien
if ("lab_meld" %in% names(full_analysis_data)) {
  full_analysis_data <- full_analysis_data %>%
    mutate(meld_category = case_when(
      lab_meld < 10 ~ "MELD < 10",
      lab_meld >= 10 & lab_meld < 20 ~ "MELD 10-19",
      lab_meld >= 20 & lab_meld < 30 ~ "MELD 20-29",
      lab_meld >= 30 ~ "MELD ≥ 30",
      TRUE ~ NA_character_
    ))
}

# Alter-Kategorien
if ("age" %in% names(full_analysis_data)) {
  full_analysis_data <- full_analysis_data %>%
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

# Geschlecht standardisieren
if ("sex" %in% names(full_analysis_data)) {
  full_analysis_data <- full_analysis_data %>%
    mutate(sex_std = case_when(
      toupper(as.character(sex)) %in% c("M", "MALE", "MÄNNLICH") ~ "M",
      toupper(as.character(sex)) %in% c("F", "W", "FEMALE", "WEIBLICH") ~ "F",
      TRUE ~ NA_character_
    ))
}

# 7.2 BCA-Datensatz
# Die gleichen Kategorien für den BCA-Datensatz erstellen
if ("bmi" %in% names(bca_analysis_data)) {
  bca_analysis_data <- bca_analysis_data %>%
    mutate(bmi_category = case_when(
      bmi < 18.5 ~ "Untergewicht",
      bmi >= 18.5 & bmi < 25 ~ "Normalgewicht",
      bmi >= 25 & bmi < 30 ~ "Übergewicht",
      bmi >= 30 ~ "Adipositas",
      TRUE ~ NA_character_
    ))
}

if ("lab_meld" %in% names(bca_analysis_data)) {
  bca_analysis_data <- bca_analysis_data %>%
    mutate(meld_category = case_when(
      lab_meld < 10 ~ "MELD < 10",
      lab_meld >= 10 & lab_meld < 20 ~ "MELD 10-19",
      lab_meld >= 20 & lab_meld < 30 ~ "MELD 20-29",
      lab_meld >= 30 ~ "MELD ≥ 30",
      TRUE ~ NA_character_
    ))
}

if ("age" %in% names(bca_analysis_data)) {
  bca_analysis_data <- bca_analysis_data %>%
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

if ("sex" %in% names(bca_analysis_data)) {
  bca_analysis_data <- bca_analysis_data %>%
    mutate(sex_std = case_when(
      toupper(as.character(sex)) %in% c("M", "MALE", "MÄNNLICH") ~ "M",
      toupper(as.character(sex)) %in% c("F", "W", "FEMALE", "WEIBLICH") ~ "F",
      TRUE ~ NA_character_
    ))
}

# 8. NORMALISIERTE BCA-VARIABLEN ERSTELLEN
# ============================================================================

# Normalisierung der BCA-Variablen nach Körpergröße
if (all(c("cm", "muscle", "sat", "vat", "imat", "eat", "pat", "tat") %in% names(bca_analysis_data))) {
  bca_analysis_data <- bca_analysis_data %>%
    mutate(
      muscle_norm = muscle / ((cm / 100)^2),
      sat_norm = sat / ((cm / 100)^2),
      vat_norm = vat / ((cm / 100)^2),
      imat_norm = imat / ((cm / 100)^2),
      eat_norm = eat / ((cm / 100)^2),
      pat_norm = pat / ((cm / 100)^2),
      tat_norm = tat / ((cm / 100)^2)
    )
}

# 9. DESKRIPTIVE STATISTIK
# ============================================================================

# 9.1 Vollständiger Datensatz
message("\nDeskriptive Statistik des vollständigen Datensatzes:")

# Demografische Variablen
demo_vars_full <- demographic_vars[demographic_vars %in% names(full_analysis_data)]
if (length(demo_vars_full) > 0) {
  message("\nDemografische Variablen:")
  print(summary(full_analysis_data[, demo_vars_full]))
}

# Klinische Variablen
clin_vars_full <- clinical_vars[clinical_vars %in% names(full_analysis_data)]
if (length(clin_vars_full) > 0) {
  message("\nKlinische Variablen:")
  print(summary(full_analysis_data[, clin_vars_full]))
}

# Outcome-Variablen
out_vars_full <- outcome_vars[outcome_vars %in% names(full_analysis_data)]
if (length(out_vars_full) > 0) {
  message("\nOutcome-Variablen:")
  print(summary(full_analysis_data[, out_vars_full]))
}

# 9.2 BCA-Datensatz
message("\nDeskriptive Statistik des BCA-Datensatzes:")

# BCA-Variablen
bca_vars_avail <- bca_vars[bca_vars %in% names(bca_analysis_data)]
if (length(bca_vars_avail) > 0) {
  message("\nBCA-Variablen:")
  print(summary(bca_analysis_data[, bca_vars_avail]))
}

# 10. DATENSÄTZE SPEICHERN
# ============================================================================

# Vollständigen Analysedatensatz speichern
saveRDS(full_analysis_data, "full_analysis_dataset_clean.rds")
write.csv(full_analysis_data, "full_analysis_dataset_clean.csv", row.names = FALSE)
message("\nVollständiger Analysedatensatz gespeichert als 'full_analysis_dataset_clean.rds' und 'full_analysis_dataset_clean.csv'")

# BCA-Analysedatensatz speichern
saveRDS(bca_analysis_data, "bca_analysis_dataset_clean.rds")
write.csv(bca_analysis_data, "bca_analysis_dataset_clean.csv", row.names = FALSE)
message("BCA-Analysedatensatz gespeichert als 'bca_analysis_dataset_clean.rds' und 'bca_analysis_dataset_clean.csv'")

# 11. ÜBERLEBENSANALYSE
# ============================================================================

# 11.1 Kaplan-Meier-Überlebenskurven für den vollständigen Datensatz
if (all(c("waitlist_time_months", "status") %in% names(full_analysis_data))) {
  message("\nErstelle Kaplan-Meier-Überlebenskurven für den vollständigen Datensatz...")
  
  # Gesamtüberlebenskurve
  km_fit_full <- survfit(Surv(waitlist_time_months, status) ~ 1, data = full_analysis_data)
  
  # Überlebenskurven nach MELD-Kategorie
  if ("meld_category" %in% names(full_analysis_data)) {
    km_fit_meld_full <- survfit(Surv(waitlist_time_months, status) ~ meld_category, data = full_analysis_data)
    
    # Plot erstellen
    pdf("survival_by_meld_full.pdf", width = 10, height = 8)
    print(ggsurvplot(
      km_fit_meld_full,
      data = full_analysis_data,
      title = "Überleben nach MELD-Kategorie (Vollständiger Datensatz)",
      xlab = "Zeit (Monate)",
      ylab = "Überlebenswahrscheinlichkeit",
      conf.int = TRUE,
      risk.table = TRUE,
      palette = "jco",
      ggtheme = theme_minimal()
    ))
    dev.off()
    message("Überlebenskurven nach MELD-Kategorie gespeichert als 'survival_by_meld_full.pdf'")
  }
}

# 11.2 Kaplan-Meier-Überlebenskurven für den BCA-Datensatz
if (all(c("waitlist_time_months", "status") %in% names(bca_analysis_data))) {
  message("\nErstelle Kaplan-Meier-Überlebenskurven für den BCA-Datensatz...")
  
  # Gesamtüberlebenskurve
  km_fit_bca <- survfit(Surv(waitlist_time_months, status) ~ 1, data = bca_analysis_data)
  
  # Überlebenskurven nach MELD-Kategorie
  if ("meld_category" %in% names(bca_analysis_data)) {
    km_fit_meld_bca <- survfit(Surv(waitlist_time_months, status) ~ meld_category, data = bca_analysis_data)
    
    # Plot erstellen
    pdf("survival_by_meld_bca.pdf", width = 10, height = 8)
    print(ggsurvplot(
      km_fit_meld_bca,
      data = bca_analysis_data,
      title = "Überleben nach MELD-Kategorie (BCA-Datensatz)",
      xlab = "Zeit (Monate)",
      ylab = "Überlebenswahrscheinlichkeit",
      conf.int = TRUE,
      risk.table = TRUE,
      palette = "jco",
      ggtheme = theme_minimal()
    ))
    dev.off()
    message("Überlebenskurven nach MELD-Kategorie gespeichert als 'survival_by_meld_bca.pdf'")
  }
}

# 12. ZUSAMMENFASSUNG
# ============================================================================

message("\nZUSAMMENFASSUNG DER ANALYSE:")
message(paste0("Vollständiger Datensatz: ", nrow(full_analysis_data), " Patienten, ", ncol(full_analysis_data), " Variablen"))
message(paste0("BCA-Datensatz: ", nrow(bca_analysis_data), " Patienten, ", ncol(bca_analysis_data), " Variablen"))
message("Analyse abgeschlossen. Ergebnisse wurden gespeichert.")