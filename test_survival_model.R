# test_survival_model.R
# Skript zum Testen der Überlebensanalyse mit dem bereinigten Datensatz

library(tidyverse)
library(survival)
library(survminer)
library(randomForestSRC)

# Bereinigten Datensatz laden
load_clean_data <- function() {
  message("Lade bereinigten Analysedatensatz...")
  data <- readRDS("analysis_dataset_clean.rds")
  message(paste0("Analysedatensatz geladen: ", nrow(data), " Zeilen, ", ncol(data), " Spalten"))
  return(data)
}

# Deskriptive Statistik
describe_data <- function(data) {
  message("Deskriptive Statistik des Analysedatensatzes:")
  
  # Zusammenfassung der wichtigsten Variablen
  message("\nZusammenfassung der wichtigsten Variablen:")
  print(summary(data[, c("age", "bmi", "lab_meld", "waitlist_time_months", "status")]))
  
  # Verteilung der kategorialen Variablen
  message("\nVerteilung der kategorialen Variablen:")
  
  cat_vars <- c("sex", "bmi_category", "meld_category", "diagnosis_group")
  for (var in cat_vars) {
    if (var %in% names(data)) {
      message(paste0("\nVerteilung von ", var, ":"))
      print(table(data[[var]], useNA = "ifany"))
    }
  }
  
  # Überlebensstatus
  message("\nÜberlebensstatus:")
  print(table(data$status, useNA = "ifany"))
  
  # Mittlere und mediane Überlebenszeit
  message("\nÜberlebenszeit (Monate):")
  print(summary(data$waitlist_time_months))
}

# Kaplan-Meier-Überlebenskurven
plot_survival_curves <- function(data) {
  message("Erstelle Kaplan-Meier-Überlebenskurven...")
  
  # Gesamtüberlebenskurve
  km_fit <- survfit(Surv(data$waitlist_time_months, data$status) ~ 1, data = data)
  
  p1 <- ggsurvplot(
    km_fit,
    data = data,
    title = "Gesamtüberleben auf der Warteliste",
    xlab = "Zeit (Monate)",
    ylab = "Überlebenswahrscheinlichkeit",
    conf.int = TRUE,
    risk.table = TRUE,
    palette = "jco",
    ggtheme = theme_minimal()
  )
  
  print(p1)
  
  # Überlebenskurven nach MELD-Kategorie
  if ("meld_category" %in% names(data)) {
    km_fit_meld <- survfit(Surv(waitlist_time_months, status) ~ meld_category, data = data)
    
    p2 <- ggsurvplot(
      km_fit_meld,
      data = data,
      title = "Überleben nach MELD-Kategorie",
      xlab = "Zeit (Monate)",
      ylab = "Überlebenswahrscheinlichkeit",
      conf.int = TRUE,
      risk.table = TRUE,
      palette = "jco",
      ggtheme = theme_minimal()
    )
    
    print(p2)
  }
  
  # Überlebenskurven nach Geschlecht
  if ("sex" %in% names(data)) {
    km_fit_sex <- survfit(Surv(waitlist_time_months, status) ~ sex, data = data)
    
    p3 <- ggsurvplot(
      km_fit_sex,
      data = data,
      title = "Überleben nach Geschlecht",
      xlab = "Zeit (Monate)",
      ylab = "Überlebenswahrscheinlichkeit",
      conf.int = TRUE,
      risk.table = TRUE,
      palette = "jco",
      ggtheme = theme_minimal()
    )
    
    print(p3)
  }
  
  # Überlebenskurven nach Diagnosegruppe
  if ("diagnosis_group" %in% names(data)) {
    # Beschränke auf die häufigsten Diagnosegruppen für bessere Lesbarkeit
    top_groups <- names(sort(table(data$diagnosis_group), decreasing = TRUE)[1:5])
    data_subset <- data %>% filter(diagnosis_group %in% top_groups)
    
    km_fit_diag <- survfit(Surv(waitlist_time_months, status) ~ diagnosis_group, 
                          data = data_subset)
    
    p4 <- ggsurvplot(
      km_fit_diag,
      data = data_subset,
      title = "Überleben nach Diagnosegruppe (Top 5)",
      xlab = "Zeit (Monate)",
      ylab = "Überlebenswahrscheinlichkeit",
      conf.int = TRUE,
      risk.table = TRUE,
      palette = "jco",
      ggtheme = theme_minimal()
    )
    
    print(p4)
  }
  
  return(list(overall = p1, meld = p2, sex = p3, diagnosis = p4))
}

# Cox-Regressionsmodell
fit_cox_model <- function(data) {
  message("Erstelle Cox-Regressionsmodell...")
  
  # Grundlegendes Modell mit den wichtigsten Prädiktoren
  cox_formula <- "Surv(waitlist_time_months, status) ~ age + sex + bmi + lab_meld"
  
  # Modell anpassen
  cox_model <- coxph(as.formula(cox_formula), data = data)
  
  # Modellzusammenfassung
  message("\nCox-Regressionsmodell:")
  print(summary(cox_model))
  
  # Forest Plot
  ggforest(cox_model, data = data)
  
  return(cox_model)
}

# Random Survival Forest
fit_rsf_model <- function(data) {
  message("Erstelle Random Survival Forest Modell...")
  
  # Variablen für das Modell
  vars <- c("age", "sex", "bmi", "lab_meld", "diagnosis_group")
  available_vars <- vars[vars %in% names(data)]
  
  # Formel erstellen
  rsf_formula <- paste0("Surv(waitlist_time_months, status) ~ ", 
                       paste(available_vars, collapse = " + "))
  
  # Modell anpassen
  rsf_model <- rfsrc(
    formula = as.formula(rsf_formula),
    data = data,
    ntree = 500,
    importance = TRUE
  )
  
  # Modellzusammenfassung
  message("\nRandom Survival Forest Modell:")
  print(rsf_model)
  
  # Variablenwichtigkeit
  message("\nVariablenwichtigkeit:")
  vi <- vimp(rsf_model)
  print(vi)
  
  # Plot der Variablenwichtigkeit
  plot(vi)
  
  return(rsf_model)
}

# Modellvergleich
compare_models <- function(data, cox_model, rsf_model) {
  message("Vergleiche Modelle...")
  
  # C-Index für Cox-Modell
  cox_cindex <- summary(cox_model)$concordance[1]
  
  # C-Index für RSF-Modell
  rsf_cindex <- rsf_model$err.rate[rsf_model$ntree]
  
  # Ergebnisse
  message("\nModellvergleich (C-Index):")
  message(paste0("Cox-Modell: ", round(cox_cindex, 3)))
  message(paste0("Random Survival Forest: ", round(1 - rsf_cindex, 3)))
  
  # Ergebnisse als Dataframe
  results <- data.frame(
    Model = c("Cox-Modell", "Random Survival Forest"),
    C_Index = c(cox_cindex, 1 - rsf_cindex)
  )
  
  return(results)
}

# Hauptfunktion
run_survival_analysis <- function() {
  # Daten laden
  data <- load_clean_data()
  
  # Deskriptive Statistik
  describe_data(data)
  
  # Kaplan-Meier-Überlebenskurven
  survival_plots <- plot_survival_curves(data)
  
  # Cox-Regressionsmodell
  cox_model <- fit_cox_model(data)
  
  # Random Survival Forest
  rsf_model <- fit_rsf_model(data)
  
  # Modellvergleich
  model_comparison <- compare_models(data, cox_model, rsf_model)
  
  # Ergebnisse zurückgeben
  return(list(
    data = data,
    survival_plots = survival_plots,
    cox_model = cox_model,
    rsf_model = rsf_model,
    model_comparison = model_comparison
  ))
}

# Ausführung
if (!interactive()) {
  results <- run_survival_analysis()
}