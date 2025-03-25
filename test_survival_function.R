# Test-Skript für die verbesserte plot_km_curve-Funktion
library(survival)
library(survminer)
library(ggplot2)
library(tidyverse)
# Theme für klare Tabellen definieren (falls nicht in survival_analysis.R definiert)
theme_cleantable <- function() {
  theme(
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 11)
  )
}

# Quellcode der verbesserten Funktion laden
source("src/survival_analysis.R")
source("src/survival_analysis.R")

# Testdaten erstellen
create_test_data <- function(n = 100) {
  set.seed(123) # Für Reproduzierbarkeit
  
  # Grundlegende Daten
  time_values <- runif(n, 1, 1000)
  status_values <- sample(c(0, 1), n, replace = TRUE, prob = c(0.7, 0.3))
  group_values <- sample(c("A", "B", "C"), n, replace = TRUE)
  
  # Einige problematische Werte einfügen
  # 5% fehlende Zeitwerte
  missing_time_idx <- sample(1:n, n*0.05)
  time_values[missing_time_idx] <- NA
  
  # 5% fehlende Statuswerte
  missing_status_idx <- sample(1:n, n*0.05)
  status_values[missing_status_idx] <- NA
  
  # 5% negative Zeitwerte
  negative_time_idx <- sample(1:n, n*0.05)
  time_values[negative_time_idx] <- -runif(length(negative_time_idx), 1, 100)
  
  # 5% ungültige Statuswerte
  invalid_status_idx <- sample(1:n, n*0.05)
  status_values[invalid_status_idx] <- sample(2:5, length(invalid_status_idx), replace = TRUE)
  
  # Dataframe erstellen
  data.frame(
    waitlist_time_days = time_values,
    status = status_values,
    group = group_values
  )
}

# Testdaten generieren
test_data <- create_test_data(100)
print(head(test_data))

# Teste die verbesserte plot_km_curve-Funktion
cat("\n\n--- TESTE GESAMTE ÜBERLEBENSANALYSE ---\n\n")
km_test <- try({
  plot_km_curve(
    data = test_data,
    title = "Test Survival Curve",
    time_var = "waitlist_time_days",
    event_var = "status"
  )
})

# Bei Erfolg das Ergebnis anzeigen/speichern
if (!inherits(km_test, "try-error")) {
  cat("\nErfolgreiche Analyse! Plot wird gespeichert...\n")
  ggsave("test_survival_plot.png", plot = km_test$plot, width = 10, height = 8)
} else {
  cat("\nFehler bei der Analyse\n")
}

# Teste stratifizierte Analyse
cat("\n\n--- TESTE STRATIFIZIERTE ÜBERLEBENSANALYSE ---\n\n")
km_test_strat <- try({
  plot_km_curve(
    data = test_data,
    title = "Stratified Test Survival Curve",
    time_var = "waitlist_time_days",
    event_var = "status",
    strata_var = "group"
  )
})

# Bei Erfolg das Ergebnis anzeigen/speichern
if (!inherits(km_test_strat, "try-error")) {
  cat("\nErfolgreiche stratifizierte Analyse! Plot wird gespeichert...\n")
  ggsave("test_survival_stratified_plot.png", plot = km_test_strat$plot, width = 10, height = 8)
} else {
  cat("\nFehler bei der stratifizierten Analyse\n")
}

cat("\nTest abgeschlossen.\n")