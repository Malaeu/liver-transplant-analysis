# ============================================================================
# SCHRITTWEISE ANALYSE VON LEBERTRANSPLANTATIONSDATEN
# ============================================================================
# Dieses Skript führt eine strukturierte, schrittweise Analyse von Patienten
# auf der Warteliste für Lebertransplantationen durch, mit besonderem Fokus
# auf die Rolle der Körperzusammensetzung (Body Composition Analysis, BCA).
# ============================================================================

# Bibliotheken laden
library(tidyverse)
library(survival)
library(survminer)
library(ggplot2)
library(gridExtra)
library(gtsummary)
library(randomForestSRC)  # für Random Survival Forest

# ============================================================================
# TEIL 1: ANALYSE DES VOLLSTÄNDIGEN DATENSATZES (1344 PATIENTEN)
# ============================================================================

cat("\n====== TEIL 1: ANALYSE DES VOLLSTÄNDIGEN DATENSATZES ======\n")

# 1.1. DATEN LADEN
# ----------------------------------------------------------------------------

# Hauptdatensatz laden
full_data <- read.csv("imputed_data_full.csv", stringsAsFactors = FALSE)
cat(paste0("Hauptdatensatz geladen: ", nrow(full_data), " Zeilen, ", ncol(full_data), " Spalten\n"))

# 1.2. DEMOGRAFISCHE ANALYSE
# ----------------------------------------------------------------------------
cat("\n--- 1.2. DEMOGRAFISCHE ANALYSE ---\n")

# Grundlegende demografische Statistiken
cat("Altersverteilung:\n")
print(summary(full_data$age))

# Geschlechterverteilung
cat("\nGeschlechterverteilung:\n")
gender_table <- table(full_data$sex, useNA = "ifany")
gender_percent <- round(prop.table(gender_table) * 100, 1)
gender_df <- data.frame(
  Geschlecht = names(gender_table),
  Anzahl = as.numeric(gender_table),
  Prozent = as.numeric(gender_percent)
)
print(gender_df)

# BMI-Verteilung
cat("\nBMI-Verteilung:\n")
print(summary(full_data$bmi))

# MELD-Score-Verteilung
cat("\nMELD-Score-Verteilung:\n")
print(summary(full_data$lab_meld))

# Erstellen von Kategorien für weitere Analysen
full_data <- full_data %>%
  mutate(
    # BMI-Kategorien
    bmi_cat = case_when(
      bmi < 18.5 ~ "Untergewicht",
      bmi >= 18.5 & bmi < 25 ~ "Normalgewicht",
      bmi >= 25 & bmi < 30 ~ "Übergewicht",
      bmi >= 30 ~ "Adipositas",
      TRUE ~ NA_character_
    ),
    
    # MELD-Kategorien
    meld_cat = case_when(
      lab_meld < 10 ~ "MELD < 10",
      lab_meld >= 10 & lab_meld < 20 ~ "MELD 10-19",
      lab_meld >= 20 & lab_meld < 30 ~ "MELD 20-29",
      lab_meld >= 30 ~ "MELD ≥ 30",
      TRUE ~ NA_character_
    ),
    
    # Alter-Kategorien
    age_cat = case_when(
      age < 30 ~ "< 30",
      age >= 30 & age < 40 ~ "30-39",
      age >= 40 & age < 50 ~ "40-49",
      age >= 50 & age < 60 ~ "50-59",
      age >= 60 & age < 70 ~ "60-69",
      age >= 70 ~ "≥ 70",
      TRUE ~ NA_character_
    )
  )

# Verteilung der Kategorien
cat("\nBMI-Kategorien:\n")
print(table(full_data$bmi_cat, useNA = "ifany"))

cat("\nMELD-Kategorien:\n")
print(table(full_data$meld_cat, useNA = "ifany"))

cat("\nAlter-Kategorien:\n")
print(table(full_data$age_cat, useNA = "ifany"))

# Zeitperioden-Einteilung hinzufügen
full_data <- full_data %>%
  mutate(time_period = case_when(
    wl_year >= 2004 & wl_year <= 2010 ~ "2004-2010",
    wl_year >= 2011 & wl_year <= 2017 ~ "2011-2017",
    wl_year >= 2018 & wl_year <= 2024 ~ "2018-2024",
    TRUE ~ "Sonstiges"
  ))

# Hier ist der zweite Teil des Skripts:

# 1.3. VERTEILUNG NACH JAHREN
# ----------------------------------------------------------------------------
cat("\n--- 1.3. VERTEILUNG NACH JAHREN ---\n")

# Prüfen, ob die Variable wl_year existiert
# Funktion zur Erstellung von Zeitreihen-Grafiken mit 5-Jahres-Intervallen
create_time_series_plot <- function(data, y_var, y_label, title, fill_color, show_labels = TRUE) {
  # Bestimme den Wertebereich der Jahre
  min_year <- min(data$wl_year)
  max_year <- max(data$wl_year)
  
  # Runde auf die nächsten 5-Jahres-Intervalle
  start_year <- floor(min_year / 5) * 5
  end_year <- ceiling(max_year / 5) * 5
  
  # Erstelle die Hauptjahre für die X-Achse (5-Jahres-Intervalle)
  main_years <- seq(start_year, end_year, by = 5)
  
  p <- ggplot(data, aes(x = factor(wl_year), y = !!sym(y_var))) +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(title = title, x = "Jahr", y = y_label) +
    scale_x_discrete(
      breaks = function(x) x,  # Alle Jahre als Striche anzeigen
      labels = function(x) ifelse(as.numeric(x) %% 5 == 0, x, "")  # Nur 5-Jahres-Intervalle beschriften
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = ifelse(as.numeric(levels(factor(data$wl_year))) %% 5 == 0, 10, 6)
      ),
      panel.grid.minor.x = element_blank()
    )
  
  # Füge Wertbeschriftungen hinzu, wenn gewünscht
  if (show_labels) {
    if (y_var == "Prozent_Verstorben") {
      p <- p + geom_text(aes(label = paste0(!!sym(y_var), "%")), vjust = -0.5, size = 3)
    } else {
      p <- p + geom_text(aes(label = !!sym(y_var)), vjust = -0.5, size = 3)
    }
  }
  
  return(p)
}

# Funktion zur Erstellung von Zeitreihen-Grafiken für Kategorien
create_category_time_series <- function(data, category_var, title_prefix) {
  # Prüfen, ob die Kategorie-Variable existiert
  if (!(category_var %in% names(data))) {
    cat(paste0("Variable '", category_var, "' nicht im Datensatz vorhanden.\n"))
    return(NULL)
  }
  
  # Aggregiere Daten nach Jahr und Kategorie
  category_year_distribution <- data %>%
    group_by(wl_year, !!sym(category_var)) %>%
    summarise(
      Anzahl = n(),
      Verstorben = sum(status == 1, na.rm = TRUE),
      Prozent_Verstorben = round(Verstorben / Anzahl * 100, 1),
      .groups = "drop"
    )
  
  # Bestimme den Wertebereich der Jahre
  min_year <- min(category_year_distribution$wl_year)
  max_year <- max(category_year_distribution$wl_year)
  
  # Runde auf die nächsten 5-Jahres-Intervalle
  start_year <- floor(min_year / 5) * 5
  end_year <- ceiling(max_year / 5) * 5
  
  # Erstelle die Hauptjahre für die X-Achse (5-Jahres-Intervalle)
  main_years <- seq(start_year, end_year, by = 5)
  
  # Erstelle eine Farbpalette basierend auf der Anzahl der Kategorien
  categories <- unique(category_year_distribution[[category_var]])
  n_categories <- length(categories)
  
  # Verschiedene Farbpaletten je nach Kategorie-Typ
  if (category_var == "meld_cat") {
    color_palette <- colorRampPalette(c("green", "yellow", "orange", "red"))(n_categories)
  } else if (category_var == "bmi_cat") {
    color_palette <- colorRampPalette(c("lightblue", "steelblue", "darkblue", "purple"))(n_categories)
  } else if (category_var == "age_cat") {
    color_palette <- colorRampPalette(c("lightgreen", "darkgreen"))(n_categories)
  } else if (category_var == "BC_MELD_cat") {
    color_palette <- colorRampPalette(c("pink", "red", "darkred", "purple"))(n_categories)
  } else {
    color_palette <- colorRampPalette(c("steelblue", "darkblue"))(n_categories)
  }
  
  # Anzahl nach Jahren und Kategorien
  p_category_count <- ggplot(category_year_distribution,
                           aes(x = factor(wl_year), y = Anzahl, fill = !!sym(category_var))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = paste0(title_prefix, " nach Jahren und ", category_var),
         x = "Jahr", y = "Anzahl") +
    scale_x_discrete(
      breaks = function(x) x,  # Alle Jahre als Striche anzeigen
      labels = function(x) ifelse(as.numeric(x) %% 5 == 0, x, "")  # Nur 5-Jahres-Intervalle beschriften
    ) +
    scale_fill_manual(values = color_palette) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = ifelse(as.numeric(levels(factor(category_year_distribution$wl_year))) %% 5 == 0, 10, 6)
      ),
      panel.grid.minor.x = element_blank(),
      legend.title = element_blank()
    )
  
  # Sterblichkeit nach Jahren und Kategorien
  p_category_mortality <- ggplot(category_year_distribution,
                                aes(x = factor(wl_year), y = Prozent_Verstorben, fill = !!sym(category_var))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste0("Sterblichkeit nach Jahren und ", category_var),
         x = "Jahr", y = "Sterblichkeit (%)") +
    scale_x_discrete(
      breaks = function(x) x,  # Alle Jahre als Striche anzeigen
      labels = function(x) ifelse(as.numeric(x) %% 5 == 0, x, "")  # Nur 5-Jahres-Intervalle beschriften
    ) +
    scale_fill_manual(values = color_palette) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = ifelse(as.numeric(levels(factor(category_year_distribution$wl_year))) %% 5 == 0, 10, 6)
      ),
      panel.grid.minor.x = element_blank(),
      legend.title = element_blank()
    )
  
  # Beide Plots in einer Liste zurückgeben
  return(list(count = p_category_count, mortality = p_category_mortality))
}

# 1.3. VERTEILUNG NACH JAHREN
# ----------------------------------------------------------------------------
cat("\n--- 1.3. VERTEILUNG NACH JAHREN ---\n")

# Prüfen, ob die Variable wl_year existiert
if ("wl_year" %in% names(full_data)) {
  # Verteilung nach Jahren
  year_distribution <- full_data %>%
    group_by(wl_year) %>%
    summarise(
      Anzahl = n(),
      Verstorben = sum(status == 1, na.rm = TRUE),
      Prozent_Verstorben = round(Verstorben / Anzahl * 100, 1)
    )
  
  cat("Verteilung nach Jahren:\n")
  print(year_distribution)
  
  # 1.3.1 VERTEILUNG NACH JAHREN UND KATEGORIEN
  cat("\n--- 1.3.1 VERTEILUNG NACH JAHREN UND KATEGORIEN ---\n")
  
  # Erstelle Grafiken für MELD-Kategorien
  if ("meld_cat" %in% names(full_data)) {
    cat("\nErstelle Grafiken für MELD-Kategorien...\n")
    meld_plots <- create_category_time_series(full_data, "meld_cat", "Patienten")
    
    if (!is.null(meld_plots)) {
      # Zeige und speichere die Plots
      grid.arrange(meld_plots$count, meld_plots$mortality, ncol = 1)
      ggsave("results/figures/patients_by_year_meld.png", meld_plots$count, width = 10, height = 6)
      ggsave("results/figures/mortality_by_year_meld.png", meld_plots$mortality, width = 10, height = 6)
    }
  }
  
  # Erstelle Grafiken für BMI-Kategorien
  if ("bmi_cat" %in% names(full_data)) {
    cat("\nErstelle Grafiken für BMI-Kategorien...\n")
    bmi_plots <- create_category_time_series(full_data, "bmi_cat", "Patienten")
    
    if (!is.null(bmi_plots)) {
      # Zeige und speichere die Plots
      grid.arrange(bmi_plots$count, bmi_plots$mortality, ncol = 1)
      ggsave("results/figures/patients_by_year_bmi.png", bmi_plots$count, width = 10, height = 6)
      ggsave("results/figures/mortality_by_year_bmi.png", bmi_plots$mortality, width = 10, height = 6)
    }
  }
  
  # Erstelle Grafiken für Alters-Kategorien
  if ("age_cat" %in% names(full_data)) {
    cat("\nErstelle Grafiken für Alters-Kategorien...\n")
    age_plots <- create_category_time_series(full_data, "age_cat", "Patienten")
    
    if (!is.null(age_plots)) {
      # Zeige und speichere die Plots
      grid.arrange(age_plots$count, age_plots$mortality, ncol = 1)
      ggsave("results/figures/patients_by_year_age.png", age_plots$count, width = 10, height = 6)
      ggsave("results/figures/mortality_by_year_age.png", age_plots$mortality, width = 10, height = 6)
    }
  }
  
  # Visualisierung der Verteilung nach Jahren
  p_year <- create_time_series_plot(
    year_distribution,
    "Anzahl",
    "Anzahl",
    "Anzahl der Patienten nach Jahren",
    "steelblue"
  )
  
  # Visualisierung der Sterblichkeit nach Jahren
  p_mortality <- create_time_series_plot(
    year_distribution,
    "Prozent_Verstorben",
    "Sterblichkeit (%)",
    "Sterblichkeit nach Jahren",
    "firebrick"
  )
  
  # Beide Plots anzeigen
  grid.arrange(p_year, p_mortality, ncol = 1)
  
  # Speichern der Plots
  ggsave("results/figures/patients_by_year.png", p_year, width = 10, height = 6)
  ggsave("results/figures/mortality_by_year.png", p_mortality, width = 10, height = 6)
} else {
  cat("Variable 'wl_year' nicht im Datensatz vorhanden.\n")
}

# 1.4. ÜBERLEBENSANALYSE
# ----------------------------------------------------------------------------
cat("\n--- 1.4. ÜBERLEBENSANALYSE ---\n")

# Kaplan-Meier-Überlebenskurve für den gesamten Datensatz
if (all(c("waitlist_time_months", "status") %in% names(full_data))) {
  # Gesamtüberlebenskurve
  km_fit <- survfit(Surv(waitlist_time_months, status) ~ 1, data = full_data)
  
  cat("Gesamtüberlebensanalyse:\n")
  print(km_fit)
  
  # Überlebensraten zu bestimmten Zeitpunkten
  surv_summary <- summary(km_fit, times = c(3, 6, 12, 24, 36, 48, 60))
  surv_table <- data.frame(
    Zeit_Monate = surv_summary$time,
    Risiko = surv_summary$n.risk,
    Ereignisse = surv_summary$n.event,
    Überlebensrate = round(surv_summary$surv * 100, 1)
  )
  
  cat("\nÜberlebensraten zu bestimmten Zeitpunkten:\n")
  print(surv_table)
  
  # Visualisierung der Gesamtüberlebenskurve
  p_surv <- ggsurvplot(
    km_fit,
    data = full_data,
    risk.table = TRUE,
    conf.int = TRUE,
    title = "Gesamtüberlebenskurve",
    xlab = "Zeit (Monate)",
    ylab = "Überlebenswahrscheinlichkeit",
    palette = "jco",
    ggtheme = theme_minimal()
  )
  
  print(p_surv)
  
  # Überlebenskurven nach MELD-Kategorie
  if ("meld_cat" %in% names(full_data)) {
    km_fit_meld <- survfit(Surv(waitlist_time_months, status) ~ meld_cat, data = full_data)
    
    cat("\nÜberlebensanalyse nach MELD-Kategorie:\n")
    print(km_fit_meld)
    
    # Log-Rank-Test
    log_rank_test <- survdiff(Surv(waitlist_time_months, status) ~ meld_cat, data = full_data)
    cat("\nLog-Rank-Test für MELD-Kategorien:\n")
    print(log_rank_test)
    
    # Visualisierung der Überlebenskurven nach MELD-Kategorie
    p_surv_meld <- ggsurvplot(
      km_fit_meld,
      data = full_data,
      risk.table = TRUE,
      pval = TRUE,
      conf.int = FALSE,
      title = "Überlebenskurven nach MELD-Kategorie",
      xlab = "Zeit (Monate)",
      ylab = "Überlebenswahrscheinlichkeit",
      palette = "jco",
      ggtheme = theme_minimal()
    )
    
    print(p_surv_meld)
  }
}
# Hier ist der dritte Teil des Skripts:

# 1.5. COX-REGRESSIONSANALYSE
# ----------------------------------------------------------------------------
cat("\n--- 1.5. COX-REGRESSIONSANALYSE ---\n")

# Univariate Cox-Regression für wichtige Variablen
if (all(c("waitlist_time_months", "status") %in% names(full_data))) {
  # Liste der Variablen für univariate Analyse
  uni_vars <- c("age", "sex", "bmi", "lab_meld")
  
  # Univariate Analyse
  uni_results <- list()
  
  for (var in uni_vars) {
    if (var %in% names(full_data)) {
      formula <- as.formula(paste("Surv(waitlist_time_months, status) ~", var))
      model <- coxph(formula, data = full_data)
      uni_results[[var]] <- summary(model)
      
      cat(paste0("\nUnivariate Cox-Regression für ", var, ":\n"))
      print(uni_results[[var]])
    }
  }
  
  # Multivariate Cox-Regression
  multi_vars <- uni_vars[uni_vars %in% names(full_data)]
  
  if (length(multi_vars) > 0) {
    formula <- as.formula(paste("Surv(waitlist_time_months, status) ~", paste(multi_vars, collapse = " + ")))
    multi_model <- coxph(formula, data = full_data)
    
    cat("\nMultivariate Cox-Regression:\n")
    print(summary(multi_model))
    
    # Forest Plot für multivariate Analyse
    ggforest(multi_model, data = full_data)
  }
} else {
  cat("Variablen 'waitlist_time_months' oder 'status' nicht im Datensatz vorhanden.\n")
}

# ============================================================================
# TEIL 2: ANALYSE DES BCA-DATENSATZES (PATIENTEN MIT BODY COMPOSITION DATEN)
# ============================================================================

cat("\n\n====== TEIL 2: ANALYSE DES BCA-DATENSATZES ======\n")

# 2.1. DATEN LADEN UND ZUSAMMENFÜHREN
# ----------------------------------------------------------------------------

# BCA-Datensatz laden
bca_data <- readRDS("wl_df_with_bca.rds")
cat(paste0("BCA-Datensatz geladen: ", nrow(bca_data), " Zeilen, ", ncol(bca_data), " Spalten\n"))

# Umbenennen der ID-Spalte im BCA-Datensatz für konsistentes Joining
if ("etnr" %in% names(bca_data) && "etnr_id" %in% names(full_data)) {
  bca_data <- bca_data %>% rename(etnr_id = etnr)
  cat("BCA-Datensatz: 'etnr' in 'etnr_id' umbenannt für konsistentes Joining\n")
}

# BCA-Variablen identifizieren
bca_vars <- c("muscle", "sat", "vat", "imat", "eat", "pat", "tat")
available_bca_vars <- bca_vars[bca_vars %in% names(bca_data)]

cat(paste0("Verfügbare BCA-Variablen: ", paste(available_bca_vars, collapse = ", "), "\n"))

# Datensätze zusammenführen
merged_data <- full_data %>%
  left_join(bca_data %>% select(etnr_id, all_of(available_bca_vars)), by = "etnr_id")

cat(paste0("Zusammengeführter Datensatz: ", nrow(merged_data), " Zeilen, ", ncol(merged_data), " Spalten\n"))

# BCA-Subset erstellen (nur Patienten mit vollständigen BCA-Daten)
bca_subset <- merged_data %>%
  filter(!is.na(muscle) & !is.na(sat) & !is.na(vat) & 
         !is.na(imat) & !is.na(eat) & !is.na(pat) & !is.na(tat))

cat(paste0("BCA-Subset: ", nrow(bca_subset), " Zeilen, ", ncol(bca_subset), " Spalten\n"))
# Hier ist der vierte Teil des Skripts:

# 2.2. DESKRIPTIVE STATISTIK DER BCA-VARIABLEN
# ----------------------------------------------------------------------------
cat("\n--- 2.2. DESKRIPTIVE STATISTIK DER BCA-VARIABLEN ---\n")

# Zusammenfassung der BCA-Variablen
cat("Zusammenfassung der BCA-Variablen:\n")
print(summary(bca_subset[, available_bca_vars]))

# Korrelationsmatrix der BCA-Variablen
cat("\nKorrelationsmatrix der BCA-Variablen:\n")
bca_cor <- cor(bca_subset[, available_bca_vars], use = "pairwise.complete.obs")
print(round(bca_cor, 2))

# Visualisierung der Verteilung der BCA-Variablen
par(mfrow = c(2, 4))
for (var in available_bca_vars) {
  hist(bca_subset[[var]], main = var, xlab = var, col = "steelblue")
}
par(mfrow = c(1, 1))

# Zeitreihen-Visualisierung der BCA-Variablen
if ("wl_year" %in% names(bca_subset)) {
  cat("\nErstelle Zeitreihen-Grafiken für BCA-Variablen...\n")
  
  # Aggregiere BCA-Daten nach Jahren
  bca_year_distribution <- bca_subset %>%
    group_by(wl_year) %>%
    summarise(
      Anzahl = n(),
      Verstorben = sum(status == 1, na.rm = TRUE),
      Prozent_Verstorben = round(Verstorben / Anzahl * 100, 1),
      Durchschnitt_Muscle = mean(muscle, na.rm = TRUE),
      Durchschnitt_VAT = mean(vat, na.rm = TRUE),
      Durchschnitt_SAT = mean(sat, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Erstelle Zeitreihen-Grafiken für BCA-Variablen
  p_muscle <- create_time_series_plot(
    bca_year_distribution,
    "Durchschnitt_Muscle",
    "Durchschnittliches Muskelvolumen",
    "Muskelvolumen nach Jahren",
    "darkgreen"
  )
  
  p_vat <- create_time_series_plot(
    bca_year_distribution,
    "Durchschnitt_VAT",
    "Durchschnittliches VAT",
    "Viszerales Fettgewebe nach Jahren",
    "orange"
  )
  
  p_sat <- create_time_series_plot(
    bca_year_distribution,
    "Durchschnitt_SAT",
    "Durchschnittliches SAT",
    "Subkutanes Fettgewebe nach Jahren",
    "purple"
  )
  
  # Zeige und speichere die Plots
  grid.arrange(p_muscle, p_vat, p_sat, ncol = 1)
  ggsave("results/figures/muscle_by_year.png", p_muscle, width = 10, height = 6)
  ggsave("results/figures/vat_by_year.png", p_vat, width = 10, height = 6)
  ggsave("results/figures/sat_by_year.png", p_sat, width = 10, height = 6)
}

# 2.3. VERGLEICH DER PATIENTEN MIT UND OHNE BCA-DATEN
# ----------------------------------------------------------------------------
cat("\n--- 2.3. VERGLEICH DER PATIENTEN MIT UND OHNE BCA-DATEN ---\n")

# Erstellen einer Indikatorvariable für BCA-Daten
merged_data$has_bca <- !is.na(merged_data$muscle)

# Vergleich der demografischen und klinischen Variablen
compare_vars <- c("age", "sex", "bmi", "lab_meld", "waitlist_time_months", "status")
available_compare_vars <- compare_vars[compare_vars %in% names(merged_data)]

for (var in available_compare_vars) {
  cat(paste0("\nVergleich von ", var, " zwischen Patienten mit und ohne BCA-Daten:\n"))
  
  if (is.numeric(merged_data[[var]])) {
    # Für numerische Variablen: t-Test
    t_test <- t.test(merged_data[[var]] ~ merged_data$has_bca)
    print(t_test)
    
    # Boxplot
    boxplot(merged_data[[var]] ~ merged_data$has_bca, 
            main = paste0("Vergleich von ", var),
            xlab = "Hat BCA-Daten", ylab = var)
  } else {
    # Für kategoriale Variablen: Chi-Quadrat-Test
    table_result <- table(merged_data[[var]], merged_data$has_bca)
    chi_test <- chisq.test(table_result)
    print(table_result)
    print(chi_test)
  }
}

# 2.4. ÜBERLEBENSANALYSE FÜR DAS BCA-SUBSET
# ----------------------------------------------------------------------------
cat("\n--- 2.4. ÜBERLEBENSANALYSE FÜR DAS BCA-SUBSET ---\n")

if (all(c("waitlist_time_months", "status") %in% names(bca_subset))) {
  # Gesamtüberlebenskurve für das BCA-Subset
  km_fit_bca <- survfit(Surv(waitlist_time_months, status) ~ 1, data = bca_subset)
  
  cat("Gesamtüberlebensanalyse für das BCA-Subset:\n")
  print(km_fit_bca)
  
  # Überlebensraten zu bestimmten Zeitpunkten
  surv_summary_bca <- summary(km_fit_bca, times = c(3, 6, 12, 24, 36, 48, 60))
  surv_table_bca <- data.frame(
    Zeit_Monate = surv_summary_bca$time,
    Risiko = surv_summary_bca$n.risk,
    Ereignisse = surv_summary_bca$n.event,
    Überlebensrate = round(surv_summary_bca$surv * 100, 1)
  )
  
  cat("\nÜberlebensraten zu bestimmten Zeitpunkten für das BCA-Subset:\n")
  print(surv_table_bca)
  
  # Visualisierung der Gesamtüberlebenskurve für das BCA-Subset
  p_surv_bca <- ggsurvplot(
    km_fit_bca,
    data = bca_subset,
    risk.table = TRUE,
    conf.int = TRUE,
    title = "Gesamtüberlebenskurve für das BCA-Subset",
    xlab = "Zeit (Monate)",
    ylab = "Überlebenswahrscheinlichkeit",
    palette = "jco",
    ggtheme = theme_minimal()
  )
  
  print(p_surv_bca)
}
# Hier ist der fünfte und letzte Teil des Skripts:

# ============================================================================
# TEIL 3: ENTWICKLUNG UND VALIDIERUNG DES BC-MELD-SCORES
# ============================================================================

cat("\n\n====== TEIL 3: ENTWICKLUNG UND VALIDIERUNG DES BC-MELD-SCORES ======\n")

# 3.1. BERECHNUNG DER BCA-INDIZES
# ----------------------------------------------------------------------------
cat("\n--- 3.1. BERECHNUNG DER BCA-INDIZES ---\n")

# Berechnung der BCA-Indizes
bca_subset <- bca_subset %>%
  mutate(
    # Umrechnung der Körpergröße in Meter
    height_m = cm / 100,
    
    # Skeletal Muscle Index (SMI)
    # Approximation: Muskelvolumen / Körpergröße für Querschnittsfläche
    muscle_area = muscle / height_m,
    SMI = muscle_area / (height_m^2),
    
    # Intramuscular Adipose Tissue Content (IMAC)
    IMAC = imat / muscle,
    
    # Visceral to Subcutaneous Adipose Tissue Ratio (VSR)
    VSR = vat / sat
  )

# Zusammenfassung der berechneten Indizes
cat("Zusammenfassung der berechneten BCA-Indizes:\n")
print(summary(bca_subset[, c("SMI", "IMAC", "VSR")]))

# Bestimmung der Medianwerte für die Kategorisierung
smi_median <- median(bca_subset$SMI, na.rm = TRUE)
imac_median <- median(bca_subset$IMAC, na.rm = TRUE)
vsr_median <- median(bca_subset$VSR, na.rm = TRUE)

cat("\nMedianwerte für die Kategorisierung:\n")
cat(paste0("SMI-Median: ", round(smi_median, 2), "\n"))
cat(paste0("IMAC-Median: ", round(imac_median, 4), "\n"))
cat(paste0("VSR-Median: ", round(vsr_median, 2), "\n"))

# Kategorisierung der BCA-Indizes
bca_subset <- bca_subset %>%
  mutate(
    # Niedriger SMI (1 wenn ja, 0 wenn nein)
    low_SMI = as.numeric(SMI < smi_median),
    
    # Hoher IMAC (1 wenn ja, 0 wenn nein)
    high_IMAC = as.numeric(IMAC > imac_median),
    
    # Hoher VSR (1 wenn ja, 0 wenn nein)
    high_VSR = as.numeric(VSR > vsr_median)
  )

# Verteilung der kategorisierten BCA-Indizes
cat("\nVerteilung der kategorisierten BCA-Indizes:\n")
cat(paste0("Niedriger SMI: ", sum(bca_subset$low_SMI), " (", 
          round(mean(bca_subset$low_SMI) * 100, 1), "%)\n"))
cat(paste0("Hoher IMAC: ", sum(bca_subset$high_IMAC), " (", 
          round(mean(bca_subset$high_IMAC) * 100, 1), "%)\n"))
cat(paste0("Hoher VSR: ", sum(bca_subset$high_VSR), " (",
          round(mean(bca_subset$high_VSR) * 100, 1), "%)\n"))

# Zeitreihen-Visualisierung der BCA-Indizes
if ("wl_year" %in% names(bca_subset)) {
  cat("\nErstelle Zeitreihen-Grafiken für BCA-Indizes...\n")
  
  # Aggregiere BCA-Indizes nach Jahren
  bca_indices_year_distribution <- bca_subset %>%
    group_by(wl_year) %>%
    summarise(
      Anzahl = n(),
      Verstorben = sum(status == 1, na.rm = TRUE),
      Prozent_Verstorben = round(Verstorben / Anzahl * 100, 1),
      Durchschnitt_SMI = mean(SMI, na.rm = TRUE),
      Durchschnitt_IMAC = mean(IMAC, na.rm = TRUE),
      Durchschnitt_VSR = mean(VSR, na.rm = TRUE),
      Prozent_Low_SMI = round(mean(low_SMI, na.rm = TRUE) * 100, 1),
      Prozent_High_IMAC = round(mean(high_IMAC, na.rm = TRUE) * 100, 1),
      Prozent_High_VSR = round(mean(high_VSR, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )
  
  # Erstelle Zeitreihen-Grafiken für BCA-Indizes
  p_smi <- create_time_series_plot(
    bca_indices_year_distribution,
    "Durchschnitt_SMI",
    "Durchschnittlicher SMI",
    "Skeletal Muscle Index nach Jahren",
    "darkblue"
  )
  
  p_imac <- create_time_series_plot(
    bca_indices_year_distribution,
    "Durchschnitt_IMAC",
    "Durchschnittlicher IMAC",
    "Intramuscular Adipose Tissue Content nach Jahren",
    "darkred"
  )
  
  p_vsr <- create_time_series_plot(
    bca_indices_year_distribution,
    "Durchschnitt_VSR",
    "Durchschnittlicher VSR",
    "Visceral to Subcutaneous Adipose Tissue Ratio nach Jahren",
    "darkgreen"
  )
  
  # Erstelle Zeitreihen-Grafiken für die Kategorisierung
  p_low_smi <- create_time_series_plot(
    bca_indices_year_distribution,
    "Prozent_Low_SMI",
    "Prozent mit niedrigem SMI",
    "Niedriger SMI nach Jahren (%)",
    "steelblue"
  )
  
  p_high_imac <- create_time_series_plot(
    bca_indices_year_distribution,
    "Prozent_High_IMAC",
    "Prozent mit hohem IMAC",
    "Hoher IMAC nach Jahren (%)",
    "firebrick"
  )
  
  p_high_vsr <- create_time_series_plot(
    bca_indices_year_distribution,
    "Prozent_High_VSR",
    "Prozent mit hohem VSR",
    "Hoher VSR nach Jahren (%)",
    "forestgreen"
  )
  
  # Zeige und speichere die Plots
  grid.arrange(p_smi, p_imac, p_vsr, ncol = 1)
  ggsave("results/figures/smi_by_year.png", p_smi, width = 10, height = 6)
  ggsave("results/figures/imac_by_year.png", p_imac, width = 10, height = 6)
  ggsave("results/figures/vsr_by_year.png", p_vsr, width = 10, height = 6)
  
  grid.arrange(p_low_smi, p_high_imac, p_high_vsr, ncol = 1)
  ggsave("results/figures/low_smi_by_year.png", p_low_smi, width = 10, height = 6)
  ggsave("results/figures/high_imac_by_year.png", p_high_imac, width = 10, height = 6)
  ggsave("results/figures/high_vsr_by_year.png", p_high_vsr, width = 10, height = 6)
}

# 3.2. BERECHNUNG DES BC-MELD-SCORES
# ----------------------------------------------------------------------------
cat("\n--- 3.2. BERECHNUNG DES BC-MELD-SCORES ---\n")

# Berechnung des BC-MELD-Scores
# Formel: BC-MELD = MELD + 3.59 × niedriger SMI + 5.42 × hoher IMAC + 2.06 × hoher VSR
bca_subset <- bca_subset %>%
  mutate(
    BC_MELD = lab_meld + 
              3.59 * low_SMI + 
              5.42 * high_IMAC + 
              2.06 * high_VSR,
    
    # Kategorisierung des BC-MELD-Scores
    BC_MELD_cat = case_when(
      BC_MELD < 10 ~ "BC-MELD < 10",
      BC_MELD >= 10 & BC_MELD < 20 ~ "BC-MELD 10-19",
      BC_MELD >= 20 & BC_MELD < 30 ~ "BC-MELD 20-29",
      BC_MELD >= 30 ~ "BC-MELD ≥ 30",
      TRUE ~ NA_character_
    )
  )

# Zusammenfassung des BC-MELD-Scores
cat("Zusammenfassung des BC-MELD-Scores:\n")
print(summary(bca_subset$BC_MELD))

# Vergleich von MELD und BC-MELD
cat("\nVergleich von MELD und BC-MELD:\n")
print(summary(bca_subset[, c("lab_meld", "BC_MELD")]))

# Durchschnittliche Erhöhung des BC-MELD gegenüber dem MELD
mean_increase <- mean(bca_subset$BC_MELD - bca_subset$lab_meld, na.rm = TRUE)
cat(paste0("\nDurchschnittliche Erhöhung des BC-MELD gegenüber dem MELD: ", 
          round(mean_increase, 2), " Punkte\n"))

# Verteilung der BC-MELD-Kategorien
cat("\nVerteilung der BC-MELD-Kategorien:\n")
print(table(bca_subset$BC_MELD_cat, useNA = "ifany"))

# Visualisierung des Vergleichs von MELD und BC-MELD
boxplot(bca_subset$lab_meld, bca_subset$BC_MELD, 
        names = c("MELD", "BC-MELD"),
        main = "Vergleich von MELD und BC-MELD",
        col = c("steelblue", "firebrick"))

# Zeitreihen-Visualisierung des BC-MELD-Scores
if ("wl_year" %in% names(bca_subset)) {
  cat("\nErstelle Zeitreihen-Grafiken für BC-MELD-Score...\n")
  
  # Aggregiere BC-MELD-Daten nach Jahren
  bc_meld_year_distribution <- bca_subset %>%
    group_by(wl_year) %>%
    summarise(
      Anzahl = n(),
      Verstorben = sum(status == 1, na.rm = TRUE),
      Prozent_Verstorben = round(Verstorben / Anzahl * 100, 1),
      Durchschnitt_MELD = mean(lab_meld, na.rm = TRUE),
      Durchschnitt_BC_MELD = mean(BC_MELD, na.rm = TRUE),
      Differenz_BC_MELD_MELD = mean(BC_MELD - lab_meld, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Erstelle Zeitreihen-Grafiken für BC-MELD-Score
  p_meld <- create_time_series_plot(
    bc_meld_year_distribution,
    "Durchschnitt_MELD",
    "Durchschnittlicher MELD-Score",
    "MELD-Score nach Jahren",
    "steelblue"
  )
  
  p_bc_meld <- create_time_series_plot(
    bc_meld_year_distribution,
    "Durchschnitt_BC_MELD",
    "Durchschnittlicher BC-MELD-Score",
    "BC-MELD-Score nach Jahren",
    "firebrick"
  )
  
  p_diff_meld <- create_time_series_plot(
    bc_meld_year_distribution,
    "Differenz_BC_MELD_MELD",
    "Durchschnittliche Differenz (BC-MELD - MELD)",
    "Differenz zwischen BC-MELD und MELD nach Jahren",
    "purple"
  )
  
  # Zeige und speichere die Plots
  grid.arrange(p_meld, p_bc_meld, p_diff_meld, ncol = 1)
  ggsave("results/figures/meld_by_year.png", p_meld, width = 10, height = 6)
  ggsave("results/figures/bc_meld_by_year.png", p_bc_meld, width = 10, height = 6)
  ggsave("results/figures/meld_diff_by_year.png", p_diff_meld, width = 10, height = 6)
  
  # Erstelle auch Zeitreihen-Grafiken für BC-MELD-Kategorien
  if ("BC_MELD_cat" %in% names(bca_subset)) {
    cat("\nErstelle Grafiken für BC-MELD-Kategorien...\n")
    bc_meld_cat_plots <- create_category_time_series(bca_subset, "BC_MELD_cat", "Patienten")
    
    if (!is.null(bc_meld_cat_plots)) {
      # Zeige und speichere die Plots
      grid.arrange(bc_meld_cat_plots$count, bc_meld_cat_plots$mortality, ncol = 1)
      ggsave("results/figures/patients_by_year_bc_meld.png", bc_meld_cat_plots$count, width = 10, height = 6)
      ggsave("results/figures/mortality_by_year_bc_meld.png", bc_meld_cat_plots$mortality, width = 10, height = 6)
    }
  }
}

# 3.3. ÜBERLEBENSANALYSE MIT BC-MELD
# ----------------------------------------------------------------------------
cat("\n--- 3.3. ÜBERLEBENSANALYSE MIT BC-MELD ---\n")

if (all(c("waitlist_time_months", "status", "BC_MELD_cat") %in% names(bca_subset))) {
  # Überlebenskurven nach BC-MELD-Kategorie
  km_fit_bc_meld <- survfit(Surv(waitlist_time_months, status) ~ BC_MELD_cat, data = bca_subset)
  
  cat("Überlebensanalyse nach BC-MELD-Kategorie:\n")
  print(km_fit_bc_meld)
  
  # Log-Rank-Test
  log_rank_test_bc_meld <- survdiff(Surv(waitlist_time_months, status) ~ BC_MELD_cat, data = bca_subset)
  cat("\nLog-Rank-Test für BC-MELD-Kategorien:\n")
  print(log_rank_test_bc_meld)
  
  # Visualisierung der Überlebenskurven nach BC-MELD-Kategorie
  p_surv_bc_meld <- ggsurvplot(
    km_fit_bc_meld,
    data = bca_subset,
    risk.table = TRUE,
    pval = TRUE,
    conf.int = FALSE,
    title = "Überlebenskurven nach BC-MELD-Kategorie",
    xlab = "Zeit (Monate)",
    ylab = "Überlebenswahrscheinlichkeit",
    palette = "jco",
    ggtheme = theme_minimal()
  )
  
  print(p_surv_bc_meld)
  
  # Vergleich der C-Indizes von MELD und BC-MELD
  cindex_meld <- concordance(Surv(waitlist_time_months, status) ~ lab_meld, data = bca_subset)$concordance
  cindex_bc_meld <- concordance(Surv(waitlist_time_months, status) ~ BC_MELD, data = bca_subset)$concordance
  
  cat("\nVergleich der C-Indizes:\n")
  cat(paste0("C-Index MELD: ", round(cindex_meld, 3), "\n"))
  cat(paste0("C-Index BC-MELD: ", round(cindex_bc_meld, 3), "\n"))
  cat(paste0("Verbesserung: ", round((cindex_bc_meld - cindex_meld) * 100, 1), "%\n"))
}

# Speichern der Ergebnisse
cat("\n\nAnalyse abgeschlossen. Ergebnisse wurden im Workspace gespeichert.\n")

# ============================================================================
# TEIL 4: ZEITPERIODEN-ANALYSE
# ============================================================================

cat("\n\n====== TEIL 4: ANALYSE NACH ZEITPERIODEN ======\n")

# Prüfe ob die Variable time_period existiert
if ("time_period" %in% names(full_data)) {
  # Für jede Zeitperiode
  periods <- unique(full_data$time_period)
  periods <- periods[order(periods)]  # Sortieren der Perioden
  
  for (period in periods) {
    if (period == "Sonstiges") next  # Skip "Sonstiges" Kategorie
    
    cat(paste0("\n\n--- ZEITPERIODE: ", period, " ---\n"))
    
    # Subset für die Zeitperiode
    period_data <- full_data %>% filter(time_period == period)
    cat(paste0("Datensatz für diese Periode: ", nrow(period_data), " Zeilen\n"))
    
    # 4.1. DEMOGRAFISCHE ANALYSE FÜR DIE ZEITPERIODE
    cat("\n--- 4.1. DEMOGRAFISCHE ANALYSE FÜR DIE ZEITPERIODE ---\n")
    
    # Grundlegende demografische Statistiken
    cat("Altersverteilung:\n")
    print(summary(period_data$age))
    
    # Geschlechterverteilung
    cat("\nGeschlechterverteilung:\n")
    gender_table <- table(period_data$sex, useNA = "ifany")
    gender_percent <- round(prop.table(gender_table) * 100, 1)
    gender_df <- data.frame(
      Geschlecht = names(gender_table),
      Anzahl = as.numeric(gender_table),
      Prozent = as.numeric(gender_percent)
    )
    print(gender_df)
    
    # BMI-Verteilung
    cat("\nBMI-Verteilung:\n")
    print(summary(period_data$bmi))
    
    # MELD-Score-Verteilung
    cat("\nMELD-Score-Verteilung:\n")
    print(summary(period_data$lab_meld))
    
    # 4.2. ÜBERLEBENSANALYSE FÜR DIE ZEITPERIODE
    cat("\n--- 4.2. ÜBERLEBENSANALYSE FÜR DIE ZEITPERIODE ---\n")
    
    if (all(c("waitlist_time_months", "status") %in% names(period_data))) {
      # Gesamtüberlebenskurve für die Zeitperiode
      km_fit_period <- survfit(Surv(waitlist_time_months, status) ~ 1, data = period_data)
      
      cat("Gesamtüberlebensanalyse für die Zeitperiode:\n")
      print(km_fit_period)
      
      # Überlebensraten zu bestimmten Zeitpunkten
      surv_summary_period <- summary(km_fit_period, times = c(3, 6, 12, 24, 36, 48, 60))
      surv_table_period <- data.frame(
        Zeit_Monate = surv_summary_period$time,
        Risiko = surv_summary_period$n.risk,
        Ereignisse = surv_summary_period$n.event,
        Überlebensrate = round(surv_summary_period$surv * 100, 1)
      )
      
      cat("\nÜberlebensraten zu bestimmten Zeitpunkten für die Zeitperiode:\n")
      print(surv_table_period)
      
      # Visualisierung der Gesamtüberlebenskurve für die Zeitperiode
      p_surv_period <- ggsurvplot(
        km_fit_period,
        data = period_data,
        risk.table = TRUE,
        conf.int = TRUE,
        title = paste0("Gesamtüberlebenskurve für ", period),
        xlab = "Zeit (Monate)",
        ylab = "Überlebenswahrscheinlichkeit",
        palette = "jco",
        ggtheme = theme_minimal()
      )
      
      print(p_surv_period)
      
      # Wenn MELD-Kategorien vorhanden, dann Überlebensanalyse nach MELD-Kategorie
      if ("meld_cat" %in% names(period_data)) {
        # Anzahl der Patienten pro MELD-Kategorie prüfen
        meld_counts <- table(period_data$meld_cat)
        valid_categories <- names(meld_counts)[meld_counts >= 5]
        
        if (length(valid_categories) > 1) {
          # Subset mit nur gültigen Kategorien erstellen
          period_data_valid <- period_data %>%
            filter(meld_cat %in% valid_categories)
          
          # Überlebenskurven nach MELD-Kategorie
          km_fit_meld_period <- survfit(Surv(waitlist_time_months, status) ~ meld_cat,
                                       data = period_data_valid)
          
          cat("\nÜberlebensanalyse nach MELD-Kategorie für die Zeitperiode:\n")
          print(km_fit_meld_period)
          
          # Log-Rank-Test
          log_rank_test_period <- survdiff(Surv(waitlist_time_months, status) ~ meld_cat,
                                         data = period_data_valid)
          cat("\nLog-Rank-Test für MELD-Kategorien in der Zeitperiode:\n")
          print(log_rank_test_period)
          
          # Visualisierung der Überlebenskurven nach MELD-Kategorie
          p_surv_meld_period <- ggsurvplot(
            km_fit_meld_period,
            data = period_data_valid,
            risk.table = TRUE,
            pval = TRUE,
            conf.int = FALSE,
            title = paste0("Überlebenskurven nach MELD-Kategorie für ", period),
            xlab = "Zeit (Monate)",
            ylab = "Überlebenswahrscheinlichkeit",
            palette = "jco",
            ggtheme = theme_minimal()
          )
          
          print(p_surv_meld_period)
        } else {
          cat("\nNicht genügend Patienten in den MELD-Kategorien für eine Überlebensanalyse.\n")
        }
      }
    }
    
    # 4.3. BCA-ANALYSE FÜR DIE ZEITPERIODE, FALLS ANWENDBAR
    if (exists("merged_data") && "muscle" %in% names(merged_data)) {
      cat("\n--- 4.3. BCA-ANALYSE FÜR DIE ZEITPERIODE ---\n")
      
      # BCA-Subset für die Zeitperiode
      bca_period_subset <- merged_data %>%
        filter(time_period == period) %>%
        filter(!is.na(muscle) & !is.na(sat) & !is.na(vat) &
               !is.na(imat) & !is.na(eat) & !is.na(pat) & !is.na(tat))
      
      cat(paste0("BCA-Subset für diese Periode: ", nrow(bca_period_subset), " Zeilen\n"))
      
      if (nrow(bca_period_subset) >= 10) {
        # Zusammenfassung der BCA-Variablen für diese Zeitperiode
        cat("\nZusammenfassung der BCA-Variablen für diese Zeitperiode:\n")
        print(summary(bca_period_subset[, available_bca_vars]))
        
        # C-Index-Berechnung für die Zeitperiode, falls anwendbar
        if (exists("bca_period_subset") &&
            "lab_meld" %in% names(bca_period_subset) &&
            "BC_MELD" %in% names(bca_period_subset) &&
            all(c("waitlist_time_months", "status") %in% names(bca_period_subset))) {
          
          # Hier müssen wir zuerst sicherstellen, dass BC_MELD für dieses Subset berechnet wurde
          if (!"BC_MELD" %in% names(bca_period_subset)) {
            cat("\nBerechnung des BC-MELD-Scores für dieses Subset...\n")
            
            # SMI, IMAC, VSR berechnen für dieses Subset
            bca_period_subset <- bca_period_subset %>%
              mutate(
                height_m = cm / 100,
                muscle_area = muscle / height_m,
                SMI = muscle_area / (height_m^2),
                IMAC = imat / muscle,
                VSR = vat / sat
              )
            
            # Medianwerte für dieses Subset bestimmen
            smi_median_period <- median(bca_period_subset$SMI, na.rm = TRUE)
            imac_median_period <- median(bca_period_subset$IMAC, na.rm = TRUE)
            vsr_median_period <- median(bca_period_subset$VSR, na.rm = TRUE)
            
            # BC_MELD berechnen
            bca_period_subset <- bca_period_subset %>%
              mutate(
                low_SMI = as.numeric(SMI < smi_median_period),
                high_IMAC = as.numeric(IMAC > imac_median_period),
                high_VSR = as.numeric(VSR > vsr_median_period),
                BC_MELD = lab_meld +
                          3.59 * low_SMI +
                          5.42 * high_IMAC +
                          2.06 * high_VSR
              )
          }
          
          # Vergleich der C-Indizes von MELD und BC-MELD für diese Zeitperiode
          cindex_meld_period <- concordance(Surv(waitlist_time_months, status) ~ lab_meld,
                                         data = bca_period_subset)$concordance
          cindex_bc_meld_period <- concordance(Surv(waitlist_time_months, status) ~ BC_MELD,
                                            data = bca_period_subset)$concordance
          
          cat("\nVergleich der C-Indizes für diese Zeitperiode:\n")
          cat(paste0("C-Index MELD: ", round(cindex_meld_period, 3), "\n"))
          cat(paste0("C-Index BC-MELD: ", round(cindex_bc_meld_period, 3), "\n"))
          cat(paste0("Verbesserung: ",
                   round((cindex_bc_meld_period - cindex_meld_period) * 100, 1), "%\n"))
        }
      } else {
        cat("\nNicht genügend Patienten mit BCA-Daten für diese Zeitperiode.\n")
      }
    }
  }
} else {
  cat("Variable 'time_period' nicht im Datensatz vorhanden.\n")
}