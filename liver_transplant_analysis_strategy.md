# Analysestrategie für Lebertransplantationsdaten mit Körperzusammensetzungsanalyse

## Übersicht der Datensätze

### Hauptdatensatz (imputed_data_full.csv)
- **Größe**: 1344 Patienten, 66 Variablen
- **Wichtige Variablen**:
  - Demografische Daten: age, sex, kg, cm, bmi
  - Klinische Daten: lab_meld, exc_meld, diagnosis_group, diagnosis_subgroup
  - Outcome-Daten: waitlist_time_days, waitlist_time_months, status
- **Fehlende Werte**: 316 Patienten haben fehlende MELD-Werte

### BCA-Datensatz (wl_df_with_bca.rds)
- **Größe**: 1064 Patienten, 64 Variablen
- **Wichtige Variablen**:
  - BCA-Daten: muscle, sat, vat, imat, eat, pat, tat
  - Outcome-Daten: surv_status, surv_time
- **Fehlende Werte**: Nur 294 Patienten haben BCA-Daten (770 fehlende Werte)

## Analysestrategie

### 1. Datenvorbereitung und -bereinigung

1. **Zusammenführen der Datensätze**
   - Join über die gemeinsame ID-Variable (etnr_id)
   - Beibehaltung aller Patienten aus dem Hauptdatensatz
   - Hinzufügen der BCA-Variablen aus dem BCA-Datensatz

2. **Erstellung von zwei Analysedatensätzen**
   - **Vollständiger Datensatz**: Alle Patienten (1344), auch ohne BCA-Daten
   - **BCA-Datensatz**: Nur Patienten mit vollständigen BCA-Daten (ca. 294)

3. **Variablenauswahl und -bereinigung**
   - Demografische Variablen: age, sex, kg, cm, bmi
   - Klinische Variablen: lab_meld, exc_meld, diagnosis_group, diagnosis_subgroup
   - BCA-Variablen: muscle, sat, vat, imat, eat, pat, tat
   - Outcome-Variablen: waitlist_time_days, waitlist_time_months, status

4. **Erstellung kategorischer Variablen**
   - BMI-Kategorien: Untergewicht (<18.5), Normalgewicht (18.5-24.9), Übergewicht (25-29.9), Adipositas (≥30)
   - MELD-Kategorien: MELD < 10, MELD 10-19, MELD 20-29, MELD ≥ 30
   - Alter-Kategorien: < 30, 30-39, 40-49, 50-59, 60-69, ≥ 70
   - Standardisierung des Geschlechts: M, F

5. **Normalisierung der BCA-Variablen**
   - Normalisierung nach Körpergröße: Wert / (Größe in m)²

### 2. Deskriptive Analyse

1. **Demografische Charakteristika**
   - Alters- und Geschlechtsverteilung
   - BMI-Verteilung
   - Diagnosegruppen

2. **Klinische Charakteristika**
   - MELD-Score-Verteilung
   - Komplikationen (Aszites, Varizen, Enzephalopathie, Splenomegalie)

3. **BCA-Charakteristika**
   - Verteilung der BCA-Variablen
   - Korrelation zwischen BCA-Variablen und klinischen Parametern

4. **Outcome-Charakteristika**
   - Wartezeit auf der Transplantationsliste
   - Überlebensstatus

### 3. Überlebensanalyse

1. **Kaplan-Meier-Überlebenskurven**
   - Gesamtüberleben
   - Überleben nach MELD-Kategorie
   - Überleben nach Diagnosegruppe
   - Überleben nach BCA-Parametern (für den BCA-Datensatz)

2. **Cox-Regressionsanalyse**
   - Univariate Analyse für jede potenzielle Einflussvariable
   - Multivariate Analyse mit Adjustierung für wichtige Kovariaten

### 4. Entwicklung eines BC-MELD-Scores

1. **Erweiterung des MELD-Scores mit BCA-Parametern**
   - Identifizierung der relevantesten BCA-Parameter
   - Entwicklung einer Formel zur Integration in den MELD-Score

2. **Validierung des BC-MELD-Scores**
   - Vergleich der Vorhersagekraft mit dem Standard-MELD-Score
   - Berechnung von C-Index, Brier-Score und R²

## Erwartete Ergebnisse

### 1. Deskriptive Statistik

- **Demografische Daten**:
  - Alter: Median ca. 55 Jahre
  - Geschlecht: ca. 65% männlich, 35% weiblich
  - BMI: Median ca. 26 (leichtes Übergewicht)

- **Klinische Daten**:
  - MELD-Score: Median ca. 15
  - Häufigste Diagnosegruppen: Virale Hepatitis, Alkoholische Lebererkrankung, Hepatozelluläres Karzinom

- **BCA-Daten**:
  - Muskelmasse: Median ca. 6941 cm³
  - Viszerales Fettgewebe (VAT): Median ca. 2833 cm³
  - Subkutanes Fettgewebe (SAT): Median ca. 6632 cm³

### 2. Überlebensanalyse

- **Überlebensraten**:
  - Gesamtüberlebensrate: ca. 60-65%
  - Überlebensraten nach MELD-Kategorie:
    - MELD < 10: ca. 80-85%
    - MELD 10-19: ca. 65-70%
    - MELD 20-29: ca. 50-55%
    - MELD ≥ 30: ca. 30-35%

- **Risikofaktoren für schlechteres Überleben**:
  - Höherer MELD-Score
  - Höheres Alter
  - Bestimmte Diagnosegruppen (z.B. Hepatozelluläres Karzinom)
  - Möglicherweise: Niedrigere Muskelmasse, höheres viszerales Fettgewebe

### 3. BC-MELD-Score

- **Erwartete Verbesserung**:
  - Verbesserte Vorhersagekraft im Vergleich zum Standard-MELD-Score
  - Bessere Stratifizierung von Patienten mit mittlerem MELD-Score (10-19)
  - Identifizierung von Hochrisikopatienten mit Sarkopenie

## Limitationen

1. **Fehlende BCA-Daten**:
   - Nur 294 von 1344 Patienten (ca. 22%) haben BCA-Daten
   - Mögliche Selektionsverzerrung

2. **Fehlende MELD-Werte**:
   - 316 Patienten (ca. 24%) haben fehlende MELD-Werte

3. **Heterogenität der Diagnosen**:
   - Verschiedene Lebererkrankungen können unterschiedliche Auswirkungen auf die Körperzusammensetzung haben

4. **Retrospektive Datenanalyse**:
   - Keine Möglichkeit zur Kontrolle von Störfaktoren
   - Keine Möglichkeit zur Überprüfung von Kausalzusammenhängen

## Schlussfolgerung

Die Analyse der Lebertransplantationsdaten mit Körperzusammensetzungsanalyse bietet die Möglichkeit, den prognostischen Wert von BCA-Parametern für das Überleben auf der Warteliste zu untersuchen. Trotz der Limitationen durch fehlende Daten kann diese Analyse wertvolle Einblicke in die Rolle der Körperzusammensetzung bei Patienten auf der Warteliste für Lebertransplantationen liefern und möglicherweise zur Entwicklung eines verbesserten Prognosemodells (BC-MELD) beitragen.