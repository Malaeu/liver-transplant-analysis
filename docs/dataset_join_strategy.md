# Strategie zum Zusammenführen der Datasets

## Problemstellung

Bei der Ausführung des Skripts tritt der Fehler "Merge column 'etnr_id' not found in both datasets" auf. Dies liegt daran, dass die ID-Spalte in den beiden Datasets unterschiedlich benannt ist:
- In `bca_data`: "etnr"
- In `full_data`: "etnr_id"

Darüber hinaus gibt es viele weitere Variablen, die ähnliche Informationen enthalten, aber unterschiedlich benannt sind.

## Variablenanalyse und Mapping

### 1. ID-Variablen (Schlüssel für den Join)
- `bca_data$etnr` → `full_data$etnr_id` (Primärschlüssel für den Join)

### 2. Demografische Variablen
- Name/Vorname: Identisch in beiden Datasets
- Geschlecht: `sex` in beiden vorhanden
- Geburtsdatum: `date_of_birth` vs. `birth_date`
- Alter: `age` in beiden vorhanden

### 3. Anthropometrische Daten
- Gewicht: `weight` vs. `kg`
- Größe: `height` vs. `cm`
- BMI: `bmi` in beiden vorhanden

### 4. Klinische Daten
- Blutgruppe: `blood_type` vs. `blood_type`
- Rhesusfaktor: `rh` vs. `rh_factor`
- MELD-Scores: `lab_meld`, `exc_meld`, `ped_meld` in beiden vorhanden

### 5. Diagnosen
- Primärdiagnose: `primary_diagnosis` vs. `diagnosis_1`
- Sekundärdiagnosen: `diagnose_2/3` vs. `diagnosis_2/3`
- Relisting-Informationen: Unterschiedliche Namenskonventionen

### 6. Wartelisten-Informationen
- Wartelistendatum: `date_of_wl` vs. `waitlist_date`
- Dringlichkeit: `urgency` vs. `urg`/`urgency`

### 7. Outcome-Variablen
- Todesdatum: `todesdatum` vs. `death_date`
- Todesursache: `todesursache` vs. `death_cause`

### 8. Körperzusammensetzungs-Variablen (nur in bca_data)
- `bone`, `muscle`, `sat`, `vat`, `imat`, `eat`, `pat`, `tat`

### 9. Zusätzliche Analysen (nur in full_data)
- Wartezeit: `waitlist_time_days`, `waitlist_time_months`
- Diagnosegruppen: `diagnosis_group`, `diagnosis_subgroup`
- Überlebensstatus: `status`

## Empfohlene Join-Strategie

1. **Umbenennung der ID-Spalte**:
   ```R
   names(bca_data)[names(bca_data) == "etnr"] <- "etnr_id"
   ```

2. **Harmonisierung weiterer Schlüsselvariablen vor dem Join**:
   ```R
   # Harmonisierung der Variablennamen für konsistente Datenstruktur
   names(bca_data)[names(bca_data) == "date_of_birth"] <- "birth_date"
   names(bca_data)[names(bca_data) == "weight"] <- "kg"
   names(bca_data)[names(bca_data) == "height"] <- "cm"
   names(bca_data)[names(bca_data) == "rh"] <- "rh_factor"
   names(bca_data)[names(bca_data) == "primary_diagnosis"] <- "diagnosis_1"
   names(bca_data)[names(bca_data) == "date_of_wl"] <- "waitlist_date"
   names(bca_data)[names(bca_data) == "todesdatum"] <- "death_date"
   names(bca_data)[names(bca_data) == "todesursache"] <- "death_cause"
   ```

3. **Join-Strategie**:
   - Verwende einen `left_join` mit `full_data` als Basis und füge die BCA-Variablen hinzu
   - Behalte alle Variablen aus `full_data`
   - Füge nur die einzigartigen Variablen aus `bca_data` hinzu (insbesondere die Körperzusammensetzungsdaten)
   - Bei Konflikten (gleiche Variable in beiden Datasets) bevorzuge die Version aus `full_data`

4. **Implementierung in der `create_analysis_dataset`-Funktion**:
   ```R
   # Identifiziere die einzigartigen BCA-Variablen, die wir behalten wollen
   bca_unique_vars <- c("bone", "muscle", "sat", "vat", "imat", "eat", "pat", "tat", 
                        "date_of_ct", "acsn_nr")
   
   # Füge diese zum Merge hinzu
   merged_data <- merge(full_data, 
                        bca_data[, c("etnr_id", bca_unique_vars)], 
                        by = "etnr_id", 
                        all.x = TRUE)
   ```

Diese Strategie stellt sicher, dass:
1. Wir alle wichtigen Variablen aus beiden Datasets behalten
2. Wir Duplikate vermeiden
3. Wir eine konsistente Namenskonvention verwenden
4. Die Körperzusammensetzungsdaten aus dem BCA-Dataset mit den umfangreichen klinischen und demografischen Daten aus dem Full-Dataset kombiniert werden