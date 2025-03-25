# Plan zur Datenaufbereitung für die Überlebensanalyse

## Probleme im aktuellen Ansatz

1. **Ungeeignete Variablen**: Der Datensatz enthält viele Variablen, die für die Analyse nicht relevant sind (IDs, Datumsfelder, Diagnose-Strings).
2. **Character-Variablen**: Character-Variablen werden nicht korrekt in Faktoren umgewandelt, was zu Fehlern in den Modellen führt.
3. **Fehlende Werte**: Es gibt fehlende Werte in wichtigen Variablen, die nicht angemessen behandelt werden.
4. **Inkonsistente Variablennamen**: Die Variable `age` wird als `age.x` bezeichnet, was zu Fehlern führt.

## Lösungsansatz

### 1. Erstellung eines separaten Skripts zur Datenvorbereitung

Wir benötigen ein dediziertes Skript `create_analysis_dataset.R`, das folgende Aufgaben übernimmt:

- Laden der Rohdaten
- Auswahl relevanter Variablen
- Umwandlung von Character-Variablen in Faktoren
- Behandlung fehlender Werte
- Speichern des bereinigten Datensatzes

### 2. Identifizierung relevanter Variablen

Vorschlag für relevante Variablen:

**Demografische Daten**:
- `age.x` (Alter)
- `sex` (Geschlecht)
- `bmi` (Body Mass Index)

**Klinische Daten**:
- `lab_meld` (MELD-Score)

**BCA-Variablen**:
- `muscle` (Muskelmasse)
- `sat` (Subkutanes Fettgewebe)
- `vat` (Viszerales Fettgewebe)
- `imat` (Intramuskuläres Fettgewebe)
- `eat` (Epikardiales Fettgewebe)
- `pat` (Periarterielles Fettgewebe)
- `tat` (Gesamtes Fettgewebe)

**Outcome-Variablen**:
- `waitlist_time_months` (Wartezeit in Monaten)
- `status` (Ereignisstatus)

### 3. Umwandlung von Character-Variablen in Faktoren

Für jede Character-Variable:
1. Analyse der Häufigkeitsverteilung
2. Identifizierung der häufigsten Werte
3. Umwandlung in Faktoren mit sinnvollen Levels
4. Bei zu vielen Levels: Zusammenfassung zu Top-10 + "Sonstige"

### 4. Behandlung fehlender Werte

Für jede Variable mit fehlenden Werten:
1. Analyse des Anteils fehlender Werte
2. Entscheidung über Strategie (Imputation, Ausschluss если болше чем 40% fehlen.)
3. Dokumentation der gewählten Strategie

### 5. Erstellung des finalen Analysedatensatzes

- Speichern als `analysis_dataset_clean.rds`
- Dokumentation der Datensatzstruktur
- Validierung der Datenqualität

## Implementierungsplan

1. Erstellung des Skripts `create_analysis_dataset.R`
2. Ausführung und Analyse der Ergebnisse
3. Iterative Verbesserung basierend auf den Ergebnissen
4. Finalisierung des bereinigten Datensatzes
5. Anpassung der Analyseskripte zur Verwendung des neuen Datensatzes

## Vorteile dieses Ansatzes

1. **Transparenz**: Klare Dokumentation aller Schritte der Datenaufbereitung
2. **Reproduzierbarkeit**: Der gesamte Prozess ist reproduzierbar
3. **Qualität**: Sicherstellung der Datenqualität vor der Analyse
4. **Effizienz**: Vermeidung von Fehlern in späteren Analyseschritten