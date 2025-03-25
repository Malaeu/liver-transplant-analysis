# Architekturplan: Verbessertes Überlebensvorhersagemodell für Lebertransplantations-Wartelisten

## 1. Systemübersicht

```mermaid
graph TD
    A[Datenquellen] --> B[Datenaufbereitung]
    B --> C[Explorative Datenanalyse]
    C --> D[Modellentwicklung]
    D --> E[Modellvalidierung]
    E --> F[Berichterstellung]
    
    subgraph "Datenquellen"
    A1[Vollständiger Datensatz\n~1200 Patienten] 
    A2[BCA-Teilmenge\n~600 Patienten]
    end
    
    subgraph "Modellentwicklung"
    D1[BC-MELD Formel]
    D2[AORSF Modelle]
    D3[H2O Modelle]
    D4[Ensemble-Modelle]
    end
```

## 2. Technische Anforderungen

### 2.1 Systemanforderungen
- **Flexibilität**: Code muss auf verschiedenen Systemen lauffähig sein
- **Skalierbarkeit**: Anpassungsfähig an verfügbare Ressourcen (RAM, CPU-Kerne)
- **Reproduzierbarkeit**: Feste Random Seeds, dokumentierte Abhängigkeiten

### 2.2 Softwareanforderungen
- **R-Version**: ≥ 4.2.0
- **Kernpakete**: 
  - Datenverarbeitung: tidyverse, data.table
  - Überlebensanalyse: survival, survminer
  - Maschinelles Lernen: AORSF, h2o
  - Visualisierung: ggplot2, plotly
  - Berichterstellung: rmarkdown, knitr, flexdashboard

### 2.3 Leistungsanforderungen
- **Validierungsmetriken**: Brier Score, C-Index, R²
- **Kreuzvalidierung**: Verschachtelte Kreuzvalidierung für Modellauswahl
- **Konfidenzintervalle**: Bootstrap oder Monte-Carlo-Methoden

## 3. Detaillierter Implementierungsplan

### 3.1 Datenaufbereitung und -validierung

```mermaid
graph TD
    A[Daten laden] --> B[Datenvalidierung]
    B --> C[Datenbereinigung]
    C --> D[Feature Engineering]
    D --> E[Datensatzaufteilung]
    
    subgraph "Datenvalidierung"
    B1[Negative Werte prüfen]
    B2[Inkonsistenzen identifizieren]
    B3[Fehlende Werte analysieren]
    end
    
    subgraph "Feature Engineering"
    D1[Abgeleitete Variablen erstellen]
    D2[Variablennormalisierung]
    D3[Kategorische Kodierung]
    end
```

#### Implementierungsdetails:
- **Flexibler Datenpfad-Mechanismus**: Relativer und absoluter Pfad unterstützt
- **Validierungsfunktionen**: Automatisierte Prüfungen für Datenqualität
- **Fehlende Werte**: Multiple Imputation mit mice-Paket
- **Feature Engineering**: 
  - Verhältnisse zwischen Körperzusammensetzungsvariablen
  - Normalisierung nach Körpergröße/Gewicht
  - Interaktionsterme für klinisch relevante Variablen

### 3.2 Explorative Datenanalyse

```mermaid
graph TD
    A[Deskriptive Statistik] --> B[Univariate Visualisierungen]
    B --> C[Bivariate Analysen]
    C --> D[Zeitreihenanalyse]
    D --> E[Korrelationsanalyse]
    
    subgraph "Visualisierungen"
    B1[Histogramme/Dichteplotts]
    B2[Boxplots]
    B3[Balkendiagramme]
    end
    
    subgraph "Bivariate Analysen"
    C1[Streudiagramme]
    C2[Gruppierte Boxplots]
    C3[Heatmaps]
    end
```

#### Implementierungsdetails:
- **Visualisierungsframework**: Modulares Design mit einheitlichem Theming
- **Publikationsqualität**: Hohe Auflösung, konsistente Farbpaletten, lesbare Schriftgrößen
- **Interaktive Optionen**: HTML-Widgets für interaktive Exploration
- **Automatisierte Berichterstellung**: Zusammenfassung der EDA in HTML-Format

### 3.3 Kaplan-Meier-Überlebensanalyse

```mermaid
graph TD
    A[Gesamtüberlebensanalyse] --> B[Stratifizierte Analyse]
    B --> C[Log-Rank-Tests]
    C --> D[Visualisierung]
    
    subgraph "Stratifizierungsvariablen"
    B1[Jahr der Wartelistenaufnahme]
    B2[Diagnosegruppen]
    B3[MELD-Score-Kategorien]
    B4[Altersgruppen]
    end
    
    subgraph "Visualisierungen"
    D1[Kaplan-Meier-Kurven]
    D2[Risikotabellen]
    D3[Waldplots für Hazard Ratios]
    end
```

#### Implementierungsdetails:
- **Zeitpunkte**: Analyse für 3, 6, 12, 36 und 60 Monate
- **Stratifizierung**: Dynamische Stratifizierungsfunktion für verschiedene Variablen
- **Visualisierung**: Publikationsreife KM-Kurven mit Risikotabellen
- **Statistische Tests**: Log-Rank-Tests mit p-Wert-Adjustierung für multiples Testen

### 3.4 BC-MELD-Formelentwicklung

```mermaid
graph TD
    A[Originale BC-MELD implementieren] --> B[Univariate Cox-Modelle]
    B --> C[Multivariate Cox-Modelle]
    C --> D[Regularisierte Regression]
    D --> E[Formelableitung]
    E --> F[Validierung]
    
    subgraph "Regularisierte Regression"
    D1[LASSO]
    D2[Ridge]
    D3[Elastic Net]
    end
    
    subgraph "Validierung"
    F1[Kreuzvalidierung]
    F2[Bootstrap für Konfidenzintervalle]
    F3[Leistungsmetriken]
    end
```

#### Implementierungsdetails:
- **Proportionale Hazards-Annahme**: Schoenfeld-Residuen-Tests
- **Variablenauswahl**: Elastic Net mit optimiertem Alpha-Parameter
- **Kreuzvalidierung**: 10-fache Kreuzvalidierung für Hyperparameter-Optimierung
- **Konfidenzintervalle**: 1000 Bootstrap-Stichproben für Koeffizientenschätzungen
- **Vergleichsmetriken**: Brier-Score, C-Index und R² zu verschiedenen Zeitpunkten

### 3.5 Fortgeschrittene ML-Überlebensmodelle

```mermaid
graph TD
    A[Datenvorbereitung] --> B[AORSF-Modellierung]
    A --> C[H2O-Modellierung]
    B --> D[Hyperparameter-Tuning]
    C --> E[Verteilte Berechnung]
    D --> F[Ensemble-Modellierung]
    E --> F
    F --> G[Modellinterpretation]
    
    subgraph "AORSF-Hyperparameter"
    D1[Baumanzahl]
    D2[Variablen pro Split]
    D3[Minimale Knotengröße]
    end
    
    subgraph "Modellinterpretation"
    G1[SHAP-Werte]
    G2[Variablenwichtigkeit]
    G3[Partielle Abhängigkeitsplots]
    end
```

#### Implementierungsdetails:
- **AORSF-Konfiguration**: 
  - Ressourcenadaptive Implementierung (Anzahl der Threads basierend auf verfügbaren Kernen)
  - Hyperparameter-Grid mit 5-facher Kreuzvalidierung
  
- **H2O-Konfiguration**:
  - Dynamische Speicherzuweisung basierend auf verfügbarem RAM
  - Automatische Erkennung und Nutzung verfügbarer Kerne
  - Frühzeitiges Stoppen zur Vermeidung von Überanpassung
  
- **Ensemble-Methodik**:
  - Gewichtete Kombination von Modellvorhersagen
  - Optimierung der Gewichte durch Stacked Regression
  - Kreuzvalidierte Ensemble-Leistung
  
- **Interpretationsmethoden**:
  - SHAP-Werte für lokale und globale Interpretation
  - Variablenwichtigkeitsplots mit Konfidenzintervallen
  - Partielle Abhängigkeitsplots für Schlüsselvariablen

### 3.6 Berichterstellung und Visualisierung

```mermaid
graph TD
    A[Datenaufbereitung für Bericht] --> B[Statische Visualisierungen]
    A --> C[Interaktive Visualisierungen]
    B --> D[Tabellenerstellung]
    C --> D
    D --> E[HTML-Berichterstellung]
    E --> F[Klinische Interpretation]
    
    subgraph "Visualisierungstypen"
    B1[Überlebenskurven]
    B2[Waldplots]
    B3[SHAP-Zusammenfassungen]
    C1[Interaktive Überlebenskurven]
    C2[Dynamische Risikokalkulation]
    end
```

#### Implementierungsdetails:
- **Berichtsstruktur**: Modularer R Markdown-Bericht mit parametrisierten Abschnitten
- **Visualisierungsframework**: 
  - Einheitliches Theming für alle Grafiken
  - Hohe Auflösung für Publikationen (600 dpi)
  - Konsistente Farbpaletten (viridis für kontinuierliche, ColorBrewer für kategoriale Variablen)
  
- **Interaktive Elemente**:
  - Plotly für interaktive Überlebenskurven
  - DT für durchsuchbare/filterbare Tabellen
  - Shiny-Elemente für dynamische Risikokalkulation
  
- **Klinische Interpretation**:
  - Automatisierte Textgenerierung für Schlüsselergebnisse
  - Vergleichstabellen mit farblicher Hervorhebung signifikanter Unterschiede
  - Nomogramme für klinische Anwendung

## 4. Modulare Codestruktur

```mermaid
graph TD
    A[main.R] --> B[setup.R]
    A --> C[data_preprocessing.R]
    A --> D[exploratory_analysis.R]
    A --> E[survival_analysis.R]
    A --> F[bc_meld_formula.R]
    A --> G[ml_models.R]
    A --> H[report_generation.R]
    
    subgraph "Hilfsfunktionen"
    I[utils.R]
    J[visualization.R]
    K[validation.R]
    end
    
    B --> I
    C --> I
    C --> K
    D --> J
    E --> J
    F --> K
    G --> K
    H --> J
```

### 4.1 Hauptmodule

- **setup.R**: Umgebungseinrichtung, Paketladung, Konfiguration
- **data_preprocessing.R**: Datenladung, -validierung und -aufbereitung
- **exploratory_analysis.R**: Deskriptive Statistik und Visualisierungen
- **survival_analysis.R**: Kaplan-Meier und Cox-Modelle
- **bc_meld_formula.R**: BC-MELD-Implementierung und -Verbesserung
- **ml_models.R**: AORSF, H2O und Ensemble-Modelle
- **report_generation.R**: HTML-Berichterstellung

### 4.2 Hilfsfunktionen

- **utils.R**: Allgemeine Hilfsfunktionen, Datenmanipulation
- **visualization.R**: Visualisierungsfunktionen mit einheitlichem Theming
- **validation.R**: Kreuzvalidierung, Leistungsmetriken, Bootstrapping

## 5. Implementierungsplan

### Phase 1: Grundlegende Infrastruktur (Woche 1)
- Umgebungseinrichtung und Paketinstallation
- Datenladung und -validierung
- Explorative Datenanalyse
- Grundlegende Überlebensanalyse

### Phase 2: BC-MELD-Entwicklung (Woche 2)
- Implementierung der originalen BC-MELD-Formel
- Cox-Modellierung für Körperzusammensetzungsvariablen
- Regularisierte Regression für Koeffizientenableitung
- Validierung der verbesserten Formel

### Phase 3: ML-Modellentwicklung (Woche 3)
- AORSF-Implementierung und Hyperparameter-Tuning
- H2O-Modellierung und verteilte Berechnung
- Ensemble-Modellentwicklung
- Modellinterpretation und SHAP-Analyse

### Phase 4: Berichterstellung und Finalisierung (Woche 4)
- Visualisierungserstellung und -optimierung
- HTML-Berichterstellung
- Klinische Interpretation
- Dokumentation und Code-Bereinigung

## 6. Risiken und Abhilfemaßnahmen

| Risiko | Wahrscheinlichkeit | Auswirkung | Abhilfemaßnahme |
|--------|-------------------|------------|-----------------|
| Unzureichende Rechenressourcen für H2O | Mittel | Hoch | Ressourcenadaptive Implementierung, Fallback auf leichtgewichtige Modelle |
| Verletzung der proportionalen Hazards-Annahme | Hoch | Mittel | Stratifizierte Modelle, zeitabhängige Kovariaten |
| Überanpassung der ML-Modelle | Mittel | Hoch | Rigorose Kreuzvalidierung, Regularisierung, Ensemble-Methoden |
| Fehlende Werte in BCA-Variablen | Hoch | Mittel | Multiple Imputation, Sensitivitätsanalyse |
| Heterogene Systemumgebungen | Mittel | Niedrig | Containerisierung oder umgebungsunabhängige Implementierung |

## 7. Qualitätssicherung

- **Codequalität**: Einheitlicher Codierungsstil, umfassende Dokumentation
- **Reproduzierbarkeit**: Feste Random Seeds, versionierte Abhängigkeiten
- **Validierung**: Kreuzvalidierung, externe Validierung wenn möglich
- **Fehlerbehandlung**: Robuste Fehlerbehandlung und Logging
- **Leistungsüberwachung**: Benchmarking und Profilierung für Ressourcenoptimierung

## 8. Liefergegenstände

1. **R-Skripte**: Vollständige, kommentierte R-Skripte für alle Analysen
2. **HTML-Bericht**: Umfassender Bericht mit eingebetteten Visualisierungen
3. **Visualisierungen**: Publikationsreife Grafiken in verschiedenen Formaten
4. **Dokumentation**: Technische Dokumentation und Benutzerhandbuch
5. **Modelle**: Gespeicherte Modellobjekte für zukünftige Anwendungen

## 9. Zusammenfassung

Dieser Architekturplan bietet einen umfassenden Rahmen für die Entwicklung eines verbesserten Überlebensvorhersagemodells für Patienten auf der Lebertransplantations-Warteliste. Der Plan integriert traditionelle statistische Methoden mit fortschrittlichen maschinellen Lernansätzen und legt besonderen Wert auf Flexibilität, Reproduzierbarkeit und klinische Interpretierbarkeit. Die modulare Struktur ermöglicht eine effiziente Implementierung und einfache Wartung, während die adaptiven Ressourcenmechanismen die Ausführung auf verschiedenen Systemen gewährleisten.