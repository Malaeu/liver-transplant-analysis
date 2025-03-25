АНАЛИЗ МЕДИЦИНСКИХ ДАННЫХ ПО ТРАНСПЛАНТАЦИИ ПЕЧЕНИ

## СИСТЕМНАЯ РОЛЬ
Ты эксперт по анализу медицинских данных, специализирующийся на трансплантационной медицине и анализе медицинских регистров. Твоя задача — методично проанализировать два датасета, связанных с трансплантацией печени (BCA данные и основные клинические данные), без кат-оффов и галлюцинаций. Если какой-то шаг невыполним, честно об этом сообщи.

## КОНТЕКСТ
У нас есть два связанных датасета с данными взрослых пациентов (старше 18 лет) на листе ожидания трансплантации печени:

1. **bca_data**: Содержит данные композиции тела (body composition analysis) и основные клинические параметры пациентов.
2. **full_data**: Полный клинический датасет с дополнительными параметрами, статусами и исходами.

Необходимо проанализировать эти данные и создать очищенный аналитический датасет, включающий только значимые клинические параметры и правильно преобразованные категориальные переменные.

## ИНСТРУКЦИИ
Выполни следующие шаги строго по порядку, без пропусков:

> **ВАЖНО:** Запрещено выдумывать данные. Если не можешь выполнить шаг из-за недоступности данных, **прямо укажи**, какие именно данные тебе нужны для анализа.

### 1. ЗАГРУЗКА ДАННЫХ И ПЕРВИЧНЫЙ ОБЗОР
```r
# Загрузка данных (если это не выполнено)
# bca_data уже должен быть загружен (.rds файл)
# full_data уже должен быть загружен (.csv файл)

# Проверка размерности обоих датасетов
dim(bca_data)
dim(full_data)

# Просмотр первых нескольких строк каждого датасета
head(bca_data, 3)
head(full_data, 3)
```

### 2. ОПРЕДЕЛЕНИЕ ПЕРЕМЕННЫХ ДЛЯ АНАЛИЗА
После внимательного анализа обоих датасетов, определим переменные, которые мы сохраним и исключим:

```r
# 1. Исключаемые переменные - не нужны для анализа
exclude_vars <- c(
    # ID и персональные данные
    "etnr_id", "lebensfallnr", "name", "vorname", "acsn_nr", "tx.nr",
    
    # Даты
    "birth_date", "relisting_date", "re.list_datum", "death_date", 
    "last_contact", "letzter_kontakt", "date_of_ltx", "date_of_ct",
    
    # Диагностические тексты
    "diagnosis_1", "diagnose_2", "diagnose_3", "diagnose", "diagnosis",
    "relisting_diagnosis", "diagnose_relistung", "diagnosis_to_clean",
    
    # Дополнительные флаги и неактуальные показатели
    "relisting_flag", "relist", "ltx_number", "adults", "source",
    "exc_meld", "ped_meld", "study", "studie", "za_wl", "za.wl",
    "notes", "bemerkungen", "received_ltx", "time_to_ltx",
    "abdominal_surgeries", "abdominelle_opâ.s", "infektionen",
    "infections", "pfortader", "extend_alloc_on_wl"
)

# 2. Основные переменные для анализа
core_vars <- c(
    # Демографические
    "sex", "age", "nationality", "Race", "RaceGroup",
    
    # Антропометрические
    "kg", "cm", "bmi", "bmi_group",
    
    # Клинические показатели
    "lab_meld", "meld_group", "urgency",
    
    # BCA переменные - основные данные анализа состава тела
    "bone", "muscle", "sat", "vat", "imat", "eat", "pat", "tat",
    
    # Портальная гипертензия и осложнения
    "has_ascites", "has_varices", "has_encephalopathy", "has_splenomegaly",
    "portal_vein_thrombosis", "portal_vein_known", "portal_hypertension",
    "portal_vein_open", "portal_vein_abnormal",
    
    # Дополнительные клинические признаки
    "abd_op", "abd_op_binary", "has_remarks", "covid19_related",
    "hcv_mention", "transplant_related",
    
    # Временные и категориальные переменные
    "wl_year", "studie_cat", "wl_start", "event_date",
    
    # Исходы и длительности
    "waitlist_time_days", "waitlist_time_months", "status", "surv_time", "surv_status",
    
    # Сгруппированные диагнозы (без текстовых строк)
    "diagnosis_clean", "base_disease", "diagnosis_final", "diagnosis_grouped",
    "diagnosis_impact_category", "diagnosis_group", "diagnosis_subgroup",
    "top10_diagnosis_by_count", "top10_diagnosis_death_rate", "top10_nationality_by_death_rate"
)

# Проверяем наличие переменных в датасетах
cat("Наличие основных переменных:\n")
cat("В bca_data:", sum(core_vars %in% names(bca_data)), "из", length(core_vars), "\n")
cat("В full_data:", sum(core_vars %in% names(full_data)), "из", length(core_vars), "\n")
```

### 3. АНАЛИЗ КЛЮЧЕВЫХ ПЕРЕМЕННЫХ ПО ГРУППАМ
Проанализируем ключевые переменные, разделенные на логические группы:

```r
# 1. Демографические данные
demographic_vars <- c("sex", "age", "nationality", "Race", "RaceGroup")

# 2. Антропометрические данные
anthropometric_vars <- c("kg", "cm", "bmi", "bmi_group")

# 3. Показатели MELD и клинические
clinical_vars <- c("lab_meld", "meld_group", "urgency")

# 4. Данные анализа композиции тела (BCA)
bca_vars <- c("bone", "muscle", "sat", "vat", "imat", "eat", "pat", "tat")

# 5. Портальная гипертензия и осложнения
portal_vars <- c(
    "has_ascites", "has_varices", "has_encephalopathy", "has_splenomegaly",
    "portal_vein_thrombosis", "portal_vein_known", "portal_hypertension",
    "portal_vein_open", "portal_vein_abnormal"
)

# 6. Временные переменные и исходы
outcome_vars <- c(
    "waitlist_time_days", "waitlist_time_months", "status", "surv_time", "surv_status"
)

# 7. Категории диагнозов
diagnosis_cat_vars <- c(
    "diagnosis_clean", "base_disease", "diagnosis_final", "diagnosis_grouped",
    "diagnosis_impact_category", "diagnosis_group", "diagnosis_subgroup"
)

# Анализируем переменные по группам
analysis_groups <- list(
    "demographic_vars" = demographic_vars,
    "anthropometric_vars" = anthropometric_vars,
    "clinical_vars" = clinical_vars,
    "bca_vars" = bca_vars,
    "portal_vars" = portal_vars,
    "outcome_vars" = outcome_vars,
    "diagnosis_cat_vars" = diagnosis_cat_vars
)

for (group_name in names(analysis_groups)) {
    cat("\n\n== АНАЛИЗ ГРУППЫ:", group_name, "==\n")
    group_vars <- analysis_groups[[group_name]]
    
    # Выбираем из обоих датасетов
    for (var in group_vars) {
        if (var %in% names(bca_data)) {
            cat("\nПеременная:", var, "(в bca_data)\n")
            cat("Тип:", class(bca_data[[var]]), "\n")
            
            if (is.character(bca_data[[var]]) || is.factor(bca_data[[var]])) {
                # Анализ категориальных переменных
                unique_vals <- unique(bca_data[[var]])
                cat("Количество уникальных значений:", length(unique_vals), "\n")
                if (length(unique_vals) <= 10) {
                    # Показываем все уникальные значения и их частоты
                    value_counts <- table(bca_data[[var]], useNA = "ifany")
                    print(value_counts)
                } else {
                    # Показываем только топ-10 значений
                    top_values <- sort(table(bca_data[[var]], useNA = "ifany"), decreasing = TRUE)[1:10]
                    cat("Топ-10 значений:\n")
                    print(top_values)
                    cat("ВНИМАНИЕ: Более 10 уровней. Требуется группировка.\n")
                }
            } else if (is.numeric(bca_data[[var]])) {
                # Анализ числовых переменных
                cat("Статистика:\n")
                print(summary(bca_data[[var]]))
                # Проверка выбросов
                if (sum(!is.na(bca_data[[var]])) > 5) { # Достаточно данных для анализа
                    q1 <- quantile(bca_data[[var]], 0.25, na.rm = TRUE)
                    q3 <- quantile(bca_data[[var]], 0.75, na.rm = TRUE)
                    iqr <- q3 - q1
                    lower_bound <- q1 - 1.5 * iqr
                    upper_bound <- q3 + 1.5 * iqr
                    outliers_count <- sum(bca_data[[var]] < lower_bound | bca_data[[var]] > upper_bound, na.rm = TRUE)
                    cat("Количество выбросов:", outliers_count, "\n")
                    cat("Процент выбросов:", round(outliers_count / sum(!is.na(bca_data[[var]])) * 100, 2), "%\n")
                }
            }
            # Показать первые 5 значений
            cat("Первые 5 значений:", head(bca_data[[var]], 5), "\n")
            # Показать % пропущенных значений
            na_percent <- sum(is.na(bca_data[[var]])) / nrow(bca_data) * 100
            cat("Процент пропущенных значений:", round(na_percent, 2), "%\n")
        }
        
        if (var %in% names(full_data)) {
            cat("\nПеременная:", var, "(в full_data)\n")
            cat("Тип:", class(full_data[[var]]), "\n")
            
            if (is.character(full_data[[var]]) || is.factor(full_data[[var]])) {
                # Анализ категориальных переменных
                unique_vals <- unique(full_data[[var]])
                cat("Количество уникальных значений:", length(unique_vals), "\n")
                if (length(unique_vals) <= 10) {
                    # Показываем все уникальные значения и их частоты
                    value_counts <- table(full_data[[var]], useNA = "ifany")
                    print(value_counts)
                } else {
                    # Показываем только топ-10 значений
                    top_values <- sort(table(full_data[[var]], useNA = "ifany"), decreasing = TRUE)[1:10]
                    cat("Топ-10 значений:\n")
                    print(top_values)
                    cat("ВНИМАНИЕ: Более 10 уровней. Требуется группировка.\n")
                }
            } else if (is.numeric(full_data[[var]])) {
                # Анализ числовых переменных
                cat("Статистика:\n")
                print(summary(full_data[[var]]))
                # Проверка выбросов
                if (sum(!is.na(full_data[[var]])) > 5) { # Достаточно данных для анализа
                    q1 <- quantile(full_data[[var]], 0.25, na.rm = TRUE)
                    q3 <- quantile(full_data[[var]], 0.75, na.rm = TRUE)
                    iqr <- q3 - q1
                    lower_bound <- q1 - 1.5 * iqr
                    upper_bound <- q3 + 1.5 * iqr
                    outliers_count <- sum(full_data[[var]] < lower_bound | full_data[[var]] > upper_bound, na.rm = TRUE)
                    cat("Количество выбросов:", outliers_count, "\n")
                    cat("Процент выбросов:", round(outliers_count / sum(!is.na(full_data[[var]])) * 100, 2), "%\n")
                }
            }
            # Показать первые 5 значений
            cat("Первые 5 значений:", head(full_data[[var]], 5), "\n")
            # Показать % пропущенных значений
            na_percent <- sum(is.na(full_data[[var]])) / nrow(full_data) * 100
            cat("Процент пропущенных значений:", round(na_percent, 2), "%\n")
        }
    }
}
```

### 4. АНАЛИЗ ПЕРЕСЕЧЕНИЯ НАБОРОВ ДАННЫХ
Проверим, насколько датасеты пересекаются и согласуются между собой:

```r
# Проверяем количество общих ID
common_ids <- intersect(bca_data$etnr_id, full_data$etnr_id)
cat("Количество общих ID между датасетами:", length(common_ids), "\n")
cat("Процент от bca_data:", round(length(common_ids) / nrow(bca_data) * 100, 2), "%\n")
cat("Процент от full_data:", round(length(common_ids) / nrow(full_data) * 100, 2), "%\n")

# Проверяем согласованность данных на общих ID
if (length(common_ids) > 0) {
    # Выбираем общие переменные для сравнения (исключая ID и даты)
    common_vars <- intersect(names(bca_data), names(full_data))
    common_vars <- setdiff(common_vars, exclude_vars)
    
    cat("\nОбщие переменные для сравнения:", common_vars, "\n")
    
    # Для каждой общей переменной проверяем согласованность
    for (var in common_vars[1:min(5, length(common_vars))]) { # Ограничимся первыми 5 для краткости
        cat("\nСравнение переменной:", var, "\n")
        
        # Создаем подмножества данных
        bca_subset <- bca_data[bca_data$etnr_id %in% common_ids, c("etnr_id", var)]
        full_subset <- full_data[full_data$etnr_id %in% common_ids, c("etnr_id", var)]
        
        # Объединяем
        comparison <- merge(bca_subset, full_subset, by = "etnr_id", suffixes = c(".bca", ".full"))
        
        # Проверяем совпадение
        if (is.numeric(comparison[[paste0(var, ".bca")]])) {
            # Для числовых - рассчитываем разницу
            comparison$diff <- comparison[[paste0(var, ".bca")]] - comparison[[paste0(var, ".full")]]
            cat("Минимальная разница:", min(comparison$diff, na.rm = TRUE), "\n")
            cat("Максимальная разница:", max(comparison$diff, na.rm = TRUE), "\n")
            cat("Средняя разница:", mean(comparison$diff, na.rm = TRUE), "\n")
        } else {
            # Для категориальных - считаем % совпадений
            match_count <- sum(comparison[[paste0(var, ".bca")]] == comparison[[paste0(var, ".full")]], na.rm = TRUE)
            total_count <- sum(!is.na(comparison[[paste0(var, ".bca")]]) & !is.na(comparison[[paste0(var, ".full")]]))
            if (total_count > 0) {
                cat("Процент совпадений:", round(match_count / total_count * 100, 2), "%\n")
            } else {
                cat("Недостаточно данных для сравнения\n")
            }
        }
    }
}
```

### 5. ПРЕОБРАЗОВАНИЕ КАТЕГОРИАЛЬНЫХ ПЕРЕМЕННЫХ
Проанализируем и преобразуем категориальные переменные, которые имеют более 10 уровней:

```r
# Функция для анализа и предложения группировки категориальных переменных
analyze_categorical_vars <- function(data, var_name) {
    if (!(var_name %in% names(data))) {
        return(cat("Переменная", var_name, "отсутствует в датасете\n"))
    }
    
    var_data <- data[[var_name]]
    if (!is.character(var_data) && !is.factor(var_data)) {
        return(cat("Переменная", var_name, "не является категориальной\n"))
    }
    
    # Анализируем количество и частоту уровней
    value_counts <- table(var_data, useNA = "ifany")
    cat("Переменная:", var_name, "\n")
    cat("Количество уникальных значений:", length(value_counts) - (sum(is.na(names(value_counts))) > 0), "\n")
    
    # Показываем топ-10 значений
    top_values <- sort(value_counts, decreasing = TRUE)[1:min(10, length(value_counts))]
    cat("Топ-10 значений:\n")
    print(top_values)
    
    # Процент охвата топ-10 значениями
    top_coverage <- sum(top_values) / sum(value_counts) * 100
    cat("Процент охвата топ-10 значениями:", round(top_coverage, 2), "%\n")
    
    # Предлагаем стратегию группировки, если слишком много уровней
    if (length(value_counts) > 10) {
        cat("\nСтратегия группировки для", var_name, ":\n")
        
        if (grepl("nationality|Race", var_name, ignore.case = TRUE)) {
            cat("1. Оставить топ-5 самых частых национальностей/рас\n")
            cat("2. Остальные объединить в категорию 'Other'\n")
        } else if (grepl("diagnosis", var_name, ignore.case = TRUE)) {
            cat("1. Использовать уже существующие группировки диагнозов (diagnosis_group)\n")
            cat("2. Или группировать по этиологии: вирусные, метаболические, аутоиммунные и т.д.\n")
        } else {
            # Общая стратегия для других переменных
            if (top_coverage > 80) {
                cat("1. Оставить топ-10 значений как отдельные категории\n")
                cat("2. Остальные объединить в 'Other'\n")
            } else {
                cat("1. Разработать логическую группировку на основе предметной области\n")
                cat("2. Или применить кластеризацию по частоте\n")
            }
        }
    } else {
        cat("\nКоличество уровней не превышает 10, группировка не требуется\n")
    }
    
    cat("\n")
}

# Анализируем категориальные переменные в обоих датасетах
cat("=== АНАЛИЗ КАТЕГОРИАЛЬНЫХ ПЕРЕМЕННЫХ В BCA_DATA ===\n\n")
cat_vars_bca <- names(bca_data)[sapply(bca_data, function(x) is.character(x) || is.factor(x))]
cat_vars_bca <- setdiff(cat_vars_bca, exclude_vars)
for (var in cat_vars_bca) {
    analyze_categorical_vars(bca_data, var)
}

cat("\n=== АНАЛИЗ КАТЕГОРИАЛЬНЫХ ПЕРЕМЕННЫХ В FULL_DATA ===\n\n")
cat_vars_full <- names(full_data)[sapply(full_data, function(x) is.character(x) || is.factor(x))]
cat_vars_full <- setdiff(cat_vars_full, exclude_vars)
for (var in cat_vars_full) {
    analyze_categorical_vars(full_data, var)
}
```

### 6. СОЗДАНИЕ ФУНКЦИИ ДЛЯ ОБЪЕДИНЕНИЯ И ОЧИСТКИ ДАННЫХ

```r
# Определяем функцию для создания аналитического датасета
create_clean_analysis_dataset <- function(bca_data, full_data) {
    # Определяем ID переменную для объединения датасетов
    id_var <- "etnr_id"
    
    # Определяем переменные, которые мы хотим включить в итоговый датасет
    # Исключаем переменные, которые были определены как ненужные
    vars_to_include <- setdiff(core_vars, exclude_vars)
    
    # Список переменных из каждого датасета
    bca_vars_to_include <- intersect(vars_to_include, names(bca_data))
    full_vars_to_include <- intersect(vars_to_include, names(full_data))
    
    # Создаем подмножества данных
    bca_subset <- bca_data[, c(id_var, bca_vars_to_include)]
    full_subset <- full_data[, c(id_var, full_vars_to_include)]
    
    # Объединяем датасеты
    merged_data <- merge(bca_subset, full_subset, by = id_var, all = TRUE, suffixes = c(".bca", ".full"))
    
    # Обрабатываем дублирующиеся переменные, отдавая предпочтение тем, у которых меньше NA
    vars_with_suffixes <- grep("\\.(bca|full)$", names(merged_data), value = TRUE)
    base_vars <- unique(sub("\\.(bca|full)$", "", vars_with_suffixes))
    
    for (base_var in base_vars) {
        bca_var <- paste0(base_var, ".bca")
        full_var <- paste0(base_var, ".full")
        
        # Если обе версии существуют
        if (bca_var %in% names(merged_data) && full_var %in% names(merged_data)) {
            bca_na_count <- sum(is.na(merged_data[[bca_var]]))
            full_na_count <- sum(is.na(merged_data[[full_var]]))
            
            # Создаем новую переменную, предпочитая ту, у которой меньше NA
            if (bca_na_count <= full_na_count) {
                merged_data[[base_var]] <- merged_data[[bca_var]]
            } else {
                merged_data[[base_var]] <- merged_data[[full_var]]
            }
            
            # Удаляем дублирующиеся переменные
            merged_data[[bca_var]] <- NULL
            merged_data[[full_var]] <- NULL
        } else if (bca_var %in% names(merged_data)) {
            # Только в bca_data
            merged_data[[base_var]] <- merged_data[[bca_var]]
            merged_data[[bca_var]] <- NULL
        } else if (full_var %in% names(merged_data)) {
            # Только в full_data
            merged_data[[base_var]] <- merged_data[[full_var]]
            merged_data[[full_var]] <- NULL
        }
    }
    
    # Преобразуем категориальные переменные в факторы
    # Переменная sex
    if ("sex" %in% names(merged_data)) {
        merged_data$sex <- factor(merged_data$sex, levels = c("M", "F"))
    }
    
    # Переменная status
    if ("status" %in% names(merged_data)) {
        # Проверяем уникальные значения
        status_values <- unique(merged_data$status)
        if (any(grepl("deceased|death", status_values, ignore.case = TRUE))) {
            merged_data$status <- ifelse(
                grepl("deceased|death", merged_data$status, ignore.case = TRUE),
                1, 0
            )
        }
    }
    
    # Бинарные переменные portal_vein_*
    portal_vars <- grep("^portal_vein_", names(merged_data), value = TRUE)
    for (var in portal_vars) {
        if (is.character(merged_data[[var]])) {
            merged_data[[var]] <- ifelse(
                grepl("yes|true|1", merged_data[[var]], ignore.case = TRUE),
                1,
                ifelse(grepl("no|false|0", merged_data[[var]], ignore.case = TRUE), 0, NA)
            )
        }
    }
    
    # Бинарные переменные has_*
    has_vars <- grep("^has_", names(merged_data), value = TRUE)
    for (var in has_vars) {
        if (is.character(merged_data[[var]])) {
            merged_data[[var]] <- ifelse(
                grepl("yes|true|1", merged_data[[var]], ignore.case = TRUE),
                1,
                ifelse(grepl("no|false|0", merged_data[[var]], ignore.case = TRUE), 0, NA)
            )
        }
    }
    
    # Группировка диагнозов
    # Используем существующие переменные группировки, если они есть
    
    return(merged_data)
}

# Код для создания и сохранения финального датасета
cat('
# Создаем финальный датасет
analysis_data <- create_clean_analysis_dataset(bca_data, full_data)

# Проверяем получившийся датасет
dim(analysis_data)
str(analysis_data)
summary(analysis_data)

# Сохраняем результат
saveRDS(analysis_data, "clean_analysis_data.rds")
write.csv(analysis_data, "clean_analysis_data.csv", row.names = FALSE)
')
```

### 7. ФИНАЛЬНЫЕ РЕКОМЕНДАЦИИ ПО АНАЛИЗУ
После создания очищенного датасета, рекомендуем следующие шаги для анализа:

```r
# Рекомендации для дальнейшего анализа
cat("
1. ОПИСАТЕЛЬНЫЙ АНАЛИЗ:
   - Таблица демографических характеристик
   - Распределение непрерывных переменных (гистограммы, boxplots)
   - Частоты категориальных переменных (bar plots)

2. АНАЛИЗ КОРРЕЛЯЦИЙ:
   - Корреляции между BCA переменными и клиническими параметрами
   - Корреляционная матрица всех числовых переменных

3. АНАЛИЗ ВЫЖИВАЕМОСТИ:
   - Кривые Каплана-Мейера для разных групп
   - Cox пропорциональные модели риска с включением BCA переменных

4. МОДЕЛИ ПРОГНОЗИРОВАНИЯ:
   - Логистическая регрессия для бинарных исходов
   - Регрессия Кокса для времени до события
   - Рассмотреть использование машинного обучения для предсказания исходов

5. ВИЗУАЛИЗАЦИЯ РЕЗУЛЬТАТОВ:
   - Forest plots для коэффициентов моделей
   - ROC кривые для оценки качества моделей
   - Калибровочные графики
")
```

## ОГРАНИЧЕНИЯ
- Не выполняй визуализации, если данные не предоставлены.
- Не пропускай шаги анализа - все этапы должны быть выполнены.
- Не делай предположений о распределениях данных без фактических доказательств.
- Если сомневаешься в значении переменной, укажи это явно.
- Чётко разделяй анализ bca_data и full_data, указывая источник.

## ФОРМАТ ВЫВОДА
Представь результаты анализа в виде структурированного отчета, следуя этим шагам:

1. Сначала покажи основные характеристики датасетов (размеры, пересечения).
2. Для каждой группы переменных детально опиши их характеристики.
3. Предложи конкретную стратегию объединения датасетов и обработки переменных.
4. Предоставь готовый код для создания финального аналитического датасета.
5. Для каждой категориальной переменной предложи точную схему преобразования в факторы.
```