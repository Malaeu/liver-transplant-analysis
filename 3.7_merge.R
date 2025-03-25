# create_analysis_dataset.R
# Скрипт для создания чистого датасета для анализа трансплантационных данных

library(tidyverse)
library(lubridate)

# 1. Загрузка данных
load_data <- function(full_data_path, bca_data_path) {
  message("Загружаем данные...")
  
  # Загрузка основного датасета
  full_data <- read.csv(full_data_path, stringsAsFactors = FALSE)
  message(paste0("Основной датасет: ", nrow(full_data), " строк, ", ncol(full_data), " переменных"))
  
  # Загрузка BCA-датасета (данные состава тела)
  bca_data <- readRDS(bca_data_path)
  message(paste0("BCA датасет: ", nrow(bca_data), " строк, ", ncol(bca_data), " переменных"))
  
  return(list(full_data = full_data, bca_data = bca_data))
}

# 2. Объединение датасетов
merge_datasets <- function(full_data, bca_data) {
  message("Объединяем датасеты...")
  
  # Проверяем наличие ID-полей в обоих датасетах
  has_etnr_id_full <- "etnr_id" %in% names(full_data)
  has_etnr_id_bca <- "etnr_id" %in% names(bca_data)
  has_etnr_bca <- "etnr" %in% names(bca_data)
  
  message(paste0("ID в full_data: ", ifelse(has_etnr_id_full, "etnr_id", "отсутствует")))
  message(paste0("ID в bca_data: ", 
                ifelse(has_etnr_id_bca, "etnr_id", 
                      ifelse(has_etnr_bca, "etnr", "отсутствует"))))
  
  # Преобразуем имена ID-полей для объединения
  if (has_etnr_id_full && has_etnr_bca && !has_etnr_id_bca) {
    message("Переименовываем etnr → etnr_id в BCA датасете")
    bca_data <- bca_data %>% rename(etnr_id = etnr)
    join_col <- "etnr_id"
  } else if (has_etnr_id_full && has_etnr_id_bca) {
    message("Используем etnr_id для объединения")
    join_col <- "etnr_id"
  } else {
    stop("Не удалось найти общие ID-поля для объединения датасетов")
  }
  
  # Объединяем датасеты
  message(paste0("Объединяем по полю: ", join_col))
  merged_data <- full_data %>%
    full_join(bca_data, by = join_col, suffix = c(".full", ".bca"))
  
  message(paste0("Объединенный датасет: ", nrow(merged_data), " строк, ", ncol(merged_data), " переменных"))
  
  return(merged_data)
}

# 3. Исключаем ненужные переменные
exclude_variables <- function(data) {
  message("Исключаем ненужные переменные...")
  
  # Переменные для исключения (ОБНОВЛЕНО - добавлены персональные данные с суффиксами)
  exclude_vars <- c(
    # ID и персональные данные - ОБНОВЛЕНО
    "etnr_id", "lebensfallnr", "name", "vorname", "acsn_nr", "tx.nr",
    "name.full", "vorname.full", "name.bca", "vorname.bca",
    
    # Даты - обрабатываются отдельно
    "birth_date", "relisting_date", "re.list_datum", "death_date", 
    "last_contact", "letzter_kontakt", "date_of_ltx", "date_of_ct",
    "waitlist_date", "date_of_birth", "date_of_wl", "wl_start", "event_date",
    "todesdatum", # дата смерти на немецком
    
    # Диагностические тексты
    "diagnosis_1", "diagnose_2", "diagnose_3", "diagnose", "diagnosis",
    "relisting_diagnosis", "diagnose_relistung", "diagnosis_to_clean",
    
    # Дополнительные флаги и неактуальные показатели
    "relisting_flag", "relist", "ltx_number", "adults", "source",
    "exc_meld", "ped_meld", "study", "studie", "za_wl", "za.wl",
    "notes", "bemerkungen", "received_ltx", "time_to_ltx",
    "abdominal_surgeries", "abdominelle_opâ.s", "infektionen",
    "infections", "pfortader", "extend_alloc_on_wl",
    
    # Добавляем суффиксы для exc_meld и ped_meld
    "exc_meld.full", "exc_meld.bca", "ped_meld.full", "ped_meld.bca"
  )
  
  # Исключаем переменные
  exclude_cols <- intersect(names(data), exclude_vars)
  data <- data %>% select(-all_of(exclude_cols))
  message(paste0("Исключено ", length(exclude_cols), " переменных"))
  
  return(data)
}

# 4. Объединение дублирующихся переменных
consolidate_variables <- function(data) {
  message("Объединяем дублирующиеся переменные...")
  
  # Находим все переменные с суффиксами .full и .bca
  full_vars <- grep("\\.full$", names(data), value = TRUE)
  bca_vars <- grep("\\.bca$", names(data), value = TRUE)
  
  # Извлекаем базовые имена
  base_names_full <- sub("\\.full$", "", full_vars)
  base_names_bca <- sub("\\.bca$", "", bca_vars)
  
  # Находим общие базовые имена (для переменных, которые есть в обоих датасетах)
  common_base_names <- intersect(base_names_full, base_names_bca)
  
  # Обрабатываем каждую пару дублирующихся переменных
  for (base_name in common_base_names) {
    full_var <- paste0(base_name, ".full")
    bca_var <- paste0(base_name, ".bca")
    
    message(paste0("Объединяем ", full_var, " и ", bca_var))
    
    # Создаем новую переменную с базовым именем
    # Предпочитаем значения из full_data, но если там NA, используем bca_data
    data[[base_name]] <- ifelse(is.na(data[[full_var]]), data[[bca_var]], data[[full_var]])
    
    # Удаляем исходные переменные
    data <- data %>% select(-c(full_var, bca_var))
  }
  
  # Переименовываем оставшиеся переменные с суффиксами
  remaining_full <- grep("\\.full$", names(data), value = TRUE)
  for (var in remaining_full) {
    base_name <- sub("\\.full$", "", var)
    data <- data %>% rename(!!base_name := !!var)
  }
  
  remaining_bca <- grep("\\.bca$", names(data), value = TRUE)
  for (var in remaining_bca) {
    base_name <- sub("\\.bca$", "", var)
    data <- data %>% rename(!!base_name := !!var)
  }
  
  message(paste0("После объединения: ", ncol(data), " переменных"))
  return(data)
}

# 5. Обработка категориальных переменных
process_categorical_vars <- function(data) {
  message("Обрабатываем категориальные переменные...")
  
  # Определяем категориальные переменные
  cat_vars <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  
  for (var in cat_vars) {
    # Получаем количество уникальных значений
    unique_vals <- unique(data[[var]])
    n_unique <- length(unique_vals)
    
    # Анализируем распределение
    val_table <- table(data[[var]], useNA = "ifany")
    
    message(paste0("Переменная: ", var, " - ", n_unique, " уникальных значений"))
    
    # Если больше 10 уровней, группируем
    if (n_unique > 10 && n_unique < 100) {  # не трогаем очень большие категориальные переменные
      message(paste0("  Группируем переменную ", var, " (> 10 уровней)"))
      
      # Группировка в зависимости от типа переменной
      if (grepl("diagnosis|disease", var)) {
        # Для диагнозов используем более осмысленную группировку
        # ОБНОВЛЕНО: используем существующие категории из diagnosis_group, если возможно
        if ("diagnosis_group" %in% names(data) && !var %in% c("diagnosis_group", "diagnosis_subgroup")) {
          message(paste0("  Используем diagnosis_group для группировки ", var))
          # Создаем таблицу соответствия
          diagnosis_mapping <- data %>%
            filter(!is.na(!!sym(var)) & !is.na(diagnosis_group)) %>%
            distinct(!!sym(var), diagnosis_group) %>%
            group_by(!!sym(var)) %>%
            summarise(diagnosis_group = first(diagnosis_group))
          
          # Применяем маппинг
          for (i in 1:nrow(diagnosis_mapping)) {
            diag_val <- diagnosis_mapping[[var]][i]
            diag_group <- diagnosis_mapping$diagnosis_group[i]
            data[[var]][data[[var]] == diag_val] <- diag_group
          }
        } else {
          # Топ-7 самых частых значений
          top_values <- names(sort(val_table, decreasing = TRUE)[1:min(7, length(val_table))])
          data[[var]] <- ifelse(data[[var]] %in% top_values, data[[var]], "Other")
        }
      } else if (grepl("nationality|race", var, ignore.case = TRUE)) {
        # Для национальностей группируем в региональные группы
        # Топ-5 самых частых + остальные в "Other"
        top_values <- names(sort(val_table, decreasing = TRUE)[1:min(5, length(val_table))])
        data[[var]] <- ifelse(data[[var]] %in% top_values, data[[var]], "Other")
      } else {
        # Для остальных категориальных переменных - топ-10 + "Other"
        top_values <- names(sort(val_table, decreasing = TRUE)[1:min(10, length(val_table))])
        data[[var]] <- ifelse(data[[var]] %in% top_values, data[[var]], "Other")
      }
    }
    
    # Переводим в фактор
    data[[var]] <- factor(data[[var]])
  }
  
  # Специфическая обработка важных категориальных переменных
  
  # Sex - приводим к бинарному M/F
  if ("sex" %in% names(data)) {
    message("Стандартизируем переменную sex")
    data$sex <- toupper(as.character(data$sex))
    data$sex <- ifelse(data$sex %in% c("M", "MALE", "MÄNNLICH"), "M",
                       ifelse(data$sex %in% c("F", "W", "FEMALE", "WEIBLICH"), "F", NA))
    data$sex <- factor(data$sex)
  }
  
  # Status - приводим к бинарному 0/1
  if ("status" %in% names(data)) {
    message("Преобразуем переменную status в бинарную")
    # Проверяем текущие значения
    if (is.character(data$status) || is.factor(data$status)) {
      data$status <- ifelse(
        grepl("deceased|death|dead|died|1", tolower(as.character(data$status))),
        1, 0
      )
    }
  }
  
  # Бинарные переменные с префиксом has_ или portal_ приводим к 0/1
  binary_vars <- grep("^(has_|portal_)", names(data), value = TRUE)
  for (var in binary_vars) {
    if (is.character(data[[var]]) || is.factor(data[[var]])) {
      message(paste0("Преобразуем бинарную переменную ", var))
      data[[var]] <- ifelse(
        grepl("yes|true|1|y", tolower(as.character(data[[var]]))),
        1,
        ifelse(grepl("no|false|0|n", tolower(as.character(data[[var]]))), 0, NA)
      )
    }
  }
  
  return(data)
}

# 6. Обработка пропущенных значений - ОБНОВЛЕНО для BCA переменных
handle_missing_values <- function(data) {
  message("Обрабатываем пропущенные значения...")
  
  # Анализ пропущенных значений
  na_counts <- sapply(data, function(x) sum(is.na(x)))
  na_percent <- round(na_counts / nrow(data) * 100, 2)
  
  # Создаем датафрейм с информацией о пропущенных значениях
  na_df <- data.frame(
    Variable = names(na_counts),
    Missing_Count = na_counts,
    Missing_Percent = na_percent
  ) %>% arrange(desc(Missing_Percent))
  
  # Выводим информацию о пропущенных значениях
  vars_with_na <- na_df %>% filter(Missing_Count > 0)
  if (nrow(vars_with_na) > 0) {
    message("Переменные с пропущенными значениями:")
    print(vars_with_na)
    
    # ОБНОВЛЕНО: сохраняем BCA переменные даже с высоким % пропусков
    bca_vars <- c("muscle", "sat", "vat", "imat", "eat", "pat", "tat", "bone")
    
    # Удаляем переменные с >50% пропущенных значений, КРОМЕ BCA переменных
    vars_to_drop <- vars_with_na %>% 
      filter(Missing_Percent > 50) %>%
      filter(!Variable %in% bca_vars) %>%
      pull(Variable)
    
    if (length(vars_to_drop) > 0) {
      message(paste0("Удаляем ", length(vars_to_drop), " переменных с >50% пропущенных значений (кроме BCA переменных)"))
      data <- data %>% select(-all_of(vars_to_drop))
    }
    
    # Для переменных с 10-50% пропущенных значений - простая импутация
    vars_to_impute <- vars_with_na %>% 
      filter(Missing_Percent <= 50 & Missing_Percent > 10) %>%
      filter(!Variable %in% bca_vars) %>% # Не импутируем BCA переменные
      pull(Variable)
    
    if (length(vars_to_impute) > 0) {
      message(paste0("Импутируем значения для ", length(vars_to_impute), " переменных с 10-50% пропущенных значений"))
      
      for (var in vars_to_impute) {
        if (is.numeric(data[[var]])) {
          # Для числовых - медиана
          median_val <- median(data[[var]], na.rm = TRUE)
          data[[var]][is.na(data[[var]])] <- median_val
        } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
          # Для факторов - мода
          mode_val <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
          data[[var]][is.na(data[[var]])] <- mode_val
        }
      }
    }
    
    # Для переменных с <10% пропущенных значений - импутация или удаление строк
    vars_with_few_na <- vars_with_na %>% 
      filter(Missing_Percent <= 10) %>%
      filter(!Variable %in% bca_vars) %>% # Не импутируем BCA переменные
      pull(Variable)
    
    if (length(vars_with_few_na) > 0) {
      message(paste0("Обрабатываем ", length(vars_with_few_na), " переменных с <10% пропущенных значений"))
      
      # Список критически важных переменных - строки удаляем, если в них пропущены эти значения
      critical_vars <- c("status", "waitlist_time_months", "lab_meld", "sex", "age")
      critical_vars <- intersect(critical_vars, names(data))
      
      if (length(critical_vars) > 0) {
        before_rows <- nrow(data)
        data <- data %>% drop_na(all_of(critical_vars))
        message(paste0("Удалено ", before_rows - nrow(data), " строк с пропусками в критических переменных"))
      }
      
      # Для остальных переменных - простая импутация
      other_vars <- setdiff(vars_with_few_na, critical_vars)
      for (var in other_vars) {
        if (is.numeric(data[[var]])) {
          # Для числовых - медиана
          median_val <- median(data[[var]], na.rm = TRUE)
          data[[var]][is.na(data[[var]])] <- median_val
        } else if (is.factor(data[[var]]) || is.character(data[[var]])) {
          # Для факторов - мода
          mode_val <- names(sort(table(data[[var]]), decreasing = TRUE))[1]
          data[[var]][is.na(data[[var]])] <- mode_val
        }
      }
    }
  } else {
    message("Пропущенных значений не обнаружено")
  }
  
  return(data)
}

# 7. Обработка выбросов - УЛУЧШЕНО
handle_outliers <- function(data) {
  message("Обрабатываем выбросы в числовых переменных...")
  
  # Обрабатываем только числовые переменные
  num_vars <- names(data)[sapply(data, is.numeric)]
  
  for (var in num_vars) {
    # Пропускаем бинарные переменные
    if (length(unique(na.omit(data[[var]]))) <= 2) {
      message(paste0("Пропускаем бинарную переменную: ", var))
      next
    }
    
    # ОБНОВЛЕНО: Используем более мягкий 3*IQR для обнаружения выбросов
    q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
    q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    
    lower_bound <- q1 - 3 * iqr  # используем 3*IQR вместо 1.5*IQR
    upper_bound <- q3 + 3 * iqr
    
    # Подсчитываем количество выбросов
    outliers <- which(data[[var]] < lower_bound | data[[var]] > upper_bound)
    
    if (length(outliers) > 0) {
      outlier_percent <- round(length(outliers) / sum(!is.na(data[[var]])) * 100, 2)
      message(paste0("Переменная ", var, ": ", length(outliers), 
                    " выбросов (", outlier_percent, "%)"))
      
      # ОБНОВЛЕНО: Используем 0.5% и 99.5% перцентили вместо 1% и 99%
      if (outlier_percent > 5) {
        p005 <- quantile(data[[var]], 0.005, na.rm = TRUE)
        p995 <- quantile(data[[var]], 0.995, na.rm = TRUE)
        
        data[[var]] <- pmin(pmax(data[[var]], p005), p995)
        message(paste0("  Винсоризация по 0.5% и 99.5% перцентилям"))
      } else {
        # Для небольшого количества выбросов - более прицельная обработка
        data[[var]][outliers] <- ifelse(
          data[[var]][outliers] < lower_bound,
          lower_bound,
          upper_bound
        )
        message(paste0("  Заменяем выбросы на границы 3*IQR"))
      }
    }
  }
  
  return(data)
}

# 8. Создание производных переменных - УЛУЧШЕНО
create_derived_variables <- function(data) {
  message("Создаем производные переменные...")
  
  # BMI категории
  if ("bmi" %in% names(data)) {
    message("Создаем категории BMI")
    data$bmi_cat <- cut(data$bmi, 
                         breaks = c(0, 18.5, 25, 30, Inf),
                         labels = c("Недостаточный", "Нормальный", "Избыточный", "Ожирение"),
                         right = FALSE)
  }
  
  # MELD категории
  if ("lab_meld" %in% names(data)) {
    message("Создаем категории MELD")
    data$meld_cat <- cut(data$lab_meld,
                          breaks = c(0, 10, 20, 30, Inf),
                          labels = c("<10", "10-19", "20-29", "≥30"),
                          right = FALSE)
  }
  
  # Возрастные категории
  if ("age" %in% names(data)) {
    message("Создаем возрастные категории")
    data$age_cat <- cut(data$age,
                         breaks = c(0, 30, 40, 50, 60, 70, Inf),
                         labels = c("<30", "30-39", "40-49", "50-59", "60-69", "≥70"),
                         right = FALSE)
  }
  
  # Нормализованные значения BCA переменных
  bca_vars <- c("muscle", "sat", "vat", "imat", "eat", "pat", "tat")
  bca_vars_present <- intersect(bca_vars, names(data))
  
  if (length(bca_vars_present) > 0) {
    message("Создаем нормализованные BCA переменные")
    
    if ("height_cm" %in% names(data) && "weight_kg" %in% names(data)) {
      # Рассчитываем BSA (площадь поверхности тела) по формуле Дюбуа
      data$bsa <- 0.007184 * (data$height_cm ^ 0.725) * (data$weight_kg ^ 0.425)
      
      # Нормализуем BCA переменные по BSA
      for (var in bca_vars_present) {
        norm_var <- paste0(var, "_norm")
        data[[norm_var]] <- data[[var]] / data$bsa
      }
    } else if ("height_cm" %in% names(data)) {
      # Нормализация по росту в квадрате (как BMI)
      height_m <- data$height_cm / 100
      
      for (var in bca_vars_present) {
        norm_var <- paste0(var, "_norm")
        data[[norm_var]] <- data[[var]] / (height_m ^ 2)
      }
    }
    
    # ОБНОВЛЕНО: создаем флаг наличия полных BCA данных
    data$has_bca_data <- rowSums(is.na(data[bca_vars_present])) == 0
    message(paste0("Пациентов с полными BCA данными: ", sum(data$has_bca_data)))
  }
  
  return(data)
}

# 9. Создание подмножества с полными BCA данными (НОВАЯ ФУНКЦИЯ)
create_bca_subset <- function(data) {
  message("Создаем подмножество с полными BCA данными...")
  
  # Определяем BCA переменные
  bca_vars <- c("muscle", "sat", "vat", "imat", "eat", "pat", "tat")
  
  # Проверяем, какие переменные присутствуют
  bca_vars_present <- intersect(bca_vars, names(data))
  
  if (length(bca_vars_present) > 0) {
    # Определяем строки с полными BCA данными
    complete_bca_rows <- complete.cases(data[, bca_vars_present])
    message(paste0("Строк с полными BCA данными: ", sum(complete_bca_rows), " из ", nrow(data)))
    
    # Создаем подмножество
    bca_subset <- data[complete_bca_rows, ]
    
    # Проверяем критические переменные
    critical_vars <- c("sex", "age", "status", "lab_meld", "waitlist_time_months")
    critical_vars_present <- intersect(critical_vars, names(bca_subset))
    
    if (length(critical_vars_present) > 0) {
      na_in_critical <- rowSums(is.na(bca_subset[critical_vars_present])) > 0
      if (any(na_in_critical)) {
        message(paste0("Удаляем ", sum(na_in_critical), " строк с пропусками в критических переменных из BCA подмножества"))
        bca_subset <- bca_subset[!na_in_critical, ]
      }
    }
    
    message(paste0("Финальное BCA подмножество: ", nrow(bca_subset), " строк, ", ncol(bca_subset), " переменных"))
    return(bca_subset)
  } else {
    message("BCA переменные отсутствуют в датасете")
    return(NULL)
  }
}

# 10. Главная функция
create_analysis_dataset <- function(full_data_path, bca_data_path, output_path) {
  # Загружаем данные
  datasets <- load_data(full_data_path, bca_data_path)
  full_data <- datasets$full_data
  bca_data <- datasets$bca_data
  
  # СНАЧАЛА объединяем датасеты, потом чистим (важно!)
  merged_data <- merge_datasets(full_data, bca_data)
  
  # ПОСЛЕ объединения исключаем ненужные переменные
  clean_data <- exclude_variables(merged_data)
  
  # НОВЫЙ ШАГ: объединяем дублирующиеся переменные
  consolidated_data <- consolidate_variables(clean_data)
  
  # Обрабатываем категориальные переменные
  cat_processed_data <- process_categorical_vars(consolidated_data)
  
  # Обрабатываем пропущенные значения
  clean_data <- handle_missing_values(cat_processed_data)
  
  # Обрабатываем выбросы
  outlier_free_data <- handle_outliers(clean_data)
  
  # Создаем производные переменные
  final_data <- create_derived_variables(outlier_free_data)
  
  # НОВЫЙ ШАГ: Создаем подмножество с полными BCA данными
  bca_subset <- create_bca_subset(final_data)
  
  # Сохраняем основной результат
  saveRDS(final_data, output_path)
  write.csv(final_data, sub("\\.rds$", ".csv", output_path), row.names = FALSE)
  
  # Сохраняем BCA подмножество, если оно существует
  if (!is.null(bca_subset)) {
    bca_output_path <- sub("\\.rds$", "_bca_complete.rds", output_path)
    saveRDS(bca_subset, bca_output_path)
    write.csv(bca_subset, sub("\\.rds$", "_bca_complete.csv", output_path), row.names = FALSE)
    message(paste0("BCA подмножество сохранено в: ", bca_output_path))
  }
  
  message(paste0("Готово! Финальный датасет: ", nrow(final_data), " строк, ", ncol(final_data), " переменных"))
  message(paste0("Данные сохранены в: ", output_path, " и ", sub("\\.rds$", ".csv", output_path)))
  
  return(list(full_data = final_data, bca_subset = bca_subset))
}

# Выполнение скрипта при запуске как основной программы
if (!interactive()) {
  # Пути к файлам
  full_data_path <- "imputed_data_full.csv"
  bca_data_path <- "wl_df_with_bca.rds"
  output_path <- "clean_liver_transplant_data.rds"
  
  # Создание аналитического датасета
  results <- create_analysis_dataset(full_data_path, bca_data_path, output_path)
  analysis_data <- results$full_data
  bca_subset <- results$bca_subset
} else {
  # Если скрипт запущен интерактивно, выводим инструкцию
  message("Скрипт загружен. Для создания датасета выполните:")
  message('results <- create_analysis_dataset("imputed_data_full.csv", "wl_df_with_bca.rds", "clean_liver_transplant_data.rds")')
  message('analysis_data <- results$full_data')
  message('bca_subset <- results$bca_subset')
}
