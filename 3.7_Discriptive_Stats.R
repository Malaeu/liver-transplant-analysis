# analyze_transplant_data.R
# Скрипт для анализа данных трансплантации печени
# НЕ СОХРАНЯЕТ файлы, просто отображает в консоли

library(tidyverse)
library(survival)
library(survminer)
library(gtsummary)

# Загружаем данные (проверяем, существуют ли они в памяти)
if (!exists("analysis_data")) {
  # Пытаемся загрузить из файлов
  if (file.exists("clean_liver_transplant_data.rds")) {
    analysis_data <- readRDS("clean_liver_transplant_data.rds")
    message("Загружен основной датасет из файла")
  } else {
    stop("Основной датасет не найден. Сначала запустите скрипт предобработки данных.")
  }
}

if (!exists("bca_subset")) {
  # Пытаемся загрузить из файлов
  if (file.exists("clean_liver_transplant_data_bca_complete.rds")) {
    bca_subset <- readRDS("clean_liver_transplant_data_bca_complete.rds")
    message("Загружен BCA датасет из файла")
  } else {
    # Если файла нет, но есть анализ данные с has_bca_data, создаем подмножество
    if (exists("analysis_data") && "has_bca_data" %in% names(analysis_data)) {
      bca_subset <- analysis_data[analysis_data$has_bca_data, ]
      message("Создано BCA подмножество из основного датасета")
    } else {
      warning("BCA подмножество не найдено и не может быть создано")
    }
  }
}

# Функция для печати структуры данных
print_data_structure <- function(data, title) {
  cat("\n==================================================\n")
  cat(title, "\n")
  cat("==================================================\n")
  cat("Размер:", nrow(data), "строк,", ncol(data), "переменных\n\n")
  
  # Печатаем имена всех переменных, разделенные по группам
  # Числовые переменные
  num_vars <- names(data)[sapply(data, is.numeric)]
  cat("ЧИСЛОВЫЕ ПЕРЕМЕННЫЕ (", length(num_vars), "):\n")
  cat(paste(num_vars, collapse=", "), "\n\n")
  
  # Факторные переменные
  fac_vars <- names(data)[sapply(data, is.factor)]
  cat("ФАКТОРНЫЕ ПЕРЕМЕННЫЕ (", length(fac_vars), "):\n")
  cat(paste(fac_vars, collapse=", "), "\n\n")
  
  # Другие переменные
  other_vars <- setdiff(names(data), c(num_vars, fac_vars))
  if (length(other_vars) > 0) {
    cat("ДРУГИЕ ПЕРЕМЕННЫЕ (", length(other_vars), "):\n")
    cat(paste(other_vars, collapse=", "), "\n\n")
  }
  
  # Печатаем базовую статистику по числовым переменным
  if (length(num_vars) > 0) {
    cat("БАЗОВАЯ СТАТИСТИКА ПО ЧИСЛОВЫМ ПЕРЕМЕННЫМ:\n")
    
    # Выбираем до 6 ключевых числовых переменных для краткости
    key_vars <- intersect(c("age", "bmi", "lab_meld", "waitlist_time_months", "muscle", "sat", "vat"), num_vars)
    if (length(key_vars) > 0) {
      for (var in key_vars) {
        cat("  ", var, ": ", sep="")
        cat("мин =", min(data[[var]], na.rm=TRUE), ", ")
        cat("макс =", max(data[[var]], na.rm=TRUE), ", ")
        cat("сред =", round(mean(data[[var]], na.rm=TRUE), 2), ", ")
        cat("медиана =", round(median(data[[var]], na.rm=TRUE), 2), ", ")
        cat("NA =", sum(is.na(data[[var]])), "\n")
      }
    }
  }
  
  # Печатаем частоты для ключевых факторных переменных
  key_factors <- intersect(c("sex", "status", "meld_cat", "diagnosis_group"), fac_vars)
  if (length(key_factors) > 0) {
    cat("\nЧАСТОТЫ ДЛЯ КЛЮЧЕВЫХ ФАКТОРНЫХ ПЕРЕМЕННЫХ:\n")
    for (var in key_factors) {
      cat("  ", var, ":\n", sep="")
      print(table(data[[var]], useNA="ifany"))
      cat("\n")
    }
  }
}

# 1. СТРУКТУРА ДАННЫХ
# ---------------------
print_data_structure(analysis_data, "ОСНОВНОЙ ДАТАСЕТ")
if (exists("bca_subset")) {
  print_data_structure(bca_subset, "BCA ПОДМНОЖЕСТВО")
}

# 2. СРАВНЕНИЕ ДАТАСЕТОВ
# ---------------------
if (exists("bca_subset")) {
  cat("\n==================================================\n")
  cat("СРАВНЕНИЕ ОСНОВНОГО ДАТАСЕТА И BCA ПОДМНОЖЕСТВА\n")
  cat("==================================================\n")
  
  # Создаем функцию для расчета статистики
  calc_stats <- function(data, name) {
    # Проверяем наличие ключевых переменных
    stats <- data.frame(Dataset = name)
    
    # Добавляем основные демографические и клинические показатели
    if ("age" %in% names(data)) {
      stats$Age_mean <- round(mean(data$age, na.rm=TRUE), 2)
      stats$Age_sd <- round(sd(data$age, na.rm=TRUE), 2)
    }
    
    if ("sex" %in% names(data)) {
      stats$Male_percent <- round(100 * mean(data$sex == "M", na.rm=TRUE), 2)
    }
    
    if ("bmi" %in% names(data)) {
      stats$BMI_mean <- round(mean(data$bmi, na.rm=TRUE), 2)
      stats$BMI_sd <- round(sd(data$bmi, na.rm=TRUE), 2)
    }
    
    if ("lab_meld" %in% names(data)) {
      stats$MELD_mean <- round(mean(data$lab_meld, na.rm=TRUE), 2)
      stats$MELD_sd <- round(sd(data$lab_meld, na.rm=TRUE), 2)
    }
    
    if ("status" %in% names(data)) {
      stats$Mortality_percent <- round(100 * mean(data$status == 1, na.rm=TRUE), 2)
    }
    
    return(stats)
  }
  
  # Рассчитываем статистику для обоих датасетов
  full_stats <- calc_stats(analysis_data, paste0("Полный датасет (n=", nrow(analysis_data), ")"))
  bca_stats <- calc_stats(bca_subset, paste0("BCA подмножество (n=", nrow(bca_subset), ")"))
  
  # Объединяем и выводим
  comparison <- rbind(full_stats, bca_stats)
  print(comparison)
  
  # Тестируем на значимость различий
  cat("\nТЕСТЫ НА ЗНАЧИМОСТЬ РАЗЛИЧИЙ МЕЖДУ ДАТАСЕТАМИ:\n")
  
  # Возраст
  if (all(c("age") %in% names(analysis_data)) && all(c("age") %in% names(bca_subset))) {
    age_test <- t.test(analysis_data$age, bca_subset$age)
    cat("Возраст (t-тест): p =", round(age_test$p.value, 4), "\n")
  }
  
  # Пол (хи-квадрат)
  if (all(c("sex") %in% names(analysis_data)) && all(c("sex") %in% names(bca_subset))) {
    # Создаем таблицу сопряженности
    sex_table <- table(
      c(rep("Full", nrow(analysis_data)), rep("BCA", nrow(bca_subset))),
      c(as.character(analysis_data$sex), as.character(bca_subset$sex))
    )
    sex_test <- chisq.test(sex_table)
    cat("Пол (хи-квадрат): p =", round(sex_test$p.value, 4), "\n")
  }
  
  # MELD
  if (all(c("lab_meld") %in% names(analysis_data)) && all(c("lab_meld") %in% names(bca_subset))) {
    meld_test <- t.test(analysis_data$lab_meld, bca_subset$lab_meld)
    cat("MELD (t-тест): p =", round(meld_test$p.value, 4), "\n")
  }
  
  # Смертность (хи-квадрат)
  if (all(c("status") %in% names(analysis_data)) && all(c("status") %in% names(bca_subset))) {
    # Создаем таблицу сопряженности
    status_table <- table(
      c(rep("Full", nrow(analysis_data)), rep("BCA", nrow(bca_subset))),
      c(analysis_data$status, bca_subset$status)
    )
    status_test <- chisq.test(status_table)
    cat("Смертность (хи-квадрат): p =", round(status_test$p.value, 4), "\n")
  }
}

# 3. АНАЛИЗ РАСПРЕДЕЛЕНИЯ ПО ГОДАМ
# ---------------------
if ("wl_year" %in% names(analysis_data)) {
  cat("\n==================================================\n")
  cat("РАСПРЕДЕЛЕНИЕ ПАЦИЕНТОВ ПО ГОДАМ\n")
  cat("==================================================\n")
  
  # Создаем сводку по годам
  year_summary <- analysis_data %>%
    filter(!is.na(wl_year)) %>%
    group_by(wl_year) %>%
    summarise(
      total_patients = n(),
      deceased = sum(status == 1, na.rm = TRUE),
      percent_deceased = round(100 * sum(status == 1, na.rm = TRUE) / n(), 1)
    ) %>%
    arrange(wl_year)
  
  print(year_summary)
  
  # Выводим график (без сохранения)
  year_plot <- ggplot(year_summary, aes(x = factor(wl_year))) +
    geom_bar(aes(y = total_patients, fill = "Всего пациентов"), stat = "identity") +
    geom_bar(aes(y = deceased, fill = "Умерло"), stat = "identity") +
    geom_text(aes(y = total_patients, label = total_patients), vjust = -0.5, size = 3) +
    geom_text(aes(y = deceased, label = paste0(deceased, " (", percent_deceased, "%)")), 
              vjust = 1.5, color = "white", size = 3) +
    labs(title = "Количество пациентов в листе ожидания по годам",
         x = "Год", y = "Количество пациентов") +
    scale_fill_manual(values = c("Всего пациентов" = "#3498db", "Умерло" = "#e74c3c"),
                     name = "Статус") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(year_plot)
}

# 4. АНАЛИЗ MELD SCORE
# ---------------------
if ("lab_meld" %in% names(analysis_data)) {
  cat("\n==================================================\n")
  cat("АНАЛИЗ MELD SCORE\n")
  cat("==================================================\n")
  
  cat("Статистика MELD Score:\n")
  meld_stats <- summary(analysis_data$lab_meld)
  print(meld_stats)
  
  # Гистограмма MELD
  meld_hist <- ggplot(analysis_data, aes(x = lab_meld)) +
    geom_histogram(bins = 20, fill = "#3498db", color = "white") +
    labs(title = "Распределение значений MELD",
         x = "MELD Score", y = "Количество пациентов") +
    theme_minimal()
  
  print(meld_hist)
  
  # Распределение по категориям MELD
  if ("meld_cat" %in% names(analysis_data)) {
    cat("\nРаспределение по категориям MELD:\n")
    meld_cat_table <- table(analysis_data$meld_cat, useNA = "ifany")
    print(meld_cat_table)
    
    meld_cat_plot <- ggplot(analysis_data, aes(x = meld_cat, fill = meld_cat)) +
      geom_bar() +
      geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
      labs(title = "Распределение пациентов по категориям MELD",
           x = "Категория MELD", y = "Количество пациентов") +
      theme_minimal() +
      theme(legend.position = "none")
    
    print(meld_cat_plot)
    
    # Анализ смертности по категориям MELD
    cat("\nСмертность по категориям MELD:\n")
    meld_mortality <- analysis_data %>%
      group_by(meld_cat) %>%
      summarise(
        n = n(),
        deaths = sum(status == 1, na.rm = TRUE),
        mortality_pct = round(100 * sum(status == 1, na.rm = TRUE) / n(), 1)
      )
    
    print(meld_mortality)
    
    meld_mort_plot <- ggplot(meld_mortality, aes(x = meld_cat, y = mortality_pct, fill = meld_cat)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(mortality_pct, "%\n(", deaths, "/", n, ")")), 
                vjust = 1.5, color = "white") +
      labs(title = "Смертность по категориям MELD",
           x = "Категория MELD", y = "Смертность (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    print(meld_mort_plot)
  }
}

# 5. АНАЛИЗ ВЫЖИВАЕМОСТИ КАПЛАНА-МЕЙЕРА
# ---------------------
if (all(c("status", "waitlist_time_months") %in% names(analysis_data))) {
  cat("\n==================================================\n")
  cat("АНАЛИЗ ВЫЖИВАЕМОСТИ КАПЛАНА-МЕЙЕРА\n")
  cat("==================================================\n")
  
  # Создаем объект survival
  surv_obj <- Surv(time = analysis_data$waitlist_time_months, event = analysis_data$status)
  
  # Строим кривую Каплана-Мейера для всей группы
  km_fit <- survfit(surv_obj ~ 1, data = analysis_data)
  
  cat("Общая кривая выживаемости Каплана-Мейера:\n")
  print(km_fit)
  
  # Таблица выживаемости по времени
  survival_table <- summary(km_fit, times = c(0, 3, 6, 12, 24, 36, 48, 60))
  cat("\nВыживаемость в ключевых временных точках:\n")
  surv_summary <- data.frame(
    "Время (мес)" = survival_table$time,
    "Риск" = survival_table$n.risk,
    "События" = survival_table$n.event,
    "Выживаемость (%)" = round(survival_table$surv * 100, 1),
    "Нижняя граница 95% CI" = round(survival_table$lower * 100, 1),
    "Верхняя граница 95% CI" = round(survival_table$upper * 100, 1)
  )
  print(surv_summary)
  
  # Визуализация
  km_plot <- ggsurvplot(km_fit,
             data = analysis_data,
             risk.table = TRUE,
             pval = FALSE,
             conf.int = TRUE,
             title = "Кривая выживаемости Каплана-Мейера",
             xlab = "Время (месяцы)",
             ylab = "Вероятность выживания",
             palette = "#2980b9",
             risk.table.height = 0.25,
             ggtheme = theme_minimal())
  
  print(km_plot)
  
  # Стратификация по категориям MELD (если есть)
  if ("meld_cat" %in% names(analysis_data)) {
    cat("\nКривые выживаемости по категориям MELD:\n")
    km_fit_meld <- survfit(surv_obj ~ meld_cat, data = analysis_data)
    print(km_fit_meld)
    
    # Лог-ранг тест для сравнения кривых
    log_rank_test <- survdiff(surv_obj ~ meld_cat, data = analysis_data)
    cat("\nЛог-ранг тест для сравнения кривых:\n")
    print(log_rank_test)
    
    # Таблица выживаемости по группам MELD
    cat("\nВыживаемость по группам MELD через 12 месяцев:\n")
    meld_surv_table <- summary(km_fit_meld, times = 12)
    meld_summary <- data.frame(
      "Группа MELD" = levels(analysis_data$meld_cat),
      "Риск" = meld_surv_table$n.risk,
      "События" = meld_surv_table$n.event,
      "Выживаемость (%)" = round(meld_surv_table$surv * 100, 1)
    )
    print(meld_summary)
    
    km_meld_plot <- ggsurvplot(km_fit_meld,
               data = analysis_data,
               risk.table = TRUE,
               pval = TRUE,
               conf.int = FALSE,
               title = "Кривые выживаемости по категориям MELD",
               xlab = "Время (месяцы)",
               ylab = "Вероятность выживания",
               risk.table.height = 0.25,
               ggtheme = theme_minimal())
    
    print(km_meld_plot)
  }
  
  # Стратификация по полу
  if ("sex" %in% names(analysis_data)) {
    cat("\nКривые выживаемости по полу:\n")
    km_fit_sex <- survfit(surv_obj ~ sex, data = analysis_data)
    print(km_fit_sex)
    
    # Лог-ранг тест для сравнения кривых
    log_rank_test_sex <- survdiff(surv_obj ~ sex, data = analysis_data)
    cat("\nЛог-ранг тест для сравнения кривых по полу:\n")
    print(log_rank_test_sex)
    
    km_sex_plot <- ggsurvplot(km_fit_sex,
               data = analysis_data,
               risk.table = TRUE,
               pval = TRUE,
               conf.int = FALSE,
               title = "Кривые выживаемости по полу",
               xlab = "Время (месяцы)",
               ylab = "Вероятность выживания",
               palette = c("#E41A1C", "#377EB8"),
               risk.table.height = 0.25,
               ggtheme = theme_minimal())
    
    print(km_sex_plot)
  }
}

# 6. КОРРЕЛЯЦИОННЫЙ АНАЛИЗ (ДЛЯ BCA ПОДМНОЖЕСТВА)
# ---------------------
if (exists("bca_subset")) {
  cat("\n==================================================\n")
  cat("КОРРЕЛЯЦИОННЫЙ АНАЛИЗ BCA ПЕРЕМЕННЫХ\n")
  cat("==================================================\n")
  
  # Выбираем ключевые переменные для корреляции
  bca_vars <- c("muscle", "sat", "vat", "imat", "eat", "pat", "tat")
  clinical_vars <- c("age", "bmi", "lab_meld", "waitlist_time_months")
  
  # Проверяем наличие переменных
  bca_vars_present <- intersect(bca_vars, names(bca_subset))
  clinical_vars_present <- intersect(clinical_vars, names(bca_subset))
  
  if (length(bca_vars_present) > 0 && length(clinical_vars_present) > 0) {
    # Создаем корреляционную матрицу
    corr_vars <- c(bca_vars_present, clinical_vars_present)
    corr_data <- bca_subset[, corr_vars]
    
    # Удаляем строки с NA
    corr_data <- na.omit(corr_data)
    
    if (nrow(corr_data) > 5) {  # Проверяем, что достаточно данных
      cat("Корреляционная матрица (коэффициенты Пирсона):\n")
      corr_matrix <- round(cor(corr_data, method = "pearson"), 3)
      print(corr_matrix)
      
      # Визуализация корреляционной матрицы
      # Преобразуем в длинный формат для ggplot
      corr_long <- as.data.frame(as.table(corr_matrix))
      names(corr_long) <- c("Var1", "Var2", "Correlation")
      
      corr_plot <- ggplot(corr_long, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "#E41A1C", mid = "white", high = "#377EB8", 
                            midpoint = 0, limits = c(-1, 1)) +
        geom_text(aes(label = round(Correlation, 2)), size = 3) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = "Корреляционная матрица BCA и клинических переменных")
      
      print(corr_plot)
      
      # Тесты на значимость корреляций
      cat("\nЗначимые корреляции (p < 0.05):\n")
      
      for (i in 1:length(bca_vars_present)) {
        for (j in 1:length(clinical_vars_present)) {
          bca_var <- bca_vars_present[i]
          clinical_var <- clinical_vars_present[j]
          
          # Проверяем корреляцию
          corr_test <- cor.test(bca_subset[[bca_var]], bca_subset[[clinical_var]], 
                               method = "pearson", use = "complete.obs")
          
          if (corr_test$p.value < 0.05) {
            cat(sprintf("%s и %s: r = %.3f, p = %.4f\n", 
                        bca_var, clinical_var, corr_test$estimate, corr_test$p.value))
          }
        }
      }
    }
  }
}

# 7. ПРЕДВАРИТЕЛЬНАЯ МОДЕЛЬ ПРОПОРЦИОНАЛЬНЫХ РИСКОВ КОКСА
# ---------------------
if (all(c("status", "waitlist_time_months") %in% names(analysis_data))) {
  cat("\n==================================================\n")
  cat("МОДЕЛЬ ПРОПОРЦИОНАЛЬНЫХ РИСКОВ КОКСА\n")
  cat("==================================================\n")
  
  # Основная модель для полного датасета
  cat("Модель для полного датасета:\n")
  
  # Выбираем переменные, которые доступны и могут быть предикторами
  predictors <- intersect(c("age", "sex", "bmi", "lab_meld"), names(analysis_data))
  
  if (length(predictors) > 0) {
    # Строим формулу для модели
    formula_str <- paste("Surv(waitlist_time_months, status) ~", paste(predictors, collapse = " + "))
    cox_formula <- as.formula(formula_str)
    
    # Создаем модель
    cox_model <- coxph(cox_formula, data = analysis_data)
    
    # Выводим резюме модели
    cox_summary <- summary(cox_model)
    print(cox_summary)
    
    # Отношения рисков с 95% CI
    cat("\nОтношения рисков (HR) с 95% доверительными интервалами:\n")
    hr_table <- data.frame(
      "Переменная" = names(cox_summary$coefficients[, 1]),
      "HR" = round(exp(cox_summary$coefficients[, 1]), 2),
      "95% CI нижняя" = round(exp(cox_summary$coefficients[, 1] - 1.96 * cox_summary$coefficients[, 3]), 2),
      "95% CI верхняя" = round(exp(cox_summary$coefficients[, 1] + 1.96 * cox_summary$coefficients[, 3]), 2),
      "p-value" = round(cox_summary$coefficients[, 5], 4)
    )
    print(hr_table)
    
    # Визуализация отношений рисков
    hr_plot <- ggplot(hr_table, aes(x = reorder(Переменная, HR), y = HR)) +
      geom_point(size = 3) +
      geom_errorbar(aes(ymin = `95% CI нижняя`, ymax = `95% CI верхняя`), width = 0.2) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
      coord_flip() +
      labs(title = "Отношения рисков (HR) с 95% CI",
           x = "", y = "Отношение рисков (HR)") +
      theme_minimal()
    
    print(hr_plot)
    
    # Проверка допущения о пропорциональных рисках
    cat("\nПроверка допущения о пропорциональных рисках:\n")
    ph_test <- cox.zph(cox_model)
    print(ph_test)
  } else {
    cat("Недостаточно предикторов для построения модели Кокса\n")
  }
  
  # Если есть BCA подмножество, строим отдельную модель
  if (exists("bca_subset") && all(c("status", "waitlist_time_months") %in% names(bca_subset))) {
    cat("\nМодель для BCA подмножества:\n")
    
    # Выбираем переменные для BCA модели
    bca_predictors <- intersect(
      c("age", "sex", "bmi", "lab_meld", "muscle", "sat", "vat", "imat"), 
      names(bca_subset)
    )
    
    if (length(bca_predictors) > 0) {
      # Строим формулу для модели
      bca_formula_str <- paste("Surv(waitlist_time_months, status) ~", paste(bca_predictors, collapse = " + "))
      bca_cox_formula <- as.formula(bca_formula_str)
      
      # Создаем модель
      bca_cox_model <- coxph(bca_cox_formula, data = bca_subset)
      
      # Выводим резюме модели
      bca_cox_summary <- summary(bca_cox_model)
      print(bca_cox_summary)
      
      # Отношения рисков с 95% CI
      cat("\nОтношения рисков (HR) с 95% доверительными интервалами (BCA модель):\n")
      bca_hr_table <- data.frame(
        "Переменная" = names(bca_cox_summary$coefficients[, 1]),
        "HR" = round(exp(bca_cox_summary$coefficients[, 1]), 2),
        "95% CI нижняя" = round(exp(bca_cox_summary$coefficients[, 1] - 1.96 * bca_cox_summary$coefficients[, 3]), 2),
        "95% CI верхняя" = round(exp(bca_cox_summary$coefficients[, 1] + 1.96 * bca_cox_summary$coefficients[, 3]), 2),
        "p-value" = round(bca_cox_summary$coefficients[, 5], 4)
      )
      print(bca_hr_table)
    } else {
      cat("Недостаточно предикторов для построения BCA модели Кокса\n")
    }
  }
}

cat("\nАнализ завершен! Все результаты выведены в консоль.\n")
