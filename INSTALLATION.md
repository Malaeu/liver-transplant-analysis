# Руководство по установке и запуску проекта анализа выживаемости пациентов в листе ожидания трансплантации печени

## 1. Предварительные требования

### Языки программирования и фреймворки
- **R**: версия 4.2.0 или выше
- **RStudio**: рекомендуется для удобства работы (не обязательно)

### Зависимости и библиотеки
Основные пакеты R:
- **tidyverse**: для обработки данных и визуализации
- **survival**, **survminer**: для анализа выживаемости
- **glmnet**: для регуляризованной регрессии
- **rms**: для стратегий моделирования регрессии
- **ggplot2**, **plotly**, **viridis**: для визуализации
- **aorsf**: для ускоренных наклонных случайных лесов выживаемости (Accelerated Oblique Random Survival Forests)
- **h2o**: для распределенных вычислений
- **rmarkdown**, **knitr**, **DT**, **htmlwidgets**: для создания отчетов

### Требования к системе
- **ОЗУ**: минимум 8 ГБ (16 ГБ рекомендуется для h2o)
- **Процессор**: многоядерный процессор для параллельных вычислений
- **Дисковое пространство**: минимум 2 ГБ свободного места
- **Операционная система**: Windows, macOS или Linux

## 2. Пошаговый процесс установки

### Клонирование репозитория
```bash
git clone https://github.com/username/liver-transplant-analysis.git
cd liver-transplant-analysis
```

### Установка зависимостей
Запустите R или RStudio и выполните:

```r
# Установка и загрузка необходимых пакетов
source("src/setup.R")
```

Если вы предпочитаете устанавливать пакеты вручную:

```r
# Основные пакеты
install.packages(c(
  "tidyverse",    # Обработка данных и визуализация
  "survival",     # Анализ выживаемости
  "survminer",    # Визуализация кривых выживаемости
  "glmnet",       # Регуляризованная регрессия
  "rms",          # Стратегии моделирования регрессии
  "pec",          # Кривые ошибок прогнозирования
  "timeROC",      # Зависимые от времени ROC-кривые
  "randomForestSRC", # Случайные леса выживаемости
  "ggplot2",      # Визуализация
  "viridis",      # Цветовые палитры
  "knitr",        # Создание отчетов
  "rmarkdown",    # R Markdown
  "DT",           # Интерактивные таблицы
  "plotly",       # Интерактивные графики
  "htmlwidgets"   # HTML-виджеты
))

# Установка aorsf из CRAN
install.packages("aorsf")

# Установка h2o и fastshap
install.packages(c(
  "h2o",          # Распределенные вычисления H2O
  "fastshap"      # SHAP-значения для интерпретации модели
))

# Если вы хотите установить разрабатываемую версию aorsf из GitHub:
if (!require("remotes")) install.packages("remotes")
remotes::install_github("ropensci/aorsf")

# Инициализация H2O
library(h2o)
h2o.init()
```

### Подготовка данных
Поместите файлы данных в корневой каталог проекта:
- `imputed_data_full.csv`: полный набор данных с оценками MELD и данными о выживаемости
- `wl_df_with_bca.rds`: подмножество с данными анализа состава тела (BCA)

```bash
# Создайте структуру каталогов, если она еще не существует
mkdir -p results/figures reports
```

## 3. Команды запуска

### Запуск полного анализа
В R или RStudio выполните:

```r
# Запуск полного рабочего процесса
source("main.R")
```

### Запуск отдельных компонентов
Если вы хотите запустить только определенные части анализа:

```r
# Загрузка и проверка данных
source("src/setup.R")
data_list <- load_datasets(
  full_data_path = "imputed_data_full.csv",
  bca_data_path = "wl_df_with_bca.rds",
  validate = TRUE
)

# Только разведочный анализ данных
source("src/exploratory_analysis.R")
# Код для EDA...

# Только анализ выживаемости по Каплану-Мейеру
source("src/survival_analysis.R")
# Код для анализа выживаемости...

# Только формула BC-MELD
source("src/bc_meld_formula.R")
# Код для формулы BC-MELD...

# Только модели машинного обучения
source("src/ml_models.R")
# Код для моделей ML...
```

### Пример использования aorsf для анализа выживаемости

```r
# Загрузка библиотек
library(aorsf)
library(tidyverse)
library(survival)

# Подготовка данных
data <- readRDS("wl_df_with_bca.rds")

# Создание модели выживаемости с помощью aorsf
survival_model <- orsf(
  data = data,
  formula = Surv(waitlist_time_days, status) ~ . - etnr_id,
  n_tree = 500
)

# Просмотр модели
print(survival_model)

# Оценка важности переменных с помощью различных методов
# 1. Метод негации (умножение коэффициентов на -1)
vi_negate <- orsf_vi_negate(survival_model)
print(vi_negate)

# 2. Метод перестановки (permutation)
vi_permute <- orsf_vi_permute(survival_model)
print(vi_permute)

# 3. Метод дисперсионного анализа (ANOVA)
vi_anova <- orsf_vi_anova(survival_model)
print(vi_anova)

# Частичная зависимость для топ-2 переменных
pd_results <- orsf_summarize_uni(survival_model, n_variables = 2)
print(pd_results)

# Прогнозирование для новых данных
predictions <- predict(survival_model, newdata = data)
```

### Параметры и опции
Вы можете изменить параметры в файле `main.R` перед запуском:

- Изменение соотношения обучающего/валидационного набора: `prop = 0.7`
- Изменение количества деревьев в AORSF: `n_tree = 500`
- Изменение временных точек для оценки: `times = c(90, 180, 365)`

## 4. Проверка корректности запуска

### Признаки успешного запуска
- В консоли R должно появиться сообщение: `Analysis complete. Results saved in 'results/' directory.`
- Должно появиться сообщение: `HTML report generated: reports/Liver_Transplant_Survival_Analysis_Report.html`

### Проверка результатов
После успешного запуска вы должны увидеть:

1. Созданные каталоги:
   - `results/`: содержит сохраненные результаты анализа
   - `results/figures/`: содержит сохраненные визуализации
   - `reports/`: содержит сгенерированный HTML-отчет

2. Сгенерированные файлы:
   - `reports/Liver_Transplant_Survival_Analysis_Report.html`: полный отчет
   - Различные файлы `.png` и `.pdf` в каталоге `results/figures/`
   - Файлы данных `.rds` и `.csv` в каталоге `results/`

### Доступ к отчету
Откройте HTML-отчет в любом веб-браузере:

```bash
# На macOS
open reports/Liver_Transplant_Survival_Analysis_Report.html

# На Windows
start reports/Liver_Transplant_Survival_Analysis_Report.html

# На Linux
xdg-open reports/Liver_Transplant_Survival_Analysis_Report.html
```

## 5. Устранение распространенных проблем

### Проблемы с установкой пакетов

#### Проблема: Ошибка при установке aorsf
```
Error: package 'aorsf' is not available for your R version
```

**Решение:**
```r
# Установка из GitHub
install.packages("remotes")
remotes::install_github("ropensci/aorsf")
```

#### Проблема: Ошибка при инициализации H2O
```
Error: Unable to initialize h2o.
```

**Решение:**
```r
# Попробуйте с явным выделением памяти
h2o.init(max_mem_size = "4g")

# Если возникают конфликты портов
h2o.init(port = 54321)
```

### Проблемы с данными

#### Проблема: Файлы данных не найдены
```
Error: cannot open file 'imputed_data_full.csv': No such file or directory
```

**Решение:**
- Убедитесь, что файлы данных находятся в корневом каталоге проекта
- Проверьте имена файлов на опечатки
- Если файлы находятся в другом месте, обновите пути в `main.R`

#### Проблема: Ошибки проверки данных
```
Warning: Data validation found issues
```

**Решение:**
- Проверьте выходные данные на наличие конкретных проблем
- Исправьте проблемы с данными (отрицательные значения, несоответствия)
- Если проблемы несущественны, можно отключить проверку: `validate = FALSE`

### Проблемы с памятью

#### Проблема: Ошибка нехватки памяти
```
Error: cannot allocate vector of size X Mb
```

**Решение:**
- Для систем Windows: `memory.limit(size = 8000)`
- Для Unix/Mac: запустите R с увеличенным объемом памяти: `R --max-mem-size=8G`
- Уменьшите параметры модели (например, меньше деревьев в AORSF)
- Отключите H2O, если он не нужен

### Диагностические проверки

Если у вас возникают проблемы, выполните следующие диагностические проверки:

```r
# Проверка версии R
R.version.string

# Проверка установленных пакетов
installed.packages()[c("tidyverse", "survival", "aorsf", "h2o"), "Version"]

# Проверка доступной памяти
gc()

# Проверка рабочего каталога
getwd()

# Проверка наличия файлов данных
file.exists("imputed_data_full.csv")
file.exists("wl_df_with_bca.rds")

# Проверка структуры данных
str(read.csv("imputed_data_full.csv", nrows = 5))
str(readRDS("wl_df_with_bca.rds")[1:5, ])
```

## 6. Дополнительная информация

### Структура проекта
```
.
├── main.R                  # Главный скрипт, запускающий весь рабочий процесс
├── README.md               # Документация проекта
├── INSTALLATION.md         # Руководство по установке (этот файл)
├── data/                   # Каталог данных (не включен в репозиторий)
│   ├── imputed_data_full.csv  # Полный набор данных с оценками MELD и данными о выживаемости
│   └── wl_df_with_bca.rds     # Подмножество с данными анализа состава тела
├── src/                    # Исходный код
│   ├── setup.R                # Настройка окружения и загрузка пакетов
│   ├── data_preprocessing.R   # Загрузка, проверка и предобработка данных
│   ├── exploratory_analysis.R # Описательная статистика и визуализации
│   ├── survival_analysis.R    # Модели Каплана-Мейера и пропорциональных рисков Кокса
│   ├── bc_meld_formula.R      # Реализация и проверка формулы BC-MELD
│   ├── ml_models.R            # Модели AORSF, H2O и ансамбли
│   ├── report_generation.R    # Создание HTML-отчета
│   └── report_template.Rmd    # Шаблон R Markdown для отчета
├── results/                # Каталог результатов (создается скриптами)
│   ├── figures/               # Сохраненные визуализации
│   └── interactive/           # Интерактивные визуализации
└── reports/                # Сгенерированные отчеты
    └── Liver_Transplant_Survival_Analysis_Report.html  # Итоговый HTML-отчет
```

### Документация API
Подробная документация функций доступна в файле [docs/API.md](docs/API.md).

### Описание моделей
Техническая информация о реализованных моделях доступна в файле [docs/MODELS.md](docs/MODELS.md).

### Описание данных
Подробное описание наборов данных и переменных доступно в файле [docs/DATA.md](docs/DATA.md).

### Результаты анализа
Сводка результатов и визуализаций доступна в файле [docs/RESULTS.md](docs/RESULTS.md).

### Дополнительная документация по aorsf
Для получения дополнительной информации о пакете aorsf посетите:
- Официальная документация: [https://docs.ropensci.org/aorsf/](https://docs.ropensci.org/aorsf/)
- GitHub репозиторий: [https://github.com/ropensci/aorsf](https://github.com/ropensci/aorsf)
- Научная статья: [https://doi.org/10.1080/10618600.2023.2231048](https://doi.org/10.1080/10618600.2023.2231048)

## 7. Контакты и поддержка

Если у вас возникли проблемы с установкой или запуском проекта, пожалуйста, создайте issue в репозитории GitHub или свяжитесь с автором проекта.

---

**Примечание**: Это руководство предполагает, что вы имеете базовые знания R и работы с командной строкой. Если вы новичок в R, рекомендуется сначала ознакомиться с основами языка R и среды RStudio.