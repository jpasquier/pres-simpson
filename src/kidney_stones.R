# Load packages
.packages <- c("broom", "dplyr", "here", "tidyr", "xtable")
for (.package in .packages) {
  if (!require(.package, character.only = TRUE)) {
    install.packages(.package)
    library(.package, character.only = TRUE)
  }
}

options(width = 180)

# Set up project directory
i_am("src/kidney_stones.R") |> suppressMessages()

# Create the folder where the tables will be saved if it does not exist
if (!dir.exists(here("tables"))) dir.create(here("tables"))

# Read data from the CSV file.
# Data were found in Charig (1986)
data <- read.csv(here("data/kidney_stones.csv"))

# Function to add overall number of successes and failures across stone sizes
# and to calculate success rate
overall <- function(.x) {
  .x %>%
    group_by(surgery) %>%
    summarise(success = sum(success), failure = sum(failure)) %>%
    mutate(stone_size = "overall") %>%
    bind_rows(.x, .) %>%
    mutate(
      n = success + failure,
      rate = success / (success + failure)
    ) %>%
    relocate(n, .before = success)
}

# Add overall number of successes and failures, add success rate
data <- overall(data)

# Function to export a table in LaTeX format
latexify <- function(.x, file_name) {
  bold <- function(x) paste('{\\textbf{',x,'}}', sep ='')
  .x %>%
    rename_with(gsub, pattern = "_", replacement = " ") %>%
    rename_with(gsub, pattern = "((^| )[[:alpha:]])", replacement = "\\U\\1",
                perl = TRUE) %>%
    xtable() %>%
    print(include.rownames = FALSE, booktabs = TRUE,
          sanitize.colnames.function = bold, file = file_name)
}
latexify <- function(.x, file_name) {
  sanitize_colnames <- function(x) {
    x <- gsub("_", " ", x)
    x <- gsub("((^| )[[:alpha:]])", "\\U\\1", x, perl = TRUE)
    x <- gsub("(P|p)cnl", "PCNL", x)
    paste0('{\\textbf{', x, '}}')
  }
  .x %>%
    rename_with(gsub, pattern = "_", replacement = " ") %>%
    rename_with(gsub, pattern = "((^| )[[:alpha:]])", replacement = "\\U\\1",
                perl = TRUE) %>%
    xtable() %>%
    print(include.rownames = FALSE, booktabs = TRUE,
          sanitize.colnames.function = sanitize_colnames, file = file_name)
}

# Export a Latex table of the data
latexify(data, here("tables/kidney_stones.tex"))

# Function to calculate the risk differnces, risk ratios, and odds ratios
stat <- function(.x) {
  .x %>%
    mutate(success = paste0(success, "/", n)) %>%
    select(!c(n, failure)) %>%
    pivot_wider(names_from = surgery, values_from = c(success, rate)) %>%
    mutate(
      risk_difference = rate_pcnl - rate_open,
      risk_ratio = rate_pcnl / rate_open,
      odds_ratio = rate_pcnl * (1 - rate_open) / ((1 - rate_pcnl) * rate_open)
    ) %>%
    select(!starts_with("rate_"))
}

# Calculate the risk differnces, risk ratios, and odds ratios and export the
# table in LaTeX format
latexify(stat(data), here("tables/kidney_stones_stats.tex"))

# Fictive balanced data
# * Keep the original proportion of success in each surgery/stone_size group
# * Allocate an equal number of large and small stones inside each surgical
#   technique
# * Recalculate the number of successes and failures
# * Sum the number of successes and failures across stone sizes for each
#   surgery
# * Calculate the overall success rate
balanced_data <- data %>%
  filter(stone_size != "overall") %>%
  group_by(surgery) %>%
  mutate(.N = sum(success + failure)) %>%
  mutate(
    success = round(.N / 2 * rate),
    failure = round(.N / 2 * (1 - rate))
  ) %>%
  select(!.N) %>%
  overall()
latexify(balanced_data, here("tables/kidney_stones_balanced.tex"))
latexify(stat(balanced_data), here("tables/kidney_stones_stats_balanced.tex"))

# Logistic regression
logistic_regression <- function(.data, .model) {
  .data <- filter(.data, stone_size != "overall")
  .formula <- rate ~ surgery
  if (.model == 2) {
    .formula <- update(.formula, . ~ . + stone_size)
  }
  fit <- glm(.formula, family = binomial, weights = n, data = .data)
  tidy(fit, conf.int = TRUE, exponentiate = TRUE) %>%
    select(term, estimate, conf.low, conf.high) %>%
    mutate(
      term = sub("surgerypcnl", "PCNL Surgery", term),
      term = sub("stone_sizesmall", "Small Stone", term),
    ) %>%
    rename(
      `OR` = "estimate",
      `2.5\\%` = "conf.low",
      `97.5\\%` = "conf.high"
    )
}
logistic_regression(data, 1) %>%
  latexify(here("tables/kidney_stones_logistic_regression_1.tex"))
logistic_regression(data, 2) %>%
  latexify(here("tables/kidney_stones_logistic_regression_2.tex"))
logistic_regression(balanced_data, 1) %>%
  latexify(here("tables/kidney_stones_logistic_regression_1_balanced.tex"))
logistic_regression(balanced_data, 2) %>%
  latexify(here("tables/kidney_stones_logistic_regression_2_balanced.tex"))
