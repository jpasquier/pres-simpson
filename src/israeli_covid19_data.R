# Load packages
.packages <- c("dplyr", "ggplot2", "here", "readxl", "tidyr", "tikzDevice",
               "xtable")
for (.package in .packages) {
  if (!require(.package, character.only = TRUE)) {
    install.packages(.package)
    library(.package, character.only = TRUE)
  }
}

# ggplot theme
theme_set(theme_bw())

# Set up project directory
i_am("src/israeli_covid19_data.R") |> suppressMessages()

# Create the folder where the tables will be saved if it does not exist
if (!dir.exists(here("tables"))) dir.create(here("tables"))

# Create the folder where the figure(s) will be saved if it does not exist
if (!dir.exists(here("figures"))) dir.create(here("figures"))

# Read the data
file_name <- here("data/Israeli_data_August_15_2021.xlsx")
data <- read_xlsx(file_name, skip = 2)

# Replace `not vax` with `unvax` in the variable names
names(data) <- gsub("not vax", "unvax", names(data))

# ╭───────────────────────────────────────────────────────────────────────────╮
# │ Variables in the original data used in the analysis                       │
# │ ───────────────────────────────────────────────────                       │
# │                                                                           │
# │ * age group: 12-15, 16-19, 20-29, 30-39, 40-49, 50-59, 60-69, 70-79,      │
# │              80-89, 90+                                                   │
# │ * unvax: number of hospitalized patients who are not vaccinated           │
# │ * vax: number of hospitalized patients who are fully vaccinated           │
# │ * unvax per 100k: number of unvaccinated cases (hospitalized patients)    │
# │                   per 100k unvaccinated people in the population (age     │
# │                   group)                                                  │
# │ * vax per 100k: number of vaccinated cases  (hospitalized patients) per   │
# │                 100k vaccinated people in the population (age group)      │
# │ * severe unvax: number of severe cases (hospitalized patients) who are    │
# │                 not vaccinated                                            │
# │ * severe vax: number of severe cases (hospitalized patients) who are      │
# │               fully vaccinated                                            │
# ╰───────────────────────────────────────────────────────────────────────────╯

# Table 0: Raw number of severe cases per vaccination status
add_pct <- function(n, N) paste0(paste(n), " (",round(n / N * 100, 1), "%)")
bold <- function(x) paste0('{\\textbf{',x,'}}')
file_name_0 <- here("tables/israeli_data_summary_0.tex")
data %>%
  summarise(
    `severe unvax` =  sum(`severe unvax`),
    `severe vax` =  sum(`severe vax`),
  ) %>%
  mutate(
    `severe cases` = `severe unvax` + `severe vax`,
    `severe unvax` = add_pct(`severe unvax`, `severe cases`),
    `severe vax` = add_pct(`severe vax`, `severe cases`),
    `severe cases` = as.character(`severe cases`),
  ) %>%
  relocate(`severe cases`) %>%
  xtable() %>%
  print(include.rownames = FALSE, booktabs = TRUE,
        sanitize.colnames.function = bold, file = file_name_0)

# Add variables
data <- data %>% mutate(
  `age group all` = "All (≥12)",
  `age group 2cat` = factor(grepl("^[5-9]", `age group`) * 1,
    levels = 0:1, labels = c("<50", "≥50")),
  `population unvax` = unvax * 1e5 / `unvax per 100k`,
  `population vax` = vax * 1e5 / `vax per 100k`,
)

# Function to summarise the data
summarise_data <- function(.x) {
  .x %>%
    summarise(
      `population unvax` = sum(`population unvax`),
      `population vax` = sum(`population vax`),
      `severe unvax` = sum(`severe unvax`),
      `severe vax` = sum(`severe vax`),
    ) %>%
    mutate(
      .population = `population vax` + `population unvax`,
      `prop population unvax` = `population unvax` / .population,
      `prop population vax` = `population vax` / .population,
      `severe unvax per 100k` = 1e5 * `severe unvax` / `population unvax`,
      `severe vax per 100k` = 1e5 * `severe vax` / `population vax`,
      effectiveness = 1 - `severe vax per 100k` / `severe unvax per 100k`,
    ) %>%
    select(!.population)
}

# Function to format the summarised data
format_summarised_data <- function(.x) {
  join <- function(.x, .y) paste0(.x, " (", .y, ")")
  to_pct <- function(.x) paste0(round(.x * 100, 1), "%")
  .x %>%
    mutate(
      `population unvax` =
        join(round(`population unvax`), to_pct(`prop population unvax`)),
      `population vax` =
        join(round(`population vax`), to_pct(`prop population vax`)),
      `severe unvax` =
        join(round(`severe unvax`), round(`severe unvax per 100k`, 1)),
      `severe vax` =
        join(round(`severe vax`), round(`severe vax per 100k`, 1)),
      effectiveness = to_pct(effectiveness),
    ) %>%
  select(starts_with("age"), `population unvax`, `population vax`,
         `severe unvax`, `severe vax`, effectiveness)
}

# Function to summarise the data by group and format the result
summarise_data_by <- function(.x, grp) {
  .x %>%
    group_by(`age group` = {{ grp }}) %>%
    summarise_data() %>%
    format_summarised_data()
}

# Function to convert the formated summarised data into a latex table
latexify <- function(.x, file_name) {
  addtorow <- list()
  addtorow$pos <- list(0)
  addtorow$command <- paste(
    "& \\multicolumn{2}{c}{\\textbf{Population (\\%)}} &",
    "\\multicolumn{2}{c}{\\textbf{Severe Cases (per 100k)}} & \\\\\n",
    "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}\n \\textbf{Age Group} &",
    "\\textbf{Unvax} & \\textbf{Vax} & \\textbf{Unvax}"
    ,"& \\textbf{Vax} & \\textbf{Effectiveness} \\\\\n")
  .x %>%
    mutate_at(vars(starts_with("age")), gsub, pattern = "≥",
             replacement = "$\\\\geq$") %>%
    mutate_all(gsub, pattern = "%",
             replacement = "\\\\%") %>%
    xtable(align = c("l", "l", "r", "r", "r", "r", "r")) %>%
    print(include.rownames = FALSE, include.colnames = FALSE,
          add.to.row = addtorow, booktabs = TRUE,
          sanitize.text.function = identity, file = file_name)
}

# Table 1: Summarised data for the whole population
file_name_1 <- here("tables/israeli_data_summary_1.tex")
summarised_data_1 <- summarise_data_by(data, `age group all`)
latexify(summarised_data_1, file_name_1)

# Add a footnote mark to the table
.text  <- readLines(file_name_1)
.text  <- gsub("per 100k", replace = "per 100k\\\\footnotemark{}", x = .text)
writeLines(.text, con = file_name_1)

# Table 2: Summarised data by age group (<50, ≥50)
file_name_2 <- here("tables/israeli_data_summary_2.tex")
summarised_data_2 <- summarise_data_by(data, `age group 2cat`)
summarised_data_2 <- bind_rows(summarised_data_1, summarised_data_2)
latexify(summarised_data_2, file_name_2)

# Table 2: Intermediate table
summarised_data_2 %>%
  mutate(
    across(!matches("age group"), ~ ifelse(`age group` == "≥50", "", .))
  ) %>%
  latexify(sub("\\.tex", "_intermediate.tex", file_name_2))

# Table 3: Summarised data by more age groups
file_name_3 <- here("tables/israeli_data_summary_3.tex")
summarised_data_3 <- summarise_data_by(data, `age group`)
latexify(summarised_data_3, file_name_3)

# Function to plot the summarised data
plot_summarised_data <- function(.x) {
  .x %>%
    select(`age group`, starts_with("population"), ends_with("per 100k")) %>%
    pivot_longer(
      cols = !`age group`,
      names_to = c(".value", "vax status"),
      names_pattern = "(population|severe) (unvax|vax)"
    ) %>%
    rename(`severe per 100k` = severe) %>%
    mutate(
      strat = factor(if_else(`age group` == "All", 0, 1), levels = 0:1,
                     labels = c("All population", "By age group")),
      `age group` = gsub("≥", "$\\\\geq$", `age group`)
    ) %>%
    ggplot(aes(x = `vax status`, y = `severe per 100k`, color = `age group`)) +
    geom_point(aes(size = population), position = position_dodge(0.5),
               alpha = 0.5) +
    geom_point(position = position_dodge(0.5), size = .7) +
    scale_y_log10() +
    scale_size(range = c(3, 15)) +
    facet_wrap(vars(strat), nrow = 1) +
    labs(x = "Vaccination status", y = "Severe cases per 100k",
         color = "Age group") +
    guides(size = "none")
}

# Figure: graphical view of the data in table 2
tikz(here("figures/israeli_data_summary_2.tex"), width = 6, height = 2.5)
bind_rows(
  mutate(summarise_data(data), `age group` = "All"),
  summarise_data(group_by(data, `age group` = `age group 2cat`))
) %>%
  plot_summarised_data()
dev.off()

# Fictive balanced data:
# * The proportion of vaccinated people is set to 80% in each age group
#   (<50, ≥50)
# * The rate of severe cases is kept the same in each age group.
# * The number of severe cases is recalculated consequently.
# * The number of severe cases is sumed up across all age groups for the
#   vaccinated and unvaccinated groups.
# * The rates of severe cases for the whole population are recalculated
#   consequently
balanced_data <- data %>%
  group_by(`age group` = `age group 2cat`) %>%
  summarise_data() %>%
  mutate(
    .population = `population unvax` + `population vax`,
    `prop population vax` = summarise_data(data)$`prop population vax`,
    `prop population unvax` = 1 - `prop population vax`,
    `population vax` = .population * `prop population vax`,
    `population unvax` = .population * `prop population unvax`,
    `severe vax` = `severe vax per 100k` * `population vax` / 1e5,
    `severe unvax` = `severe unvax per 100k` * `population unvax` / 1e5,
  ) %>%
  select(!.population)
balanced_data <- balanced_data %>%
  summarise(across(matches("^(population|severe) (un)?vax$"), sum)) %>%
  mutate(`age group` = "All") %>%
  relocate(`age group`) %>%
  bind_rows(balanced_data) %>%
  mutate(
    .population = `population vax` + `population unvax`,
    `prop population unvax` = `population unvax` / .population,
    `prop population vax` = `population vax` / .population,
    `severe unvax per 100k` = 1e5 * `severe unvax` / `population unvax`,
    `severe vax per 100k` = 1e5 * `severe vax` / `population vax`,
    effectiveness = 1 - `severe vax per 100k` / `severe unvax per 100k`,
  )

# Table 2 for the balanced data
balanced_data %>%
  format_summarised_data() %>%
  latexify(sub("\\.tex", "_balanced.tex", file_name_2))

# Figure: graphical view of the balanced data in table 2
tikz(here("figures/israeli_data_summary_2_balanced.tex"),
     width = 6, height = 2.5)
plot_summarised_data(balanced_data)
dev.off()
