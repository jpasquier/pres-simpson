# Load packages
.packages <- c("dplyr", "ggplot2", "here", "tikzDevice")
for (.package in .packages) {
  if (!require(.package, character.only = TRUE)) {
    install.packages(.package)
    library(.package, character.only = TRUE)
  }
}

# Set seed of random number generator
set.seed(666)

# Set ggplot theme
theme_set(theme_bw())

options(width = 180)

# Set up project directory
i_am("src/smoking.R") |> suppressMessages()

# Create the folder where the figures will be saved if it does not exist
if (!dir.exists(here("figures"))) dir.create(here("figures"))

# Generate random data
data <- tibble(
  male = rep(0:1, each = 50),
  smoking = rpois(length(male), lambda = 10 * (1 + male)),
  score = 100 + 50 * male - smoking + rnorm(length(male), sd = 15)
) %>%
  mutate(sex = factor(male, 0:1, c("faVenus", "faMars")))

# Plot the data
tikz(here("figures/smoking_1.tex"), width = 4, height = 2)
ggplot(data, aes(x = smoking, y = score)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
              color = "steelblue") +
  labs(x = "Number of cigarettes smoked per week", y = "Sport performance")
dev.off() -> .null

# Plot the data per sex
file_name_2 <- here("figures/smoking_2.tex")
tikz(file_name_2, width = 4, height = 2)
ggplot(data, aes(x = smoking, y = score, color = sex)) +
  geom_point() +
  scale_color_manual(values = c(`faMars` = "#4682B4", `faVenus` = "#FF6961")) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  labs(x = "Number of cigarettes smoked per week", y = "Sport performance",
       color = NULL) +
  theme(legend.position = "inside", legend.position.inside = c(.875, .19))
dev.off() -> .null
.text  <- readLines(file_name_2)
.text  <- gsub("fa(Mars|Venus)", replace = "\\\\fa\\1", x = .text)
writeLines(.text, con = file_name_2)

# Plot smoking density
file_name_3 <- here("figures/smoking_density.tex")
tikz(file_name_3, width = 4, height = 2)
ggplot(data, aes(x = smoking, fill = sex)) +
  geom_density(bw = 2, alpha = 0.5) +
  scale_fill_manual(values = c(`faMars` = "#4682B4", `faVenus` = "#FF6961")) +
  labs(x = "Number of cigarettes smoked per week", y = "Density", fill = NULL)
dev.off() -> .null
.text  <- readLines(file_name_3)
.text  <- gsub("fa(Mars|Venus)", replace = "\\\\fa\\1", x = .text)
writeLines(.text, con = file_name_3)
