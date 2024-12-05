Sys.setenv(LANG = "en")
setwd = "D:/Files/Projects/Code/practical"

library(ggplot2)
library(gridExtra)
library(knitr)
library(dplyr)

df <- read.csv("dvis.csv")
cols <- c("female", "hhkids", "married", "employed", "addins", "privateins", "eductype")
df[cols] <- lapply(df[cols], factor) 
df$female <- factor(df$female, levels = c(0, 1), labels = c("Male", "Female"))
df$married <- factor(df$married, levels = c(0, 1), labels = c("Not Married", "Married"))
df$employed <- factor(df$employed, levels = c(0, 1), labels = c("Otherwise", "In Paid Work"))
df$addins <- factor(df$addins, levels = c(0, 1), labels = c("Otherwise", "Additional Insurance"))
df$privateins <- factor(df$privateins, levels = c(0, 1), labels = c("Otherwise", "Private health insurance"))
df$hhkids <- factor(df$hhkids, levels = c(0, 1), labels = c("Otherwise", "Children in household"))
df <- df %>%
  select(docvis, where(is.numeric), where(is.factor))

# Data Exploration
summary_table <- summary(df)

latex_table <- kable(
  t(summary_table), 
  format = "latex", 
  booktabs = TRUE,
  caption = "Summary of the Dataset"
)

cat(latex_table)

means <- sapply(df, mean, na.rm = TRUE)
sd <- sapply(df[c("docvis", "age", "hhninc", "educyrs")], sd)

print(means)
print(1-means)

# univariate
hist_female <- ggplot(df, aes(x = docvis, fill = female)) +
  geom_histogram(aes(y = ..density..), position = "dodge", binwidth = 1) +
  labs(y = "Density", x = "Visits to a doctor")

hist_married <- ggplot(df, aes(x = docvis, fill = married)) +
  geom_histogram(aes(y = ..density..), position = "dodge", binwidth = 1) +
  labs(y = "Density", x = "Visits to a doctor")

hist_employed <- ggplot(df, aes(x = docvis, fill = employed)) +
  geom_histogram(aes(y = ..density..), position = "dodge", binwidth = 1) +
  labs(y = "Density", x = "Visits to a doctor")

hist_privateins <- ggplot(df, aes(x = docvis, fill = privateins)) +
  geom_histogram(aes(y = ..density..), position = "dodge", binwidth = 1) +
  labs(y = "Density", x = "Visits to a doctor")

hist_addins <- ggplot(df, aes(x = docvis, fill = addins)) +
  geom_histogram(aes(y = ..density..), position = "dodge", binwidth = 1) +
  labs(y = "Density", x = "Visits to a doctor")

hist_hhkids <- ggplot(df, aes(x = docvis, fill = hhkids)) +
  geom_histogram(aes(y = ..density..), position = "dodge", binwidth = 1) +
  labs(y = "Density", x = "Visits to a doctor")

plot_1 <- grid.arrange(hist_female, hist_addins, ncol = 2)

density_hhninc <- ggplot(df, aes(x = hhninc)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5) +
  geom_density(col = "red", size = 1) +
  labs(y = "Density", x = "Net monthly household income (German Marks/100)")

density_educyrs <- ggplot(df, aes(x = educyrs)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(col = "red", size = 1) +
  labs(y = "Density", x = "Years of schooling")

density_age <- ggplot(df, aes(x = age)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  geom_density(col = "red", size = 1) +
  labs(y = "Density", x = "Age (Years)")

plot_2 <- grid.arrange(density_hhninc, density_educyrs, density_age, ncol = 3)

# bivariate

ggplot(df, aes(y = docvis, x = educyrs)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.1)) +
  facet_grid(~ female) +
  labs(y = "Visits to a doctor", x = "Years of Schooling")

ggplot(df, aes(y = docvis, x = hhninc)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.1)) +
  facet_grid(~ female) +
  labs(y = "Visits to a doctor", x = "Net monthly household income (German Marks/100)")

ggplot(df, aes(y = docvis, x = age)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.1)) +
  facet_grid(~ female) +
  labs(y = "Visits to a doctor", x = "Age (Years)")

boxplot_married <- ggplot(df, aes(y = docvis, x = married, fill = female)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Relationship")

boxplot_employed <- ggplot(df, aes(y = docvis, x = employed, fill = female)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Employment")

boxplot_hhkids <- ggplot(df, aes(y = docvis, x = hhkids, fill = female)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Child in the family")

boxplot_addins <- ggplot(df, aes(y = docvis, x = addins, fill = female)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Additional Insurance")

boxplot_privateins <- ggplot(df, aes(y = docvis, x = privateins, fill = female)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Private Insurance")

boxplot_eductype <- ggplot(df, aes(y = docvis, x = eductype, fill = female)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Highest Level of Schooling Obtained")

layout <- rbind(c(1, 2),
                c(3, 4),
                c(5, 5))

plot_3 <- grid.arrange(boxplot_married, boxplot_hhkids, boxplot_addins, boxplot_privateins, boxplot_eductype, layout_matrix = layout)
grid.arrange(plot_3, boxplot_eductype, col = 1)
