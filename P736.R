Sys.setenv(LANG = "en")
setwd = "D:/Files/Projects/Code/practical"

library(ggplot2)
library(gridExtra)
library(knitr)
library(dplyr)
library(ggrepel)

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

sum(df$docvis == 0) / nrow(df)
sort(table(df$educyrs), decreasing = TRUE)
male <- mean(df$docvis[df$female == "Male"], na.rm = TRUE)
female <- mean(df$docvis[df$female == "Female"], na.rm = TRUE)
male - female

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

plot_2 <- grid.arrange(density_hhninc, density_age, density_educyrs, ncol = 3)

# bivariate

ggplot(df, aes(y = docvis, x = educyrs)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.1)) +
  facet_grid(~ female) +
  geom_smooth(color = "red", se = FALSE, size = 0.75)
  labs(y = "Visits to a doctor", x = "Years of Schooling")

plot_3 <- ggplot(df, aes(y = docvis, x = hhninc)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.1)) +
  facet_grid(~ female) +
  geom_smooth(color = "red", se = FALSE, size = 0.75)
  labs(y = "Visits to a doctor", x = "Net monthly household income (German Marks/100)")

plot_4 <- ggplot(df, aes(y = docvis, x = age)) +
  geom_point(alpha = 0.3, position = position_jitter(width = 0.1)) +
  facet_grid(~ female) +
  geom_smooth(color = "red", se = FALSE, size = 0.75) +
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

plot_4 <- grid.arrange(boxplot_married, boxplot_hhkids, boxplot_addins, boxplot_privateins, boxplot_eductype, layout_matrix = layout)

boxplot_married2 <- ggplot(df, aes(y = docvis, x = married)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Relationship")

boxplot_employed2 <- ggplot(df, aes(y = docvis, x = employed)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Employment")

boxplot_hhkids2 <- ggplot(df, aes(y = docvis, x = hhkids)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Child in the family")

boxplot_addins2 <- ggplot(df, aes(y = docvis, x = addins)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Additional Insurance")

boxplot_privateins2 <- ggplot(df, aes(y = docvis, x = privateins)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Private Insurance")

boxplot_eductype2 <- ggplot(df, aes(y = docvis, x = eductype)) +
  geom_boxplot() +
  labs(y = "Visits to a doctor", x = "Highest Level of Schooling Obtained")

grid.arrange(boxplot_employed2, boxplot_hhkids2, boxplot_addins2, ncol = 3)


# Modelling
df$age = df$age - mean(df$age)
df$hhninc = df$hhninc - mean(df$hhninc)
df$educyrs = df$educyrs - mean(df$educyrs)

# lm <- glm(docvis ~ . + . * female, family = poisson, data = df)
# plot(lm, 1)

glm_1 <- glm(docvis ~ ., family = poisson, data = df)
summary(glm_1)

glm_2 <- glm(docvis ~ . * female, family = poisson, data = df)
summary(glm_2)

glm_3 <- step(glm_2, trace = T)
summary(glm_3)
anova(glm_3)


# Diagnostics
n <- length(glm_3$fitted.values)
p <- length(coef(glm_3))
thres_infl <-  8/(n - 2 * p)
id_hi = (cooks.distance(glm_3) > thres_infl)
id_hl = (influence(glm_3)$hat > p/n*2)

ggplot(glm_3, aes(x = predict(glm_3), y = rstandard(glm_3))) +
  geom_point(alpha = 0.4, position = position_jitter(width = 0.1, height = 0.1)) +
  labs(x = "Linear Predictor", y = "")

ggplot(glm_3, aes(sample = rstandard(glm_3))) +
  stat_qq() +
  stat_qq_line() + 
  labs(x = "Theoretical quantiles", y = "Sample quantiles")

ggplot(glm_3, aes(x = 1:n, y = influence(glm_3)$hat / (p/n))) +
  geom_point() +
  geom_hline(yintercept = 2, linetype = 2) + 
  labs(x = "Observation number", y = "Leverage / (p/n)") +
  geom_text_repel(data = df, aes(label = ifelse(id_hl, 1:n, "")), size=3)

ggplot(glm_3, aes(x = 1:n, y = cooks.distance(glm_3))) +
  geom_point() +
  geom_hline(yintercept = thres, linetype = 2) + 
  labs(x = "Observation number", y = "Cook's distance") +
  geom_text_repel(data = df, aes(label = ifelse(id_hi, 1:n, "")), size=3)



