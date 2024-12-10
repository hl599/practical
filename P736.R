Sys.setenv(LANG = "en")
setwd = "D:/Files/Projects/Code/practical"

library(ggplot2)
library(gridExtra)
library(knitr)
library(dplyr)
library(ggrepel)
library(rsq)
library(ResourceSelection)
library(grid)


df <- read.csv("dvis.csv")
df$female <- factor(df$female, levels = c(0, 1), labels = c("Male", "Female"))
df$married <- factor(df$married, levels = c(0, 1), labels = c("Not Married", "Married"))
df$employed <- factor(df$employed, levels = c(0, 1), labels = c("No Paid Work", "In Paid Work"))
df$addins <- factor(df$addins, levels = c(0, 1), labels = c("No AddIns", "Additional Insurance"))
df$privateins <- factor(df$privateins, levels = c(0, 1), labels = c("No PrivateIns", "Private insurance"))
df$hhkids <- factor(df$hhkids, levels = c(0, 1), labels = c("No Children", "Children in household"))
df$eductype <- factor(df$eductype, ordered = T, levels = c(0, 1, 2, 3, 4, 5), labels = c("None", "Hauptschule", "Realschule", "Fachhochschule", "Abitur", "University"))

df <- df %>%
  select(docvis, where(is.numeric), where(is.factor))

# Data Exploration
# Summary statistics
summary_table <- summary(df)

latex_table <- kable(
  t(summary_table), 
  format = "latex", 
  booktabs = TRUE,
  caption = "Summary of the Dataset"
)

cat(latex_table)

# Miscellaneous calculation
means <- sapply(df, mean, na.rm = TRUE)
sd <- sapply(df[c("docvis", "age", "hhninc", "educyrs")], sd)

print(means)
print(1-means)
sum(df$docvis == 0) / nrow(df)
sort(table(df$educyrs), decreasing = TRUE)
male <- mean(df$docvis[df$female == "Male"], na.rm = TRUE)
female <- mean(df$docvis[df$female == "Female"], na.rm = TRUE)
male - female

df %>% select(eductype, educyrs) %>% unique %>% arrange(educyrs)

table(df$educyrs)

# univariate
plot_1 <- ggplot(df, aes(x = docvis)) +
  geom_histogram(aes(y = ..density..), col = "black", binwidth = 1) +
  labs(y = "Frequency", x = "Number of visits to a doctor in a month before interview")

density_hhninc <- ggplot(df, aes(x = hhninc)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5) +
  geom_density(col = "red", size = 1) +
  labs(y = "Frequency", x = "Net monthly household income (k DM)")

density_educyrs <- ggplot(df, aes(x = educyrs)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(col = "red", size = 0.5) +
  labs(y = "Frequency", x = "Years of schooling")

density_age <- ggplot(df, aes(x = age)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  geom_density(col = "red", size = 1) +
  labs(y = "Frequency", x = "Age (years)")

proportion_eductype <- ggplot(df, aes(x = eductype)) +
  geom_bar() +
  labs(y = "Count", x = "Distribution")

density_eductype <- df %>%
  count(eductype) %>%
  mutate(proportion = n / sum(n))

density_eductype <- ggplot(density_eductype, aes(x = eductype, y = proportion)) +
  geom_bar(stat = "identity") +
  labs(y = "Frequency", x = "Highest degree of schooling") +
  theme(
    axis.text.x = element_text(
      angle = 45,    
      hjust = 1,    
      vjust = 1      
    )
  )

plot_2 <- grid.arrange(density_hhninc, density_age, density_educyrs, density_eductype , ncol = 2)

# bivariate
plot_docvis_by_deciles <- function(df, x_var, y_var) {
  x_col <- rlang::ensym(x_var)
  y_col <- rlang::ensym(y_var)
  
  decile_data <- df %>%
    mutate(x_decile = ntile(!!x_col, 10)) %>%
    group_by(x_decile) %>%
    summarise(mean_y = mean(!!y_col, na.rm = TRUE))
  
  ggplot(decile_data, aes(x = x_decile * 10, y = mean_y)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = seq(10, 100, by = 10), labels = paste0(seq(10, 100, by = 10), "%"))
}

decile_age <- plot_docvis_by_deciles(df, x_var = age, y_var = docvis) +
  labs(
    x = "Deciles of age") +
  theme(axis.title.y = element_blank())

decile_hhninc <- plot_docvis_by_deciles(df, x_var = hhninc, y_var = docvis) + 
  labs(
    x = "Deciles of net monthly household income") +
  theme(axis.title.y = element_blank())
        
decile_educyrs <- plot_docvis_by_deciles(df, x_var = educyrs, y_var = docvis) + 
  labs(
    x = "Deciles of years of schooling") +
  theme(axis.title.y = element_blank())

quantile(df$age, probs = 0.4)
quantile(df$educyrs, probs = 0.3)
quantile(df$hhninc, probs = 0.3)
quantile(df$hhninc, probs = 0.7)
cor(as.numeric(df$eductype), df$educyrs, method = "kendall")
cor(as.numeric(df$eductype), df$educyrs, method = "spearman")


plot_3 <- grid.arrange(
  arrangeGrob(
    decile_age, decile_hhninc, decile_educyrs,
    nrow = 3
  ),
  left = textGrob("Average number of visits to a doctor", rot = 90, vjust = 1, gp = gpar(fontsize = 12))
)


boxplot_female <- ggplot(df, aes(y = docvis, x = female)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

boxplot_hhkids <- ggplot(df, aes(y = docvis, x = hhkids)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

boxplot_married <- ggplot(df, aes(y = docvis, x = married)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

boxplot_employed <- ggplot(df, aes(y = docvis, x = employed)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

boxplot_addins <- ggplot(df, aes(y = docvis, x = addins)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

boxplot_privateins <- ggplot(df, aes(y = docvis, x = privateins)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

boxplot_eductype <- ggplot(df, aes(y = docvis, x = eductype)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, color = "red", size = 3) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())

layout <- rbind(c(1, 2, 3),
                c(4, 5, 6),
                c(7, 7, 7))

plot_4 <- grid.arrange(boxplot_female, boxplot_hhkids, boxplot_married, boxplot_employed, boxplot_addins, boxplot_privateins, boxplot_eductype, layout_matrix = layout)

# Modelling
df$c_age = df$age - mean(df$age)
df$c_hhninc = df$hhninc - mean(df$hhninc)
df$c_educyrs = df$educyrs - mean(df$educyrs)

full_model <- glm(docvis ~ . * female, family = poisson, data = df %>% select(-c("age", "hhninc", "educyrs")))
summary(full_model)

step(full_model, trace = F)
glm_baseline <- glm(docvis ~ female * c_age + c_hhninc + c_educyrs + hhkids + addins, family = poisson, data = df %>% select(-c("age", "hhninc", "educyrs")))
estimate_final_model <- round(summary(glm_baseline)$coefficient, 3)
anova(glm_baseline)
glm_reorder <- glm(docvis ~ c_hhninc + c_educyrs +  female * c_age + hhkids + addins, family = poisson, data = df %>% select(-c("age", "hhninc", "educyrs")))
anova(glm_reorder)

# Goodness of Fit
rsq(glm_baseline, adj = FALSE, type = "kl")

# Diagnostics
n <- length(glm_baseline$fitted.values)
p <- length(coef(glm_baseline))
top_leverage <- rank(-influence(glm_baseline)$hat) <= 5
top_influence <- rank(-cooks.distance(glm_baseline)) <= 5

diag_qqplot <- ggplot(glm_baseline, aes(sample = rstandard(glm_baseline))) +
  stat_qq() +
  stat_qq_line() + 
  labs(x = "Theoretical quantiles", y = "Sample quantiles")

diag_res <- ggplot(glm_baseline, aes(x = predict(glm_baseline, type = "link"), y = rstandard(glm_baseline))) +
  geom_point(alpha = 0.4, position = position_jitter(width = 0.1, height = 0.1)) +
  labs(x = "Linear Predictor", y = "Deviance Residuals")

diag_influence <- ggplot(glm_baseline, aes(x = 1:n, y = influence(glm_baseline)$hat / (p/n))) +
  geom_point() +
  geom_hline(yintercept = 2, linetype = 2, , col = "red", size = 0.75) + 
  labs(x = "Observation number", y = "Leverage / (p/n)") +
  geom_text_repel(data = df, aes(label = ifelse(top_leverage, 1:n, "")), size=3)

diag_leverage <- ggplot(glm_baseline, aes(x = 1:n, y = cooks.distance(glm_baseline))) +
  geom_point() +
  geom_hline(yintercept = 8/(n - 2 * p), linetype = 2, col = "red", size = 0.75) + 
  labs(x = "Observation number", y = "Cook's distance") +
  geom_text_repel(data = df, aes(label = ifelse(top_influence, 1:n, "")), size=3)
print(8/(n - 2 * p))

plot_5 <- grid.arrange(diag_qqplot, diag_res, diag_influence, diag_leverage, ncol = 2)

rbind(df[top_leverage,], df[top_influence,])

glm_rob <- glm(docvis ~ female * c_age + c_hhninc + c_educyrs + hhkids + addins, family = poisson, data = df[!top_influence,] %>% select(-c("age", "hhninc", "educyrs")))

estimate_robustness <- round(summary(glm_rob)$coefficient, 3)
estimate_dispersion <- round(summary(glm_dispersion)$coefficient, 3)
cbind(estimate_final_model[,1:2], estimate_dispersion[,1:2])

# Interpretation
estimate_final_model

coefs <- coef(summary(glm_baseline))
vcov_matrix <- vcov(glm_baseline)

beta_c_age <- coefs["c_age", "Estimate"]
beta_female <- coefs["femaleFemale", "Estimate"]
beta_female_c_age <- coefs["femaleFemale:c_age", "Estimate"]
var_c_age <- vcov_matrix["c_age", "c_age"]
var_female_c_age <- vcov_matrix["femaleFemale:c_age", "femaleFemale:c_age"]
cov_c_age_female_c_age <- vcov_matrix["c_age", "femaleFemale:c_age"]

se_female <- sqrt(vcov_matrix["femaleFemale", "femaleFemale"])
se_female_age <- sqrt(vcov_matrix["femaleFemale:c_age", "femaleFemale:c_age"])
cov_female_age <- vcov_matrix["femaleFemale", "femaleFemale:c_age"]

z_value <- qnorm(0.975)

# Function to calculate the effect and confidence interval
calc_effect <- function(c_age) {
  effect <- beta_female + beta_female_c_age * c_age
  se <- sqrt(se_female^2 + (se_female_age^2 * c_age^2) + 
               (2 * cov_female_age * c_age))
  ci_lower <- exp(effect - z_value * se)
  ci_upper <- exp(effect + z_value * se)
  estimate <- exp(effect)
  data.frame(c_age = c_age, estimate = estimate, ci_lower = ci_lower, ci_upper = ci_upper)
}

c_age_values <- seq(min(df$c_age), max(df$c_age), by = 0.1)  # Adjust range and step as needed
effect_data <- do.call(rbind, lapply(c_age_values, calc_effect))
effect_data$age <- effect_data$c_age + mean(df$age)

plot_6 <- ggplot(effect_data, aes(x = age)) +
  geom_line(aes(y = estimate), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "blue", alpha = 0.2) +
  labs(
    x = "Age (years)",
    y = "Effect of female on # doctor visits"
  ) 

beta_female <- beta_c_age + beta_female_c_age
var_female <- var_c_age + var_female_c_age + 2 * cov_c_age_female_c_age

CI_female_lower <- beta_female - z_value * sqrt(var_female)
CI_female_upper <- beta_female + z_value * sqrt(var_female)
exp(c(CI_female_lower, CI_female_upper))

# Dispersion
glm_dispersion <- glm(docvis ~ female * c_age + c_hhninc + c_educyrs + hhkids + addins, family = quasipoisson, data = df %>% select(-c("age", "hhninc", "educyrs")))
estimate_dispersion <- round(summary(glm_dispersion)$coefficient, 3)
cbind(estimate_final_model, estimate_dispersion[,2:4])
rsq(glm_baseline, type = "kl")
rsq(glm_dispersion, type = "kl")

n <- length(glm_dispersion$fitted.values)
p <- length(coef(glm_dispersion))
top_influence <- rank(-cooks.distance(glm_dispersion)) <= 5

ggplot(glm_dispersion, aes(x = 1:n, y = cooks.distance(glm_dispersion))) +
  geom_point() +
  geom_hline(yintercept = 8/(n - 2 * p), linetype = 2, col = "red", size = 0.75) + 
  labs(x = "Observation number", y = "Cook's distance") +
  geom_text_repel(data = df, aes(label = ifelse(top_influence, 1:n, "")), size=3)
print(8/(n - 2 * p))

ggsave(filename = "Figure 1.png", plot = plot_1, width = 6, height = 3, dpi = 300)
ggsave(filename = "Figure 2.png", plot = plot_2, width = 8, height = 5, dpi = 300)
ggsave(filename = "Figure 3.png", plot = plot_3, width = 6, height = 6, dpi = 300)
ggsave(filename = "Figure 4.png", plot = plot_4, width = 7, height = 7, dpi = 300)
ggsave(filename = "Figure 5.png", plot = plot_5, width = 6, height = 6, dpi = 300)
ggsave(filename = "Figure 6.png", plot = plot_6, width = 6, height = 3, dpi = 300)



