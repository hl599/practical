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
df$privateins <- factor(df$privateins, levels = c(0, 1), labels = c("No PrivateIns", "Private health insurance"))
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
  geom_histogram(aes(y = ..density..), position = "dodge", binwidth = 1) +
  labs(y = "Density", x = "Visits to a doctor")

density_hhninc <- ggplot(df, aes(x = hhninc)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5) +
  geom_density(col = "red", size = 1) +
  labs(y = "Density", x = "Net monthly household income (German Marks/100)")

density_educyrs <- ggplot(df, aes(x = educyrs)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(col = "red", size = 0.5) +
  labs(y = "Density", x = "Years of schooling")

density_age <- ggplot(df, aes(x = age)) +
  geom_histogram(aes(y = ..density..), binwidth = 1) +
  geom_density(col = "red", size = 1) +
  labs(y = "Density", x = "Age (Years)")

proportion_eductype <- ggplot(df, aes(x = eductype)) +
  geom_bar() +
  labs(y = "Count", x = "Distribution")

density_eductype <- df %>%
  count(eductype) %>%
  mutate(proportion = n / sum(n))

density_eductype <- ggplot(density_eductype, aes(x = eductype, y = proportion)) +
  geom_bar(stat = "identity") +
  labs(y = "Density", x = "Highest degree of schooling")

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


grid.arrange(
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
df$demeaned_age = df$age - mean(df$age)
df$demeaned_hhninc = df$hhninc - mean(df$hhninc)
df$demeaned_educyrs = df$educyrs - mean(df$educyrs)

glm_1 <- glm(docvis ~ ., family = poisson, data = df %>% select(-c("age", "hhninc", "educyrs")))
summary(glm_1)

glm_2 <- glm(docvis ~ . * female, family = poisson, data = df %>% select(-c("age", "hhninc", "educyrs")))
summary(glm_2)

glm_3 <- step(glm_2, trace = T)
summary(glm_3)

anova(glm_3)
rsq(glm_3, adj = FALSE, type = "kl")

# Goodness of Fit
hoslem.test(fitted(glm_3), df$docvis, g = 10)

# Diagnostics
n <- length(glm_3$fitted.values)
p <- length(coef(glm_3))
top_10_leverage <- rank(-influence(glm_3)$hat) <= 10
top_10_influence <- rank(-cooks.distance(glm_3)) <= 10

diag_qqplot <- ggplot(glm_3, aes(sample = rstandard(glm_3))) +
  stat_qq() +
  stat_qq_line() + 
  labs(x = "Theoretical quantiles", y = "Sample quantiles")

diag_res <- ggplot(glm_3, aes(x = predict(glm_3), y = rstandard(glm_3))) +
  geom_point(alpha = 0.4, position = position_jitter(width = 0.1, height = 0.1)) +
  labs(x = "Linear Predictor", y = "")

diag_influence <- ggplot(glm_3, aes(x = 1:n, y = influence(glm_3)$hat / (p/n))) +
  geom_point() +
  geom_hline(yintercept = 2, linetype = 2) + 
  labs(x = "Observation number", y = "Leverage / (p/n)") +
  geom_text_repel(data = df, aes(label = ifelse(top_10_leverage, 1:n, "")), size=3)

diag_leverage <- ggplot(glm_3, aes(x = 1:n, y = cooks.distance(glm_3))) +
  geom_point() +
  geom_hline(yintercept = 8/(n - 2 * p), linetype = 2) + 
  labs(x = "Observation number", y = "Cook's distance") +
  geom_text_repel(data = df, aes(label = ifelse(top_10_influence, 1:n, "")), size=3)

plot_5 <- grid.arrange(diag_qqplot, diag_res, diag_influence, diag_leverage, ncol = 2)

df[top_10_leverage,]
df[top_10_influence,]


