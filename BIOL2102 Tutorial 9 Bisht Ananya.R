df <- read.csv("/Users/bishtananya/Downloads/Daphniagrowth.csv")
library(ggplot2)

View(df)

#Use boxplots to plot and explore the dataset

ggplot( df, aes(x = parasite, y = growth.rate, fill = parasite)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Growth rate distribution by parasite")



#### Create a boxplot figure with colors for each parasite box (x axis) and growth rate in the y axis. Re-order the figure with parasites from the greatest to the lowest difference compared to control. Add red bars inside, showing 99% confidence intervals.
means <- aggregate(growth.rate ~ parasite, df, mean)
control_mean <- means$growth.rate[means$parasite == "control"]
means$diff_from_control <- abs(means$growth.rate - control_mean)
# Order parasites based on difference from control descending
order_parasites <- means$parasite[order(means$diff_from_control, decreasing = TRUE)]

df$parasite <- factor(df$parasite, levels = order_parasites)

# Function to calculate 99% confidence interval for the mean
ci99 <- function(x) {
  m <- mean(x)
  s <- sd(x)
  n <- length(x)
  error <- qt(0.995, df = n-1) * s / sqrt(n)
  return(c(y = m, ymin = m - error, ymax = m + error))
}

# Plot with re-ordered parasite levels and red 99% CI bars
ggplot(df, aes(x = parasite, y = growth.rate, fill = parasite)) + 
  geom_boxplot() +
  stat_summary(fun.data = ci99, geom = "errorbar", color = "red", width = 0.3, size = 1) +
  labs(title = "Growth Rate by Parasite with 99% CI") +
  theme(legend.position = "none")


# Check assumptions by plotting residuals AND by using statistical tests

mod_aov <- aov(growth.rate ~ parasite, data = df)

residuals_aov <- residuals(mod_aov)

anova_table <- anova(mod_aov)
print(anova_table)

sse <- round(anova_table["Residuals", "Sum Sq"], 3)
print(sse)

sst <- sum(anova_table$"Sum Sq")
round(sst, 4)


#Residual Plot

ggplot(data.frame(residuals = residuals_aov, fitted = fitted(mod_aov)), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values for ANOVA", x = "Fitted values", y = "Residuals")

#Normality test of residuals
shapiro_test <- shapiro.test(residuals_aov)

#Homogeneity of variance test
library(car) 
levene_test <- leveneTest(growth.rate ~ parasite, data = df)



### Define your null and alternative hypothesis
# Null hypothesis H0: Parasites do NOT alter growth rates (means of groups are equal)
# Alternative hypothesis H1: At least one parasite group differs in growth rate


# Run an ANOVA to test using aov or lm
summary(mod_aov)

mod_lm <- lm(mod_aov)
summary(mod_lm)$coefficients



#Now, consider the independent variable and plot the relationships of growth rate with energy efficiency. Include in the figure the regression equation for each group of parasites
ggplot(df, aes(x = energy, y = growth.rate, color = parasite)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Growth Rate vs Energy by Parasite")


#Test assumptions of the ANCOVA model

mod_ancova <- lm(growth.rate ~ parasite * energy, data = df)
residuals_ancova <- residuals(mod_ancova)

# Residual plot for ANCOVA
ggplot(data.frame(residuals = residuals_ancova, fitted = fitted(mod_ancova)), aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted for ANCOVA")

# Normality and homogeneity tests
shapiro_ancova <- shapiro.test(residuals_ancova)
levene_ancova <- leveneTest(residuals_ancova ~ df$parasite)


#Run the ANCOVA test with aov or lm considering interaction and no interaction between the factor parasite and the covariate energy
mod_ancova_no_int <- lm(growth.rate ~ parasite + energy, data = df)
mod_ancova_int <- lm(growth.rate ~ parasite * energy, data = df)

summary(mod_ancova_no_int)
summary(mod_ancova_int)

anova_summary <- anova(mod_ancova_int)
print(anova_summary)

#Use AIC to compare the models of ANOVA, ANCOVA with and without interactions
aic_values <- AIC(mod_aov, mod_ancova_no_int, mod_ancova_int)
aic_values

