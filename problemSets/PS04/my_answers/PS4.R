##### PREAMBLE ----
### Clear global .environment

# Remove all objects in environment
rm(list=ls())
# Detach all loaded libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

### Load packages

# Create function for checking for required packages
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# Load packages: {car}

lapply(c("car", "ggplot2", "stargazer"), pkgTest)

### Set working directory

# Check working directory
getwd()
# Set WD to folder of current R file.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# COnfirm WD now correct
getwd()

##### PROBLEM SET QUESTIONS ----

### Question 1 ----

# Load [Prestige] dataset
data("Prestige")

# Check R documentation for [Prestige] dataset
help("Prestige")
# Check top 5 observations and variable names
head(Prestige, 5)
# Check details about data set type, its dimensions, and
# the names and types for each variable
str(Prestige)
# Gives summary statistics for each variable
summary(Prestige) 

### a) New variable 'professional'

# Check levels of variable "type".
levels(Prestige$type)
# "prof" = professional; "bc" = blue-collar; "wc" = white-collar

# Create new dummy variable "professional"
# where prof = 1 and remaining (bc and wc) = 0.
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
# Coerce as factor to ensure model doe not
# interpret as a continuous numerical variable
Prestige$professional <- as.factor(Prestige$professional)
# Check new variable
Prestige$professional



### b) linear model, y = prestige, x = income, professional, and interaction betw income and professional
# Create linear regression model
model_1 <- lm(prestige ~ income + professional + income:professional, Prestige, na.action = "na.omit")
model_1_table <- summary(model_1)
model_1_table
stargazer(model_1, title = "Regression Table", align = TRUE, out = "model_1_regression_table.tex")

# Plotting Marginal Effect of Income on Prestige
model_1_nas <- lm(prestige ~ income + professional + income:professional, Prestige)
predicted_values <- predict(model_1, type = "response")
Prestige_na <- Prestige[complete.cases(Prestige),]
Prestige_na$predicted <- predicted_values
marginal_effect_plot <- ggplot(Prestige_na, mapping = aes(income, predicted, color = factor(professional))) +
  geom_line(size = 1.5) +
  labs(x = "Income",
       y = "Predicted Prestige",
       color = "Professional") +
  theme_minimal()
ggsave("marginal_effect_plot.png", plot = marginal_effect_plot, dpi = 300)

# See interpretation in LaTeX file.

### c) prediction equation
# Check coefficients
model_1$coefficients

# See equation in LaTeX file.

### d) interpret coefficient for income

# See interpretation in LaTeX file.

### e) Interpret coefficient for professional

# See interpretation in LaTeX file.

### f) effect of $1000 income increase on prestige score for professionals

# Calculate average change in prestige associated with income increase
prof_zero_income <- sum(model_1$coefficients[1], model_1$coefficients[3]*1)
prof_zero_income
prof_1k_income <- sum(model_1$coefficients[1], model_1$coefficients[2]*1000, model_1$coefficients[3]*1, model_1$coefficients[4]*1000*1)
prof_1k_income

# Calculate the marginal (average) effect of $1000
f_prestige <- prof_1k_income - prof_zero_income
f_prestige

### g) effect between non-prof to prof when income is $6000

# Calculate average change in prestige associated with employment change
income_non_prof <- sum(model_1$coefficients[1], model_1$coefficients[2]*6000)
income_non_prof
income_prof <- sum(model_1$coefficients[1], model_1$coefficients[2]*6000, model_1$coefficients[3]*1, model_1$coefficients[4]*6000*1)
income_prof

# Calculate the marginal (average) effect of $6000
g_prestige <- income_prof - income_non_prof
g_prestige



### Question 2 ----

### a) Hypothesis Test: yard signs affects vote share

# Calculate t-statistic
coefficient_a <- 0.042
se_a <- 0.016
n_a <- 30
null_hyp <- 0
t_score_a <- (coefficient_a - null_hyp) / se_a
t_score_a

# Calculate critical value and check
alpha <- 0.05
df_residual_a <- n_a - 2 - 1
critical_value_a <- qt(alpha, df = df_residual_a, lower.tail = FALSE)
critical_a <- ifelse(abs(t_score_a) > critical_value_a, "Greater than critical value", "Less than critical value")
critical_a

# Calculate p-value and check
p_value_a <- 2 * pt(abs(t_score_a), df = df_residual_a, lower.tail = FALSE)
null_hyp_a <- ifelse(p_value_a < alpha, "Less than 0.05", "Greater than 0.05")
null_hyp_a

# See interpretation in LaTeX file.

### b) hypothesis test: adjacent to yard signs affects vote share
# Calculate t-statistic
coefficient_b <- 0.042
se_b <- 0.013
n_b <- 76
t_score_b <- (coefficient_b - null_hyp) / se_b
t_score_b

# Calculate critical value and check
df_residual_b <- n_b - 2 - 1
critical_value_b <- qt(alpha, df = df_residual_b, lower.tail = FALSE)
critical_b <- ifelse(abs(t_score_b) > critical_value_b, "Greater than critical value", "Less than critical value")
critical_b

# Calculate p-value and check
p_value_b <- 2 * pt(abs(t_score_b), df = df_residual_b, lower.tail = FALSE)
null_hyp_b <- ifelse(p_value_b < alpha, "Less than 0.05", "Greater than 0.05")
null_hyp_b

# See interpretation in LaTeX file.

### c) interpret the coefficient for the constant term

# See interpretation in LaTeX file.

### d) evaluate model fit $R^2$ . What say about importance of yard signs vs other factors not included?