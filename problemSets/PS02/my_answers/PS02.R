#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}


# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("dplyr"),  pkgTest)
lapply(c("stargazer"),  pkgTest)

#####################
# Problem 1: Political Science
#####################

### a: Calculate the χ2 (i.e. chi-squared) test statistic by hand/manually ----

# In order to perform the χ2 test, I assume a) both variables are categorical, and b) both are nominal (i.e. not ranked, in contrast to ordinal which means ranked/ordered).

# The χ2 test compares the observed distribution of variable observations with the expected distribution IF the predictor variable has no relationship with the effect/outcome variable (i.e. the null hypothesis). A statistically significant difference gives evidence to reject the null hypothesis i.e. that the variables are independent.

# First, calculate the totals for observations (rows)

upper_class_observed <- 14 + 6 + 7
lower_class_observed <- 7 + 7 + 1
grand_total_class <- upper_class_observed + lower_class_observed

# Second,  calculate the totals for outcome variables (columns)
not_stopped <- 14 + 7
bribe_requested <- 6 + 7
stopped_or_warning <- 7 + 1

# Third, calculate expected observations
upper_class_expected_not_stopped <- (upper_class_observed/grand_total_class)*not_stopped
lower_class_expected_not_stopped <- (lower_class_observed/grand_total_class)*not_stopped

upper_class_expected_bribe_requested <- (upper_class_observed/grand_total_class)*bribe_requested
lower_class_expected_bribe_requested <- (lower_class_observed/grand_total_class)*bribe_requested

upper_class_expected_stopped_or_warning <- (upper_class_observed/grand_total_class)*stopped_or_warning
lower_class_expected_stopped_or_warning <- (lower_class_observed/grand_total_class)*stopped_or_warning

# Fourth, calculate the sum of the squared differences over expected values to find the χ2 test. Note that for negative numbers, I had to swap the sqrt() function with ^2 to avoid NaN warnings.

chi_squared_statistic <- sum(
  sqrt(14 - upper_class_expected_not_stopped)/upper_class_expected_not_stopped,
  (7 - lower_class_expected_not_stopped)^2/lower_class_expected_not_stopped,
  (6 - upper_class_expected_bribe_requested)^2/upper_class_expected_bribe_requested,
  sqrt(7 - lower_class_expected_bribe_requested)/lower_class_expected_bribe_requested,
  sqrt(7 - upper_class_expected_stopped_or_warning)/upper_class_expected_stopped_or_warning,
  (1 - lower_class_expected_stopped_or_warning)^2/lower_class_expected_stopped_or_warning
)
chi_squared_statistic

### b: Now calculate the p-value. What do you conclude if α = 0.1? ----

# Calculate the degrees of freedom, which is the product of the number of rows (minus 1) and number of columns (minus 1). I choose the upper tail as it is a chi-squared hypothesis test.

df <- (2-1)*(3-1)
p <- pchisq(chi_squared_statistic, df = df, lower.tail = FALSE)
p

# p > 0.1, therefore not providing evidence to reject the null hypothesis (that the outcome of a police stop after an illegal left turn is independent of the class of the driver).

### c: Calculate the standardized residuals for each cell and put them in the table below. ----

# The standarised residual is the difference between the observed and the expected counts, divided by the square root of expected count. Note I have rounded each value to 2 decimal places.

sr_upper_class_not_stopped <- round((14 - upper_class_expected_not_stopped) / sqrt(upper_class_expected_not_stopped), 2)
sr_lower_class_not_stopped <- round((7 - lower_class_expected_not_stopped) / sqrt(lower_class_expected_not_stopped), 2)

sr_upper_class_bribe_requested <- round((6 - upper_class_expected_bribe_requested) / sqrt(upper_class_expected_bribe_requested), 2)
sr_lower_class_bribe_requested <- round((7 - lower_class_expected_bribe_requested) / sqrt(lower_class_expected_bribe_requested), 2)

sr_upper_class_stopped_or_warning <- round((7 - upper_class_expected_stopped_or_warning) / sqrt(upper_class_expected_stopped_or_warning), 2)
sr_lower_class_stopped_or_warning <- round((1 - lower_class_expected_stopped_or_warning) / sqrt(lower_class_expected_stopped_or_warning), 2)

# <table inserted in LaTeX here>

### d: How might the standardized residuals help you interpret the results? ----

# Standardised residuals measure the strength of the difference between observed and expected values. I note two details: the positive and negative values largely cancel each other out, and also each value is relatively small (less than +/- 1.1). These both indicate that the differences are so minor that it would require a large n-size for such results to be statistically significant.

#####################
# Problem 2: Economics
#####################

### a: State a null and alternative (two-tailed) hypothesis. ----

# $H_0$: The number of new or repaired drinking water facilities is identical in villages with a reservation policy and without a reservation policy: $water_1 = water_0$
# $H_a$: The number of new or repaired drinking water facilities is different in villages with a reservation policy and without a reservation policy: $water-1 \neq water_0$

### b: Run a bivariate regression to test this hypothesis. ----

# I extract the data
WB_df <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

# I create a linear regression model with the presence of a reservation policy (0 denotes no policy, 1 denotes  policy present)
model <- lm(water ~ reserved, data = WB_df)
summary(model)

# I then print this for input into LaTeX
stargazer(model)

### c: Interpret the coefficient estimate for reservation policy. ----

# The linear regression indicates an average increase of 9.252 drinking water facilities for villages which have a reservation policy compared to villages which don't. Further, p < 0.05, indicating this result is statistically significant.

# In other words, I have evidence to reject the null hypothesis (the number of new and repaired facilities in villages with and without a reservation policy is the same) and I have evidence supporting the alternative hypothesis (the number of facilities in villages with the reservation policy is different to the number in villages without the reservation policy).