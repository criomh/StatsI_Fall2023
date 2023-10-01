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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

# Load up data (pre-existing)
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# Q1: Find 90% confidence interval for the average student IQ in the school

# This question is asking to use statistics from a sample (25 random students) to estimate population parameters (the whole school) - in this case, a confidence interval, which is a range of numbers within which the parameter is believed to fall 90\% of the time with repeated sampling. This requires us to assume a normal sampling distribution.

# First I find the point estimate - or sample mean - which is the sum of all the observation values divided by the total number of observations. I also find the sample's n-size. 
n_y <- length(y)
mean(y) == sum(y)/n_y # is TRUE
mean_y <- mean(y)

# Now I find the sample standard deviation - which is a measure of the spread/dispersion within the sample. This is calculated by first calculating the variance (the sum of the squared distances of each observation from the sample mean (ensuring observations above and below the mean don't cancel out) divided by the total number of observations less one ie n-1). The SD is the square root of the variance, returning the value from squared to original (and more easily comparable) units. I further divide this by the square root of total observations to find the standard error for the sampling distribution ie an estimator for the deviation of sample means with repeated sampling).

sqrt(var(y)) == sd(y) # is TRUE
sd_y <- sd(y)
se_y <- sd_y/sqrt(n_y)

# I now define the corresponding p-value for 90\% confidence. Will the calculated values, I can manually approximate the 90\% confidence intervals by multiplying the corresponding Z-score (aka critical value) to the alpha value for 90\% confidence (ie 0.1 including both tails of the distribution, or 0.05 each tail) with the standard error, and then adding/subtracting this figure from the sample mean. In other words, I am adding/subtracting the 'margin for error' to calculate the confidence intervals.

p = (1-0.9)
z_score <- qnorm(p/2, lower.tail = FALSE) # finds corresponding Z-score to the p-value 0.05.
lower_90 <- mean_y - z_score*se_y
upper_90 <- mean_y + z_score*se_y

# Alternatively, I can use the qnorm() function to calculate the lower and upper values. This can be more precise if some figures such as z-score or standard deviation/error have been rounded.

lower_90 == qnorm(p/2, mean = mean_y, sd = se_y) # TRUE confirms the values are the same 
upper_90 == qnorm(1-(p/2), mean = mean_y, sd = se_y) # TRUE confirms the values are the same

# These bounds indicate that, with 90\% confidence with repeated sampling, the population mean (ie the school average IQ) is within the values 94.13 and 102.75 (rounded to two decimal places). 

# Q2:  Next, the school counselor was curious whether the average student IQ in
# her school is higher than the average IQ score (100) among all the schools in
# the country. Using the same sample, conduct the appropriate hypothesis test
# with α = 0.05.

# This question requires a significance test for a mean using the t.test() function. I do not specify alpha as the default is 0.05. I specity the alternarive "greater" to ensure the test is directional and in the correct direction.

# H0: The average IQ score among all schools (mean\_all) is NOT less than or equal to the average IQ score of our sample (mean\_y).

# HA: The average IQ score among all schools (mean\_all) IS INDEED less than or equal to the sample (mean\_y).

mean_all <- 100
t_test_result <- t.test(x = y, mu = mean_all, alternative = "greater")

# I install and load the "stargazer" package to enable outputting the t-test results to LaTeX.
install.packages("stargazer")
library(stargazer)

# This creates the function necessary for outputting correctly.
output_stargazer <- function(outputFile, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=outputFile, append=TRUE)
}

# I use the funciton to output the results into a readable file for LaTeX.
output_stargazer("t_test_result.tex", t_test_result)

# The result indicates that p > 0.05, failing to reject the null hypothesis. In full, this means I cannot reject the null hypothesis that the counsellor's school's average IQ score is higher than the average IQ score for all schools.


#####################
# Problem 2
#####################

# Explore the expenditure data set and import data into R.

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)

str(expenditure)
summary(expenditure)
attributes(expenditure)

# Plotting relationships between 4 numeric variables - Y, X1, X2, and X3

### ------
### P1: Please plot the relationship between Y and Region. On average, which region has the highest per capita expenditure on housing assistance?

# I plot the variables accordingly. Most plots appear to indicate some positive correlation between the two variables. I highlight the correlations between Y and X1, as well as X1 and X2 as being relatively clear. Meanwhile, the (positive) relationship between Y and X3 appears relatively weak or non-existent. I use the png() function to print the plots.

png(filename="scatter_y_x1_x2_x3.png")
plot(expenditure[c("Y","X1","X2","X3")],
     main = "Correlations between Y, X1, X2, and X3")
dev.off()
  
### ------  
### P2: Please plot the relationships among \emph{Y}, \emph{X1}, \emph{X2}, and \emph{X3}? What are the correlations among them (you just need to describe the graph and the relationships among them)?

# First I need to coerce the variable "Region" from integer type to factor type, and then rename the levels to the region names for clarity.

expenditure$Region <- as.factor(expenditure$Region)
levels(expenditure$Region) <- c("Northeast", "North Central", "South", "West")

# Now I create a box plot showing expenditure on shelters/housing assistance across region. I then export it as a png.

png(filename="boxplot_Expenditure_by_Region.png")
Expenditure_by_Region <- boxplot(expenditure$Y ~ expenditure$Region,
        main = "Boxplot of expenditure on shelters/housing assistance",
        ylab = "Dollars per capita",
        xlab = "Region")
dev.off()

# The thick black horizontal bars indicate the mean average for each region, indicating the West region has the highest average per capita spending on shlters/housing assistance.

### -----
### P3:  Plot Y and X1. Describe graph and relationship. Reproduce the above graph including one more variable Region and display diff regions with diff symbols and colours.

# I produce the first graph, adding a title and axis labels for clarity. The scatter plot indicates a positive relationship between state expenditure on shelters/housing assistance, and personal incomes. I print this first graph as a png.

# Using the function cor(), I find a 0.53 correlation coefficient, indicating a positive correlation and supporting with my visual assessment.

png(filename="expenditure_by_personal_income.png")
plot(expenditure$Y, expenditure$X1,
     main = "Expenditure on shelters/housing assistance by personal income",
     ylab = "State expenditure per capita",
     xlab = "Personal Income per capita")
dev.off()
cor(expenditure$Y, expenditure$X1)

# Now I reproduce the plot including a legend, with Region indicated by unique symbols and colours.

# Visual indications suggests North Central-states cluster (aka are more similar) with higher state spending AND personal incomes compared to South-states. Meanwhile, West states may have variation in state expenditure compared to personal incomes. No clear associations are indicated for Northeast-states.  

png(file="expenditure_by_personal_income_with_legend.png")
plot(expenditure$Y, expenditure$X1,
     main = "Expenditure on shelters/housing assistance by personal income",
     ylab = "State expenditure per capita",
     xlab = "Personal Income per capita",
     col = expenditure$Region,
     pch = c(1,2,3,4)[expenditure$Region])
legend(x = "bottomright",
       legend = levels(expenditure$Region),
       col = c(1,2,3,4),
       pch = c(1,2,3,4)
       )
dev.off()


## misc












# Note the sample has a relatively small n-size of n = 25 (compared to standard practice for validating the central limit theorem of n = 30). Therefore, I will be using the t-test distribution and so the critical value used will be the t-value. Also note I am comparing a sample statistic to a population parameter, and so the corresponding degrees of freedom is calculated as n-1.

df_y <- n_y - 1


# Note the sample has a relatively small n-size of n = 25, compared to standard threshold of n = 30.


# As stated in Logan (n.d.), a confidence interval is "the interval that contains
# the population parameter with probability 1 - α." In other words, the CI is the
# range of values within which the relevant population statistic is expected to sit
# within - to the stated level of certainty. This is calculated using values from
# a randomly-selected sample.

# If calculating manually for the t-test method, this requires finding the sample
# mean, it's standard deviation and then standard error for this mean, the
# corresponding t-score for the chosen confidence level, and the margin of error.
# Link: Logan (n.d.) https://bookdown.org/logan_kelly/r_practice/p09.html 

# There are two ways of finding the 90% confidence interval - firstly, through
# using the t-distribution and manually calculating each component value before
# inputting these into the qt() function:

# Find the sample mean ie the central tendency
mean_y <- mean(y)

# Find the standard error for the population statistic
n_y <- length(y) # find the n-size of the sample
standard_deviation_y <- sd(y) # also possible to calculate this manually by finding
# the sample variability using var() and finding its squareroot ie sqrt(var(y))  
standard_error_for_y <- standard_deviation_y/sqrt(n_y)

# Find the corresponding t-score for 90% confidence interval - we are not assuming
# direction, and so we are using a two-tail test
confidence_interval <- 0.1
degrees_of_freedom <- n_y - 1
t_score <- qt(p = confidence_interval/2, df = degrees_of_freedom, lower.tail = FALSE) # where
# p is the vector of probabilities for each tail, df is one less the N-size as we
# exclude the mean_y value, and we indicate we are using two-tail test

# Find the margin of error
margin_of_error_y <- t_score * standard_error_for_y

# Find the confidence interval
lower_bound <- mean_y - margin_of_error_y
upper_bound <- mean_y + margin_of_error_y

# Print the 90% confidence intervals and sample y mean, using round() to give
# each value to two decimal places.
CI_values <- c(round(lower_bound, 2), round(mean_y, 2), round(upper_bound, 2))
names(CI_values) <- c("lower-bound", "sample mean", "upper-bound")
print(CI_values)

# The same can be approximated using Z-scores instead. The corresponding Z-score
# to the 90% confidence interval is 1.64.

lower_bound_z <- mean_y - (1.64 * standard_deviation_y / sqrt(n_y))
upper_bound_z <- mean_y + (1.64 * standard_deviation_y / sqrt(n_y))
# And now printing these values
CI_values_z <- c(round(lower_bound_z, 2), round(mean_y, 2), round(upper_bound_z, 2))
names(CI_values_z) <- c("lower-bound", "sample mean", "upper-bound")
print(CI_values_z)

# Note the values are similar, though the approximated confidence interval is
# slightly reduced (due to the Z-score slightly under-shooting the true 90% threshold)


# Q2:  Next, the school counselor was curious whether the average student IQ in
# her school is higher than the average IQ score (100) among all the schools in
# the country. Using the same sample, conduct the appropriate hypothesis test
# with α = 0.05.

# Now we are conducting a one-tail significance test for the difference between two
# means in order to reject the null hypothesis that mean_y is equal to or lower than
# mean_population

mean_population <- 100
t.test(x = y, mu = mean_population, alternative = "greater")

# The result indicates that p > 0.05, failing to reject the null hypothesis ie
# we fail to reject that the class's average IQ score is equal to or lower than
# the average IG score among all schools in the country.
