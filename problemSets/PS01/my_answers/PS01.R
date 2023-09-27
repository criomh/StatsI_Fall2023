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


#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
