##### Preamble ----
# load libraries
# set wd
# clear global .envir

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

# Load packages: ggplot2

lapply(c("ggplot2", "ggthemes", "stargazer", "car"), pkgTest)

# ex: stringr
# lapply(c("stringr"),  pkgTest)

# Check working directory
getwd()
# If necessary, set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# COnfirm wd now correct
getwd()

##### Data Preparation  ----

# Read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")

# Familiarise with CSV data
typeof(inc.sub) # check type
head(inc.sub, 5) # Checking first 5 observations and their values
colnames(inc.sub) # checking column names for which are relevant
dim(inc.sub) # Find number of rows (observations) and columns (variables)

# Extract relevant data
df <- inc.sub[, c("difflog", "voteshare", "presvote")]

# Review extracted data, confirm correct
colnames(df) # checking column names
head(df, 5) # Checking first 5 observations and their values
dim(df) # number of rows (observations) and columns (variables)



### Question 1 ----

### 1. Regression w/ explanatory = difflog, outcome = voteshare

# Assign model and inspect data through summary
diff_vs_share <- lm(voteshare ~ difflog, data = df)
summary(diff_vs_share)
# Create and print LaTeX code for manual copy-and-paste.
LaTeX_Q1 <- stargazer(diff_vs_share, title = "Linear Regression Results: Positive Correlation between Vote Share of Incumbent and Difference", align = TRUE, type = "latex")
cat(LaTeX_Q1)

# See interpretation in LaTeX file.

### 2. Scatterplot w/ explanatory = difflog, outcome = voteshare + Regression line
pdf("plot_Q1.pdf")
# Set dataframe and axes for scatter plot
ggplot(df, aes(x = difflog, y = voteshare)) +
  # Set point symbol and size
  geom_point(pch = 1, size = 3, color = "navy") +
  # Set regression line details
  geom_smooth(data = diff_vs_share, method = "lm", se = FALSE, color = "blue", size = 3) +
  # Add axes labels, exclude main title
  labs(
    # Will add main = "Scatterplot and Regression Line: Positive Correlation between Vote Share of Incumbent and Difference in Spending" in LaTeX
    x = "Difference in Spending betweeen Incumbent and Challenger",
    y = "Vote Share of Incumbent") +
  # Set theme "par" and axis text formatting
  theme_par() + 
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))
# Ensure graphics instructions end
dev.off()

### 3. Save residuals

# Assign residuals
residuals_diff_vs_share <- diff_vs_share$residuals
# Inspect output
head(residuals_diff_vs_share, 10)
typeof(residuals_diff_vs_share)
length(residuals_diff_vs_share) == dim(inc.sub)[1]

### 4. Write the prediction equation

equation_diff_vs_share <- paste("y =", diff_vs_share$coefficients[1], "+ (", diff_vs_share$coefficients[2], "* Xi )")
# Check output correct
equation_diff_vs_share

# Note maximum and minimum values of difference in spending between the incumbent and the challenger.
max(df$difflog)
min(df$difflog)

# See formal notation in LaTeX



### Question 2 ----

### 1. Regression w/ explanatory = difflog, outcome = presvote

# Assign model and inspect through summary
diff_vs_presvote <- lm(presvote ~ difflog, data = df)
summary(diff_vs_presvote)

# Create and print LaTeX code for manual copy-and-paste.
LaTeX_Q2 <- stargazer(diff_vs_presvote, title = "Linear Regression Results:
                      Positive Correlation between Vote Share of Presidential
                      Candidate of Incumbent's Party and Difference in Spending",
                      align = TRUE,
                      type = "latex")
cat(LaTeX_Q2)

# See interpretation in LaTeX file.

### 2. Scatterplot w/ explanatory = difflog, outcome = presvote + Regression line

pdf("plot_Q2.pdf")
# Set dataframe and axes for scatter plot
ggplot(df, aes(x = difflog, y = presvote)) +
  # Set point symbol and size
  geom_point(pch = 2, size = 2, color = "maroon") +
  # Set regression line details
  geom_smooth(data = diff_vs_presvote, method = "lm", se = FALSE, color = "red", size = 3) +
  # Add axes labels, exclude main title
  labs(
    # Will add main = "Scatterplot and Regression Line: Positive Correlation between Vote Share of Presidential Candidate of Incumbent's Party and Difference in Spending" in LaTeX
    x = "Difference in Spending betweeen Incumbent and Challenger",
    y = "Vote Share of Presidential Candidate \nof Incumbent's Party") +
  # Set theme "par" and axis text formatting
  theme_par() + 
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))
# Ensure graphics instructions end
dev.off()

### 3. Save residuals

# Assign residuals
residuals_diff_vs_presvote <- diff_vs_presvote$residuals
# Check output
head(residuals_diff_vs_presvote, 10)
typeof(residuals_diff_vs_presvote)
length(residuals_diff_vs_presvote) == dim(inc.sub)[1]


### 4. Write the prediction equation

equation_diff_vs_presvote <- paste("y =", diff_vs_presvote$coefficients[1], "+ (", diff_vs_presvote$coefficients[2], "* Xi )")
# Check output correct
equation_diff_vs_presvote

# See formal notation in LaTeX



### Question 3 ----

### 1. Regression w/ explanatory = presvote, outcome = voteshare

# Assign model and inspect through summary
presvote_vs_share <- lm(voteshare ~ presvote, data = df)
summary(presvote_vs_share)

# Create and print LaTeX code for manual copy-and-paste.
LaTeX_Q3 <- stargazer(presvote_vs_share, title = "Linear Regression Results: Positive Correlation\\
                      between Vote Share of Presidential Candidate of Incumbent's Party\\
                      and Vote Share of Incumbent",
                      align = TRUE,
                      type = "latex")
cat(LaTeX_Q3)

# See interpretation in LaTeX file.

### 2. Scatterplot w/ explanatory = presvote, outcome = voteshare + Regression line

pdf("plot_Q3.pdf")
# Set dataframe and axes for scatter plot
ggplot(df, aes(x = presvote, y = voteshare)) +
  # Set point symbol and size
  geom_point(pch = 3, size = 3, color = "brown") +
  # Set regression line details
  geom_smooth(data = presvote_vs_share, method = "lm", se = FALSE, color = "orange", size = 3) +
  # Add axes labels, exclude main title
  labs(
    # Will add main = "Scatterplot and Regression Line: Positive Correlation between Vote Share of Presidential Candidate of Incumbent's Party and Vote Share of Incumbent" in LaTeX
    x = "Vote Share of Presidential Candidate \nof Incumbent's Party",
    y = "Vote Share of Incumbent") +
  # Set theme "par" and axis text formatting
  theme_par() + 
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))
# Ensure graphics instructions end
dev.off()

### 3. Write the prediction equation

equation_presvote_vs_share <- paste("y =", presvote_vs_share$coefficients[1], "+ (", presvote_vs_share$coefficients[2], "* Xi )")
# Check output correct
equation_diff_vs_share

# Note maximum and minimum values of vote share for the incumbent party's presidential candidate.
max(df$presvote)
min(df$presvote)

# See formal notation on LaTeX file.



### Question 4 ----

### 1. Regression w/ explanatory = Q1 residuals, outcome = Q2 residuals

# Assign model and inspect through summary
not_diff_share_vs_presvote <- lm(residuals_diff_vs_share ~ residuals_diff_vs_presvote)
summary(not_diff_share_vs_presvote)

# Create and print LaTeX code for manual copy-and-paste.
LaTeX_Q4 <- stargazer(not_diff_share_vs_presvote, title = "Linear Regression Results: Positive Correlation\\
                      between Variation in Vote Share of Presidential Candidate of Incumbent's Party\\
                      and Variation in Vote Share of Incumbent\\
                      NOT Correlated with Variation in Difference in Spending",
                      align = TRUE,
                      type = "latex")
cat(LaTeX_Q4)

# See interpretation in LaTeX file.

### 2. Scatterplot w/ explanatory = Q1 residuals, outcome = Q2 residuals + Regression line

pdf("plot_Q4.pdf")
# Set dataframe and axes for scatter plot
ggplot(df, aes(x = presvote, y = voteshare)) +
  # Set point symbol and size
  geom_point(pch = 5, size = 2, color = "darkgreen") +
  # Set regression line details
  geom_smooth(data = presvote_vs_share, method = "lm", se = FALSE, color = "green", size = 3) +
  # Add axes labels, exclude main title
  labs(
    # Will add main title in LaTeX
    x = "Residuals from Question 2:\nVariation in Presidential Candidate \nof Incumbent's Party Vote Share \nNOT Correlated with Variation in \nSpending Difference",
    y = "Residuals from Question 1:\nVariation in Incumbent Vote Share \nNOT Correlated with Variation in \nSpending Difference") +
  # Set theme "par" and axis text formatting
  theme_par() + 
  theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))
# Ensure graphics instructions end
dev.off()

### 3. Write the prediction equation

equation_not_diff_share_vs_presvote <- paste("y =",
                                             not_diff_share_vs_presvote$coefficients[1],
                                             "+ (",
                                             not_diff_share_vs_presvote$coefficients[2],
                                             "* Xi )")
# Check output correct
equation_not_diff_share_vs_presvote

# See formal notation on LaTeX file.



### Question 5 ----

### 1. Regression w/ explanatory = difflog + presvote, outcome = voteshare

# Assign model and inspect data through summary
diff_and_presvote_vs_share <- lm(voteshare ~ difflog + presvote, data = df)
summary(diff_and_presvote_vs_share)

# Create and print LaTeX code for manual copy-and-paste.
LaTeX_Q5 <- stargazer(diff_and_presvote_vs_share, title = "Multi-Variate Linear Regression Results",
                      align = TRUE,
                      type = "latex")
cat(LaTeX_Q5)

### 2. Write the prediction equation

equation_diff_and_presvote_vs_share <- paste("y =",
                                             diff_and_presvote_vs_share$coefficients[1],
                                             "+ (",
                                             diff_and_presvote_vs_share$coefficients[2],
                                             "* Xi ) + (",
                                             diff_and_presvote_vs_share$coefficients[3],
                                             "* Xii )")
# Check output correct
equation_diff_and_presvote_vs_share

# See formal notation on LaTeX file.

### 3. Which outputs identical between Q4 and Q5? Why are they identical?

# Creating Added Variable Plots
pdf("plot_Q5.pdf", height = 8)
avPlots(diff_and_presvote_vs_share, 
        main = "", # Will add "main = " title in LaTeX
        layout = c(2,1),
        pch = 9,
        col.lines = "purple")
dev.off()