# SIX SIGMA - HYPOTHESIS TESTING
# Coursera Guided Project
# Instructor - Moses Gummadi

# This is an R Script file
# You can open this in R Studio
# Use # symbol for comments in R Script file
# The script file contains code and comments
# You can run the code by typing it next to ">" symbol
#   in the Console windown (bottom left)
# You can also run the code by selecting the code in
#   the script file and hit "Run" in the top-left pane

# R Studio is already downloaded in your Rhyme Desktop
# To download on your own desktop
# Use this link https://rstudio.com/products/rstudio/
# Choose the RStudio Desktop Free Option

# In this Guided Project You Will Learn
# Hypothesis Testing Using RStudio
#
# What Is A Hypothesis?

mtcars             

plot(mtcars$disp, mtcars$mpg, col = "blue", pch =16)

plot(mtcars$gear, mtcars$mpg, col = "blue", pch =16)

# Approach:
#    1. Identify Independent (X) and Dependent (Y) Variables
#    2. Determine Data Type of X and Y (Continuous or Discrete)
#    3. Choose the Hypothesis Test from the Roadmap
#    4. Propose the Null & Alternate Hypothesis
#       (Default Null & Alternate Hypothesis Used Here)
#    5. Interpret the results using p-value
#    6. Reject Null Hypothesis when p <= 0.05
#
# Hypothesis Testing Roadmap
#
# Cont X, Cont Y : Correlation, Simple Regression
# Cont X, Disc Y : Logistic Regression
# Disc X, Disc Y : Chi-Square Test
# Disc X, Cont Y : Compare Groups:
#         A. Study Stability (For Time Ordered)
#            Note: Exit If Process Is Not Stable Over Time 
#         B. Study Shape  - Normal vs Non-Normal Data
#         C. Study Spread - Equal vs Unequal Variance 
#         D  Study Centering - Based on B and C
#            1 Group:   Null Hyp: Mean / Median equal to Reference Value
#            2 Groups:  Null Hyp: 2 groups have equal Means/ Medians
#            3+ Groups: Null Hyp: 3+ groups have equal Means/ Medians
#
#         Various Tests Under Disc X, Cont Y:
#            1-Sample t-Test
#            2-Sample t-Test
#            1-Way ANOVA
#            Mood's Median Test 
#            Mann-Whitney (Wilcoxon Rank Sum) 
#            Kruskal Wallis
#

browseURL("https://pasteboard.co/JIpCqVQ.png")

#----------------------------------------------------------

# Load Libraries & Functions
source("https://pastebin.com/raw/d1UpgBxi") 

# Task 1 - (Continuous X, Continuous Y) ----------------
# Scatterplot, Correlation, Simple Regression --------

dt = fread("ContX vs ContY.csv")
head(dt)

x = dt$EngineSize
y = dt$Mpg

# Check Data Type
class(x) ; class(y)
summary(x) ; summary(y)

# Scatter Plot
graphics.off()
plot(x, y, col = "blue", pch = 16,
     xlab = "Engine Size", 
     ylab = "Miles Per Gallon")

# Correlation Test
cor(x, y , method = "pearson")

# Linear Regression for Hypothesis Testing
#
# Null Hypothesis - No Relationship between X and Y
# Alternate Hypothesis - X and Y are related
# When p <= 0.05, Reject Null Hypothesis

model <- lm(y ~ x)
summary(model) # Coeffs, R-Sq, etc

# Exercise - Linear Regression for X = Car Height, Y = Mpg

dt = fread("ContX vs ContY.csv")

x = dt$CarHeight

#++++++++++++++++++++++++++++++Solution+++++++++++++++++++++++++++

model <- lm(y ~ x)
summary(model) # Coeffs, R-Sq, etc

# *** Advanced Topics ***

dt = fread("ContX vs ContY.csv")

x = dt$EngineSize
y = dt$Mpg

model <- lm(y ~ x)
summary(model) # Coeffs, R-Sq, etc

# Plot Regression Statistics
graphics.off()
layout(matrix(c(1,2,3,4),2,2)) 
plot(model, pch = 16, col = "blue") 

# Residuals vs Fitted
# Should randomly spread around 0 line
# No trend in average and spread

# Normal Q-Q - checks normal distribution of residuals

# Plot - Fits, Confidence Levels, Prediction Intervals
graphics.off()
x0 = seq(min(x), max(x), length = 20)
outp = as.data.frame(predict(model, newdata = list(x = x0), interval = "prediction"))
outc = as.data.frame(predict(model, newdata = list(x = x0), interval = "confidence"))
graphics.off()
plot(x,y, col = "blue", pch = 16)     ## plot x-y scatter (blue)
lines(x0, outp$fit, lwd = 2)  
lines(x0, outp$lwr, col = "red",   lwd = 1.5)  
lines(x0, outp$upr, col = "red",   lwd = 1.5 ) 
lines(x0, outc$lwr, lwd = 1, lty = 2 )  
lines(x0, outc$upr, lwd = 1, lty = 2 ) 

# Predict values
x1 = c(1339, 2499, 3499)
predict(model, data.frame(x = x1), interval =  "prediction")
predict(model, data.frame(x = x1), interval =  "confidence")



# *********************************************************
# Task 2 - Logistic Regression (Continuous X, Discrete Y) -----------

dt = fread("ContX vs DiscY.csv")
head(dt)

x = dt$HrsStudy
y = dt$ExamResult

# Null Hypothesis - No Relationship between X and Y
# Alternate Hypothesis - X and Y are related
# When p <= 0.05, Reject Null Hypothesis

result = glm(y ~ x, family = "binomial")
summary(result)

# Logistic Regression Plot

graphics.off()
plot(x, y, col = "blue", pch = 16)
par(new = TRUE)
plot(sort(x), sort(result$fitted.values), type = "l", col="red", lwd=2, xlab = "", ylab = "", axes = F)
axis(side = 4)

# Exercise - Run the code for PrevGrade as x, and ExamResult2 as y


dt = fread("ContX vs DiscY.csv")

x = dt$PrevGrade
y = dt$ExamResult

#++++++++++++++++++++++++++++++Solution+++++++++++++++++++++++++++
result = glm(y ~ x, family = "binomial")
summary(result)

# Logistic Regression Plot

graphics.off()
plot(x, y, col = "blue", pch = 16)
par(new = TRUE)
plot(sort(x), sort(result$fitted.values), type = "l", col="red", lwd=2, xlab = "", ylab = "", axes = F)
axis(side = 4)

# *********************************************************
# Task 3 - Chi-Sq Test (Discrete X, Discrete Y) -------
dt = fread("DiscX vs DiscY.csv")
head(dt)

x = dt$PropType
y = dt$Age

unique(x)
unique(y)

# Cross Tabulation
table(x, y)

# Null Hypothesis - No Relationship between X and Y
# Alternate Hypothesis - X and Y are related
# When p <= 0.05, Reject Null Hypothesis

# Chi-Square Test
summary(table(x, y))
chisq.test(table(x, y))

# Exercise - use dataset dt1 (excluding Flats) 
#            run Chi-Sq test for x = PropType, y = Age

dt = fread("DiscX vs DiscY.csv")
dt = droplevels(subset(dt, PropType != "Flat"))
#++++++++++++++++Solution+++++++++++++++++++++++++++
# Chi-Square Test

x = dt$PropType
y = dt$Age
summary(table(x, y))
chisq.test(table(x, y))


# ****************************************************************
# Task 4 - Compare Groups (Discrete X, Continuous Y) -------
dt = fread("DiscX vs ContY.csv")
head(dt)

# Before hypothesis testing, we need to do these 3 things:
# 1. Analyse STABILITY  - if time ordered
# 2. Analyse the SHAPE of the data (normality)
# 3. Analyse the SPREAD of the data (variance)

dt = dt[order(dt$TransferDate),] # sort by date

y1 = subset(dt, Age == "PreOwned")$Price
y2 = subset(dt, Age == "NewBuild")$Price


# 4-1 Analyse Stability
out = XMRChartX(y1, label = "Price")
out = XMRChartX(y2, label = "Price")


# Western Electric Rules for Stability
browseURL("https://pasteboard.co/JDdF5q6.png")

#
#================================ Control Limit
#        Zone A
#--------------------------------
#        Zone B
#--------------------------------
#        Zone C
#================================ Centre Line
#        Zone C
#--------------------------------
#        Zone B
#--------------------------------
#        Zone A
#================================ Control Limit
#
# Rule 1: Any point beyond Zone A (Control Limits)
# Rule 2: 2 out of 3 consecutive points in Zone A or beyond
# Rule 3: 4 out of 5 consecutive points in Zone B or beyond
# Rule 4: 9 consecutive points on the same side of Centre Line


# 4-2 Analyse SHAPE - Histogram, Normality, 

hist(y1, col = "gold2")  
hist(y2, col = "gold2")  

# Anderson Darling Normality - Needs "nortest" library (loaded)
ad.test(y1)                       # p > 0.05 for Normality
ad.test(y2)                       # p > 0.05 for Normality

# 4-3 Analyse SPREAD -Check Variances 
      # Homogeneity of Variance (Bartlett's Test)
## H0 = Equal variances
## When p<0.05, Reject H0
bartlett.test(Price ~ Age, data = dt)


# Exercise - Control Chart, Normality Test, Bartlett's Test
#            For Y = Price and X = Duration (Leasehold vs Freehold)
#

dt = fread("DiscX vs ContY.csv")
dt = dt[order(dt$TransferDate),] # sort by date

y1 = subset(dt, Duration == "Leasehold")$Price
y2 = subset(dt, Duration == "Freehold")$Price

#++++++++++++Solution+++++++++++++

# 1st step Analyse Stability
out = XMRChartX(y1, label = "Price")
out = XMRChartX(y2, label = "Price")

## OBSERVATION: y1 violates couple of instances of rule 2 and rule 4


# 4-2 Analyse SHAPE - Histogram, Normality, 

hist(y1, col = "gold2")  
hist(y2, col = "gold2")  

# Anderson Darling Normality - Needs "nortest" library (loaded)
ad.test(y1)                       # p > 0.05 for Normality
ad.test(y2)                       # p > 0.05 for Normality

## OBSERVATION: Not normal as p values are 1.37e-5, 0.03877


# 4-3 Analyse SPREAD -Check Variances 
# Homogeneity of Variance (Bartlett's Test)
## H0 = Equal variances
## When p<0.05, Reject H0
bartlett.test(Price ~ Duration, data = dt)

## OBSERVATION: p value 0.27 , so fail to reject





# ****************************************************************
# Task 5 - 1 Sample t-Test, 2 Sample t-Test, ANOVA (Cont Y, Disc X)
#         Non-parametric tests (Wilcoxon, Moods-Median, Kruskal)               
#
#  Test only if time-ordered data is stable (control chart)


dt = fread("DiscX vs ContY.csv")

y1 = subset(dt, Age == "PreOwned")$Price
y2 = subset(dt, Age == "NewBuild")$Price


# Single sample compared to reference (1 Sample t-Test)
#
# Null Hypothesis - Reference Mean is within the range
#                   of Statistically Estimated Mean
# Alternate Hypothesis - Reference Mean is outside the true range
# When p <= 0.05, Reject Null Hypothesis.

mean(y1)
t.test(y1, mu = 450000) 

# Check Normality
ad.test(y1)                       # p > 0.05 for Normality

# When Non-Normal (1 Sample), Use Mann-Whitney (Wilcoxon Rank Sum)
median(y1)
wilcox.test(y1, mu = 380000, alternative = "two.sided")

# 2 Samples to compare (2 Sample t-Test / Wilcoxon)
#
# Null Hypothesis - Means of two groups are statistically equal
# Alternate Hypothesis - Means of two groups are statistically NOT equal
# When p <= 0.05, Reject Null Hypothesis.
#
# A. When Normal, Equal Variances, Use 2 Sample t-Test
# B. When Normal, UnEqual Variances, Use 2 Sample t-Test
#                 With var.equal = FALSE option
# C. Non-normal   Use Wilcoxon Rank Sum (n <= 20)
#                 Use 2-Sample t-Test   (n >  20)
#                 With var.equal = TRUE / FALSE as applicable
   
mean(y1); mean(y2)
t.test(y1, y2, var.equal = TRUE) 
t.test(dt$Price ~ dt$Age, var.equal = FALSE) 

# Check Normality
ad.test(y1)                       # p > 0.05 for Normality
ad.test(y2)                       # p > 0.05 for Normality

# Check Number of Samples
length(y1); length(y2)

# When Non-Normal (2 Sample), Use Wilcoxon Rank Sum (n <= 20)
boxplot(Price ~ Age, dt, col = "gold2")
median(y1); median(y2)
wilcox.test(y1, y2, alternative = "two.sided")

# Mood's Median Test 
mood.test(y1, y2, alternative = "two.sided")

# 3 Samples or more to compare (ANOVA / Kruskal-Wallis)
#
# Null Hypothesis - Means of THREE or more groups are statistically equal
# Alternate Hypothesis - Means of THREE or more groups are  NOT equal
# When p <= 0.05, Reject Null Hypothesis.

# ANOVA
dt[, .(.N, AvgPrice = mean(Price)), by = PropType]

boxplot(Price ~ PropType, dt, col = "gold2")

summary(aov(dt$Price ~ dt$PropType))

# for non-normal
kruskal.test(dt$Price, dt$PropType)

# Exercise - Perform ANOVA and Kruskal Tests
#            For Y = Price, X = Duration
#            There are only 2 groups, but ignore this

dt = fread("DiscX vs ContY.csv")

#++++++++++++++++Solution+++++++++++++++++++++++

summary(aov(dt$Price ~ dt$Duration))

# for non-normal
kruskal.test(dt$Price, dt$Duration)

boxplot(Price ~ Duration, dt, col = "red")



# End of Guided Project

