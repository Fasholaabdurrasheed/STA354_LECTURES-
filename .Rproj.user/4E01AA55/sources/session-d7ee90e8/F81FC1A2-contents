#-----------------------------------------
# STATISTICAL COMPUTING ASSIGNMENT
# One Sample Analysis in R
#-----------------------------------------

# What is the central tendency of the data?

#The analyst wants to know the average behavior of the observations.

# Step 1: Enter the data

X <- c(0.42,1.35,0.88,2.10,0.57,1.92,0.33,
       3.45,0.76,10.00,1.28,0.64,2.87,0.51,1.74,
       0.29,4.12,0.95,1.16,0.48,12.5,2.54,0.73,
       3.01,0.67,1.39,15.20)

Y <- c(48.7,51.2,49.8,52.1,50.6,47.9,53.0,
       49.5,50.1,51.7,48.9,52.4,50.3,49.2,
       51.0,48.4,52.0,50.8,49.9,51.4,47.8,
       53.2,50.5,49.6,51.1,48.8,50.0)

#-----------------------------------------
# Step 2: Descriptive statistics
#-----------------------------------------

summary(X)
summary(Y)

mean(X)
mean(Y)

median(X)
median(Y)

sd(X)
sd(Y)

#-----------------------------------------
# Step 3: Data Visualization
#-----------------------------------------

par(mfrow=c(2,2))

hist(X, main="Histogram of X", col="lightblue")
boxplot(X, main="Boxplot of X")

hist(Y, main="Histogram of Y", col="lightgreen")
boxplot(Y, main="Boxplot of Y")

#-----------------------------------------
# Step 4: Test for Normality
#-----------------------------------------

shapiro.test(X)
shapiro.test(Y)

#-----------------------------------------
# Step 5: Parametric Test (if normal)
# One-sample t-test
#-----------------------------------------

t.test(Y, mu=50)

#-----------------------------------------
# Step 6: Nonparametric Test (if not normal)
# Wilcoxon Signed-Rank Test
#-----------------------------------------

wilcox.test(X, mu=1)

#-----------------------------------------
# Step 7: Additional visualization
#-----------------------------------------

qqnorm(X)
qqline(X)

qqnorm(Y)
qqline(Y)
