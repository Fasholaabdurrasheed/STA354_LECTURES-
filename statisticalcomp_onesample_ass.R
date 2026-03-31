#
# STATISTICAL COMPUTING ASSIGNMENT
# One Sample Analysis in R

sink("statistical_computing_assignment.txt")
cat("STA354(STATISTICAL COMPUTING ASSIGNMENT)\n")
cat("------------------------------------------\n\n")

# Data Entry
cat("DATA USED IN ANALYSIS\n\n")

X <- c(0.42,1.35,0.88,2.10,0.57,1.92,0.33,
       3.45,0.76,10.00,1.28,0.64,2.87,0.51,1.74,
       0.29,4.12,0.95,1.16,0.48,12.5,2.54,0.73,
       3.01,0.67,1.39,15.20)

Y <- c(48.7,51.2,49.8,52.1,50.6,47.9,53.0,
       49.5,50.1,51.7,48.9,52.4,50.3,49.2,
       51.0,48.4,52.0,50.8,49.9,51.4,47.8,
       53.2,50.5,49.6,51.1,48.8,50.0)

cat("\nSample size of X:\n")
length(X)
cat("\nSample size of Y:\n")
length(Y)

# Question 1

cat("\n\nQuestion 1\n")
cat("The three question an analyst would ask when working with one-sample data\n")
cat("\n1. What is the central tendency of the dataset?\n")
cat("This involves calculating the mean or median of the data.\n")

cat("\n2. Does the sample mean differ from a hypothesized population mean?\n")
cat("This is usually tested using a one-sample t-test.\n")

cat("\n3. Does the data follow a normal distribution?\n")
cat("This determines whether parametric or nonparametric tests should be used.\n")

# Question 2

cat("\n\nQUESTION 2\n")
cat("Three important distribution characteristics that must be confirmed:\n")

cat("\n1. Normality of the data distribution.\n")
cat("2. Presence of outliers.\n")
cat("3. Independence of observations.\n")

# Step 2: Descriptive statistics

cat("\n\nDESCRIPTIVE STATISTICS\n")

cat("\nSummary of X:\n")
summary(X)

cat("\nSummary of Y:\n")
summary(Y)

cat("\nMean of X:\n")
mean(X)

cat("\nMean of Y:\n")
mean(Y)

cat("\nMeadian of X:\n")
median(X)

cat("\nMeadian of Y:\n")
median(Y)

cat("\nStandard deviation of X:\n")
sd(X)

cat("\nStandard deviation of Y:\n")
sd(Y)


# Step 3: Data Visualization

cat("\n\nDATA VISUALIZATION OF X & Y")
par(mfrow=c(2,2))

hist(X, main="Histogram of X", col="lightblue")
boxplot(X, main="Boxplot of X")

hist(Y, main="Histogram of Y", col="lightgreen")
boxplot(Y, main="Boxplot of Y")


# QUESTION 3

cat("\n\nQUESTION 3\n")
cat("Performing normality test using Shapiro-Wilk test.\n")

cat("\nShapiro test for X:\n")
shapiro.test(X)

cat("\nShapiro test for Y:\n")
shapiro.test(Y)

cat("\nInterpretation:\n")
cat("If p-value < 0.05, data is not normally distributed.\n")
cat("If p-value > 0.05, data may be normally distributed.\n")

# Parametric Test (if normal)

cat("\n\nOne-sample t-test for Y (mu = 50)\n")
t.test(Y, mu=50)

cat("\nInterpretation:\n")
cat("If p-value <= 0.05, reject the null hypothesis.\n")
cat("If p-value > 0.05, fail to reject the null hypothesis.\n")



# Nonparametric Test (if not normal)

cat("\n\nWilcoxon Signed Rank Test for X (mu = 1)\n")
wilcox.test(X, mu=1)

cat("\nInterpretation:\n")
cat("This test is used when normality assumption is violated.\n")



# Additional visualization
cat("\n\nAdditional Visualization of X\n")
qqnorm(X)
qqline(X, col = "red")

cat("\n\nAdditional Visualization of Y\n")
qqnorm(Y)
qqline(Y, col = "red")

# QUESTION 5

cat("\n\nQUESTION 5\n")

cat("\nDifference between parametric and nonparametric tests:\n")

cat("\nParametric Tests:\n")
cat("Assume normal distribution and involve parameters such as mean and variance.\n")

cat("\nExample in R:\n")
cat("t.test(Y, mu=50)\n")

cat("\nNonparametric Tests:\n")
cat("Do not assume normal distribution and are based on ranks.\n")

cat("\nExample in R:\n")
cat("wilcox.test(X, mu=1)\n")

# QUESTION 6

cat("\n\nQUESTION 6\n")

cat("\nEffects of small sample size and large measurement error:\n")

cat("\n1. Reduced statistical power.\n")
cat("2. Increased standard errors.\n")
cat("3. Wider confidence intervals.\n")
cat("4. Less reliable statistical inference.\n")

sink()
