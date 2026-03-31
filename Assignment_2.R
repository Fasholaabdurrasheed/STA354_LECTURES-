getwd()
# MATRIC NUMBER: 24/56EG152
# Sample data (30 observations)

set.seed(123)

score <- rnorm(30, mean = 70, sd = 8)
dim_len(score)
cat("Generated Sample Daa:\n")
print(score)

# Descriptive Statistics

cat("\nDescriptive Statistics:\n")
print(summary(score))
summary(score)
cat("\nMean:",mean(score))
cat("\nStandard Deviation:",sd(score))


# Visualization
# 1. Check Normality using Histogram

par(mfrow=c(2,2))
hist(score,
     main = "Histogram of Sample Scores",
     xlab = "Scores",
     col = "lightblue",
     border = "black")

# 2. Normality Test (Shapiro-Wilk Test)
cat("\n\nShapiro-Wilk Normality Test:\n")
normality_test <- shapiro.test(score)
print(normality_test)

# Q-Q Plot for Normality
qqnorm(score)
qqline(score, col = "red")


# 3. Check for Outlier

boxplot(score,
        main = "Boxplot for Outlier Detection",
        col = "lightgreen")
cat("\nOutlier Detection using IQR:\n")
Q1 <- quantile(score, 0.25)
Q3 <- quantile(score, 0.75)

IQR_value <- IQR(score)

lower_bond <- Q1 - 1.5*IQR_value
upper_bond <- Q3 + 1.5*IQR_value

outliers <- score[score < lower_bond | score > upper_bond]
print(outliers)
# 4. Check Serial Correlation
# (Only relevant if data is time series)

acf(score,
    main = "Autocorrelation Check")

# 5. Choose Appropriate Test

cat("\nTest Selection Based on Normality:\n")

if(normality_test$p.value > 0.05){
  cat("Data is normally distributed\n")
  cat("Using Parametric Test: One-Sample t-test\n")
  result <- t.test(score, mu=70)
} else{
  cat("Data is NOT normally distributed\n")
  cat("Using Nonparametric Test: Wilconxon Test\n")
  
  result <- wilcox.test(score, mu=70)
  
}

print(result)

#---- INTERPRETATION---
# If p-value > 0.05 in shapiro test,
# we fail to reject the null hypothesis,
# meaning the data follow a normal distribution.
# Therefore, the parametric t-test is appropriate.

# If p-value <= 0.05, 
# normality assumption is violated,
# so the wilcoxon signed-rank test is used.