
# MATRIC NUMBER: 24/56EG152
# Sample data (30 observations)

set.seed(123)

score <- rnorm(30, mean = 70, sd = 8)

score

# Descriptive Statistics
summary(score)
mean(score)
sd(score)


# 1. Check Normality using Histogram

hist(score,
     main = "Histogram of Sample Scores",
     xlab = "Scores",
     col = "lightblue",
     border = "black")

# 2. Normality Test (Shapiro-Wilk Test)

shapiro.test(score)

# Q-Q Plot for Normality
qqnorm(score)
qqline(score, col = "red")


# 3. Check for Outlier

boxplot(score,
        main = "Boxplot for Outlier Detection",
        col = "lightgreen")

Q1 <- quantile(score, 0.25)
Q3 <- quantile(score, 0.75)

IQR_value <- IQR(score)

lower_bond <- Q1 - 1.5*IQR_value
upper_bond <- Q3 + 1.5*IQR_value

score[score < lower_bond | score > upper_bond]

# 4. Check Serial Correlation
# (Only relevant if data is time series)

acf(score,
    main = "Autocorrelation Check")

# 5. Choose Appropriate Test

if(shapiro.test(score)$p.value > 0.05){
  cat("Data is normally distributed -> Parametric Test(t-test)\n")
  t.test(score, mu = 70)
} else{
  cat("Data is NOT normally distributed -> Use Nonparametric Test (Wilconxon Test)\n")
  wilcox.test(score, mu = 70)
}

#---- INTERPRETATION---
# If p-value > 0.05 in shapiro test,
# we fail to reject the null hypothesis,
# meaning the data follow a normal distributioin.
# Therefore, the parametric t-test is appropriate.

# If p-value <= 0.05, 
# normality assumpiton is violated,
# so the wilcoxon signed-rank test is used.