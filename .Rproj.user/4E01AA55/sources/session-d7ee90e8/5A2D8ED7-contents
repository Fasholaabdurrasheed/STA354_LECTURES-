

# Sample data (30 observations)

set.seed(123)

score <- rnorm(30, mean = 70, sd = 8)

score

# 1. Check Normality using Histogram

hist(score,
     main = "Histogram of Sample Scores",
     xlab = "Scores",
     col = "lightblue",
     border = "black")

# 2. Normality Test (Shapiro-Wilk Test)

shapiro.test(score)


# 3. Check for Outlier

boxplot(score,
        main = "Boxplot for Outlier Detection",
        col = "lightgreen")

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