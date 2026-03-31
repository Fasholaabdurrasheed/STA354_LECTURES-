#install.packages("sn")
sink("Monte_Carlo_Stimulate.txt")
cat("MONTE CARLO SIMULATION RESULTS\n")
cat("================================\n\n")
library(sn)
# True Model & Parameters
set.seed(1234)
n_sim <- 1000; n <- 100
beta = c(2.0,0.5,1.2,-0.8,0.4) # true values
fit <- matrix(0, nrow = 5, ncol = n_sim)

### Data  Generating Process (4 predictors via rnorm)
x1 <- rnorm(n, mean = 5, sd = 1)
x2 <- rnorm(n, mean = 3, sd = 2)
x3 <- rnorm(n, mean = 0, sd = 1)
x4 <- rnorm(n, mean = 8, sd = 0.5)
#x=cbind(x1,x2,x3,x4)

# Monte Carlo loop
for(i in 1:n_sim){
  eps <- rsn(n, xi = 0, omega = 1, alpha = -5)
  # Generate y+++++++++++++++++++++ using full beta (intercept + 4 slopes) via design matrix
  y <- beta[1] + x1*beta[2] + x2*beta[3] + x3*beta[4] + x4*beta[4] + eps
  # fit the OLS and store the 5 estimated coefficients in column i
  fit[,i] <- coef(lm(y ~ x1 + x2 + x3 + x4))
  # stores the coefs,SEs,p-values,R2
}
# Summarise across the n_sim replications (apply across columns, margin=2)

#meanfit <- apply(fit, 2, mean) # 2 column, 1 is row
meanfit <- apply(fit, 1, mean)
varfit = apply(fit, 1, var)
# Bias and MSE
bias <- meanfit - beta
mse <- bias^2 + varfit
cat("View Result")
# View results 

results <- data.frame(
  parameter = c("Intercept", "b1", "b2", "b3", "b4"),
  True_Beta = beta,
  mean_EST = round(meanfit, 4),
  Bias = round(bias, 4),
  Variance = round(varfit, 4),
  MSE = round(mse, 4)
)
cat("Summary of Monte Carlo Estimates:\n\n")
print(results)
sink()