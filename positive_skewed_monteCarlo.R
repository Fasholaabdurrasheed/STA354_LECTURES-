############################################################
# Monte Carlo Simulation for Positively Skewed Error Regression
# Statistical Computing Assignment
# Iterations: 10000
# Independent Variables: X1, X2, X3
# Metrics: Bias, MSE, RMSE
############################################################

# set seed for Reproducibility 
set.seed(123)

# save output 
sink("Monte_Carlo_Results.txt")

cat("MONTE CARLO SIMULATION FOR POSITIVELY SKEWED ERROR REGRESSION\n")
cat("============================================================\n\n")

# Parameters
n <- 100
n_sim <- 10000

cat("Sample Size:", n, "\n")
cat("Number of Simulations:", n_sim, "\n\n")

# True beta values
beta_true <- c(2, 0.5, 1.2, -0.8)

cat("True Parameters:\n")
print(beta_true)
cat("\n")

# Store estimates
beta_estimates <- matrix(0, nrow = n_sim, ncol = 4)

cat("Running Monte Carlo Simulation...\n\n")

# Simulation Loop
for (i in 1:n_sim) {
  
  X1 <- rnorm(n)
  X2 <- rnorm(n)
  X3 <- rnorm(n)
  
  # Positively skewed error (Exponential centered at 0)
  error <- rexp(n, rate = 1) - 1
  
  Y <- beta_true[1] +
    beta_true[2]*X1 +
    beta_true[3]*X2 +
    beta_true[4]*X3 +
    error
  
  model <- lm(Y ~ X1 + X2 + X3)
  
  beta_estimates[i, ] <- coef(model)
}

cat("Simulation Completed\n\n")

############################################################
# BIAS
############################################################

bias <- colMeans(beta_estimates) - beta_true

cat("BIAS OF ESTIMATORS\n")
cat("==================\n")
print(bias)
cat("\n")

############################################################
# MSE
############################################################

mse <- colMeans(
  (beta_estimates - matrix(beta_true, n_sim, 4, byrow = TRUE))^2
)

cat("MEAN SQUARE ERROR (MSE)\n")
cat("========================\n")
print(mse)
cat("\n")

############################################################
# RMSE
############################################################

rmse <- sqrt(mse)

cat("ROOT MEAN SQUARE ERROR (RMSE)\n")
cat("==============================\n")
print(rmse)
cat("\n")

############################################################
# RESULT TABLE
############################################################

results <- data.frame(
  Parameter = c("Intercept", "X1", "X2", "X3"),
  True_Value = beta_true,
  Bias = bias,
  MSE = mse,
  RMSE = rmse
)

cat("FINAL RESULT TABLE\n")
cat("==================\n")
print(results)

sink()

############################################################
# PLOTS
############################################################

png("Coefficient_Distribution.png", width = 800, height = 600)

par(mfrow = c(2,2))

hist(beta_estimates[,1],
     main = "Intercept Distribution",
     col = "skyblue",
     xlab = "Intercept")

hist(beta_estimates[,2],
     main = "X1 Coefficient Distribution",
     col = "lightgreen",
     xlab = "Beta1")

hist(beta_estimates[,3],
     main = "X2 Coefficient Distribution",
     col = "orange",
     xlab = "Beta2")

hist(beta_estimates[,4],
     main = "X3 Coefficient Distribution",
     col = "pink",
     xlab = "Beta3")

dev.off()

############################################################
# ERROR DISTRIBUTION PLOT
############################################################

png("Positively_Skewed_Error.png", width = 800, height = 600)

error_sample <- rexp(1000, rate = 1)

hist(error_sample,
     col = "red",
     main = "Positively Skewed Error Distribution",
     xlab = "Error")

dev.off()

##########################################################