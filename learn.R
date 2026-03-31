library(sn)
set.seed(123)
n <- 100 # sample size
num_sim <- 10000  # number of simulations
beta <- c(2, 1.2, 1.5, -0.8) # True value
beta_estimate <- matrix(0, nrow = num_sim, ncol = 4) # initiate store for the estimate 

# simulation loops
for(i in 1:num_sim){
 # generate the independent variable 
  x1 <- rnorm(n, mean = 5, sd = 1)
  x2 <- rnorm(n, mean = 3, sd = 1)
  x3 <- rnorm(n, mean = 0, sd = .5)
  
  # generate error using exponential 
  eps <- rsn(n, xi = 0, omega = 1, alpha = -5)
  # generate the dependent variable 
  y <- beta[1] + beta[2]*x1 + beta[3]*x2 + beta[4]*x3 + eps
  
  # fit the ols regression model
  model <- lm(y ~ x1 + x2 + x3)
  # store the coefficient 
  beta_estimate[i, ] <- coef(model)
  
}
# compute the bias
bias <- colMeans(beta_estimate) - beta
# mse
mse <- colMeans(
  (beta_estimate - matrix(beta,num_sim, 4, byrow = TRUE))^2
)
# rmse
rmse <- sqrt(mse)

# result 
result <- data.frame(
  Parameters = c("intercept", "x1", "x2", "x3"),
  Beta = beta,
  Bias = bias,
  MSE = mse,
  RMSE = rmse
)
print(result)