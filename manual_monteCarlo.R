sink("Monte_Carlo_Simulate.txt")

cat("MONTE CARLO SIMULATION RESULTS\n")
cat("==============================\n\n")

library(sn)

set.seed(1234)
n_sim <- 1000; n <- 100

beta = c(2.0,0.5,1.2,-0.8,0.4)
fit <- matrix(0, nrow = 5, ncol = n_sim)

for(i in 1:n_sim){
  x1 <- rnorm(n, mean = 5, sd = 1)
  x2 <- rnorm(n, mean = 3, sd = 2)
  x3 <- rnorm(n, mean = 0, sd = 1)
  x4 <- rnorm(n, mean = 8, sd = 0.5)
  
  eps <- rsn(n, xi = 0, omega = 1, alpha = -5)
  
  y <- beta[1] + x1*beta[2] + x2*beta[3] + x3*beta[4] + x4*beta[5] + eps
  
  fit[,i] <- coef(lm(y ~ x1 + x2 + x3 + x4))
}

meanfit <- apply(fit, 1, mean)
varfit  <- apply(fit, 1, var)

bias <- meanfit - beta
mse  <- bias^2 + varfit

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
for (x in 1:10){
  print(x)
}
text <- "awesome"
paste("R is", text)
text_1 <- "R is"
paste(text_1, text)
num1 <- 5
num2 <- 10
y <- TRUE
num1 + num2
num1 + text
class(x)
class(y)
class(num1)
15%/%2
15%%2
before <- c(85,90,88,75,95,80,78)
after <- c(88,92,90,78,97,85,80)
t.test(before, after, paired=TRUE)
