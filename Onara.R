# MATRIC NUMBER:22/56EG069

# Generate sample data
set.seed(123)
gender <- factor(rep(c("Male", "Female"), each = 10))

# Generate the random dependent variables
x_1 <- rnorm(20, mean = 75, sd = 5)
x_2 <- rnorm(20, mean = 65, sd = 6)

data <- data.frame(gender, x_1, x_2)

# view data 
data

# Perform MANOVA
manova_model <- manova(cbind(x_1, x_2) ~ gender, data = data)
summary(manova_model)

# Run the test Statistics
summary.aov(manova_model)