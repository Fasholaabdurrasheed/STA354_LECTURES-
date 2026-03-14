# MATRIC NUMBER: 24/56EG152

# Generate Sample Data

set.seed(123)
group <- factor(rep(c("A", "B", "C"), each=10))

# Generate random dependent variables
score1 <- rnorm(30, mean = 70, sd = 5)
score2 <- rnorm(30, mean = 65, sd = 6)

data <- data.frame(group, score1, score2)

#view data
data

# Perform MANOVA

manova_model <- manova(cbind(score1, score2) ~ group, data = data)

# Display MANOVA Result 
summary(manova_model)

# Run the Test Statistics 
summary.aov(manova_model)