x = c(23,67,23,45,12,17,16)
# 12,16,17,23,23,45,67
par(mfrow = c(2,2))
plot(density(x), main = "Scattered Plot")

boxplot(x)
#hist(x, freq = FALSE, main="class rep")
hist(x, freq = FALSE, ylim = c(0, 0.03), col = "gray90", main = "Actual KDE vs Theoretical Normal")
lines(density(x), col = "red", lwd = 2)
curve_range <- seq(min(x)-10, max(x)+10, length=100)
lines(curve_range, dnorm(curve_range, mean=mean(x), sd=sd(x)), 
      col = "black", lty = 2, lwd = 2)
legend("topright", legend=c("Actual KDE", "Normal Curve"), 
       col=c("red", "black"), lty=c(1, 2), lwd=2)

plot(density(x), main = "KDE Curve only")

# Test for normality
norms <- shapiro.test(x)
summary(norms)
norms
#parametric test
t.test(x)

# Non_Parametric test
wilcox.test(x, mu =  990)
