source("scripts/prepare_data.R")


cat("confidence interval for turnover variance\n")
# Chi-square distribution
n <- length(turnover)
alpha <- 0.02
s2 <- var(turnover)

chi_upper = qchisq(1-alpha/2, df=n-1)
chi_lower = qchisq(alpha/2, df=n-1)

upper <- (n-1) * s2 / chi_lower
lower <- (n-1) * s2 / chi_upper

cat("variance:", s2, "\n")
cat("chi lower:", chi_lower, "\n")
cat("chi upper:", chi_upper, "\n")

cat("\nconfidence interval for variance:\n")
cat("[", lower, ",", upper, "]\n")

# Relative precision
margin <- (upper - lower) / 2
relative_precision <- margin / s2

cat("relative precision:", relative_precision, "\n")



