source("scripts/prepare_data.R")


cat("confidence interval for mean costs\n")
# Student's t: mean, standard deviation, sample size and distribution quantile.
n <- length(costs)
alpha <- 0.02

sample_mean <- mean(costs)
s <- sd(costs)
t <- qt(1-alpha/2, df = n -1)
# 
margin <- t * s / sqrt(n)

lower <- sample_mean - margin
upper <- sample_mean + margin


cat("mean:", sample_mean, "\n")
cat("standard deviation:", s, "\n")
cat("t:", t, "\n")

cat("confidence interval:\n")
cat("[", lower, ",", upper, "]\n")

# Relative precision
relative_precision <- margin / sample_mean

cat("relative precision:", relative_precision, "\n")

#
