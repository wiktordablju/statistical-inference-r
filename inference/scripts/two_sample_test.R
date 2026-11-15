source("scripts/prepare_data.R")

test_result <- t.test(turnover,costs,alternative = "greater",var.equal = FALSE, conf.level = 0.95)

# Calculated t statistic
print(test_result$statistic)
# Degrees of freedom
print(test_result$parameter)

# Calculate the critical value.
alpha <- 0.05
n1 <- 25
n2 <- 25
df <- n1+n2-2

critical_t <- qt(1-alpha,df)

print(critical_t)
