source("../shared/example_data.R")

# Kolmogorov-Smirnov normally assumes known distribution parameters.
# These calls estimate parameters from the sample; interpret them with that limitation.

cost_mean <- mean(costs)
cost_sd <- sd(costs)

turnover_mean <- mean(turnover)
turnover_sd <- sd(turnover)

# Normality tests
library(nortest)

cost_ks_test = ks.test(costs, "pnorm", cost_mean, cost_sd)
turnover_ks_test = ks.test(turnover, "pnorm", turnover_mean, turnover_sd)

cat("\n--- KOLMOGOROV-SMIRNOV TESTS: COSTS AND TURNOVER ---\n")
print(cost_ks_test)
print(turnover_ks_test)


cost_lilliefors_test = lillie.test(costs)
turnover_lilliefors_test = lillie.test(turnover)

cat("\n--- LILLIEFORS TESTS: COSTS AND TURNOVER ---\n")
print(cost_lilliefors_test)
print(turnover_lilliefors_test)


cat("\nCompare the printed p-values with your chosen significance level. These are synthetic examples.\n")
