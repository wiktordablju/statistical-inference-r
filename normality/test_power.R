source("main.R")

library(nortest)
# Measures of location

arithmetic_mean <- function(values){
    return(mean(values))
}
sample_median <- function(values){
    return(median(values))
}
modal_interval <- function(frequency_table) {
  # Find the row index with the largest count.
  idx <- which.max(frequency_table$Count)
  
  # Return the interval label at that index.
  return(frequency_table$Interval[idx])
}
# Measures of dispersion
data_range <- function(values){
    return(diff(range(values)))
}
sample_variance <- function(values){
    return(var(values))
}
standard_deviation <- function(values){
    return(sd(values))
}
coefficient_of_variation <- function(values){
    return((standard_deviation(values)/arithmetic_mean(values))*100)
}
interquartile_range <- function(values){
    return(IQR(values))
}
# Measures of skewness and kurtosis
skewness <- function(values){
    n <- length(values)
    total <- 0
    for(i in values)
    {
        total <- total + (i - arithmetic_mean(values))^3
    }

    return((total/n)/(standard_deviation(values)^3))
}
kurtosis <- function(values){
    n <- length(values)
    total <- 0
    for(i in values)
    {
        total <- total + (i - arithmetic_mean(values))^4
    }

    return((total/n)/(standard_deviation(values)^4))
}


cost_frequency_table <- create_frequency_table(costs, cost_breaks)
cost_modal_interval <- modal_interval(cost_frequency_table)
turnover_frequency_table <- create_frequency_table(turnover, turnover_breaks)
turnover_modal_interval <- modal_interval(turnover_frequency_table)
print("---COSTS---")
print(paste("Arithmetic mean: ",arithmetic_mean(costs)))
print(paste("Median: ",sample_median(costs)))
print(paste("Modal interval: ",cost_modal_interval))
print(paste("Range: ",data_range(costs)))
print(paste("Variance: ",sample_variance(costs)))
print(paste("Standard deviation: ",standard_deviation(costs)))
print(paste("Interquartile range: ",interquartile_range(costs)))
print(paste("Coefficient of variation: ",coefficient_of_variation(costs)))
print(paste("Skewness: ",skewness(costs)))
print(paste("Kurtosis: ",kurtosis(costs)))
print(" ")
print("---TURNOVER---")
print(paste("Arithmetic mean: ",arithmetic_mean(turnover)))
print(paste("Median: ",sample_median(turnover)))
print(paste("Modal interval: ",turnover_modal_interval))
print(paste("Range: ",data_range(turnover)))
print(paste("Variance: ",sample_variance(turnover)))
print(paste("Standard deviation: ",standard_deviation(turnover)))
print(paste("Interquartile range: ",interquartile_range(turnover)))
print(paste("Coefficient of variation: ",coefficient_of_variation(turnover)))
print(paste("Skewness: ",skewness(turnover)))
print(paste("Kurtosis: ",kurtosis(turnover)))

set.seed(67)

simulation_count <- 5000
sample_size <- 25

# Symmetric distributions

# Uniform U(0,1)
u_01 <- matrix(runif(simulation_count * sample_size, 0, 1), nrow = simulation_count, ncol = sample_size)
#Beta(2,2)
b_22 <- matrix(rbeta(simulation_count * sample_size, 2, 2),nrow = simulation_count, ncol = sample_size)
# Student's t, df = 300
t_300 <- matrix(rt(simulation_count * sample_size, df = 300), nrow = simulation_count, ncol = sample_size)
# Student's t, df = 10
t_10 <- matrix(rt(simulation_count * sample_size, df = 10), nrow = simulation_count, ncol = sample_size)
# Student's t, df = 7
t_7 <- matrix(rt(simulation_count * sample_size, df = 7), nrow = simulation_count, ncol = sample_size)
# Student's t, df = 5
t_5 <- matrix(rt(simulation_count * sample_size, df = 5), nrow = simulation_count, ncol = sample_size)
# Laplace
laplace_values <- rexp(simulation_count * sample_size, rate = 1) - rexp(simulation_count * sample_size, rate = 1)
lap_01 <- matrix(laplace_values, nrow = simulation_count)
# Triangular
triang_values <- (runif(simulation_count * sample_size) + runif(simulation_count * sample_size)) / 2
tri_01 <- matrix(triang_values, nrow = simulation_count)

# Asymmetric distributions

#Beta(6,2)
b_62 <- matrix(rbeta(simulation_count*sample_size,6,2), nrow = simulation_count, ncol = sample_size)
#Beta(2,1)
b_21 <- matrix(rbeta(simulation_count*sample_size,2,1), nrow = simulation_count, ncol = sample_size)
#Beta(3,2)
b_32 <- matrix(rbeta(simulation_count*sample_size,3,2), nrow = simulation_count, ncol = sample_size)
# Chi-square, df = 20, 4
chi_20 <- matrix(rchisq(simulation_count*sample_size, df= 20), nrow = simulation_count, ncol = sample_size)
#Gamma(4,5)
gam_45 <- matrix(rgamma(simulation_count*sample_size, shape = 4, rate = 5), nrow = simulation_count, ncol = sample_size)
#Gamma(1,5)
gam_15 <- matrix(rgamma(simulation_count*sample_size, shape = 1, rate = 5), nrow = simulation_count, ncol = sample_size)

# Chi-square, df = 4
chi_4 <- matrix(rchisq(simulation_count * sample_size, df = 4), nrow = simulation_count, ncol = sample_size)


# Shapiro-Wilk test
#p_shapiro <- apply(u_01,1,function(x) shapiro.test(x)$p.value)
# Kolmogorov-Smirnov test
#p_ks <- apply(u_01, 1, function(x) ks.test(x, "pnorm", mean(x), sd(x))$p.value)

#Jarque-Bera
jarque_bera_test <- function(values)
{
    n <- length(values)

    S <- skewness(values)
    K <- kurtosis(values)

    # Jarque-Bera test statistic
    JB <- (n/6) * (S^2 + ((K - 3)^2 / 4))

    p_val <- 1 - pchisq(JB, df = 2)

    return(p_val)
}

calculate_test_power <- function(data_matrix)
{
    # Shapiro-Wilk test
    p_shapiro <- apply(data_matrix, 1, function(x) shapiro.test(x)$p.value)
    # Kolmogorov-Smirnov test
    p_ks <- apply(data_matrix, 1, function(x) ks.test(x, "pnorm", mean(x), sd(x))$p.value)
    # Lilliefors test
    p_lillie <- apply(data_matrix, 1,function(x) lillie.test(x)$p.value)
    # Anderson-Darling test
    p_ad <- apply(data_matrix, 1, function(x) ad.test(x)$p.value)
    # Jarque-Bera test
    p_jb <- apply(data_matrix, 1, function(x) jarque_bera_test(x))

    # Calculate rejection rates as percentages, using p-value < 0.05.
    power <- c(
        Shapiro_Wilk = mean(p_shapiro < 0.05) * 100,
        Kolmogorov_Smirnov = mean(p_ks < 0.05) * 100,
        Lilliefors = mean(p_lillie < 0.05) * 100,
        Anderson_Darling = mean(p_ad < 0.05) * 100,
        Jarque_Bera = mean(p_jb < 0.05) * 100
    )
    return(power)
}

# Test power
#print(mean(p_shapiro <0.05))
#print(mean(p_ks <0.05)*100)

distributions <- list(
    "U(0,1)" = u_01,
    "Beta(2,2)" = b_22,
    "t(300)" = t_300,
    "t(10)" = t_10,
    "t(7)" = t_7,
    "t(5)" = t_5,
    "Laplace" = lap_01,
    "Triangular" = tri_01,
    "Beta(6,2)" = b_62,
    "Beta(2,1)" = b_21,
    "Beta(3,2)" = b_32,
    "Chi-square(20)" = chi_20,
    "Chi-square(4)" = chi_4,
    "Gamma(4,5)" = gam_45,
    "Gamma(1,5)" = gam_15
)

# Calculate test power for each distribution.
power_results <- lapply(distributions, calculate_test_power)

# Build the results table.
power_table <- as.data.frame(do.call(rbind, power_results))
power_table <- round(power_table, 2)

print("---NORMALITY TEST POWER (%)---")
print(power_table)
