source("main.R")

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

