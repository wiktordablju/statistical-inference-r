source("../shared/example_data.R")

n <- length(costs)


calculate_breaks <- function(values) {
  sample_size <- length(values)
  k <- round(sqrt(sample_size))                  
  span <- max(values) - min(values)     
  width <- span / k   

  rounded_width <- ceiling(width * 100) / 100 
  
  breaks <- seq(from = min(values), to = min(values) + k * rounded_width, by = rounded_width)
  
  return(breaks)
}

cost_breaks <- calculate_breaks(costs)
turnover_breaks <- calculate_breaks(turnover)


create_frequency_table <- function(values, breaks) {
  intervals <- cut(values, breaks = breaks, right = FALSE, include.lowest = TRUE)
  
  ni <- as.numeric(table(intervals))
  pi <- ni / length(values)   
  cumulative_frequency <- cumsum(pi)   
  midpoints <- (breaks[-1] + breaks[-length(breaks)]) / 2 
  
  df <- data.frame(
    Interval = levels(intervals),
    Count = ni,
    Frequency = pi,
    CumulativeFrequency = cumulative_frequency,
    Midpoint = midpoints
  )
  return(df)
}


cost_table <- create_frequency_table(costs, cost_breaks)
turnover_table <- create_frequency_table(turnover, turnover_breaks)

