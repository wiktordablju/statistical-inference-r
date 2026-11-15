source("../shared/prepare_data.R")

cat("\n--- FREQUENCY TABLE: COSTS ---\n")
print(cost_table)

cat("\n--- FREQUENCY TABLE: TURNOVER ---\n")
print(turnover_table)


par(mfrow = c(1, 2), mar = c(7, 4, 4, 1) + 0.1) 

# --- COST HISTOGRAM ---
hist(costs, breaks = cost_breaks, right = FALSE, include.lowest = TRUE,
     col = "skyblue", border = "black",
     main = "Histogram - Costs",
     xlab = "", ylab = "Number of businesses",
     xaxt = "n")    

axis(1, at = cost_table$Midpoint, labels = cost_table$Interval, las = 2, cex.axis = 0.8)
title(xlab = "Costs (thousands of synthetic currency units)", line = 5.5)


# --- TURNOVER HISTOGRAM ---
hist(turnover, breaks = turnover_breaks, right = FALSE, include.lowest = TRUE,
     col = "lightcoral", border = "black",
     main = "Histogram - Turnover",
     xlab = "", ylab = "Number of businesses",
     xaxt = "n")

axis(1, at = turnover_table$Midpoint, labels = turnover_table$Interval, las = 2, cex.axis = 0.8)
title(xlab = "Turnover (thousands of synthetic currency units)", line = 5.5)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

