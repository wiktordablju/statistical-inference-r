# Synthetic data provenance

The public examples use no supplied observations, subsets, permutations, perturbations, or fitted statistics from an external dataset.

The script shared/example_data.R generates both input vectors from scratch with base R:

```r
set.seed(20261115)
costs <- round(rnorm(25, mean = 60, sd = 8), 3)
turnover <- round(rnorm(25, mean = 90, sd = 12), 3)
```
