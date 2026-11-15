# Independent synthetic example data; no supplied observations or fitted parameters are used.
# Seed and parameters are documented in DATA_PROVENANCE.md.
set.seed(20261115)
costs <- round(rnorm(25, mean = 60, sd = 8), 3)
turnover <- round(rnorm(25, mean = 90, sd = 12), 3)
