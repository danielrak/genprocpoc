# Minimal & parallelized operations
# Using: furrr::

# Packages ----------------------------------------------------------------

library(dplyr)
library(furrr)
library(readr)

# Mask --------------------------------------------------------------------

mask <- read_csv(file.path(path_mask, "mask.csv"))

# Conversion_function -----------------------------------------------------

load("./operations/conversion_function.rda")
