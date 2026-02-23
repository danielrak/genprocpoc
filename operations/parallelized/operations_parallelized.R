# Minimal & parallelized operations
# Using: furrr::

# Packages ----------------------------------------------------------------

library(dplyr)
library(furrr)
library(readr)

# Parameters --------------------------------------------------------------

path_mask <- "./mask"

# Mask --------------------------------------------------------------------

mask <- read_csv(file.path(path_mask, "mask.csv"))

# Conversion_function -----------------------------------------------------

load("./operations/conversion_function.rda")

# Operation ---------------------------------------------------------------

plan(multisession, workers = 8)

mask %>% 
  select(param_1 = input_dir, param_2 = input_file, 
         param_3 = output_dir, param_4 = output_file) %>% 
  future_pmap(.f = \(...) {
    # error-robust version of conversion_function
    out <- safely(conversion_function)(...)
    if (is.null(out$error)) {NULL} else conditionMessage(out$error)
  })
