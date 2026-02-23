# Minimal & parallelized & non-blocking
# Using: furrr:: & callr::

# Packages ----------------------------------------------------------------

library(callr)
library(readr)

# Parameters --------------------------------------------------------------

path_mask <- "./mask"

# Mask --------------------------------------------------------------------

mask <- read_csv(file.path(path_mask, "mask.csv"))

# Conversion_function -----------------------------------------------------

load("./operations/conversion_function.rda")

# Operation ---------------------------------------------------------------

myop <- 
  r_bg(function (mask, conversion_function) {
    suppressPackageStartupMessages({
      library(dplyr)
    })
    future::plan("multisession", workers = 8)
    mask %>% 
      select(param_1 = input_dir, param_2 = input_file, 
             param_3 = output_dir, param_4 = output_file) %>% 
      furrr::future_pmap(.f = \(...) {
        # error-robust version of conversion_function
        out <- purrr::safely(conversion_function)(...)
        if (is.null(out$error)) {NULL} else conditionMessage(out$error)
      })  
  }, args = list(
    mask = mask, 
    conversion_function = conversion_function),
  # some meta logging options to maybe explore more in the logging phase
  stdout = "./operations/non_blocking/operations_non_blocking_stdout.log",
  stderr = "./operations/non_blocking/operations_non_blocking_stderr.log")