# A code to correct for exercise
# Correct the Operation section such that it works and produce the desired log

# Packages ----------------------------------------------------------------

library(callr)
library(dplyr)

# Mask --------------------------------------------------------------------

mask <- read_csv(file.path(path_mask, "mask.csv"))

## Adding a row number ---------------------------------------------------

mask <- mutate(mask, op_id = row_number())

# Conversion_function -----------------------------------------------------

load("./operations/conversion_function.rda")

# Operation ---------------------------------------------------------------

myop <- 
  r_bg(function (mask, conversion_function, log_path) {
    suppressPackageStartupMessages({
      library(dplyr)
    })
    future::plan("multisession", workers = 8)
    on.exit(future::plan("sequential"), add = TRUE)
    
    operations_and_log <- mask %>% 
      select(param_1 = input_dir, param_2 = input_file, 
             param_3 = output_dir, param_4 = output_file) %>% 
      furrr::future_pmap_dfr(.f = \(...) {
        
        # the case
        row <- tibble::as_tibble(list(...))
        
        # error-robust version of conversion_function
        out <- purrr::safely(conversion_function)(...)
        if (is.null(out$error)) {
          
          row %>% mutate(success = TRUE, error_message = NA)
        } else {row %>% mutate(
          success = FALSE, 
          error_message = conditionMessage(out$error))}
      })
    
    # write total log
    readr::write_csv(operations_and_log, log_path)
  }, args = list(
    mask = mask, 
    conversion_function = conversion_function,
    log_path = "./operations/logged/operations_log.csv"),
  # some meta logging options to maybe explore more in the logging phase
  stdout = "./operations/logged/operations_logged_stdout.log",
  stderr = "./operations/logged/operations_logged_stderr.log")
