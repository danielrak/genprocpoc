# Minimal & parallelized & non-blocking & logged

# Minimal & parallelized & non-blocking
# Using: furrr:: & callr::

# Parameters --------------------------------------------------------------

path_mask <- "./mask"

# Mask --------------------------------------------------------------------

mask <- read_csv(file.path(path_mask, "mask.csv"))

# Conversion_function -----------------------------------------------------

load("./operations/conversion_function.rda")

# Operation ---------------------------------------------------------------

myop <- callr::r_bg(
  function(mask, conversion_function, log_csv) {
    
    suppressPackageStartupMessages({
      library(dplyr)
      library(future)
      library(furrr)
      library(purrr)
      library(readr)
      library(tibble)
    })
    
    # Make sure futures are really parallel in this bg session
    future::plan("multisession", workers = 8)
    on.exit(future::plan("sequential"), add = TRUE)
    
    # Ensure output folder exists
    dir.create(dirname(log_csv), recursive = TRUE, showWarnings = FALSE)
    
    # Add a stable row id so we can always match back
    mask2 <- mask %>% mutate(.mask_row = dplyr::row_number())
    
    # Per-row execution: return a 1-row tibble = "log line"
    logs <- mask2 %>%
      select(.mask_row, everything()) %>%
      furrr::future_pmap_dfr(
        .options = furrr::furrr_options(seed = TRUE),
        .f = function(...) {
          row <- tibble::as_tibble(list(...))
          
          # Pull the params (names must exist in mask)
          p1 <- row$input_dir
          p2 <- row$input_file
          p3 <- row$output_dir
          p4 <- row$output_file
          
          # run safely
          res <- purrr::safely(conversion_function)(p1, p2, p3, p4)
          
          ok  <- is.null(res$error)
          msg <- if (ok) NA_character_ else conditionMessage(res$error)
          
          # Return original row + indicators
          row %>%
            mutate(
              success = ok,
              error_message = msg
            )
        }
      )
    
    # Write final log
    readr::write_csv(logs, log_csv)
    
    # Also return it (optional)
    logs
  },
  args = list(
    mask = mask,
    conversion_function = conversion_function,
    log_csv = "./operations/logged/operations_log.csv"
  ),
  stdout = "./operations/logged/operations_logged_stdout.log",
  stderr = "./operations/logged/operations_logged_stderr.log",
  wd = getwd()
)
