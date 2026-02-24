# Minimal & parallelized & non-blocking & logged

# Minimal & parallelized & non-blocking
# Using: furrr:: & callr::

# Packages ----------------------------------------------------------------

library(dplyr)

# Parameters --------------------------------------------------------------

path_mask <- "./mask"

# Mask --------------------------------------------------------------------

mask <- readr::read_csv(file.path(path_mask, "mask.csv"))

# we try to bind 100-times the mask with itself to test if r_bg() is still running
# after closing our session
# [ALTERT FOR USERS WHO WANNA TRY IT OUT] !!! : IT'S VERY MACHINE-RESOURCE-HEAVY
# mask <- purrr::map_dfr(1:100, \(x) mask %>% 
#                          mutate(output_file = stringr::str_replace(
#                            output_file, "\\.csv$", paste0("_", x, ".csv"))))
# Answer: nope. It's not running in background if RStudio is closed. 
# (It's a child process)

# Conversion_function -----------------------------------------------------

load("./operations/conversion_function.rda")

# Operation ---------------------------------------------------------------

myop <- callr::r_bg(
  function(mask, conversion_function, logs_path, final_log_filename) {
    
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
    dir.create(dirname(logs_path), recursive = TRUE, showWarnings = FALSE)
    
    # Add a stable row id so we can always match back
    mask2 <- mask %>% mutate(.mask_row = dplyr::row_number())
    
    # nrow to correctly arrange case log files
    case_log_id_width <- nchar(nrow(mask2))
    
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
          case_log <- row %>%
            mutate(
              success = ok,
              error_message = msg
            )
          
          # Create a directory for case logs
          case_logs_path <- file.path(logs_path, "case_logs")
          dir.create(case_logs_path, recursive = TRUE, showWarnings = FALSE)
          
          # Indicator of ok/ko on case log file names
          case_log_success_indicator <- if (is.na(msg)) "ok" else "ko"
          
          case_log %>% readr::write_csv(
            file.path(case_logs_path, 
                      paste0("case_log_", 
                             stringr::str_pad(
                               case_log$.mask_row, 
                               width = case_log_id_width, pad = "0"),
                             "_", case_log_success_indicator, ".csv")))
          case_log
          
        }
      )
    
    # Write final log
    readr::write_csv(logs, file.path(logs_path, final_log_filename))
  },
  args = list(
    mask = mask,
    conversion_function = conversion_function,
    logs_path = "./operations/logged", 
    final_log_filename = "operations_log.csv"
  ),
  stdout = "./operations/logged/operations_logged_stdout.log",
  stderr = "./operations/logged/operations_logged_stderr.log",
  wd = getwd()
)
