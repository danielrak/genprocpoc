# Mask

# Mapping parameters in prevision of applying the operation

# Packages ----------------------------------------------------------------

library(dplyr)
library(stringr)
library(readr)

# Parameters --------------------------------------------------------------

path <- "./mask"
path_input <- "./r_built_in_datasets"
path_output <- "./out"

# Mask creation -----------------------------------------------------------

lfiles <- list.files(path_input, full.names = TRUE)

lfiles_df <- data.frame(lfiles) %>% 
  mutate(input_dir = dirname(lfiles),
         input_file = basename(lfiles),
         output_dir = path_output,
         output_path = str_replace(input_file, "\\.rds$", ".csv"))

# Export ------------------------------------------------------------------

write_csv(lfiles_df, file.path(path, "mask.csv"))
