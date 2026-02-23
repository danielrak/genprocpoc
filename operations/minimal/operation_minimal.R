# Minimal operation execution from the mask
# Using purrr:: functionalities

# Packages ----------------------------------------------------------------

library(dplyr)
library(purrr)
library(readr)

# Parameters --------------------------------------------------------------

path_mask <- "./mask"
path_output <- "./out"
path_function <- "./from_example_to_function"

# Mask --------------------------------------------------------------------

mask <- read_csv(file.path(path_mask, "mask.csv"))

# Function ----------------------------------------------------------------

load(file.path(path_function, "from_example_to_function.rda"))

# From example to function ------------------------------------------------

# There are some datasets that are not data frame to begin with. 
# (And there is an .R file listed in the mask)

# Suppose user successfully converts one data frame among rds inputs into a csv: 
myexpr <- expression({
  data <- readr::read_rds(file.path("./r_built_in_datasets", "airquality.rds"))
  data %>% readr::write_csv(file.path("./out", "airquality.csv"))
})

eval(myexpr) # ./out/airquality.csv should now be produced

# The corresponding function: 
conversion_function <- from_example_to_function(myexpr)

conversion_function
# function (param_1 = "./r_built_in_datasets", param_2 = "airquality.rds", 
#     param_3 = "./out", param_4 = "airquality.csv") 
# {
#     data <- readr::read_rds(file.path(param_1, param_2))
#     data %>% readr::write_csv(file.path(param_3, param_4))
# }

conversion_function(param_1 = "./r_built_in_datasets", param_2 = "anscombe.rds",
                    param_3 = "./out", param_4 = "anscombe.csv")
# ./out/anscombe.csv should now be produced


# Export the function ---------------------------------------

# For next steps: 
# parallelized, 
# parallelized & non-blocking, 
# parallelized & non-blocking & logged
# parallilized & non-blocking & logged & monitored

save(conversion_function, file = "./operations/conversion_function.rda", 
     version = 2)

# Operation ---------------------------------------------------------------

mask %>% 
  select(param_1 = input_dir, param_2 = input_file, 
         param_3 = output_dir, param_4 = output_file) %>% 
  pmap(.f = \(...) {
    # error-robust version of conversion_function
    out <- safely(conversion_function)(...)
    if (is.null(out$error)) {NULL} else conditionMessage(out$error)
  })
# all rds that could be read as data.frame and written as csv shoud now be in ./out
  