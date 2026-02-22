# R built-in datasets

# Writing all available R buil-in datasets into RDS. 
# Those are very light files. 

library(datasets)
library(purrr)

path_out <- "./r_built_in_datasets"

dlist <- data()$results[, "Item"]

map(dlist, \(x) {
  tryCatch({
    data <- get(x)
    saveRDS(data, file.path(path_out, paste0(x, ".rds")))
  }, error = \(e) e$message)
})
