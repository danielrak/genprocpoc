# From an example code to a function

# This function takes an expression as input, 
# and produce the corresponding function as an output. 

# Illustration
# Input: expr({readRDS(file.path("./r_built_in_datasets", "airmiles.rds"))})
# Output: 
# function (param_1, param_2) {
#   readRDS(file.path(param_1 = "./r_built_in_datasets", 
#                     param_2 = "airmiles.rds"))}

# Parameters --------------------------------------------------------------

path_out <- "./from_example_to_function"

# The function ------------------------------------------------------------

from_example_to_function <- function(expr, env = parent.frame()) {
  # Validate that we received exactly one quoted expression to transform.
  stopifnot(is.expression(expr), length(expr) == 1)
  
  # Create a mapping store from "keys" (unique identifiers for things we want to parameterize)
  # to the generated parameter names (param_1, param_2, ...).
  # `emptyenv()` prevents accidental name resolution through parent environments:
  # this env is used purely as a hash-map.
  key_to_param <- new.env(parent = emptyenv())
  
  # Store the function parameters we will generate, along with their default values.
  params <- list()
  
  make_param <- function(key, default) {
    # Return the existing parameter name for `key` if already created,
    # otherwise create a new parameter and remember its default value.
    # This ensures structural sharing: repeated occurrences of the same constant/symbol
    # become the same function argument (stable mapping).
    if (exists(key, envir = key_to_param, inherits = FALSE)) {
      get(key, envir = key_to_param, inherits = FALSE)
    } else {
      # Generate a fresh parameter name (param_1, param_2, ...) and save it.
      # The name depends on `length(params)` *at creation time*, which makes ordering deterministic
      # given traversal order of `rewrite()`.
      pname <- paste0("param_", length(params) + 1)
      
      # Record the mapping key -> parameter name.
      # Stored in `key_to_param` to de-duplicate parameters across the expression tree.
      assign(key, pname, envir = key_to_param)
      
      # Record the default value for this parameter under its new name.
      # The `<<-` is intentionally used so the nested function updates the outer `params` list.
      params[[pname]] <<- default
      
      pname
    }
  }
  
  is_missing_arg <- function(x) {
    # Detect whether a call argument is the special "missing" placeholder,
    # in which case we must not rewrite it.
    # `rlang::missing_arg()` represents an omitted argument in call objects;
    # the symbol-with-empty-name edge case covers some malformed or special constructs.
    identical(x, rlang::missing_arg()) ||
      (is.symbol(x) &&
         length(as.character(x)) == 1 && as.character(x) == "")
  }
  
  rewrite <- function(node, bound = character()) {
    # Main recursive transformer. Walks the expression tree and replaces:
    # - literal strings with parameters
    # - symbols that resolve to non-function objects in `env` with parameters
    # while respecting locally-bound names (e.g., inside lambdas).
    # `bound` is a simple scoping tracker for formal argument names of nested functions.
    
    if (is.character(node) && length(node) == 1) {
      # Turn a literal string into a parameter symbol (e.g., "x" -> param_1).
      # We prefix the key with `chr:` to avoid collisions with symbol keys.
      key <- paste0("chr:", node)
      pname <- make_param(key, node)
      return(rlang::sym(pname))
    }
    
    if (is.symbol(node)) {
      # Decide whether a symbol should remain as-is (names, functions, bound variables),
      # or be replaced by a parameter (captured value from `env`).
      # `as.character()` on a symbol yields its name.
      s <- as.character(node)
      
      # If the symbol name is not a single valid-ish string, do not touch it.
      # Technical note: This avoids breaking special/invalid symbols, including the empty-name symbol.
      if (length(s) != 1 || is.na(s) || s == "")
        return(node)
      
      # If this symbol is locally bound (e.g., lambda argument), keep it unchanged.
      # This is the key scoping rule preventing accidental parameterization of local variables.
      if (s %in% bound)
        return(node)
      
      if (exists(s, envir = env, inherits = TRUE)) {
        # If the symbol resolves in `env`, capture its value when it is NOT a function,
        # by replacing the symbol with a generated parameter.
        # This is a common "example -> function" behavior: convert referenced constants
        # from the surrounding environment into explicit function parameters.
        val <- get(s, envir = env, inherits = TRUE)
        if (!is.function(val)) {
          # Use a symbol-key namespace and create/lookup a parameter for this external value.
          # Keying on the symbol name means repeated references to `s` map to the same parameter,
          # even if `val` is identical to another symbol's value.
          key <- paste0("sym:", s)
          pname <- make_param(key, val)
          return(rlang::sym(pname))
        }
      }
      
      return(node)
    }
    
    if (is.expression(node)) {
      # Recursively rewrite each element of an expression vector.
      # `expression()` can hold multiple top-level expressions; here we keep structure by
      # mapping over `as.list(node)` then converting back with `as.expression()`.
      out <- lapply(as.list(node), rewrite, bound = bound)
      return(as.expression(out))
    }
    
    if (is.call(node)) {
      # Handle language calls (function calls, operators, and function/lambda definitions).
      # R represents code like `f(x)` or `\(...){...}` as `call` objects.
      head <- node[[1]]
      head_name <- if (is.symbol(head))
        as.character(head)
      else
        NULL
      
      # Dots case
      if (!is.null(head_name) &&
          head_name %in% c("function", "\\")) {
        # Special-case nested function/lambda definitions to respect local scope:
        # add their formal names to `bound`, and rewrite only the body with that augmented bound set.
        # This prevents rewriting occurrences of formal parameters inside the lambda body.
        formals_pl <- node[[2]]
        new_bound <- c(bound, names(formals_pl))
        new_body <- rewrite(node[[3]], bound = new_bound)
        return(as.call(list(head, formals_pl, new_body)))
      }
      
      # Do not rewrite missing argument
      # Recursively rewrite each argument of the call, except missing arguments.
      # We start from `as.list(node)` so we can mutate elements, then rebuild via `as.call()`.
      args <- as.list(node)
      for (i in 2:length(args)) {
        if (!is_missing_arg(args[[i]])) {
          args[[i]] <- rewrite(args[[i]], bound = bound)
        }
      }
      return(as.call(args))
    }
    
    # Fallback for all other node types (numbers, NULL, language objects not handled above, etc.).
    # Most atomic constants (numeric, logical) are left untouched by design in this version.
    node
  }
  
  # Apply the tree-rewriting logic to the user's single expression to produce the new function body.
  # `expr[[1]]` extracts the only element; `bound = character()` starts with no locally-bound names.
  new_body <- rewrite(expr[[1]], bound = character())
  
  # Build the function's formal arguments from the collected defaults (param_1=..., param_2=..., ...).
  # `pairlist2(!!!params)` splices the named list into a pairlist, which is how R stores formals.
  fmls <- rlang::pairlist2(!!!params)
  
  # Create and return a new function with generated formals and rewritten body.
  # The function environment is set to `env` so that remaining names (functions, etc.)
  # resolve as they did in the original example context.
  rlang::new_function(args = fmls, body = new_body, env = env)
}

# Simple tests -------------------------------------------------------------------

library(dplyr)
library(purrr)

## 1 ---------------------------------------------------------------------

myexpr_1 <- expression({
  map(c("cars", "mtcars"), get) %>%
    map(mutate_all, as.character)
})

myfunc_1 <- from_example_to_function(myexpr_1)

myfunc_1(param_1 = "airquality", param_2 = "anscombe")

## 2 ---------------------------------------------------------------------

myexpr_2 <- expression({
  map(c("cars", "mtcars"), get) %>% 
    map(function (x) mutate_all(x, as.character))
})

myfunc_2 <- from_example_to_function(myexpr_2)

myfunc_2(param_1 = "airquality", param_2 = "anscombe")

## 3 ---------------------------------------------------------------------

myexpr_3 <- expression({
  map(c("cars", "mtcars"), get) %>% 
    map(function (x) mutate_all(x, as.character)) %>% 
    map(\(x) x[, 1:2])
})

myfunc_3 <- from_example_to_function(myexpr_3)

myfunc_3(param_1 = "airquality", param_2 = "anscombe")

# Export ------------------------------------------------------------------

save(from_example_to_function, 
     file = file.path(path_out, "from_example_to_function.rda"),
     version = 2)
