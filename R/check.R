obj_name <- function(x, env = parent.frame()) {
  deparse1(substitute(x, env))
}


check_class <- function(x, cls, null = FALSE, name = obj_name(x)) {
  if (null && is.null(x)) return()
  if (!inherits(x, cls)) {
    cls1 <- paste(class(x), collapse = "/")
    cls2 <- commas(sprintf("<%s>", cls), last = "or")
    
    sora_abort(
      sprintf("Argument `%s` is expected to be of class %s.", name, cls2),
      "*" = sprintf("Got an object of class <%s> instead", cls1)
    )
  }
}


check_vector <- function(x, type = NULL, len = NULL, null = FALSE, name = obj_name(x)) {
  if (null && is.null(x)) return()
  if (!is.null(type) && !typeof(x) %in% type) {
    sora_abort(
      sprintf("Argument `%s` must be a vector of type %s.", name, type),
      "*" = sprintf("Got an object of type %s instead.", typeof(x))
    )
  }
  
  if (!is.null(len) && !length(x) == len) {
    sora_abort(
      sprintf("Argument `%s` must be a vector of length %s.", name, len),
      "*" = sprintf("Got an object of length %s instead.", length(x))
    )
  }
}


check_string <- function(x, null = FALSE, name = obj_name(x)) {
  check_vector(x, type = "character", len = 1, null = null, name = name)
}


check_flag <- function(x, null = FALSE, name = obj_name(x)) {
  if (null && is.null(x)) return()
  if (!isTRUE(x) && !isFALSE(x)) {
    sora_abort(sprintf("Argument `%s` must be TRUE or FALSE.", name))
  }
}


check_dots_named <- function(...) {
  if (...length() && is.null(...names())) {
    sora_abort("All arguments in `...` must be named.")
  }
}


check_coercible <- function(x, len = NULL, null = FALSE, name = obj_name(x)) {
  if (null && is.null(x)) return()
  check_vector(x, len = len)
  if (!is_number(x)) {
    sora_abort(
      sprintf("All values of argument `%s` must be coercible to numeric.", name),
      "i" = sprintf("This means that some values within %s are not valid numbers.", name)
    )
  }
}


check_cols <- function(x, cols = NULL, n = NULL, null = FALSE, name = obj_name(x)) {
  if (null && is.null(x)) return()
  check_class(x, "data.frame")
  obj_name <- deparse(x)
  
  if (!is.null(cols) && !all(cols %in% names(x))) {
    sora_abort(
      sprintf("Argument `%s` must have columns %s", name, commas(cols)),
      "*" = sprintf("Instead got %s", commas(dQuote(names(x), q = FALSE)))
    )
  }

  if (!is.null(n) && !ncol(x) >= n) {
    sora_abort(
      sprintf("Argument `%s` must have %s or more columns.", name, n),
      "*" = sprintf("Got %s columns instead.", ncol(x))
    )
  }
}


check_url <- function(x, null = FALSE, name = obj_name(x)) {
  if (null && is.null(x)) return()
  check_string(x, name = name)
  parsed <- try(httr2::url_parse(x), silent = TRUE)
  if (inherits(parsed, "try-error")) {
    cond <- attr(parsed, "condition")
    sora_abort(
      sprintf("Argument `%s` must be a valid URL.", x),
      "i" = cond$message
    )
  }
}


check_loadable <- function(pkg, to) {
  if (!loadable(pkg)) {
    sora_abort(
      sprintf("Package \"%s\" is required to %s.", pkg, to),
      "*" = sprintf("You can install it using `install.packages(\"%s\")`.", pkg)
    )
  }
}


check_api_key <- function(api_key) {
  if (is.null(api_key) || all(is.na(api_key)) || !all(nzchar(api_key))) {
    abort_missing_api_key("missing")
  }
  
  if (isTRUE(!grepl("^[a-z0-9]+$", api_key))) {
    abort_missing_api_key("invalid")
  }
}
