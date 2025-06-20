#' @description Infix operator to fill in certain values with a default
#' @param x An object.
#' @param y An alternative object for x.
#' @returns y if x is null/empty, x otherwise.
#' @noRd
"%||%" <- function(x, y) if (is.null(x)) y else x
"%|||%" <- function(x, y) if (is.null(x) || all(is.na(x))) y else x
"%empty%" <- function(x, y) if (!length(x)) y else x


#' Checks if a package is installed.
#' @param x Name of a package.
#' @returns TRUE or FALSE
#' @noRd
loadable <- function(x) {
  requireNamespace(x, quietly = TRUE)
}


#' Checks if verbosity is set and if not uses the option `sora_verbose` to
#' set it. Also checks if the value is a flag.
#' @param verbose A logical passed from upstream functions.
#' @returns TRUE or FALSE
#' @noRd
peek_verbose <- function(verbose) {
  verbose <- verbose %||% getOption("sora_verbose", TRUE)
  check_flag(verbose)
  verbose
}


#' Find a regex in a string. Wrapper around regexec.
#' @param text String to be searched.
#' @param pattern Regular expression.
#' @param ... Passed to regexec.
#' @returns The regex match.
#' @noRd
regex_match <- function(text, pattern, ...) {
  regmatches(text, regexec(pattern, text, ...))
}


#' Returns the last value of a vector or list.
#' @param x A vector or list.
#' @returns The last value of a vector or list.
#' @noRd
last <- function(x) {
  x[length(x)]
}


#' Drops all NA from a vector or list.
#' @param x A vector or list.
#' @returns A vector or list free of NAs.
#' @noRd
drop_na <- function(x) {
  x[!is.na(x)]
}


#' Checks if all values in x are NULL.
#' @param x A list.
#' @returns TRUE or FALSE.
#' @noRd
all_null <- function(x) {
  all(vapply(x, is.null, FUN.VALUE = logical(1)))
}


#' Checks if a value is coercible to numeric.
#' @param x An object.
#' @returns TRUE or FALSE
#' @noRd
is_number <- function(x) {
  !anyNA(suppressWarnings(as.numeric(drop_na(x))))
}


#' Boxes single atomic values. Used for JSON serialization.
#' @param x An object.
#' @returns If x is atomic and length 1, returns x in a list.
#' @noRd
box <- function(x) {
  if (is.atomic(x) && length(x) == 1) {
    x <- list(x)
  }
  x
}


#' If tibble is installed, converts to a tibble. Otherwise, converts to a
#' dataframe
#' @param x An object that can be coerced to a dataframe.
#' @returns A tibble or a dataframe.
#' @noRd
as_df <- function(x) {
  if (loadable("tibble")) {
    tibble::as_tibble(x)
  } else {
    as.data.frame(x)
  }
}


#' Styles a string as a URL using ANSI escape sequences. If possible uses `cli`.
#' @param x A character vector.
#' @returns A styled string.
#' @noRd
url <- function(x, url, ...) {
  if (!loadable("cli")) {
    return(url)
  }
  
  cli::style_hyperlink(x, url, params = list(...)) # nocov
}


#' Styles a string as underlined using ANSI escape sequences. If possible uses `cli`.
#' @param x A character vector.
#' @returns A styled string.
#' @noRd
underline <- function(x) {
  if (!loadable("cli")) {
    return(x)
  }
  
  cli::style_underline(x) # nocov
}


#' Styles a string as bold using ANSI escape sequences. If possible uses `cli`.
#' @param x A character vector.
#' @returns A styled string.
#' @noRd
bold <- function(x) {
  if (!loadable("cli")) {
    return(x)
  }
  
  cli::style_bold(x) # nocov
}


#' Styles a string as italic using ANSI escape sequences. If possible uses `cli`.
#' @param x A character vector.
#' @returns A styled string.
#' @noRd
italic <- function(x) {
  if (!loadable("cli")) {
    return(x)
  }
  
  cli::style_italic(x) # nocov
}


#' Styles a string using ANSI colors. If `cli` is not loadable, returns its
#' input.
#' @param x A string.
#' @param color A color corresponding to a `cli::col_*` function.
#' @returns A styled string.
#' @noRd
color <- function(x, color) {
  if (!loadable("cli")) {
    return(x) # nocov
  }
  
  fun <- get0(
    paste0("col_", color),
    envir = asNamespace("cli"),
    mode = "function",
    ifnotfound = identity
  )
  
  fun(x)
}


#' Wraps a string in backticks to symbolize code.
#' @param x A string.
#' @returns A string.
#' @noRd
code <- function(x) {
  sprintf("`%s`", x)
}