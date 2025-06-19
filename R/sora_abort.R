#' @description Signal an error caused by the sora package
#' @param msg Main message, typically indicating the problem that occured.
#' @param extra Extra info used to add possible solutions or details.
#' @param ... Extra lines to print after `msg` and `extra`.
#' @param .class Subclass to add to the error object. Used for testing.
#' @param .call A `call` object printed in the error message. If `NULL`, no
#' call is shown.
#' @param .env An `environment` object from which the error is signaled.
#' @returns An error.
#' @noRd
sora_abort <- function(...,
                       .footer = FALSE,
                       .class = NULL,
                       .call = NULL,
                       .env = parent.frame()) {
  .call <- .call %||%
    get_main_caller() %||%
    get_caller()
  
  if (is.character(.call)) {
    .call_fmt <- bold(paste0(.call, "()"))
    .call <- sprintf("function `%s`", .call_fmt)
  }
  
  if (...length()) {
    msg <- format_msg(...)
    msg <- paste("!", msg)
  }
  
  if (.footer) {
    long_line <- strrep(symbols$line, 3)
    footer <- paste(long_line, "The request was unsuccessful", long_line)
    msg <- paste(msg, "\n\n", footer)
  }
  
  .class <- c(.class, "sora_error")
  abort(msg, .class = .class, .call = .call, .env = .env)
}


#' Signals an error with a custom error message
#' @param msg Error message
#' @param .class Subclasses to append to the error object classes. Used for
#' testing purposes.
#' @param .call Error call to display. If `NULL`, omits the call.
#' @param .env Calling environment. Used to determine if called from the
#' top level.
#' @returns An error.
#' @noRd
abort <- function(msg, .class = NULL, .call = sys.call(1), .env = parent.frame()) {
  if (identical(.env, globalenv())) {
    .call <- NULL # nocov
  }
  
  cnd <- errorCondition(msg, call = .call, class = .class)
  signalCondition(cnd)
  
  if (is.null(.call)) { # nocov start
    msg <- sprintf("Error:\n%s", msg)
  } else {
    if (is.language(.call)) {
      .call <- deparse1(.call)
    }
    msg <- sprintf("Error in %s:\n%s", .call, msg)
  }
  
  cat(msg, "\n", file = stderr())
  old_opts <- options(show.error.messages = FALSE)
  on.exit(options(old_opts))
  msg <- NULL
  stop(msg) # nocov end
}


#' Error message to show when the API key is missing or invalid.
#' @param problem The kind of problem with the API key. Should be either
#' missing or invalid.
#' @returns Nothing, invokes an error.
#' @noRd
abort_missing_api_key <- function(problem = "missing") {
  caller <- get_main_caller()
  link <- url(
    "https://sora.gesis.org/",
    "https://sora.gesis.org/unofficial/sora-user-mod/users/request-api-key"
  )
  
  sora_abort(
    sprintf("API key is %s.", problem),
    "i" = "An API key is necessary to communicate with the SoRa service.",
    "i" = sprintf("You can register for an API key under %s.", link),
    "You can set a key using one of the following steps:",
    " *" = "Set `SORA_API_KEY`=\"<your key>\" in your personal .Renviron file.",
    " *" = sprintf(
      "Call `%s(..., api_key = \"ask\")` to open an interactive prompt.",
      caller
    ),
    " *" = "Use `Sys.setenv(SORA_API_KEY = \"<your key>\")` to set a key for this session (not recommended).",
    " *" = sprintf(
      "Call `%s(..., api_key = \"<your key>\")` to set a key directly (not recommended).",
      caller
    ),
    "Please never publish your API key in plain text!"
  )
}


#' Error message to show when the API returns an error. Extracts information
#' from the response object.
#' @param resp Response object from the SoRa API.
#' @returns Nothing, invokes an error.
#' @noRd
abort_api_error <- function(resp) {
  body <- httr2::resp_body_json(resp)
  code <- body$code
  msg <- body$message
  msg <- sprintf("Error code %s: %s", bold(code), msg)
  sora_abort(msg)
}


#' Error message to show when the server returns an error. Extracts
#' information from the response object.
#' @param resp Response object from the SoRa API.
#' @returns Nothing, invokes an error.
#' @noRd
abort_http_error <- function(resp) {
  code <- httr2::resp_status(resp)
  msg <- httr2::resp_status_desc(resp)
  msg <- sprintf("HTTP error %s: %s", bold(code), msg)
  sora_abort(msg, "i" = paste(
    "This is an unknown error. If it's not going away, open a",
    "bug report or contact the maintainers (sora@gesis.org)."
  ))
}


#' Error message to show when the API is unavailable.
#' @returns Nothing, invokes an error.
#' @noRd
abort_api_not_available <- function() {
  sora_abort(
    "The SoRa service is currently unavailable.",
    "i" = "Please try again later or contact sora@gesis.org."
  )
}
