#' Get and set SoRa API
#' @description
#' Set a non-default URL to communicate with the SoRa API. This is necessary
#' when working from a secure room or when connecting to a different SoRa
#' service (e.g., from a different country). By default, the \code{sora}
#' package sends requests to the default public API, which is not
#' available in secure rooms.
#' 
#' If you are working inside a secure room, make sure to ask the staff for
#' the SoRa URL. You can then set it using `sora_set_api()`. To retrieve the
#' currently set API, use `sora_api()`.
#' 
#' @param url A valid URL to a SoRa API. If \code{NULL}, sets the URL to
#' the default public API of the SoRa service. Defaults to \code{NULL}.
#' @inheritParams sora_request
#' @returns Both functions return the currently (or newly) set SoRa API,
#' but \code{sora_set_api} returns it invisibly.
#' 
#' @export
#' 
#' @details
#' \code{sora_set_api} is only able to set an API URL for the current session.
#' It is reset upon starting a new session. To set a persistent path,
#' set the environment variable manually inside your system's environment
#' variable storage or use an \code{.Renviron} file.
#' 
#' 
#' @examples
#' # Get the current API
#' sora_api()
#' 
#' # Set a new API, e.g., inside an air-gapped network
#' sora_set_api("ftp://secure-room-data/")
#' sora_set_api("https://10.1.1.1/")
#' 
#' # Get the new API
#' sora_api()
#' 
#' # Reset to the public API
#' sora_set_api()
sora_api <- function() {
  Sys.getenv("SORA_API", sora_default_api())
}


#' @rdname sora_api
#' @export
sora_set_api <- function(url = NULL, verbose = NULL) {
  check_url(url, null = TRUE)
  verbose <- peek_verbose(verbose)
  old <- sora_api()
  default <- sora_default_api()
  
  if (is.null(url)) {
    Sys.unsetenv("SORA_API")
  } else {
    Sys.setenv(SORA_API = url)
  }

  url <- url %||% default
  if (verbose && !identical(url, old)) {
    sora_info(sprintf("Set new API: %s", url %||% default))
  }
  
  invisible(url)
}


#' Default public API.
#' @noRd
sora_default_api <- function() {
  "https://sora.gesis.org/unofficial/sora-public-api/"
}