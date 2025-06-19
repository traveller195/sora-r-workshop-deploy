#' Is SoRa available?
#' @description Checks whether all of the required SoRa components are up and
#' working and whether your API key is authorized. \code{sora_available}
#' always returns \code{TRUE} or \code{FALSE}. \code{sora_services} returns
#' a list of available and unavailable services. \code{sora_key_status} returns
#' a list containing details on the status of the API key.
#'
#' @param services If provided, checks if the specified services are
#' available. If \code{NULL} (default), checks if any service is available.
#' @inheritParams sora_request
#'
#' @returns \itemize{
#'  \item{\code{sora_available} returns \code{TRUE} or \code{FALSE}}
#'  \item{\code{sora_services} returns a named list of class
#'  \code{sora_services} containing information about available and unavailable
#'  services.}
#'  \item{\code{sora_key_status} returns a named list of class
#'  \code{sora_key_status.}}
#' }
#'
#' @export
#'
#' @examplesIf isTRUE(Sys.getenv("SORA_RUN_EXAMPLES")) && sora_available()
#' # Checks whether the SoRa API can be accessed
#' sora_available()
#' 
#' # See what service providers are available
#' sora_services()
#' 
#' # Get details about a provided API key
#' sora_key_status()
#' 
#' # These functions can be used to create basic control flows
#' try(stopifnot(sora_available()))
#' try(stopifnot("IOER" %in% sora_services()$available))
#' try(stopifnot("ALLOW" %in% sora_key_status()$decision))
sora_available <- function(services = NULL, api_key = NULL) {
  resp <- api_request(
    endpoint = "test-connectivity",
    api_key = api_key,
    check_key = FALSE
  )

  if (identical(resp$code, 301L)) {
    return(FALSE) # nocov
  }
  
  unavailable <- resp[["unavailable-services"]]
  status <- resp[["api-key-status"]]$decision
  has_services <- if (is.null(services)) {
    !length(unavailable)
  } else {
    services %in% unavailable
  }
  
  identical(status, "ALLOW") && has_services
}


#' @rdname sora_available
#' @export
sora_services <- function(api_key = NULL) {
  resp <- api_request(endpoint = "test-connectivity", api_key = api_key)

  available <- list(
    available = resp[["available-services"]] %empty% NULL,
    unavailable = resp[["unavailable-services"]] %empty% NULL
  )
  
  class(available) <- "sora_services"
  available
}


#' @rdname sora_available
#' @export
sora_key_status <- function(api_key = NULL) {
  resp <- api_request(endpoint = "test-connectivity", api_key = api_key)
  
  status_rep <- resp$`api-key-status`
  key_status <- list(
    code = status_rep$`response-code`,
    title = status_rep$title,
    message = status_rep$message,
    decision = status_rep$decision
  )
  
  class(key_status) <- "sora_key_status"
  key_status
}


#' @export
print.sora_services <- function(x, ...) {
  msg <- NULL
  if (!is.null(x$available)) {
    bullets_available <- paste(symbols$bullet, x$available)
    bullets_available <- paste(bullets_available, collapse = "\n")
    msg <- paste0(msg, "Available services:\n", bullets_available)
  }
  
  if (all(lengths(x) > 0)) {
    msg <- paste0(msg, "\n\n")
  }
  
  if (!is.null(x$unavailable)) {
    bullets_unavailable <- paste(symbols$bullet, x$unavailable)
    bullets_unavailable <- paste(bullets_unavailable, collapse = "\n")
    msg <- paste0(msg, "Unavailable services:\n", bullets_unavailable)
  }
  
  cat(msg, "\n", ...)
  invisible(x)
}



#' @export
print.sora_key_status <- function(x, ...) {
  decision <- x$decision
  col <- switch(decision, ALLOW = "green", DISALLOW = "red")
  decision <- color(decision, col)
  msg <- sprintf("%s %s %s", decision, symbols$line, x$message)
  cat(msg, "\n", ...)
  invisible(x)
}