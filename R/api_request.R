#' Low-level function to communicate with the SoRa API. Used in all high-level
#' SoRa functions that make API requests.
#' @param endpoint An endpoint of the API. Usually "jobs" or
#' "test-connectivity".
#' @param body A list of parameters to send along with the request body.
#' If a body is provided, assumes a POST request.
#' @param method HTTP method. Defaults to GET but POST and DELETE are also used.
#' @param api_key An API key to send with the request. Is validated before
#' sending. Necessary for all requests except if check_key = FALSE
#' @param check_key Whether to validate `api_key`. Useful if you want to
#' manually check whether an API key is valid.
#' @returns A parsed response JSON from the API.
#' @noRd
api_request <- function(endpoint,
                        body = NULL,
                        method = "GET",
                        api_key = NULL,
                        check_key = TRUE) {
  api_key <- get_sora_key(api_key)
  if (check_key) {
    check_api_key(api_key)
  }
  
  req <- httr2::request(sora_api())
  req <- httr2::req_url_path_append(req, endpoint)
  req <- httr2::req_headers(req, `api-key` = api_key)
  req <- httr2::req_method(req, method)

  if (!is.null(body)) {
    req <- httr2::req_body_json(req = req, data = body, na = "null")
  }

  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  sora_extract_body(resp, check_key = check_key)
}


#' Given a response from the SoRa API, tries to extract the JSON body. If
#' not possible, aborts using appropriately formatted error messages.
#' @param resp A httr2 response.
#' @param check_key Whether to abort if authentication fails. Used for
#' `sora_available`.
#' @returns A string or a parsed JSON.
#' @noRd
sora_extract_body <- function(resp, check_key = TRUE) {
  sora_check_error(resp, check_key = check_key)

  is_json <- grepl("json", httr2::resp_content_type(resp), fixed = TRUE)
  if (isTRUE(is_json)) {
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  } else {
    httr2::resp_body_string(resp)
  }
}


#' Given a response object from the SoRa object, checks if the response
#' returned an error and, if so, aborts. An exception can be made for
#' key validations.
#' @param resp A httr2 response.
#' @param check_key Whether to abort if authentication fails. Used for
#' `sora_available`.
#' @returns NULL or an error.
#' @noRd
sora_check_error <- function(resp, check_key = TRUE) {
  status <- httr2::resp_status(resp)
  if (identical(status, 200L)) {
    return(invisible())
  }
  
  type <- httr2::resp_content_type(resp)
  if (identical(type, "application/json")) {
    body <- httr2::resp_body_json(resp, simplifyVector = TRUE)
    
    if (!check_key && body$code %in% c(301, 302)) {
      return(invisible())
    }
    
    abort_api_error(resp)
  }
  
  if (identical(status, 503L)) {
    abort_api_not_available()
  }
  
  abort_http_error(resp)
}


#' Retrieves the SoRa API key. Performs up to three checks (in this order):
#' - If a key is passed as an argument, take it
#' - If a key is stored in the SORA_API_KEY envvar, take it
#' - If "ask" is passed as an API key, take the key from an interactive input
#' - If all fails, return NULL
#' @param api_key Either a valid API key, NULL or "ask".
#' @returns An API key or NULL
#' @noRd
get_sora_key <- function(api_key = NULL) {
  if (identical(api_key, "ask")) {
    check_loadable("askpass", "open a masked prompt") # nocov start
    api_key <- askpass::askpass("Please enter your SoRa key:")
    Sys.setenv(SORA_API_KEY = api_key) # nocov end
  }
  
  api_key %||% Sys.getenv("SORA_API_KEY", NA) %|||% NULL
}