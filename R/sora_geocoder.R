#' Geocoder which retuns coordinates to a given address.
#' @description Send a request with a list of addresses to the SoRa geocoder function.
#' SoRa will forward the request to `BKG` and will return the coordinates.
#' @param input List with the information for:
#' `ID`, `street`, `house number`, `zip code` and `place`.
#' @inheritParams sora_request
#' @returns A tibble or data.frame with the `id`, `coordinates (components)`
#' and a `score` for quality (range from 0 to 1).
#' @export
#' @examplesIf isTRUE(Sys.getenv("SORA_RUN_EXAMPLES")) && sora_available()
#' addresses <- data.frame(
#' "id" = c("1", "2"),
#' "street" = c("Einsteinufer", "An d. Wuhlheide"),
#' "house_number" = c("69", "263"),
#' "zip_code" = c("10587", "12555"),
#' "place" = c("Berlin", "Berlin")
#' )
#' result <- sora_geocoder(input = addresses)
sora_geocoder <- function(input, verbose = NULL, api_key = NULL) {
  input <- check_address_list(input = input)
  input <- list("items" = input)
  res <- api_request(endpoint = "geocoder",
                     body = input,
                     method = "POST",
                     api_key = api_key)
  notify_if_needed(res$notification, verbose)
  invisible()
  out <- as_df(res$items)
  out
}


#' Check the style of the input for the list of addresses.
#' @param input List with the information for:
#' `ID`, `street`, `house number`, `zip code` and `place`
#' @noRd
check_address_list <- function(input) {
  input <- as.data.frame(lapply(input, as.character))
  required_column <- c("id", "street", "house_number", "zip_code", "place")
  abort_req <- any(!is.element(required_column, names(input)))
  
  if (abort_req) {
    missing_column <- setdiff(required_column, names(input))
    sora_abort("i" = "missing required column:", paste("-", quote_arg(missing_column)))
  }
  input[, required_column]
}


#' @export
print.sora_results <- function(x, ...) {
  if (loadable("tibble")) {
    print(tibble::as_tibble(x))
  } else {
    print.data.frame(x)
  }
  out.provenace <- attributes(x)
  provenance.url <- out.provenace$provenance$url[1]
  provenance.citation <- out.provenace$provenance$citation[1]
  if (!is.null(provenance.url)) {
    cat("\n")
  }
  if (!is.null(provenance.citation)) {
    line.break <- "\n"
  }
  if (!is.null(provenance.url)) {
    if (!is.na(provenance.url)) {
      if (provenance.url != "") {
        cat(paste("url for provenance:", out.provenace$provenance$url[1], line.break))
      }
    }
  }
  if (!is.null(provenance.citation)) {
    if (!is.na(provenance.citation)) {
      if (provenance.citation != "") {
        cat(paste("citation:", provenance.citation))
      }
    }
  }
}
