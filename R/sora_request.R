#' Commission a linking job
#' @description Send request to the SoRa API. After the request has been
#' made, a linking job is created in the SoRa API. After it is finished,
#' you can retrieve the requests using \code{\link{sora_results}}.
#' 
#' To see if your linking job is successful without actually commissioning
#' a request you can use \code{sora_simulate}.
#'
#' @param dataset An object of class \code{\link{sora_custom}},
#' \code{\link{sora_provided}}, or \code{\link{sora_admin}}. Each object
#' type assumes a different type of data input:
#' 
#' \itemize{
#'  \item{\code{sora_custom} is used to provide your own point data.}
#'  \item{\code{sora_provided} is used to link to a dataset provided by the
#'  SoRa service. These are either sensitive microdata or structural datasets
#'  that mirror the distribution of microdata.}
#'  \item{\code{sora_admin} is used to provide identifiers from regional
#'  administrative datasets, e.g. zip codes or municipalities.}
#' }
#' 
#' @param link_to An object of class \code{\link{sora_spatial}} or a character
#' string containing a valid dataset ID in \code{\link{sora_spatial}}. This
#' argument is used to specify the spatial dataset to link your \code{dataset}
#' to. It can either be a spatial dataset provided by SoRa or a dataset provided
#' by you.
#' 
#' @param method An object of class \code{\link{sora_linking}} or a character
#' string containing a valid linking method in \code{\link{sora_linking}}.
#' This argument specifies how to link the data provided in \code{dataset}
#' and \code{link_to}.
#' 
#' @param ... If \code{method} is a character string, you can use \code{...}
#' to provide further arguments to \code{\link{sora_linking}}.
#'
#' @param provenance An object of class \code{\link{sora_provenance}} or
#' a logical value that specifies if and how to generate a provenance record.
#' If a user-provided dataset is passed, additional information are needed to
#' create such a record. By default, no provenance record is produced.
#' 
#' @param wait Whether to wait for the results using \code{\link{sora_wait}}.
#' If \code{TRUE}, runs \code{\link{sora_job_done}} repeatedly until the
#' linking is finished and then fetches the results using
#' \code{\link{sora_results}}. You can interrupt this process at any time and
#' continue manually. If \code{FALSE}, immediately returns the job ID after
#' the request has been commissioned. Defaults to \code{FALSE}. In interactive
#' sessions, a spinner is shown which can be disabled using
#' \code{options(sora_spinner = FALSE)}.
#' 
#' @param provider ID of a linking service provider to perform the
#' spatial linking. Defaults to \code{"IOER"}.
#' 
#' @param verbose A logical, character string or \code{NULL} that specifies if
#' and how to produce messages. If \code{FALSE}, suppresses all informative
#' messages. If a path is provided, stores all messages in the specified file.
#' If \code{NULL}, verbosity depends on \code{getOption("sora_verbose")}. Use
#' the latter to set verbosity across the entire session, e.g. to suppress
#' messages from all package functions. Defaults to \code{NULL} and
#' \code{getOption("sora_verbose")} defaults to \code{TRUE}, i.e., display
#' messages where reasonable.
#' 
#' @param api_key A valid SoRa API key. Required to communicate with the SoRa
#' API. You can request a key by filling out
#' \href{https://sora.gesis.org/unofficial/sora-user-mod/users/request-api-key}{this form}
#' on the SoRa website.
#' 
#' If \code{NULL}, reads a key from the \code{SORA_API_KEY}
#' environment variable.
#' 
#' If "ask" (and the session is interactive), opens a masked prompt where you
#' can enter your key. The entered key is persistently stored in an environment
#' variable and can be used across sessions.
#' 
#' Please note that you are advised not to enter your key directly using this
#' argument. If you do, please make sure that you do not publish any code files
#' containing the key in plain text. Failure to do so can compromise your
#' access to the SoRa API.
#' 
#' @returns If \code{wait = FALSE}, a job ID of the job that has been created by
#' commissioning the request. You can use this ID to poll the status of the
#' linking and fetch its results (see \code{\link{sora_results}}). If
#' \code{wait = TRUE}, automates this workflow and returns a dataframe
#' containing the results of the linking job.
#'
#' @export
#' 
#' @seealso \code{\link{sora_results}}, \code{\link{sora_job_status}},
#' \code{\link{sora_job_done}}
#'
#' @examplesIf isTRUE(Sys.getenv("SORA_RUN_EXAMPLES")) && sora_available()
#' sora_data <- sora_custom(ber_income, crs = 3035)
#' spat_data <- sora_spatial("ioer-monitor-s02rg-2023-1000m")
#' linking <- sora_linking(
#'   method = "aggregate_attribute",
#'   selection_area = "square",
#'   length = 2500,
#'   output = c("mean", "median", "sd")
#' )
#' 
#' # Check if request can be commissioned
#' sora_simulate(sora_data, link_to = spat_data, method = linking)
#' 
#' # Then commission the request
#' jid <- sora_request(sora_data, link_to = spat_data, method = linking)
#' 
#' # A commissioned request can take some time to finish. After finishing
#' # you can fetch the results.
#' sora_wait(jid)
#'
#' # Alternatively, for simpler queries, you can directly insert the raw
#' # string values instead of constructing objects first.
#' sora_simulate(
#'   sora_data,
#'   link_to = "ioer-monitor-s02rg-2023-1000m",
#'   method = "aggregate_attribute",
#'   selection_area = "circle",
#'   radius = 1000,
#'   output = "median"
#' )
sora_request <- function(dataset,
                         link_to,
                         method,
                         ...,
                         provenance = sora_provenance(),
                         wait = FALSE,
                         provider = "IOER",
                         verbose = NULL,
                         api_key = NULL) {
  check_class(dataset, dataset_types())
  link_to <- construct_object(link_to, "sora_spatial")
  method <- construct_object(method, "sora_linking", ...)
  provenance <- construct_object(provenance, "sora_provenance")
  verbose <- peek_verbose(verbose)
  
  dataset <- prepare_dataset(dataset, provenance)
  provenance <- prepare_provenance(provenance)
  method <- prepare_linking(method, link_to, provider, simulate = FALSE)
  body <- sora_body(dataset, method, provenance)
  
  res <- api_request("jobs", body = body, method = "POST", api_key = api_key)
  res <- error_to_warn(res, condition = TRUE)
  res <- order_notification(res)
  report_if_needed(res$validation_report, verbose)
  notify_if_needed(res$notification)
  jid <- res$job_id

  if (wait) {
    sora_wait(jid, verbose = verbose, api_key = api_key)
  } else {
    jid
  }
}


#' @rdname sora_request
#' @export
sora_simulate <- function(dataset,
                          link_to,
                          method,
                          ...,
                          provenance = sora_provenance(),
                          provider = "IOER",
                          api_key = NULL) {
  check_class(dataset, dataset_types())
  link_to <- construct_object(link_to, "sora_spatial")
  method <- construct_object(method, "sora_linking", ...)
  provenance <- construct_object(provenance, "sora_provenance")
  
  dataset <- prepare_dataset(dataset, provenance)
  provenance <- prepare_provenance(provenance)
  method <- prepare_linking(method, link_to, provider, simulate = TRUE)
  body <- sora_body(dataset, method, provenance)
  
  res <- api_request("jobs", body = body, method = "POST", api_key = api_key)
  res <- error_to_warn(res, condition = TRUE)
  res <- order_notification(res)
  report_if_needed(res$validation_report, verbose = TRUE)
  notify_if_needed(res$notification)
  invisible()
}


#' Waiter
#' @description
#' Helper function that takes a job ID and waits until the linking job behind
#' it has finished, then automatically fetches the results. This function is
#' used for the `wait` argument in \code{\link{sora_request}}. Starting a
#' waiter can be particularly useful in continuous workflows where the
#' next steps depend on the availability of the linking results.
#' 
#' @inheritParams sora_jobs
#' @returns The same as \code{\link{sora_results}}, a dataframe containing
#' the case IDs and all requested output statistics.
#' 
#' @details
#' By default, \code{sora_wait} waits for one second before polling the
#' job status again. You can change the option \code{sora_wait_delay} to
#' poll more or less frequently than that.
#' 
#' @export
#' 
#' @examplesIf isTRUE(Sys.getenv("SORA_RUN_EXAMPLES")) && sora_available()
#' job <- sora_request(
#'   sora_custom(ber_income, crs = 3035),
#'   link_to = sora_spatial("ioer-monitor-s02rg-2023-1000m"),
#'   method = sora_linking(
#'     method = "aggregate_attribute",
#'     selection_area = "square",
#'     length = 2500,
#'     output = c("mean", "median", "sd")
#'   )
#' )
#' 
#' sora_wait(job)
sora_wait <- function(job_id, verbose = NULL, api_key = NULL) {
  check_string(job_id)
  verbose <- peek_verbose(verbose)
  
  tryCatch(
    wait_for_results(job_id, verbose = verbose, api_key = api_key),
    interrupt = function(cnd) { # nocov start
      sora_info(
        "!" = "Process has been interrupted. Returning job ID.",
        "i" = sprintf(
          "You can fetch the results by typing `sora_results(%s)`",
          color(dQuote(job_id, q = FALSE), "green")
        )
      )
      old <- options(show.error.messages = FALSE)
      on.exit(options(old))
      job_id
    } # nocov end
  )
}


#' Wait until sora_job_done() returns TRUE, then fetch results of a job.
#' @param jid A job ID.
#' @param verbose Whether to show a spinner.
#' @param api_key A valid API key. Used to poll and fetch.
#' @returns Output of `sora_results()`.
#' @noRd
wait_for_results <- function(jid, verbose = TRUE, api_key = NULL) {
  do_spin <- isTRUE(getOption("sora_spinner", interactive()))
  
  if (loadable("cli") && verbose && do_spin) {
    cli::cli_progress_step(
      sprintf(
        "Waiting for results... Press %s to exit.",
        color("[Esc]", "blue")
      ),
      msg_done = "Successfully fetched results",
      spinner = isTRUE(getOption("sora_spinner", interactive()))
    )
    
    spin <- cli::cli_progress_update
  }

  if (verbose && !do_spin) {
    sora_info("Waiting for results...")
  }
  
  while (!sora_job_done(jid, api_key = api_key)) {
    if (verbose && do_spin) {
      spin(status = sprintf(
        "Waiting for results... Press %s to exit.",
        color("[Esc]", "blue")
      ))
    }
    
    delay <- getOption("sora_wait_delay", 1)
    Sys.sleep(delay)
  }
  
  reset_spinner()
  sora_results(jid, verbose = verbose, api_key = api_key)
}


#' Formats the request body for a SoRa request.
#' @param dataset Created by `sora_custom`, `sora_provided` or `sora_admin`.
#' @param method Created by `sora_linking`.
#' @param provenance Created by `sora_provenance`.
#' @returns A named list, the request body.
#' @noRd
sora_body <- function(dataset, method, provenance) {
  data_name <- switch(
    class(dataset)[1],
    sora_custom = "user_provided_coordinates",
    sora_provided = "sora_provided_coordinates",
    sora_admin = "administrative-areas"
  )
  dataset <- list(unclass(dataset))
  names(dataset) <- data_name
  method <- list(`geo-linking` = method)
  provenance <- list(provenance = unclass(provenance))
  c(dataset, method, provenance)
}


#' If custom data appends necessary provenance details.
#' @param dataset Created by `sora_custom`, `sora_provided` or `sora_admin`.
#' @param provenance Created by `sora_provenance`.
#' @returns A named list.
#' @noRd
prepare_dataset <- function(dataset, provenance) {
  if (inherits(dataset, "sora_custom")) {
    dataset$geocoded_dataset_name <- provenance$name
    dataset$geocoded_dataset_pid <- provenance$pid
    dataset$geocoded_dataset_url <- provenance$url
  }
  dataset
}


#' Removes custom data details if necessary.
#' @param provenance Created by `sora_provenance`.
#' @returns A named list.
#' @noRd
prepare_provenance <- function(provenance) {
  provenance$subsample_description <- provenance$description
  provenance[c("name", "pid", "url", "description")] <- NULL
  if (!is.null(provenance$subsample_description)) {
    provenance <- provenance[c("visibility", "subsample_description", "authors")]
  }
  provenance
}


#' Unclasses the linking method, checks relevant arguments, and boxes
#' the vector of method if necessary.
#' @param method Created by `sora_linking`.
#' @param link_to Created by `sora_spatial`.
#' @param provider Service provider.
#' @param simulate Whether to simulate the request.
#' @returns A named list.
#' @noRd
prepare_linking <- function(method,
                            link_to,
                            provider = "IOER",
                            simulate = FALSE) {
  check_string(provider)
  check_flag(simulate)
  
  method$output <- box(method$output)
  list(
    simulate = simulate,
    `linking-service-id` = provider,
    `linking-information` = list(
      Geospatial_Dataset = unclass(link_to),
      Linkage = unclass(method)
    )
  )
}


#' Given an object and an expected class, tries to construct the expected
#' class from the given object if the object does not already have the
#' expected class.
#' @param obj An object.
#' @param class The expected class.
#' @returns An object of class `class`.
#' @noRd
construct_object <- function(obj, class, ...) {
  if (inherits(obj, class)) {
    return(obj)
  }
  
  switch(
    class,
    sora_custom = sora_custom(obj, ...),
    sora_provided = sora_provided(obj, ...),
    sora_admin = sora_admin(obj, ...),
    sora_spatial = sora_spatial(obj, ...),
    sora_linking = sora_linking(obj, ...),
    sora_provenance = sora_provenance(obj, ...)
  )
}


#' Simple function that returns the three dataset types.
#' @noRd
dataset_types <- function() c("sora_custom", "sora_provided", "sora_admin")