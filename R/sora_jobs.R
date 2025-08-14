#' Jobs
#' @description
#' Poll, fetch and manage requested SoRa jobs.
#' 
#' \itemize{
#'  \item{\code{sora_results} fetches results from a finished job}
#'  \item{\code{sora_jobs} returns a list of jobs affiliated with an API key}
#'  \item{\code{sora_job_status} returns the detailed status of a specific
#'  job}
#'  \item{\code{sora_job_done} checks whether a job is finished
#'  (not necessarily successful)}
#'  \item{\code{sora_job_delete} removes a job from the SoRa service}
#' }
#' 
#' @param status currently status for the requested linking.
#' @param job_id A job ID as returned by \code{\link{sora_request}}.
#' @inheritParams sora_request
#' 
#' @returns \itemize{
#'  \item{\code{sora_jobs} returns a dataframe containing each job, their
#'  time of creation and their status}
#'  \item{\code{sora_job_status} returns a list of class \code{sora_status}
#'  containing the job ID, the last status update time, the status and status
#'  details}
#'  \item{\code{sora_job_done} returns \code{TRUE} or \code{FALSE}}
#'  \item{\code{sora_job_delete} returns \code{NULL}, invisibly}
#'  \item{\code{sora_results} returns a dataframe containing the case IDs
#'  and all requested output statistics}
#' }
#' 
#' @export
#' 
#' @examplesIf isTRUE(Sys.getenv("SORA_RUN_EXAMPLES")) && sora_available()
#' # First, commission a request
#' job <- sora_request(
#'   sora_custom(ber_income, crs = 3035),
#'   link_to = sora_spatial(id = "ioer-monitor-s11rg-2011-1000m"),
#'   method = sora_linking(
#'     "aggregate_attribute",
#'     selection_area = "circle",
#'     radius = 20000,
#'     output = "mean"
#'   )
#' )
#' 
#' # Then, you can poll if the status appears in the job list
#' sora_jobs()
#' 
#' # ... or poll it directly
#' sora_job_status(job)
#' 
#' # Check if it is finished
#' while (!sora_job_done(job)) {
#'   Sys.sleep(1)
#' }
#' 
#' # Then fetch
#' sora_results(job)
#' 
#' # Alternatively, remove the job
#' sora_job_delete(job)
sora_jobs <- function(status = NULL, verbose = NULL, api_key = NULL) {
  res <- api_request("jobs", api_key = api_key)
  res <- error_to_warn(res, condition = TRUE)
  res <- order_notification(res)
  notify_if_needed(res$notification, verbose)
  
  if (length(res$jobs) != 0) {
    if (!is.null(status) && is.element(status, unique(res$jobs$status))) {
      res$jobs <- res$jobs[grep(status, res$jobs$status),]
    }
    jobs <- as_df(res$jobs)
    jobs$created_at <- sora_datetime(jobs$created_at)
    class(jobs) <- c("sora_jobs", class(jobs))
    jobs
  } else {
    NULL
  }
}


#' @rdname sora_jobs
#' @export
sora_job_status <- function(job_id, verbose = NULL, api_key = NULL) {
  check_string(job_id)
  endpoint <- paste("jobs", job_id, "status", sep = "/")
  res <- api_request(endpoint, api_key = api_key)
  res <- error_to_warn(res, condition = TRUE)
  res <- order_notification(res)
  notify_if_needed(res$notification, verbose)
  status <- res[c("job_id", "timestamp", "status", "message")]
  status$timestamp <- as.POSIXct(status$timestamp)
  class(status) <- "sora_status"
  status
}


#' @rdname sora_jobs
#' @export
sora_job_done <- function(job_id, api_key = NULL) {
  check_string(job_id)
  endpoint <- paste("jobs", job_id, "status", sep = "/")
  status <- api_request(endpoint, api_key = api_key)$status
  !status %in% c("CREATED", "WAITING", "EXECUTING")
}


#' @rdname sora_jobs
#' @export
sora_job_delete <- function(job_id, verbose = NULL, api_key = NULL) {
  check_string(job_id)
  verbose <- peek_verbose(verbose)
  endpoint <- paste("jobs", job_id, sep = "/")
  res <- api_request(endpoint, method = "DELETE", api_key = api_key)
  res <- error_to_warn(res, condition = TRUE)
  res <- order_notification(res)
  report_if_needed(res, verbose)
  notify_if_needed(res$notification, verbose)
  invisible()
}


#' @rdname sora_jobs
#' @export
#' @order 0
sora_results <- function(job_id, verbose = NULL, api_key = NULL) {
  check_string(job_id)
  verbose <- peek_verbose(verbose)
  endpoint <- paste("jobs", job_id, "result", sep = "/")
  res <- api_request(endpoint, api_key = api_key)
  res <- error_to_warn(res, condition = TRUE)
  res <- order_notification(res)
  report_if_needed(res$linking_report, verbose)
  notify_if_needed(res$notification, verbose)

  out <- as_df(res$items)
  problems <- extract_problems(out)
  out$message <- NULL
  provenance <- res$provenance
  class(provenance) <- "sora_provenance"
  attr(out, "provenance") <- provenance
  attr(out, "problems") <- problems
  class(out) <- c("sora_results", class(out))
  out
}


#' Parse datetime strings returned by the SoRa API
#' @param x A datetime string.
#' @returns A POSIXct object.
#' @noRd
sora_datetime <- function(x) {
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
}


#' @export
format.sora_status <- function(x, ...) {
  ts <- format(x$timestamp, ...)
  status <- x$status
  col <- switch(
    status,
    CREATED = "yellow",
    WAITING = "blue",
    EXECUTING = "cyan",
    SUCCESSFUL = "green",
    FAILED = "red",
    CANCELLATION_REQUESTED = "magenta",
    CANCELLED = "silver",
    "none"
  )
  status <- color(status, col)
  sprintf("%s: %s %s %s", ts, status, symbols$line, x$message)
}


#' @export
print.sora_status <- function(x, ...) {
  cat(format(x, ...), sep = "\n")
}


#' @export
print.sora_jobs <- function(x, n = 5, ...) {
  max <- nrow(x)
  if (is.infinite(n)) {
    n <- max
  }

  rest <- max - n
  x$created_at <- format(x$created_at, "%x %H:%M:%S")
  print.data.frame(utils::head(x, n), row.names = FALSE, right = FALSE)
  
  if (rest > 0) {
    cat(sprintf(
      " # %s more job%s. Type print(..., n = Inf) to show all jobs.",
      rest, ifelse(rest == 1, "", "s")
    ))
  }
}
