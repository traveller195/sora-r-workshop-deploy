#' Linking problems
#' @description
#' Sometimes linking jobs do not fail but cannot compute linking results
#' for each row of the input data. In these cases, \code{\link{sora_results}}
#' informs you about which rows are erroneous. To see \emph{why} they failed
#' you can use \code{sora_problems}. It returns a dataframe containing the
#' problematic rows and the error message they produced in the SoRa API.
#' 
#' Given a results dataframe \code{out}, you can remove these cases from
#' the dataset by typing:
#' 
#' \preformatted{out[!is.na(out$count), ]}
#' 
#' @param out A dataframe of class \code{\link{sora_results}} that produced
#' row-level error messages.
#' @returns A dataframe containing those IDs that produced row-level error
#' messages and a column \code{message} that contains their error messages.
#' 
#' @export
#' 
#' @examplesIf isTRUE(Sys.getenv("SORA_RUN_EXAMPLES")) && sora_available()
#' # Create a problematic dataset
#' sora_data <- sora_custom(ber_income, crs = 3035)
#' sora_data$`coordinate-items`$x[4] <- NA
#' 
#' # Construct and send a request
#' spat_data <- sora_spatial("ioer-monitor-s02rg-2023-1000m")
#' linking <- sora_linking(
#'   method = "aggregate_attribute",
#'   selection_area = "square",
#'   length = 2500,
#'   output = c("mean", "median", "sd")
#' )
#' 
#' out <- sora_request(
#'   sora_data,
#'   link_to = spat_data,
#'   method = linking,
#'   wait = TRUE
#' )
#' 
#' # Check for errors
#' problems(out)
sora_problems <- function(out) {
  check_class(out, "sora_results")
  attr(out, "problems")
}


#' Extract problems from the response of a get_results query.
#' @param res Results from a SoRa get_results query.
#' @returns A dataframe.
#' @noRd
extract_problems <- function(res) {
  problems <- res[c("id", "message")]
  
  if (any(!is.na(problems$message))) {
    rows <- which(!is.na(problems$message))
    rows_fmt <- commas(rows, max = 5, last = "and")
    n_probs <- length(rows)
    trail_s <- ifelse(n_probs == 1, "", "s")
    plural <- ifelse(n_probs == 1, "is a problem", "are problems")
    
    sora_warn(
      sprintf("There %s with row%s %s.", plural, trail_s, rows_fmt),
      "i" = sprintf(
        "To inspect the problem%s, call `sora_problems(...)` on your output.",
        trail_s
      )
    )
  }
  
  problems[!is.na(problems$message), ]
}