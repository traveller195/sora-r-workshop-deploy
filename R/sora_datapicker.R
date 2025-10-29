#' Data picker
#' @description Download and read the information from the SoRa data picker.
#' 
#' This function provides a programmatic interface to the web version of the
#' data picker, which can be found under
#' \url{https://sora.gesis.org/unofficial/datapicker/}.
#' 
#' @param content The type of dataset for which to retrieve details. The
#' following values are allowed:
#' 
#' \itemize{
#'  \item{\code{"provided"}: Metadata of coordinate datasets provided by SoRa
#'  through \code{\link{sora_provided}}}
#'  \item{\code{"admin"}: Metadata of administrative area datasets recognized
#'  by SoRa through \code{\link{sora_admin}}}
#'  \item{\code{"spatial"}: Metadata of geospatial datasets provided by
#'  SoRa through \code{\link{sora_spatial}}}
#'  \item{\code{"linking"}: Metadata of available linking methods that can
#'  be used in \code{\link{sora_linking}}}
#' }
#' 
#' @param method A linking method that is used in \code{\link{sora_request}}
#' and \code{\link{sora_linking}} to link a (survey) dataset to a
#' spatial dataset. \code{sora_arguments} returns the arguments related to
#' the specified linking method.
#' @inheritParams sora_request
#' 
#' @returns \itemize{
#'  \item{\code{sora_datapicker} returns a dataframe containing
#'  details about provided coordinate datasets, administrative areas,
#'  geospatial datasets, or linking methods depending on the value passed to
#'  \code{content}. Some common columns include:
#' 
#' \itemize{
#'  \item{\code{provider}: The provider of the dataset.}
#'  \item{\code{dataset_id}: Dataset ID that is used in
#'  \code{\link{sora_provided}} and \code{\link{sora_spatial}}.}
#'  \item{\code{description}: Description of the dataset. Used in the
#'  provenance record.}
#'  \item{\code{pid}: Persistent identifier of a dataset, usually a DOI.}
#'  \item{\code{required_permissions}: Information about the terms of use
#'  required to access the data. These need to be accepted when registering
#'  for an API key.}
#' }}
#'  \item{\code{sora_arguments} returns a dataframe where each row is a
#'  argument that can be passed to the method specified in \code{method}
#'  and each column gives further details about the argument.}
#' }
#'
#' @export
#' 
#' @examplesIf isTRUE(Sys.getenv("SORA_RUN_EXAMPLES"))
#' sora_datapicker("admin")
#' 
#' sora_datapicker("spatial")
#' 
#' sora_arguments("nearest_poi")
sora_datapicker <- function(content = c("provided", "admin", "spatial", "linking")) {
  content <- match.arg(content)
  file <- switch(
    content,
    provided = "coordinates",
    admin = "administrative_areas",
    spatial = "geospatial_datasets",
    linking = "linking_methods"
  )
  file <- paste0(file, ".csv")
  url <- get_datapicker_url()
  as_df(request_datapicker(url, file))
}


#' @rdname sora_datapicker
#' @export
sora_arguments <- function(method, provider = "IOER") {
  docs <- sora_datapicker("linking")
  methods <- docs$linking_method
  params_json <- docs$parameter_documentation
  params <- lapply(params_json, clean_datapicker_params)
  
  names(params) <- methods
  if (!method %in% methods) {
    sora_abort(
      sprintf(
        "`%s` is not a valid linking method for provider %s.",
        method, provider
      ),
      "i" = "See `sora_datapicker(\"linking\")` for a list of linking methods."
    )
  }
  
  col_order <- c("name", "datatype", "optional", "range", "comment")
  params <- params[[method]]
  as_df(params[intersect(col_order, names(params))])
}


#' Parse the JSON sent with `sora_datapicker("linking")` and clean its
#' contents.
#' @param json JSON string returned by data picker.
#' @returns A dataframe, ideally.
#' @noRd
clean_datapicker_params <- function(json) {
  if (nzchar(json)) {
    params <- jsonlite::fromJSON(gsub("'", "\"", json))
    
    if (!is.null(params$datatype)) {
      params$datatype <- vapply(
        params$datatype,
        FUN.VALUE = character(1),
        switch,
        String = "character",
        Integer = "numeric",
        `List of Strings` = "character"
      )
    }
    
    if (!is.null(params$optional)) {
      params$optional <- isTRUE(as.logical(params$optional))
    }
    
    params
  }
}


#' Modifies the output of `sora_api` to lead to the datapicker instead of
#' the X API
#' @returns A modified URL.
#' @noRd
get_datapicker_url <- function() {
  api_parsed <- httr2::url_parse(sora_api())
  api_parsed <- httr2::url_modify(api_parsed, path = "unofficial/datapicker")
  httr2::url_build(api_parsed)
}


#' Downloads and reads the datapicker contents. Simplified version of
#' `api_request` that performs less checks.
#' @param url URL created by `get_datapicker_url`
#' @returns A dataframe.
#' @noRd
request_datapicker <- function(url, file) {
  req <- httr2::request(url)
  req <- httr2::req_url_path_append(req, file)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  utils::read.csv(text = sora_extract_body(resp))
}


#' This is a roxygen helper that extracts information about linking methods
#' from the datapicker and returns a roxygen documentation for them.
#' @returns A roxygen string.
#' @noRd
rd_get_parameters <- function() { # nocov start
  docs <- sora_datapicker("linking")
  method <- docs$linking_method
  params_json <- docs$parameter_documentation
  
  colname_map <- list(
    name = "Name",
    datatype = "Data type",
    optional = "Optional?",
    range = "Range",
    comment = "Details"
  )
  
  params <- lapply(params_json, function(json) {
    if (nzchar(json)) {
      params <- jsonlite::fromJSON(gsub("'", "\"", json))

      if (!is.null(params$datatype)) {
        params$datatype <- vapply(
          params$datatype,
          FUN.VALUE = character(1),
          switch,
          String = "character",
          Integer = "numeric",
          `List of Strings` = "character"
        )
      }
      
      if (!is.null(params$optional)) {
        params$optional <- ifelse(
          isTRUE(as.logical(params$optional)), "yes", "no"
        )
      }

      if (!is.null(params$range)) {
        params$range <- ifelse(
          vapply(params$range, is.null, FUN.VALUE = logical(1)),
          "", params$range
        )
      }
      
      params$name <- sprintf("\\code{%s}", params$name)
      col_names <- intersect(names(colname_map), names(params))
      params <- params[col_names]
      names(params) <- colname_map[names(params)]
      tabular(params)
    } else {
      "No parameters defined."
    }
  })
  roc <- sprintf("\\item{\\code{%s}}{%s}\\cr", method, params)
  roc <- sprintf("\\describe{\n%s\n}", paste(roc, collapse = "\n"))
  details <- "The following is a basic list of possible
    arguments that can be passed to the respective linking methods. Do
    note that this list is only updated with every release. In other words,
    some of the arguments might not be up-to-date and others might be
    missing. Use \\code{\\link{sora_arguments}} or visit the data picker
    (see \\code{\\link{sora_datapicker}}) for more accurate descriptions of
    possible arguments."
  sprintf("\\section{Further arguments}{\n%s\n%s\n}", details, roc)
}


#' Turns a dataframe to a roxygen table.
#' Taken from here: https://roxygen2.r-lib.org/articles/formatting.html
#' @param df A dataframe
#' @param ... Passed to format.
#' @returns A roxygen string.
#' @noRd
tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))
  
  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))
  
  cols <- lapply(df, format, ...)
  contents <- do.call(
    "paste",
    c(cols, list(sep = " \\tab ", collapse = "\\cr\n "))
  )
  
  paste(
    "\\tabular{", paste(col_align, collapse = ""), "}{\n ",
    paste0("\\strong{", names(df), "}", sep = "", collapse = " \\tab "), " \\cr\n  ",
    contents, "\n}\n",
    sep = ""
  )
} # nocov end


#' @description `sora_dp_overview()`: Get an overview of the datapicker provided by SoRa. 
#' The output is a cross table of both used arguments (`arg_1`, `arg_2`).
#' The function is based on the `table()` of the R base.
#'
#' @param data_dp Dataset (`data.frame` or `tibble`) of the datapicker, which should be analysed.
#' Can also be a subset of the datapicker.
#' @param sort_by Value is `TRUE` or `FALSE`. In some cases is it better to sort the table
#' @param arg_1 argument 1. The default is `title`.
#' @param arg_2 argument 2. The default is `spatial_resolution`.
#' @param unit Unit which will be used by sorting. Default is m.
#' @rdname sora_datapicker
#' @export
sora_dp_overview <- function(data_dp = NULL, sort_by = TRUE, content = "spatial",
                             arg_1 = "title", arg_2 = "spatial_resolution", unit = "m") {
  if (is.null(data_dp)){
    data_dp <- sora_datapicker(content = content)
  }
  colnames_dp <- names(data_dp)
  data_dp <- as.data.frame(data_dp)
  
  if (!is.null(arg_1) && !is.null(arg_2) && arg_1 == arg_2){
    arg_2 <- NULL
    }
  
  # 1. case: only one value is selected
  if (!is.null(arg_1) && is.null(arg_2) && is.element(arg_1, colnames_dp)){
    overview_dp <- as.data.frame(table(data_dp[, arg_1]))
    names(overview_dp) <- c("title", "n")
    }
  
  # 2. case: two values are selected
  if (!is.null(arg_1) && !is.null(arg_2) && is.element(arg_1, colnames_dp) && is.element(arg_2, colnames_dp)){
    overview_dp <- as.data.frame.array(table(data_dp[, arg_1], data_dp[, arg_2]))
    overview_dp <- overview_dp[, sort(names(overview_dp))]
    if (sort_by){
      overview_dp <- sort_data(input = overview_dp, unit = unit)
    }
    if (!is.element(arg_1, names(overview_dp))) {
      overview_dp <- as.data.frame(cbind(rownames(overview_dp), overview_dp))
      names(overview_dp)[1] <- arg_1
      row.names(overview_dp) <- NULL
    }
  } else {
    used_argument <- c(arg_1, arg_2)[which(!is.element(c(arg_1, arg_2), colnames_dp))]
    sora_abort(sprintf("argument: `%s` does not exist in the analysed data", used_argument[1]))
  }
  
  if (any(duplicated(names(overview_dp)))) {
    overview_dp <- overview_dp[, unique(names(overview_dp))]
  }
  as_df(overview_dp)
}


#' sort info Data Picker
#' @description if there is the information about the resolution in form of `sort_by`.
#' The output will be sorted.
#'
#' @param input Dataset datapicker.
#' @param unit Unit which will be used by sorting. Default is m.
#' Can depends of the provided data.
#' @noRd
sort_data <- function(input, unit) {
  sort.data <- names(input)
  seach_pattern <- paste0("[0-9]+", unit)
  expr.sort <- gregexpr(seach_pattern, sort.data)
  regmatches.expr <- unlist(regmatches(sort.data, expr.sort))
  list.attributes <- lapply(expr.sort, attributes)
  check.sort <- sapply(list.attributes, function(item) item$match.length)
  data.extract <- sort.data[which(check.sort == -1)]
  if (any(check.sort != -1)) {
    data.to.sort <- sort.data[which(check.sort != -1)]
    expr.to.sort <- gregexpr("[0-9]+", data.to.sort)
    regmatches.expr.sort <- as.numeric(unlist(regmatches(data.to.sort, expr.to.sort)))
    out <- sort.int(regmatches.expr.sort, index.return = TRUE)
    output <- append(data.extract, data.to.sort[out$ix])
    output <- input[, output]
  } else {
    output <- input
  }
  output
}

#' @description `sora_dp_get_id()`: Get an overview of the possible dataset id which can be selected.
#' The output has this columns: 'title', 'time_frame', 'spatial_resolution' and 'dataset_id'
#' 
#' @param indicator Is the `title` output in the `datapicker`.
#' @param col_id Argument for the `dataset_id` can be depends of the data provider.
#' Column with the information about the `dataset_id` in the datapicker.
#' @rdname sora_datapicker
#' @export
sora_dp_get_id <- function(data_dp = NULL, indicator,
                           col_id = "dataset_id", content = "spatial") {
  if (is.null(data_dp)) {
    data_dp <- sora_datapicker(content = content)
  }
  dp <- as.data.frame(data_dp)
  available_indicator <- tolower(unique(dp$title))
  indicator <- tolower(indicator)
    
  ## which indicator has the used pattern
  use_indicator <- available_indicator[grep(indicator, available_indicator)]
  indicator_list <- list()
  check_colnames <- is.element(col_id, names(dp))
  check_indicator <- is.element(use_indicator, available_indicator)
  
  if (!check_colnames) {
    sora_abort(sprintf("used input for 'col_id': `%s` is not a valid value", col_id))
  }
  if (!isTRUE(unique(check_indicator)) | indicator == "") {
    sora_abort(sprintf("used input for 'indicator': `%s` is not a valid value", indicator))
  }
   
  indicators <- as.data.frame.array(table(dp[, col_id], dp[, "title"]))
  indicators <- cbind(row.names(indicators), indicators)
  row.names(indicators) <- NULL
  names(indicators)[1] <- col_id
  names(indicators) <- tolower(names(indicators))
    
  for (i in use_indicator) {
    iteration <- which(use_indicator == i)
    output <- indicators[, i]
    id <- indicators[output > 0, col_id]
    output <- which(is.element(dp[, col_id], id))
    indicator_list[[iteration]] <- dp[output, c("title", "time_frame", "spatial_resolution", col_id)]
  } 
  names(indicator_list) <- use_indicator
  output <- indicator_list[[use_indicator[1]]]
  if (length(use_indicator) > 1) {
    use_indicator <- use_indicator[2:length(use_indicator)]
    for (i in use_indicator) {
      output <- rbind(output, indicator_list[[i]])
    }
  }
  as_df(output)
}


#' @description `sora_dp_info_id()`: Get info, all columns of the datapicker for the selected `dataset_id`.
#' @param dataset_id Specific id for a dataset.
#' @rdname sora_datapicker
#' @export
sora_dp_info_id <- function(data_dp = NULL, col_id = "dataset_id",
                            content = "spatial", dataset_id) {
  if (is.null(data_dp)) {
    data_dp <- sora_datapicker(content = content)
  }
  check_id <- is.element(dataset_id, data_dp[[col_id]])
  if (!isTRUE(unique(check_id))) {
    missing_id <- dataset_id[which(!is.element(dataset_id, data_dp[[col_id]]))]
    sora_abort(sprintf(paste0("used input for " , quote_arg(col_id), " : `%s` is not a valid value"), missing_id))
  }
  output <- data_dp[which(is.element(data_dp[[col_id]], dataset_id)),]
  as_df(output)
}
