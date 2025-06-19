#' SoRa datasets
#' @description Specify the data that you wish to link to the spatial context.
#' Generally, you have three ways to specify data to the SoRa service:
#' 
#' \itemize{
#'  \item{Provide your own data using \code{sora_custom()}.}
#'  \item{Specify a dataset provided by SoRa using \code{sora_provided()}.
#'  Note that, in the public API, you can only access structural data of these
#'  datasets. To work with the real data, you need to be inside a secure room.}
#'  \item{Provide regional keys to link areal units to their spatial
#'  context, e.g., zip code areas or municipalities, using \code{sora_admin()}.}
#' }
#' 
#' These objects can be used to feed linking jobs using
#' \code{\link{sora_request}}.
#'
#' @param .data Coordinates or identifier to coordinates, depending on the
#' function.
#' 
#' \describe{
#'  \item{\code{sora_custom()}}{A plain dataframe containing three columns
#'  \code{id}, \code{x}, and \code{y} or an sf dataframe containing point
#'  geometries. If an sf dataframe is provided, the geometry column is
#'  converted to coordinates and the CRS is extracted automatically. If the
#'  columns \code{id}, \code{x}, and \code{y} cannot be found in the data, the
#'  first three columns are selected instead.}
#'  
#'  \item{\code{sora_provided()}}{A character vector of IDs that distinctly
#'  identify pairs of coordinates in the survey dataset provided in the
#'  \code{survey} argument.}
#'  
#'  \item{\code{sora_admin()}}{A dataframe containing two columns: one column
#'  containing the respondent/case ID (\code{case_id}) and one column
#'  containing the area identifier (\code{area_id}). An area identifier could,
#'  for example, be a municipality key (AGS) or zip code. Note that these
#'  columns can either be provided by name (\code{case_id} and \code{area_id})
#'  or by position in the dataframe (positions 1 and 2).}
#' }
#' @param crs EPSG code of the coordinate reference system (CRS) of your data.
#' Can be one of the following: 3035, 3857, 4326, 4647, 25832, 25833. If not
#' provided and \code{.data} is of class \code{sf}, the EPSG code is extracted
#' automatically.
#' @param survey ID of a survey. You can look up possible values in the data
#' picker (see \code{\link{sora_datapicker}} or its web version).
#' @param schema Identifier of an administrative area schema. You can look
#' up possible values in the data picker (see \code{\link{sora_datapicker}}).
#' Example values include \code{"AGS2019"} and \code{"PLZ2024"}.
#' 
#' @returns List objects of classes \code{sora_custom}, \code{sora_provided},
#' or \code{sora_admin}, which can be used in \code{\link{sora_request}}.
#'
#' @export
#' 
#' @examples
#' sora_custom(ber_income, crs = 3035)
#' sora_provided(c("0001", "0002", "0003"), survey = "soep.core.v39")
#' 
#' admin <- data.frame(
#'   case_id = 1:3,
#'   area_id = c("05 3 15 000", "05 3 15 000", "05 3 15 000")
#' )
#' sora_admin(admin, schema = "AGS")
sora_custom <- function(.data, crs = NULL) {
  is_sf <- inherits(.data, "sf")
  check_cols(.data, n = 3 - is_sf)
  
  if (is_sf) {
    check_loadable("sf", "handle sf dataframes")
    coords <- sf::st_coordinates(.data)
    
    if (ncol(coords) > 2) {
      sora_abort(
        "Argument `.data` contains a geometry type other than points",
        "*" = "Make sure to provide only point coordinates."
      )
    }
    
    crs <- sf::st_crs(.data)$epsg %|||% NULL
    names(coords) <- c("x", "y")
    .data <- cbind(sf::st_drop_geometry(.data), coords)
  }
  
  req_cols <- c("id", "x", "y")
  if (all(req_cols %in% names(.data))) {
    .data <- .data[req_cols]
  } else {
    .data <- .data[c(1, 2, 3)]
    names(.data) <- req_cols
  }
  
  if (anyNA(.data$x) || anyNA(.data$y)) {
    sora_abort(
      "Some of the coordinates in `.data` are missing.",
      "i" = "Please make sure that all coordinate pairs are complete."
    )
  }
  
  check_coercible(.data$x, name = ".data$x")
  check_coercible(.data$y, name = ".data$y")
  .data$id <- as.character(.data$id)
  
  if (is.null(crs)) {
    sora_abort(
      "A coordinate reference system (CRS) is needed to locate the coordinates.",
      "i" = "You can try one of the following:",
      " *" = "Specify an EPSG code using the `crs` argument.",
      " *" = "Cast `.data` to an sf dataframe to automatically read the CRS."
    )
  }
  
  obj <- list(`coordinate-items` = .data, crs = crs)
  class(obj) <- "sora_custom"
  obj
}


#' @rdname sora_custom
#' @export
sora_provided <- function(.data, survey) {
  check_vector(.data, type = "character")
  check_string(survey)
  
  obj <- list(`dataset-id` = survey, `coordinates-ids` = .data)
  class(obj) <- "sora_provided"
  obj
}


#' @rdname sora_custom
#' @export
sora_admin <- function(.data, schema) {
  check_cols(.data, n = 2)
  check_vector(schema, "character", len = 1)
  
  id_cols <- c("case_id", "area_id")
  if (all(id_cols %in% names(.data))) {
    .data <- .data[id_cols]
  } else {
    .data <- .data[c(1, 2)]
    names(.data) <- id_cols
  }
  
  .data$case_id <- as.character(.data$case_id)
  .data$area_id <- as.character(.data$area_id)
  names(.data) <- c("case-id", "area-id")
  obj <- list(
    `area-items` = .data,
    `area-schema-id` = schema
  )
  class(obj) <- "sora_admin"
  obj
}


#' Provenance
#' @description Configure if and how a provenance record for a linking job
#' should be generated. This function is mainly used to be used as an
#' argument to \code{\link{sora_request}}. You can configure the visibility
#' of the provenance record. When providing custom coordinates, you can also
#' provide the dataset's metadata if applicable.
#'
#' @param visible Whether to generate provenance record or not. If
#' \code{TRUE} produces a provenance record according to the input to this
#' function. Defaults to \code{FALSE}.
#' @param name,pid,url When using a custom dataset using
#' \code{\link{sora_custom}}, these arguments specify the name, ID, description,
#' and URL of the dataset. For optimal reproducibility, make sure to provide
#' accurate information to these arguments.
#' @param description A character string describing the subsample analyzed
#' in the linking job. For optimal reproducibility, make sure to provide
#' accurate information to these arguments.
#'
#' @returns An object of class \code{sora_provenance}.
#'
#' @export
#' 
#' @examples
#' # The default, `FALSE`, does not produce a record
#' sora_provenance()
#' 
#' # Set to `TRUE` to produce a reproducible report for your linking job
#' sora_provenance(TRUE)
#' 
#' # When providing custom data, you can also use `sora_provenance()`
#' # to provide necessary information to create a record
#' data <- sora_custom(ber_income, crs = 3035)
#' sora_provenance(
#'   visible = TRUE,
#'   name = "Household income, Berlin",
#'   url = "https://sora-service.org/"
#' )
sora_provenance <- function(visible = FALSE,
                            name = NULL,
                            pid = NULL,
                            url = NULL,
                            description = NULL) {
  check_flag(visible)
  check_string(name, null = TRUE)
  check_string(pid, null = TRUE)
  check_string(url, null = TRUE)
  check_string(description, null = TRUE)
  
  visibility <- ifelse(visible, "visible", "hidden")
  obj <- list(
    name = name,
    pid = pid,
    url = url,
    description = description,
    visibility = visibility
  )
  class(obj) <- "sora_provenance"
  obj
}


#' Linking method
#' @description Specify parameters for a selected linking method to be used in
#' \code{\link{sora_request}}.
#'
#' @param method Linking method to be used. You can look
#' up possible methods in the data picker (see \code{\link{sora_datapicker}}).
#' @param ... Further arguments passed to the linking method. See
#' \code{\link{sora_datapicker}} for a list of available arguments.
#'
#' @returns An object of class \code{sora_linking}.
#' 
#' @evalRd rd_get_parameters()
#'
#' @export
#' 
#' @examples
#' # You need to always provide at least a linking method
#' sora_linking("lookup")
#' 
#' # In most cases, a linking method requires additional parameters
#' sora_linking("count_object", selection_area = "square", length = 1000)
sora_linking <- function(method, ...) {
  check_string(method)
  check_dots_named(...)
  
  obj <- list(linking_method = method, ...)
  class(obj) <- "sora_linking"
  obj
}


#' Geospatial dataset
#' @description Specify the the geospatial dataset to be used in the
#' \code{link_to} argument of \code{\link{sora_request}}.
#'
#' @param id An ID of a spatial dataset provided by SoRa. See
#' \code{\link{sora_datapicker}} or its web equivalent to see which
#' IDs are available. If you want to provide your own spatial data, see the
#' \code{.data} argument.
#' @param .data A plain dataframe containing two columns \code{x} and \code{y}
#' or an sf dataframe containing any geometry type. Use this argument if you
#' intend to provide custom spatial data. If you want to use spatial data
#' provided by the SoRa service, see the \code{id} argument.
#' @param ... Further arguments passed to the API. See
#' \code{\link{sora_datapicker}} for a list of available arguments.
#' 
#' @returns An object of class \code{sora_spatial}.
#'
#' @export
#' 
#' @examples
#' # When providing an ID you tell SoRa to select a pre-made dataset
#' # from the service provider
#' sora_spatial("ioer-monitor-s02rg-2023-1000m")
sora_spatial <- function(id = NULL, .data = NULL, ...) {
  check_string(id, null = TRUE)
  check_dots_named(...)
  
  if (!is.null(.data)) {
    sora_abort(
      "Providing your own spatial data is not currently supported.",
      "i" = "We are working on it!"
    )
  }
  
  if (is.null(.data) && is.null(id)) {
    sora_abort(paste(
      "Please either provide your own data (`.data`)",
      "or an ID to a spatial dataset by SoRa (`id`)."
    ))
  }
  
  type <- ifelse(is.null(.data), "sora_provided", "user_provided")
  obj <- list(type = type, dataset_id = id, ...)
  class(obj) <- "sora_spatial"
  obj
}


#' @export
print.sora_custom <- function(x, ...) {
  header <- sprintf(
    "%s\nUsing custom data with EPSG code %s.",
    color("<sora_custom>", "blue"), x$crs
  )
  data <- x$`coordinate-items`
  cat(header, "\n\n")
  print.data.frame(data[seq_len(min(nrow(data), 10)),])
  invisible(x)
}


#' @export
print.sora_provided <- function(x, ...) {
  msg <- sprintf(
    "%s\nUsing respondent IDs from a social survey provided by SoRa (%s).",
    color("<sora_provided>", "blue"), italic(x$`dataset-id`)
  )
  ids <- commas(x$`coordinates-ids`, last = ",")
  ids <- paste(strwrap(ids, indent = 1, exdent = 1), collapse = "\n")
  msg <- paste0(msg, "\n\nIdentifiers:\n", ids)
  cat(msg, "\n")
  invisible(x)
}


#' @export
print.sora_admin <- function(x, ...) {
  header <- sprintf(
    "%s\nUsing areal identifiers based on %s demarcations.",
    color("<sora_admin>", "blue"), x$`area-schema-id`
  )
  data <- x$`area-items`
  cat(header, "\n\n")
  print.data.frame(data[seq_len(min(nrow(data), 10)),])
  invisible(x)
}


#' @export
format.sora_provenance <- function(x, ...) {
  visibility <- switch(
    x$visibility,
    visible = "A provenance record is produced alongside your linking job.",
    hidden = "No provenance record will be produced."
  )
  fmt <- sprintf("%s\n%s", color("<sora_provenance>", "blue"), visibility)
  
  details <- c("Name", "PID", "URL", "Description")
  if (!all_null(x[tolower(details)])) {
    fmt <- paste0(fmt, "\n")
  }

  for (detail_fmt in details) {
    detail <- tolower(detail_fmt)
    if (is.null(x[[detail]])) {
      next
    }
    
    fmt <- paste0(fmt, "\n", sprintf(
      "%s:%s%s",
      bold(detail_fmt),
      ifelse(identical(detail, "Description"), "\n", " "),
      x[[detail]]
    ))
  }
  
  fmt
}


#' @export
print.sora_provenance <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}


#' @export
print.sora_spatial <- function(x, ...) {
  if (identical(x$type, "user_provided")) {
    # TODO
    header <- sprintf( # nocov start
      "%s\nLinking to the following dataset:",
      color("<sora_spatial>", "blue")
    )
    fmt <- header # nocov end
  } else {
    fmt <- sprintf(
      "%s\nLinking to the spatial dataset %s.",
      color("<sora_spatial>", "blue"), color(x$dataset_id, "green")
    )
  }
  
  cat(fmt, "\n", ...)
}


#' @export
format.sora_linking <- function(x, ...) {
  has_params <- length(x) > 1
  header <- sprintf(
    "%s\nUsing method %s with %s",
    color("<sora_linking>", "blue"),
    x$linking_method,
    ifelse(
      has_params,
      "the following parameters:",
      "no parameters."
    )
  )
  
  if (length(x) > 1) {
    params <- x[!names(x) %in% "linking_method"]
    params <- lapply(params, commas, last = ",")
    params <- paste0(bold(names(params)), ": ", paste(params))
    params <- paste(params, collapse = "\n")
    paste0(header, "\n\n", params)
  } else {
    header
  }
}


#' @export
print.sora_linking <- function(x, ...) {
  cat(format(x), "\n", ...)
  invisible(x)
}