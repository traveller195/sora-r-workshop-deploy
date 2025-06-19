#' Split and assemble IDs
#' @description
#' Helper functions to handle ID columns in the SoRa input/output.
#' 
#' \itemize{
#'  \item{\code{sora_assemble_id}: Combines multiple columns to a single ID
#'  column.}
#'  \item{\code{sora_split_id}: Splits an ID column to multiple separate
#'  columns.}
#' }
#' 
#' These functions are particularly handy (and were designed) to easily handle
#' the IDs of SOEP datasets. Use \code{sora_assemble_id} to generate ad-hoc
#' IDs from multiple dataset columns.
#' 
#' @param .data A dataframe. The dataframe should contain the columns
#' specified in \code{id_col}.
#' @param id_col A character vector of columns in which the
#' necessary ID info is stored in.
#' 
#' \itemize{
#'  \item{For \code{sora_assemble_id}, the columns from which to assemble
#'  the ID. Defaults to \code{NULL}, or all columns.}
#'  \item{For \code{sora_split_id}, a single column in which the ID strings
#'  are stored. Defaults to \code{id}.}
#' }
#' 
#' @param out_col A character vector of column names that should be generated.
#' 
#' \itemize{
#'  \item{For \code{sora_assemble_id}, a single column. Defaults to \code{id}.}
#'  \item{For \code{sora_split_id}, a character vector with the same length
#'  as there are ID components in \code{id_col}. Defaults to \code{NULL},
#'  or \code{"id_*"} where \code{*} is the component number.}
#' }
#' 
#' @param collapse,sep A character string by which to combine or split the ID
#' components. Defaults to \code{"_"}.
#' 
#' @returns A dataframe with added columns. \code{sora_assemble_id} adds a
#' single column with the assembled ID strings, \code{sora_split_id} adds a
#' number of columns equal to the number of ID components in \code{id_col}.
#' 
#' @details
#' \code{sora_split_id} can generate \code{NA} values when single ID strings
#' lack components that are present in other IDs. For example
#' \code{c("id_a", "id")} would result in
#' \code{data.frame(id_1 = c("id", "id"), id_2 = c("a", NA))}. Similarly,
#' \code{sora_assemble_id} skips \code{NA} values, i.e., \code{c(id, NA, "a")}
#' becomes \code{"id_a"}.
#' 
#' 
#' @export
#' @name sora_id
#' 
#' @examples
#' ids <- tibble::tibble(id_1 = "id", id_2 = c("a", "b", NA), value = c(1, 2, 3))
#' ids
#' 
#' assembled <- sora_assemble_id(ids, id_col = c("id_1", "id_2"))
#' assembled
#' 
#' split <- sora_split_id(assembled)
#' split
#' 
#' identical(ids, split)
sora_assemble_id <- function(.data, id_col = NULL, out_col = NULL, collapse = "_") {
  check_vector(id_col, "character", null = TRUE)
  check_string(out_col, null = TRUE)
  id_col <- id_col %||% names(.data)
  out_col <- out_col %||% "id"
  check_cols(.data, id_col)
  check_string(collapse)
  
  id_data <- .data[id_col]
  id_list <- .mapply(id_data, MoreArgs = NULL, FUN = function(...) {
    paste(drop_na(list(...)), collapse = collapse)
  })
  
  if (any(!nzchar(id_list))) {
    sora_warn(
      "Zero-character IDs have been produced. Check your input!",
      "i" = "This usually means that all ID components are missing."
    )
  }
  
  args <- list(unlist(id_list))
  names(args) <- out_col
  og_data <- .data[setdiff(names(.data), id_col)]
  as_df(do.call(cbind.data.frame, c(args, og_data)))
}


#' @rdname sora_id
#' @export
sora_split_id <- function(.data, id_col = "id", out_col = NULL, sep = "_") {
  check_cols(.data, cols = id_col)
  check_string(sep)
  check_vector(out_col, "character", null = TRUE)
  
  comps <- strsplit(.data[[id_col]], sep, fixed = TRUE)
  n_comps <- max(lengths(comps))
  comps <- lapply(comps, `length<-`, n_comps)
  out_col <- out_col %||% paste0("id_", seq_len(n_comps))
  id_df <- do.call(rbind, comps)
  if (ncol(id_df)) colnames(id_df) <- out_col
  .data[[id_col]] <- NULL
  as_df(cbind.data.frame(id_df, .data))
}
