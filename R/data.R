#' List Unique Output Types in Raw Data
#'
#' Returns all unique output types found in the ValueTB raw data.
#'
#' @return A character vector of unique output types.
#' @examples
#' outputs()
#' @export
outputs <- function() {
  td_econ <- utils::read.csv(system.file("td_econ.csv", package = "capturetb"))
  unique(td_econ$output)
}

#' Get Raw Data Filtered by Output Type
#'
#' Returns the raw ValueTB data filtered to a specified output type.
#'
#' @param output_name Optional character string specifying an output type to
#' filter by. Defaults to NULL (all data).
#'
#' @return A data frame containing only rows matching the specified output type.
#' @examples
#' get_data("IP bedday")
#' @export
#' @importFrom rlang .data
get_data <- function(output_name = NULL) {
  stopifnot(
    "'output_name' must be a string" =
      (is.null(output_name) || is.character(output_name))
  )
  dat <- utils::read.csv(system.file("td_econ.csv", package = "capturetb"))
  if (!is.null(output_name)) {
    dat <- dat |>
      dplyr::filter(.data$output == output_name)
  }

  return(dat)
}
