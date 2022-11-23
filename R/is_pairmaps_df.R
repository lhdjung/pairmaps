
#' Is an object a pairmaps data frame?
#'
#' @description `is_pairmaps_df()` tests if an object is a data frame returned
#'   by a function that was made by `as_colpair_mapper()`.
#'
#' @param x An object.
#'
#' @return A single Boolean value.
#'
#' @export
#'
#' @details The function checks whether `x` is a data frame that inherits
#'   exactly one S3 class that starts on `"pairmaps_df_"`. This is a given if
#'   `x` was returned by a function created by `as_colpair_mapper()` and its
#'   classes have not been manipulated.
#'
#' @examples
#' # Basic function, from `?corrr::colpair_map()`:
#' calc_p_value <- function(vec_a, vec_b) {
#'   stats::t.test(vec_a, vec_b)$p.value
#' }
#'
#' # Produce a new mapper function:
#' p_value_map <- as_colpair_mapper(calc_p_value)
#'
#' is_pairmaps_df(p_value_map(mtcars))
#'
#' is_pairmaps_df(mtcars)
#'
#' is_pairmaps_df(4)


is_pairmaps_df <- function(x) {
  if (!(is.data.frame(x) && inherits(x, "cor_df"))) {
    return(FALSE)
  }
  classes <- class(x)
  length(classes[grepl("^pairmaps_df_", classes)]) == 1L
}

