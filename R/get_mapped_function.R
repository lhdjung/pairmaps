
# Internal helper used within some of the unit tests:
unclass_pairmaps <- function(pairmaps_df) {
  df_classes <- class(pairmaps_df)[grepl("^pairmaps_df_", class(pairmaps_df))]
  if (length(df_classes) != 1L) {
    if (length(df_classes) == 0L) {
      rlang::abort("No \"pairmaps_df_*\" classes present.")
    }
    msg_warning <- paste(length(df_classes), "\"pairmaps_df_*\" classes")
    msg_warning <- paste(msg_warning, "present. It should only be 1.")
    rlang::warn(msg_warning)
  }
  class(pairmaps_df) <- class(pairmaps_df)[!class(pairmaps_df) == df_classes]
  pairmaps_df
}


#' Retrieve the function applied to each column pair
#'
#' @description If you pass a function `f()` to `as_colpair_mapper()`, and then
#'   call the resulting function, you will get a data frame back. Here are two
#'   functions to retrieve `f()` from that data frame:
#'
#'   - `get_mapped_function()` returns the function itself.
#'   - `get_mapped_function_name()` returns the function's name as a string.

#' @param data Data frame.
#'
#' @return Function or string (length 1).
#'
#' @include is_pairmaps_df.R
#'
#' @export
#'
#' @examples
#' # `covary()` was made by `as_colpair_mapper()`:
#' df <- covary(mtcars)
#'
#' # Since `covary()` applies `stats::cov()`,
#' # this is the function returned here:
#' cov_from_df <- get_mapped_function(data = df)
#' identical(cov_from_df, stats::cov)
#'
#' # Just get the name instead:
#' get_mapped_function_name(data = df)

get_mapped_function <- function(data) {
  name <- get_mapped_function_name(data)
  rlang::eval_tidy(rlang::parse_expr(name))
}


#' @rdname get_mapped_function
#' @export

get_mapped_function_name <- function(data) {
  if (!is_pairmaps_df(data)) {
    msg1 <- "It doesn't seem to be the output of a function"
    msg1 <- paste(msg1, "created by `as_colpair_mapper()`.")
    msg2 <- "Else, it is but its \"pairmaps_df_*\" class"
    msg2 <- paste(msg2, "was changed, removed, or supplemented by another one.")
    rlang::abort(c(
      "Invalid `data` argument.",
      "x" = msg1,
      "x" = msg2
    ))
  }
  classes <- class(data)
  sub("pairmaps_df_", "", classes[grepl("^pairmaps_df_", classes)])
}

