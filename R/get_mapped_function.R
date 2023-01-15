
# Internal helper used within some of the unit tests:
unclass_pairmaps <- function(pairmaps_df) {
  df_classes <- class(pairmaps_df)[grepl("^pairmaps_df_", class(pairmaps_df))]
  if (length(df_classes) != 1L) {
    if (length(df_classes) == 0L) {
      cli::cli_abort(c("x" = "No \"pairmaps_df_*\" classes present."))
    }
    msg_warning <- paste(length(df_classes), "\"pairmaps_df_*\" classes")
    msg_warning <- paste(msg_warning, "present. It should only be 1.")
    cli::cli_warn(msg_warning)
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
  # Retrieve a string that contains the function's name from `data`, then parse
  # and evaluate it to an actual function:
  name <- get_mapped_function_name(data)
  rlang::eval_tidy(rlang::parse_expr(name))
}


#' @rdname get_mapped_function
#' @export

get_mapped_function_name <- function(data) {
  # Check that `data` has exactly one class that starts on `pairmaps_df_`:
  if (!is_pairmaps_df(data)) {
    cli::cli_abort(c(
      "`data` must be the output of a function \\
      created by `as_colpair_mapper()`.",
      "x" = "Either it isn't or its \"pairmaps_df_*\" class \\
      was changed, removed, or supplemented by another one."
    ))
  }
  # Subset this class and remove the `pairmaps_df_` prefix. The substring left
  # behind by this procedure will be the function name:
  classes <- class(data)
  sub("pairmaps_df_", "", classes[grepl("^pairmaps_df_", classes)])
}

