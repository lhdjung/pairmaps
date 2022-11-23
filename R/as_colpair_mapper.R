
#' Adverb for new column-pair mapping functions
#'
#' @description `as_colpair_mapper()` takes a function `f` and returns a new
#'   function that applies `f` to each pair of columns in a data frame.
#'
#' @param f A function.
#' @param eval_f Boolean. If set to `FALSE`, `f` will appear in the output
#'   function by name rather than as its body and arguments. Default is `TRUE`.
#'   See details.
#'
#' @return A function. See `vary()` and `covary()` for examples of functions
#'   made by `as_colpair_mapper()`.

#' @details Setting `eval_f` to `FALSE` can be helpful if you copy and paste the
#'   resulting function into a script. In this case, it makes sense to refer to
#'   one particular function by name, rather than copying and pasting a lengthy
#'   function definition *within* your new function definition, and manually
#'   updating it when the mapped function changes.
#'
#'   However, this should only be considered if the reference point of `f` is
#'   completely unambiguous, because such ambiguity might lead to a very subtle
#'   bug. See `vignette("using-pairmaps")`, section *Assignment and copying*.

#' @seealso
#' - This function is based on `corrr::colpair_map()`. For comparisons between
#' them, see `vignette("using-pairmaps")`.
#' - For more general information on functions like `as_colpair_mapper()`, see
#' `vignette("factory-labels")`.

#' @export
#'
#' @references Wickham, H. (2019). *Advanced R* (Second Edition), CRC
#'   Press/Taylor and Francis Group. https://adv-r.hadley.nz/index.html.
#'
#' @examples
#' # From `?corrr::colpair_map()` -- a function that
#' # extracts the p-value from a t-test:
#' calc_p_value <- function(vec_a, vec_b) {
#'   stats::t.test(vec_a, vec_b)$p.value
#' }
#'
#' # Derive a new mapping function:
#' p_value_map <- as_colpair_mapper(f = calc_p_value)
#'
#' # These calls are almost equivalent. In the second
#' # and third cases, the output tibble inherits an extra
#' # class that includes the name of `f`. Here, it is
#' # `"pairmaps_df_calc_p_value"`.
#' corrr::colpair_map(.data = mtcars, .f = calc_p_value)
#'
#' p_value_map(.data = mtcars)
#'
#' as_colpair_mapper(f = calc_p_value)(.data = mtcars)
#'
#' # (In the third case, R first evaluates
#' # `as_colpair_mapper(f = calc_p_value)` to a
#' # mapping function just like `p_value_map()`,
#' # and then calls that new function on `mtcars`.)


as_colpair_mapper <- function(f, eval_f = TRUE) {
  f_value <- substitute(f)
  f_name <- deparse(f_value)
  if (eval_f) {
    f_value <- f
  }
  # Construct the new `corrr::colpair_map()` wrapper:
  rlang::new_function(
    args = as.pairlist(alist(
      .data = , ... = , .diagonal = NA, .quiet = FALSE
    )),
    body = rlang::expr({
      f_name <- `!!`(f_name)
      out <- corrr::colpair_map(
        .data = .data, .f = `!!`(f_value),
        ..., .diagonal = .diagonal
      )
      if (!.quiet) {
        rlang::inform(paste0(
          "Applying `", f_name, "()` to each column pair"
        ))
      }
      class(out) <- c(paste0("pairmaps_df_", f_name), class(out))
      out
    }),
    env = rlang::caller_env()
  )
}

