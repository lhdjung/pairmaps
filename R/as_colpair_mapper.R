
#' Adverb for new column-pair mapping functions
#'
#' @description `as_colpair_mapper()` takes a function `f` and returns a new
#'   function that applies `f` to each pair of columns in a data frame.
#'
#' @param f A function.
#' @param eval_f Boolean. If set to `FALSE`, `f` will appear in the output
#'   function by name rather than as its body and arguments. Default is `TRUE`.
#'   See details.
#' @param class Boolean. If `TRUE` (the default), the data frames returned by
#'   the output function will inherit a class that starts on `"pairmaps_df_"`,
#'   followed by the name of `f`.
#' @param default_diagonal,default_quiet Defaults for the `.diagonal` and
#'   `.quiet` arguments of the output function. By default of the *present*
#'   function, these will be `NA` and `FALSE`, respectively.
#' @param ... These dots must be empty.
#'
#' @return A function. See `vary()` and `covary()` for examples of functions
#'   made by `as_colpair_mapper()`.

#' @details Setting `eval_f` to `FALSE` can be helpful if you copy and paste the
#'   resulting function into a script. In this case, it makes sense to refer to
#'   one particular function by name, rather than copying and pasting a lengthy
#'   function definition *within* your new function definition, and manually
#'   updating it when the mapped function changes.
#'
#'   However, this should only be considered if the value of `f` is completely
#'   unambiguous, because such ambiguity might lead to a very subtle bug. See
#'   `vignette("using-pairmaps")`, section *Assignment and copying*.

#' @seealso
#' - This function is based on `corrr::colpair_map()`. For comparisons between
#' them, see `vignette("using-pairmaps")`.
#' - The function factory is constructed using `rlang::new_function()`.
#' - For more information on function factories like `as_colpair_mapper()`, see
#' Wickham (2019), ch. 10-11, and `vignette("factory-labels")`.

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
#' # Below are three near-equivalent calls.
#' # The only difference: In the first two cases,
#' # the output tibble inherits an extra class
#' # that includes the name of `f`. Here, it is
#' # `"pairmaps_df_calc_p_value"`.
#'
#' # 1. Using a ready-made mapping function:
#' p_value_map(.data = mtcars)
#'
#' # 2. Directly working with the factory:
#' as_colpair_mapper(f = calc_p_value)(.data = mtcars)
#'
#' # 3. Calling the underlying function:
#' corrr::colpair_map(.data = mtcars, .f = calc_p_value)
#'
#' # (In the second call, R first evaluates
#' # `as_colpair_mapper(f = calc_p_value)` to a
#' # mapping function just like `p_value_map()`,
#' # and then calls that new function on `mtcars`.)


as_colpair_mapper <- function(f, eval_f = TRUE, class = TRUE,
                              default_diagonal = NA, default_quiet = FALSE,
                              ...) {

  # The dots have no intrinsic meaning here. Their only purpose is to prevent a
  # CRAN warning that would otherwise be raised because the output function has
  # dots. They shouldn't be relied upon here, so they must not be used:
  rlang::check_dots_empty()

  # Capture the name of `f` and, by default, make sure to evaluate `f` itself:
  f_value <- substitute(f)
  f_name <- deparse(f_value)
  if (eval_f) {
    f_value <- f
  }

  # Also by default, add a `"pairmaps_df_"` class that captures the name of `f`:
  if (class) {
    class_expr <- rlang::expr(
      class(out) <- c(paste0("pairmaps_df_", f_name), class(out))
    )
  } else {
    class_expr <- NULL
  }

  # Construct the new `corrr::colpair_map()` wrapper from its three components
  # (see Hadley Wickham, *Advanced R*, ch. 6.2.1;
  # https://adv-r.hadley.nz/functions.html#fun-components):
  f_out <- rlang::new_function(
    # 1. List of arguments
    args = rlang::pairlist2(
      .data = , ... = , .diagonal = default_diagonal, .quiet = default_quiet
    ),
    # 2. Body code
    body = rlang::expr({
      f_name <- `!!`(f_name)
      out <- corrr::colpair_map(
        .data = .data, .f = `!!`(f_value),
        ..., .diagonal = .diagonal
      )
      if (!.quiet) {
        cli::cli_inform(c("i" = paste(
          "Applying",
          cli::col_red(paste0("`", f_name), "()`"),
          "to each column pair"
        )))
      }
      `!!`(class_expr)
      out
    }),
    # 3. Parent environment
    env = rlang::caller_env()
  )

  # Garbage collection is important within function factories:
  rm(f_value, f_name, class_expr)

  # Remove the `class_expr` placeholder from the function body if the user chose
  # not to make the output function attach a signature class to its data frames:
  if (!class) {
    body(f_out)[[length(body(f_out)) - 1]] <- NULL
  }

  f_out
}

