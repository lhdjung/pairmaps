
#' Variance and covariance data frames
#'
#' @description `vary()` and `covary()` return variance and covariance data
#'   frames instead of matrices, making them easier to explore with the [corrr
#'   package](https://corrr.tidymodels.org/articles/using-corrr.html).
#'
#' @param .data Numeric vector, matrix or data frame.
#' @param ... Further arguments passed on to `var()` or `cov()`, respectively.
#' @param .diagonal Value to which the diagonal will be set. Default is `NA`.
#' @param .quiet Boolean. Set it to `TRUE` to suppress the message about
#'   function mapping.
#'
#' @return Both functions return `cor_df` tibbles, i.e., "correlation data
#'   frames". However, they don't compute correlations: This name simply
#'   reflects the origins of the `cor_df` class in the corrr package.
#'
#' @rdname vary-covary
#'
#' @seealso `as_colpair_mapper()`, which made these functions via
#'   `corrr::colpair_map()`.
#'
#' @include as_colpair_mapper.R
#'
#' @name vary-covary
#'
#' @export
#'
#' @seealso `vary()` wraps `stats::var()`, `covary()` wraps `stats::cov()`. For
#'   the analogous `stats::cor()` wrapper, see `corrr::correlate()`.
#'
#' @examples
#' vary(mtcars)
#'
#' covary(mtcars)

vary <- as_colpair_mapper(stats::var)


#' @rdname vary-covary
#' @export

covary <- as_colpair_mapper(stats::cov)

