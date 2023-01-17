
# Basic function used in the tests, from `?colpair_map()`:
calc_p_value <- function(vec_a, vec_b) {
  stats::t.test(vec_a, vec_b)$p.value
}


test_that("functions made by `as_colpair_mapper()` produce the same
          results as `corrr::colpair_map()` if the latter is given
          the same input function as `as_colpair_mapper()`", {
  p_value_map <- as_colpair_mapper(calc_p_value)

  out1 <- corrr::colpair_map(mtcars, calc_p_value)
  out2 <- p_value_map(mtcars)
  out3 <- as_colpair_mapper(calc_p_value)(mtcars)

  out2 <- unclass_pairmaps(out2)
  out3 <- unclass_pairmaps(out3)

  expect_equal(out1, out2)
  expect_equal(out1, out3)
})


test_that("results are the same among all three approaches (see first test)", {
  # Much like `corrr::correlate()`, named `f_map()` for consistency among the
  # tests:
  f_map <- as_colpair_mapper(stats::cor)

  out1 <- corrr::colpair_map(mtcars, stats::cor)
  out2 <- f_map(mtcars)
  out3 <- as_colpair_mapper(stats::cor)(mtcars)

  # Remove the "pairmaps_df_*" classes, which are a source of difference between
  # `out2` and `out3` one the one hand, and `out1` on the other:
  out2 <- unclass_pairmaps(out2)
  out3 <- unclass_pairmaps(out3)

  expect_equal(out1, out2)
  expect_equal(out1, out3)
})


test_that("results are identical among the approaches (see first test)", {
  # A `vary()` alias, named `f_map()` for consistency among the tests:
  f_map <- as_colpair_mapper(stats::var)

  out1 <- corrr::colpair_map(mtcars, stats::var)
  out2 <- f_map(mtcars)
  out3 <- as_colpair_mapper(stats::var)(mtcars)

  # Remove the "pairmaps_df_*" classes, which are a source of difference between
  # `out2` and `out3` one the one hand, and `out1` on the other:
  out2 <- unclass_pairmaps(out2)
  out3 <- unclass_pairmaps(out3)

  expect_equal(out1, out2)
  expect_equal(out1, out3)
})


test_that("results are identical among the approaches (see first test)", {
  # A `covary()` alias, named `f_map()` for consistency among the tests:
  f_map <- as_colpair_mapper(stats::cov)

  out1 <- corrr::colpair_map(mtcars, stats::cov)
  out2 <- f_map(mtcars)
  out3 <- as_colpair_mapper(stats::cov)(mtcars)

  # Remove the "pairmaps_df_*" classes, which are a source of difference between
  # `out2` and `out3` one the one hand, and `out1` on the other:
  out2 <- unclass_pairmaps(out2)
  out3 <- unclass_pairmaps(out3)

  expect_equal(out1, out2)
  expect_equal(out1, out3)
})


test_that("`as_colpair_mapper()`'s arguments are evaluated when
          `as_colpair_mapper()` itself is called with the default
          `eval_f = TRUE`", {
  # With the danger of a lazy evaluation issue due to `f` being evaluated too
  # late; i.e., to `stats::cov()` instead of `calc_p_value()`:
  f <- calc_p_value
  p_value_map <- as_colpair_mapper(f)
  f <- stats::cov
  out1 <- p_value_map(mtcars)

  # Without the danger:
  f <- calc_p_value
  p_value_map <- as_colpair_mapper(f)
  out2 <- p_value_map(mtcars)

  # Should be the same because the issue shouldn't exist:
  expect_equal(out1, out2)
})


test_that("`as_colpair_mapper()`'s arguments are *not* evaluated when
          `as_colpair_mapper()` itself is called with `eval_f = FALSE`", {
  # With the danger of an eager evaluation issue due to `f` being evaluated too
  # early; i.e., to `calc_p_value()` instead of `stats::cov()`:
  f <- calc_p_value
  p_value_map <- as_colpair_mapper(f, eval_f = FALSE)
  f <- stats::cov
  out1 <- p_value_map(mtcars)

  # Without the danger:
  f <- calc_p_value
  p_value_map <- as_colpair_mapper(f, eval_f = FALSE)
  out2 <- p_value_map(mtcars)

  # Shouldn't be the same because the issue shouldn't exist:
  expect_false(all(out1 == out2))
})


test_that("supplying arguments via the dots, `...`, leads to an error
          with the designated error message", {
  expect_error(as_colpair_mapper(stats::cov, some_arg = 4))
  expect_error(as_colpair_mapper(stats::cov, TRUE, c("a", "b"), "dogs"))
})


test_that("named and anonymous functions evoke different messages", {
  max_sum <- function(x, y) max(sum(x), sum(y))

  msg_named <- purrr::quietly(as_colpair_mapper(max_sum))(mtcars)$messages
  msg_anonymous <- purrr::quietly(as_colpair_mapper(function(x, y) max(sum(x), sum(y))))(mtcars)$messages

  expect_false(msg_named == msg_anonymous)
})


