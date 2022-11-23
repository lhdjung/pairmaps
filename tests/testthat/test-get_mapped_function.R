
df1 <- vary(mtcars)
df2 <- covary(mtcars)


test_that("mapped functions are retrieved correctly", {
  var_from_df <- get_mapped_function(df1)
  cov_from_df <- get_mapped_function(df2)

  expect_identical(var_from_df, stats::var)
  expect_identical(cov_from_df, stats::cov)
})


test_that("mapped functions' names are retrieved correctly", {
  var_name_from_df <- get_mapped_function_name(df1)
  cov_name_from_df <- get_mapped_function_name(df2)

  expect_identical(var_name_from_df, "stats::var")
  expect_identical(cov_name_from_df, "stats::cov")
})


test_that("invalid input (without a \"pairmaps_f_\" class)
          throws an error", {
  expect_error(get_mapped_function(5))
  expect_error(get_mapped_function(mtcars))
})


test_that("invalid input (with more than 1 \"pairmaps_f_\" classes)
          throws an error", {
  df3 <- vary(mtcars)
  class(df3) <- c(class(df3), "pairmaps_df_very")
  expect_error(get_mapped_function(df3))
})

