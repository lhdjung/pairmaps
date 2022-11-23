
test_that("`is_pairmaps_df()` returns `FALSE` with clearly wrong inputs", {
  expect_false(is_pairmaps_df(1L))
  expect_false(is_pairmaps_df(63.8))
  expect_false(is_pairmaps_df("bird"))
  expect_false(is_pairmaps_df(list("a", c(3, 6, 2), TRUE)))
})


df1 <- covary(mtcars)

test_that("`is_pairmaps_df()` returns `TRUE` when it should", {
  expect_true(is_pairmaps_df(df1))
})


msg1 <- "`is_pairmaps_df()` returns `FALSE` with former pairmaps data frames"
msg2 <- paste(msg1, "the classes of which have been unacceptably manipulated")
msg3 <- paste(msg1, "the \"pairmaps_df_*\" class(es) of which were removed")

test_that(msg2, {
  df2 <- df1
  class(df2) <- c(class(df2), "pairmaps_df_some_function")
  expect_false(is_pairmaps_df(df2))
})

test_that(msg3, {
  df3 <- df1
  df3 <- unclass_pairmaps(df3)
  expect_false(is_pairmaps_df(df3))
})

