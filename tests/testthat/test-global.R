
wrong1 <- c("a", "b", "c")
wrong2 <- as.list(wrong1)
wrong3 <- `names<-`(wrong2, paste0("letter", 1:3))

correct1 <- list(.diagonal = TRUE, .quiet = FALSE)


test_that("`check_arg_defaults()` errors when it should", {
  expect_error(check_arg_defaults(wrong1))
  expect_error(check_arg_defaults(wrong2))
  expect_error(check_arg_defaults(wrong3))
})


test_that("`check_arg_defaults()` is silent when it should be", {
  expect_silent(check_arg_defaults(correct1))
})

