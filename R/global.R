
utils::globalVariables(c(
  "!!", ".data", ".diagonal", ".quiet"
))


check_arg_defaults <- function(arg_defaults) {
  arg_names <- names(arg_defaults)
  if (!(
    is.list(arg_defaults) &&
    length(arg_defaults) == length(arg_names[arg_names != ""])
  )) {
    msg_error <- "`arg_defaults` must be a named list."
    args_unnamed <- arg_defaults[is.null(arg_names)]
    if (!is.list(arg_defaults)) {
      rlang::abort(c(
        msg_error,
        "x" = paste0("It is of type ", typeof(arg_defaults), ".")
      ))
    }
    if (length(args_unnamed) == 1L) {
      msg_names <- "element is"
    } else {
      msg_names <- "elements are"
    }
    msg_names <- paste(length(args_unnamed), msg_names, "unnamed: ")
    for (i in seq_along(args_unnamed)) {
      if (is.character(args_unnamed[i][[1L]])) {
        args_unnamed[i] <- paste0("\"", args_unnamed[i], "\"")
      } else {
        args_unnamed[i] <- paste0("`", args_unnamed[i], "`")
      }
    }
    if (length(args_unnamed) == 1L) {
      msg_args <- args_unnamed
    } else {
      msg_args <- ""
      msg_sep <- if (length(args_unnamed) > 2L) ", " else " "
      for (i in seq_along(args_unnamed)[-length(args_unnamed)]) {
        msg_args <- paste0(msg_args, args_unnamed[[i]], msg_sep)
      }
      msg_args <- paste0(msg_args, "and ", args_unnamed[length(args_unnamed)])
    }
    msg_names <- paste0(msg_names, msg_args, ".")
    msg_error <- c(msg_error, "x" = msg_names)
    rlang::abort(msg_error)
  }
}

