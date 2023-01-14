
utils::globalVariables(c(
  "!!", ".data", ".diagonal", ".quiet"
))


wrap <- function(x) {
  for (i in seq_along(x)) {
    if (is.character(x[i][[1L]])) {
      x[i][[1]] <- paste0("\"", x[i][[1]], "\"")
    } else {
      x[i][[1]] <- paste0("`", x[i][[1]], "`")
    }
  }
  x
}


paste_names <- function(x) {
  if (length(x) == 1L) {
    return(wrap(as.character(x)))
  }
  out <- ""
  sep <- if (length(x) > 2L) ", " else " "
  for (i in seq_along(x)[-length(x)]) {
    out <- paste0(out, x[[i]], sep)
  }
  paste0(out, "and ", x[length(x)])
}


check_arg_defaults <- function(arg_defaults) {
  if (is.null(arg_defaults)) {
    return(NULL)
  }
  arg_names <- names(arg_defaults)
  arg_names_are_missing <-
    arg_names[arg_names == ""]
  # If names are lacking:
  if (!(
    is.list(arg_defaults) &&
    length(arg_defaults) == length(arg_names[arg_names != ""])
  )) {
    msg_error <- "`arg_defaults` must be a named list."
    args_unnamed <- arg_defaults[arg_names == ""]
    # Atomic vectors have their own error:
    if (!is.list(arg_defaults)) {
      rlang::abort(c(
        msg_error,
        "x" = paste0("It is of type ", typeof(arg_defaults), ".")
      ))
    }
    # Completely unnamed lists also have a dedicated error:
    if (is.null(arg_names)) {
      rlang::abort(c(
        msg_error,
        "x" = "All elements are unnamed."
      ))
    }
    # This long section is about partially named lists:
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
  # If each list element is named but the names are wrong:
  if (!all(arg_names %in% c(".diagonal", ".quiet"))) {
    arg_names <- paste0("`", arg_names, "`")
    if (length(arg_names) == 1L) {
      msg_names <- "The actual name is"
    } else {
      msg_names <- "The actual names are"
    }
    msg_names <- paste0(msg_names, " ", paste_names(arg_names), ".")
    rlang::abort(c(
      "The names of `arg_defaults` must be `.diagonal` and `.quiet`.",
      "x" = msg_names
    ))
  }
}


