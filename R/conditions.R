#' Environment object used to store information about when certain warnings
#' were last signaled. Used in `sora_warn`.
#' @noRd
freq_env <- new.env(parent = emptyenv())

#' Environment object used to store the current spinner state. Used in `spin`.
#' @noRd
spin_env <- new.env(parent = emptyenv())


#' List of unicode symbols.
#' @noRd
symbols <- list(
  info = "\u2139",
  bullet = "\u2022",
  tick = "\u2714",
  line = "\u2500",
  cross = "\u2716",
  arrow = "\u2192"
)


#' Resets the state of the spinner of `spin()`.
#' @returns Nothing.
#' @noRd
reset_spinner <- function() {
  assign("state", NULL, envir = spin_env)
}


#' Function that performs a single spin motion. Used in loops to iteratively
#' create a spinning animation.
#' @noRd
spin <- function(status = NULL) {
  states <- c("/", "-", "\\")
  prev_state <- spin_env$state %||% "\\"
  prev_idx <- match(prev_state, states) + 1
  if (prev_idx > length(states)) prev_idx <- 1
  cur_state <- states[prev_idx]
  spin_env$state <- cur_state
  if (!is.null(status)) status <- paste0(" ", status)
  msg <- paste0("\r", cur_state, status)
  cnd <- structure(list(message = msg), class = c("message", "condition"))
  signalCondition(cnd)
  cat(msg, sep = "", file = stderr())
}


#' High-level alternative to base::message. Formats dots using bullets.
#' Uses `cli` if possible.
#' @param msg Message to show.
#' @param ... Further lines to append to `msg`.
#' @returns Nothing.
#' @noRd
sora_info <- function(..., .class = NULL) {
  .class <- c(.class, "sora_message")
  verbose <- getOption("sora_verbose")
  log_path <- if (is.character(verbose)) {
    verbose
  }

  if (loadable("cli") && is.null(log_path)) {
    cli::cli_inform(c(...), class = .class, wrap = TRUE)
  } else {
    msg <- format_msg(...)
    
    if (is.null(log_path)) {
      info(msg, .class = .class)
    } else {
      cat(msg, file = log_path, append = TRUE, fill = 1)
    }
  }
}


#' High-level alternative to base::warning. Formats dots using bullets,
#' handles regular warnings (i.e. once per session) and pretty-prints the call.
#' @param msg Message to show.
#' @param ... Further lines to append to `msg`.
#' @param .once Whether to show the warning once per session or every time.
#' @param .class Warning class
#' @param .call Call to show in the header. Can also be an arbitrary string.
#' @param .env Environment used to determine if called from the global env
#' @returns Nothing.
#' @noRd
sora_warn <- function(msg,
                      ...,
                      .once = FALSE,
                      .class = NULL,
                      .call = NULL,
                      .env = parent.frame()) {
  msg <- paste("!", msg)
  
  .call <- .call %||%
    get_main_caller() %||%
    get_caller()
  
  if (is.character(.call)) {
    .call_fmt <- bold(paste0(.call, "()"))
    .call <- sprintf("function `%s`", .call_fmt)
  }
  
  if (...length()) {
    msg <- format_msg(msg, ...)
  }
  
  if (.once) {
    id <- id_gen(msg)
    
    if (is.null(freq_env[[id]])) {
      footer <- "This message is only shown once per session."
      msg <- paste(msg, footer, sep = "\n")
    } else {
      return(invisible())
    }
    
    freq_env[[id]] <- Sys.time()
  }
  
  .class <- c(.class, "sora_warning")
  warn(msg, .class = .class, .call = .call, .env = .env)
}


#' Low-level alternative to base::message that adds a class to the message
#' condition.
#' @param msg Message to show.
#' @param .class Message class.
#' @returns Nothing.
#' @noRd
info <- function(msg, .class = NULL) {
  cnd <- list(message = msg, call = NULL)
  class(cnd) <- c(.class, "message", "condition")
  message(cnd)
}


#' Low-level alternative to base::warning that allows for more freedom in
#' designing warning headers.
#' @param msg Message to show.
#' @param .class Warning class
#' @param .call Call to show in the header. Can also be an arbitrary string.
#' @param .env Environment used to determine if called from the global env
#' @returns Nothing.
#' @noRd
warn <- function(msg, .class = NULL, .call = sys.call(1), .env = parent.frame()) {
  if (identical(.env, globalenv())) {
    .call <- NULL # nocov
  }
  
  cnd <- warningCondition(msg, call = .call, class = .class)
  
  if (is.null(.call)) {
    msg_out <- sprintf("Warning:\n%s", msg)
  } else {
    if (is.language(.call)) {
      .call <- deparse1(.call) # nocov
    }
    msg_out <- sprintf("Warning in %s:\n%s", .call, msg)
  }

  withRestarts(
    {
      signalCondition(cnd)
      invokeRestart("soraWarning") # nocov
    },
    soraWarning = function(cnd) {
      cat(msg_out, "\n", file = stderr()) # nocov
    },
    muffleWarning = function(cnd) {
      # this ensures that warnings can be muffled, e.g., by testthat
      invisible()
    }
  )
}


#' Helper function to prefix messages with bullets if needed.
#' @param ... Optional message lines. Names are interpreted as bullets.
#' @returns A formatted message.
#' @noRd
format_msg <- function(...) {
  dots <- c(...)
  bullets <- names(dots)
  sym_repr <- c("i", "*", "v", "-", "x", ">")
  for (sym in intersect(sym_repr, bullets)) {
    i <- which(sym == sym_repr)
    bullets <- gsub(sym, symbols[[i]], bullets, fixed = TRUE)
  }
  bullets[nzchar(bullets)] <- paste0(bullets[nzchar(bullets)], " ")
  dots <- paste0(bullets, dots)
  msg <- strwrap(dots, exdent = 2)
  paste(msg, collapse = "\n")
}


#' Given a report from a SoRa response, prints messages if verbosity is
#' enabled and a report is available.
#' @param report `linking_report` field from a SoRa response.
#' @param verbose Whether to show messages. If NULL, reads from
#' `sora_notifications` option.
#' @returns Nothing.
#' @noRd
report_if_needed <- function(report, verbose = TRUE) {
  if (verbose && !is.null(report)) {
    for (msg in report$message) {
      sora_info(">" = msg)
    }
  }
}


#' Given notifications from a SoRa response, prints warnings if verbosity
#' is enabled and notifications are available
#' @param notes `notifcations` field from a SoRa response.
#' @param verbose Whether to show warnings. If NULL, reads from
#' `sora_notifications` option.
#' @returns Nothing.
#' @noRd
notify_if_needed <- function(notes, verbose = NULL) {
  verbose <- verbose %||% getOption("sora_notifications", TRUE)
  if (verbose && !is.null(notes)) {
    notes <- notes[notes$level %in% c("warn", "error"), ]
    
    for (i in seq_len(nrow(notes))) {
      level <- notes[i, ]$level
      msg <- notes[i, ]$text
      switch(
        level,
        warn = sora_warn(msg, .once = TRUE),
        error = sora_abort(msg)
      )
    }
  }
}


#' Generates a cropped hash value from a string. Used to generate reproducible
#' string IDs for regular warnings
#' @param x A string.
#' @param n Maximum number of chars.
#' @returns A string.
#' @noRd
id_gen <- function(x, n = 20) {
  raw <- as.integer(charToRaw(x))
  let <- letters[(raw %% 26) + 1]
  substr(paste(let, collapse = ""), 1, n)
}


#' Retrieves the function name of the caller.
#' @returns A string.
#' @noRd
get_caller <- function() {
  call <- sys.call(-3)
  deparse1(call[[1]])
}


#' Retrieves the highest exported sora function on the call stack if possible.
#' If not possible, returns NULL.
#' @returns A string.
#' @noRd
get_main_caller <- function() {
  calls <- sys.calls()
  calls <- vapply(sys.calls(), deparse1, character(1))
  rgx <- main_call_rgx()
  caller <- regex_match(calls, rgx)
  last(unlist(caller)) %|||% NULL
}


#' Returns a regular expression that finds exported functions of the sora
#' package in a call stack.
#' @returns A string.
#' @noRd
main_call_rgx <- function() {
  if (loadable("sora")){
    exports <- getNamespaceExports("sora")
  } else {
    exports <- ls(environment())
    exports <- exports[grep("sora", exports)]
  }
  rgx <- paste(exports, collapse = "|")
  sprintf("^(%s)\\(", rgx)
}


#' Formats a vector or list as a single string divided by commas. The last
#' value is separated specially.
#' @param x A character vector or list.
#' @param last String to separate the last word with.
#' @param max Maximum number of values to show before skipping with ...
#' @returns A string.
#' @noRd
commas <- function (x, last = "and", max = 10) {
  if (length(x) <= 1) {
    return(x)
  }

  ends_with_comma <- identical(last, ",")
  last <- paste0(ifelse(ends_with_comma, "", " "), last, " ")
  if (length(x) > 2 && !ends_with_comma) {
    last <- paste0(",", last)
  }
  
  is_long <- length(x) > max
  if (is_long) {
    limit <- max
    x <- append(x, "...", after = limit - 1)
  } else {
    limit <- length(x) - 1
  }
  
  first_words <- paste(x[seq(1, limit)], collapse = ", ")
  paste(first_words, last(x), sep = last)
}