suppress_one_warning <- function(expr, regexp) {
  withCallingHandlers(expr,
    warning = function(w) {
        if(grepl(regexp, w$message))
          invokeRestart("muffleWarning")
    })
}

suppress_one_message <- function(expr, regexp) {
  withCallingHandlers(expr,
    message = function(w) {
        if(grepl(regexp, w$message))
          invokeRestart("muffleMessage")
    })
}
