#' Show a shiny notification while code runs
#'
#' Uses the function format of shiny::withProgress (i.e. wrap an expression in this function), but without a progress bar.
#' Useful when a long-running function can't be broken down into smaller chunks (e.g. loading a large RDS or target).
#'
#' @param expr Code to execute
#' @param message Main message to display in notification panel. Shown in bold with strong().
#' @param detail Secondary message to display after message, in smaller text
#' @param ... to showNotification (e.g. type)
#'
#' @export
#'
withNotification <- function(expr,
                             message,
                             detail = NULL,
                             ...) {

  msg <- span(strong(message),
              span(detail, style = "font-size:85%;"))

  notify <- shiny::showNotification(msg, duration = NULL,
                                    ...)

  on.exit(shiny::removeNotification(notify))

  force(expr)

}
