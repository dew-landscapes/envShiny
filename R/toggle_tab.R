#' Enable/disable `bslib::nav_panel` tabs based on some condition
#'
#' For example, to 'hide' tabs that aren't valid because outputs don't exist, or sections of the app that aren't available before loading.
#' Works for nav_panels in e.g. page_navbar, navset_bar, nav_menu.
#'
#' @param tab_id Character vector of tab ids set in UI by `value` argument of `bslib::nav_panel`
#' @param enable Logical of length 1 that defines if the tab should be enabled (TRUE) or disabled (FALSE)
#'
#' @export
#'
toggle_tab <- function(tab_id,
                       enable = TRUE) {

  purrr::walk(tab_id,
              \(x){

                selector <- paste0(".navbar .nav-link[data-value='", x, "'], ",
                                   ".navbar .dropdown-item[data-value='", x, "']")

                if(enable) {
                  shinyjs::removeClass(class = "disabled",
                                       selector = selector)
                } else {
                  shinyjs::addClass(class = "disabled",
                                    selector = selector)
                }
              })
}
