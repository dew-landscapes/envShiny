#' An input_task_button that disables once its task is complete
#'
#' This button type combines the 'processing' function of `bslib::input_task_button` with the 'disabled' functionality of `shiny::actionButton`.
#'
#' @details
#' Use within a `renderUI` function in the server function, and not in the UI, as it will need reactives to track 'load' status.
#'
#' @param id inputID to track input/action (as any other shiny input).
#' @param label_ready Label to display before clicking i.e. ready state of input_task_button.
#' @param loaded #Logical; how to track loaded status (usually a reactiveValues value).
#' @param changed #Optional logical; how to track if the button should be clickable again, when `loaded==TRUE`.
#' @param label_busy Label to display when processing. Used directly in same argument of `input_task_button.`
#' @param label_changed Label to display when `loaded==TRUE` but new action is available and button is clickable again.
#' @param class Sent to `class` argument of all buttons (e.g. 'btn-sm')
#' @param type Sent to `type` argument of input_task_buttons (default 'primary')
#'
#' @returns An `input_task_button`, or a disabled `actionButton` when action is complete.
#'
#' @details
#' The code in Examples below is necessary to track the various changes to load/ready status based on a button-triggered selectInput load.
#'
#' Tracking 'changed' is tricky, because updating the ui button triggers an eventReactive in the same way as if it was clicked, so an
#' eventReactive around the data load won't work (it will load data before the load button is clicked). Instead,
#' use `bindCache(loaded$selection)` on the data (e.g. a tars() reactive), where `loaded$selection` stores the currently-loaded data name
#' (see Examples).
#'
#'
#' @examples
#' \dontrun{
#'
#'   ## in UI:
#' uiOutput("loadbutton_ui")
#'
#'   ## In server:
#' output$loadbutton_ui <- renderUI(
#'   taskbutton_dynamic("load",
#'   label_ready = "Load data",
#'   loaded = loaded$loaded, #see below
#'   changed = loaded$changed)
#' )
#'
#'   ### To track top-level data loading (e.g at the start of an app), use:
#' loaded <- reactiveValues(
#'   loaded = FALSE,   #is loaded?
#'   selection = NULL, #track extent & grain selections
#'   changed = FALSE   #selection != loaded
#' )
#'
#' observeEvent({
#'   input$select_extent
#'   input$select_grain
#' }, {
#'   loaded$changed = ifelse(loaded$loaded, TRUE, loaded$loaded)
#' }, priority = 1)
#'
#' observeEvent(input$load_dataset, {
#'   loaded$loaded = TRUE
#'   loaded$selection = paste(input$select_extent, input$select_grain)
#'   loaded$changed = FALSE
#' })
#' observeEvent({
#'   input$select_extent
#'   input$select_grain
#' }, {
#'   if(loaded$loaded){
#'     loaded$changed = ifelse(paste(input$select_extent, input$select_grain) == loaded$selection,
#'                             FALSE,
#'                             TRUE)
#'   }
#' })
#'
#'   ### To only load new data when button is clicked, use
#' data <- reactive({
#' ...
#' }) |> bindCache(loaded$selected)
#'
#' }
#'
#' @export

taskbutton_dynamic <- function(id,
                               label_ready,
                               loaded,
                               changed = NULL,
                               label_busy = "Processing...",
                               label_done = "Loaded",
                               label_changed = "Update",
                               class = NULL,
                               type = 'primary'
) {

  ui <- input_task_button(id = id,
                          label = label_ready,
                          label_busy = label_busy,
                          type = type,
                          class = class)

  if(loaded){
    ui <- actionButton(inputId = paste0(id, "_inop"), #dummy name to ensure its not listened to
                       label = label_done,
                       icon = icon("circle-check"),
                       class = class,
                       disabled = TRUE)

  }

  if(isTRUE(changed)){
    ui <- input_task_button(id = id,
                            label = label_changed,
                            label_busy = label_busy,
                            type = type,
                            class = class)
    # ui <- actionButton(inputId = id,
    #                    label = label_changed,
    #                    icon = NULL,
    #                    )
  }

  ui

}
