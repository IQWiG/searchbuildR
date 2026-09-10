#' Runs the shiny app of the package
#'
#' @returns a shiny app
#' @import shiny
#' @export
#'
#'@examplesIf interactive()
#'
#'run_app()
#'#see vignette
#'
run_app <- function(){
  shinyApp(ui = app_ui, server = app_server)}
