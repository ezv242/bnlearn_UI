# ui/ui_main.R
ui_main <- function() {
  semanticPage(
    h2("Aplicación BNLearn"),
    fluidRow(
      column(width = 4, ui_sidebar()),
      column(width = 8, ui_results())
    )
  )
}
