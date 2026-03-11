# ui/ui_results.R
ui_results <- function() {
  tagList(
    plotOutput("bn_plot"),
    verbatimTextOutput("inference_output"),
    verbatimTextOutput("fitted_output")
  )
}
