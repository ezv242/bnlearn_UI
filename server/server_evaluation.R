server_evaluation <- function(input, output, session, shared_data) {

available_method_score <- reactive({
  all_methods <- c("loglik", "aic", "bic", "ebic", 
                   "pred-loglik", "bde", "bds", "mbde", "k2",
                   "fnml", "qnml", "nal", "pnal", "loglik-g", "aic-g",
                   "bic-g", "ebic-g", "pred-loglik-g", "bge", "nal-g",
                   "loglik-cg", "aic-cg", "bic-cg", "ebic-cg", "pred-loglik-cg", "nal-cg")

  # Si no hay preprocesado, mostrar todas las opciones
  if (is.null(shared_data$data_type)) {
    return(all_methods)
  }

  data_type <- shared_data$data_type
  data_continuous <- isTRUE(shared_data$data_continuous)
  data_discrete <- isTRUE(shared_data$data_discrete)

  if (data_type == "cualitativos" || data_discrete) {
    c("loglik", "aic", "bic", "ebic", "pred-loglik", "bde", "bds", "mbde", "k2",
      "fnml", "qnml", "nal", "pnal")
  } else if (data_continuous) {
    c("loglik-g", "aic-g", "bic-g", "ebic-g", "pred-loglik-g", "bge", "nal-g")
  } else if (!data_continuous && !data_discrete) {
    c("loglik-cg", "aic-cg", "bic-cg", "ebic-cg", "pred-loglik-cg", "nal-cg")
  } else {
    all_methods
  }
})

# Mostrar por pantalla la lista desplegable dinamica de metodos de ajuste disponibles
output$method_score_selector <- renderUI({
  choices <- available_method_score()
  selected <- if (!is.null(input$method_score) && input$method_score %in% choices) {
    input$method_score
  } else {
    choices[1]
  }
  dropdown_input("method_score", choices = choices, value = selected)
})

}