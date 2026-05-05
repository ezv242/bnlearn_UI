# server/server_parameters.R
server_parameters <- function(input, output, session, shared_data) {

data_type_r <- reactiveVal(NULL)

available_method_parameters <- reactive({
  all_methods <- c("bayes", "mle", "hdir", "hard-em",
                   "mle-g", "hard-em-g", "mle-cg", "hard-em-cg")

  # Si no hay preprocesado, mostrar todas las opciones
  if (is.null(shared_data$data_type)) {
    return(all_methods)
  }

  data_type <- shared_data$data_type
  data_continuous <- isTRUE(shared_data$data_continuous)
  data_discrete <- isTRUE(shared_data$data_discrete)

  if (data_type == "cualitativos" || data_discrete) {
    c("bayes", "mle", "hdir", "hard-em")
  } else if (data_continuous) {
    c("mle-g", "hard-em-g")
  } else if (!data_continuous && !data_discrete) {
    c("mle-cg", "hard-em-cg")
  } else {
    all_methods
  }
})

# Mostrar por pantalla la lista desplegable dinamica de metodos de ajuste disponibles
output$method_parameters_selector <- renderUI({
  choices <- available_method_parameters()
  selected <- if (!is.null(input$method_parametershm) && input$method_parametershm %in% choices) {
    input$method_parametershm
  } else {
    choices[1]
  }
  dropdown_input("method_parametershm", choices = choices, value = selected)
})

# Se ejecuta cada vez que se ajusta el modelo, 
# para actualizar el tipo de datos y los parámetros dinámicos
observeEvent(input$fit_model, {

  bn <- shared_data$network
  data <- shared_data$dataset
  data_type_r(shared_data$data_type)
  req(data, bn)  # Esperar a que tanto dataset como network estén disponibles

  # Validar que el dataset y el modelo estén disponibles
  output$server_params_error <- renderText({
     if (is.null(data)) return("Error: dataset no cargado")
     if (is.null(bn)) return("Error: modelo no cargado")
     NULL  # No hay error
  })

  #Se selecciona el método de ajuste
  method <- input$method_parametershm
  shared_data$method_parameters <- method

  # Construir lista de argumentos dinámicamente
  args <- list(x = bn, data = data, method = method)

  # Si método bayes
  if (method == "bayes") {
    args$iss <- input$iss
  }

  # Si el tipo de datos no es ni discreto ni continuo, 
  # se añade el parámetro replace.unidentifiable
  if (method == "mle" || method == "mle-g" || method == "mle-cg") {
    args$replace.unidentifiable <- input$replace_unidentifiable
  }

  # Parámetros comunes
  args$keep.fitted <- input$keep_fitted
  args$debug <- input$debug

  # Ejecutar bn.fit con argumentos dinámicos
  fitted <- do.call(bn.fit, args)

  # Guardar el modelo ajustado
  shared_data$bn_fitted <- fitted

  # Notificación de que el modelo se ha ajustado correctamente
  toast("¡Se ha guardado la configuración del modelo!", class = "success")
})

# Mostrar el modelo ajustado
output$fitted_output <- renderPrint({
  req(shared_data$bn_fitted)   # Espera a que bn_fitted exista
  shared_data$bn_fitted
})

}