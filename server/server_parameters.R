# server/server_parameters.R
server_parameters <- function(input, output, session, shared_data) {

# ReactiveVal para exponer el tipo a la UI 
# (discreta, continua, mixta) (invisible)
data_type_r <- reactiveVal(NULL)

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
  method <- input$method

  # Construir lista de argumentos dinámicamente
  args <- list(x = bn, data = data, method = method)

  # Si método bayes
  if (method == "bayes") {
    args$iss <- input$iss
  }

  # Si el tipo de datos es mixto, se añade el parámetro replace.unidentifiable
  if (data_type_r() == "mixed") {
    args$replace.unidentifiable <- input$replace_unidentifiable
  }

  # Parámetros comunes
  args$keep.fitted <- input$keep_fitted
  args$debug <- input$debug

  # Ejecutar bn.fit con argumentos dinámicos
  fitted <- do.call(bn.fit, args)

  # Guardar el modelo ajustado
  shared_data$bn_fitted <- fitted
})

# Output oculto para usar en conditionalPanel
output$data_type_hidden <- renderText({ data_type_r()})
outputOptions(output, "data_type_hidden", suspendWhenHidden = FALSE)

# Mostrar el modelo ajustado
output$fitted_output <- renderPrint({
  req(shared_data$bn_fitted)   # Espera a que bn_fitted exista
  shared_data$bn_fitted
})

}