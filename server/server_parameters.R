# server/server_parameters.R
server_parameters <- function(input, output, session) {

# ReactiveVal para exponer el tipo a la UI 
# (discreta, continua, mixta) (invisible)
data_type_r <- reactiveVal(NULL)

# Se ejecuta cada vez que se ajusta el modelo, 
# para actualizar el tipo de datos y los parámetros dinámicos
observe({

  bn <- session$userData$bn_model
  dataset_reactive <- session$userData$dataset
  req(dataset_reactive, bn)  # Esperar a que tanto dataset como bn estén disponibles
  data <- req(dataset_reactive())

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

  # Se comprueba el tipo de datos y se almacena en data_type_r y
  # session$userData$data_type
  if(all(sapply(data, is.factor))){
    session$userData$data_type <- "discrete"
    data_type_r("discrete")
  } else if(all(sapply(data, is.numeric))){
    session$userData$data_type <- "continuous"
    data_type_r("continuous")
  } else {
    session$userData$data_type <- "mixed"
    data_type_r("mixed")
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
  session$userData$bn_fitted <- fitted
})

# Output oculto para usar en conditionalPanel
output$data_type_hidden <- renderText({ data_type_r()})
outputOptions(output, "data_type_hidden", suspendWhenHidden = FALSE)

# Mostrar el modelo ajustado
output$fitted_output <- renderPrint({
  req(session$userData$bn_fitted)   # Espera a que bn_fitted exista
  session$userData$bn_fitted
})
}