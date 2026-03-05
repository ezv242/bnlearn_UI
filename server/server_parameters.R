# server/server_parameters.R
server_parameters <- function(input, output, session) {

#Se ejecuta al hacer clic en el botón de ajustar modelo
observe({

  # ReactiveVal para exponer el tipo a la UI (invisible)
  data_type_r <- reactiveVal(NULL)

  req(session$userData$bn_model)
  req(dataset())

  bn <- session$userData$bn_model
  data <- dataset()

  method <- input$method

  # Construir lista de argumentos dinámicamente
  args <- list(x = bn, data = data, method = method)

  # Si método bayes
  if (method == "bayes") {
    args$iss <- input$iss
  }

  # Se comprueba el tipo de datos
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

  if (data_type_r == "mixed") {
    args$replace.unidentifiable <- input$replace_undentifiable
  }

  args$keep.fitted <- input$keep_fitted
  args$debug <- input$debug

  # Ejecutar bn.fit con argumentos dinámicos
  fitted <- do.call(bn.fit, args)

  # Guardar el modelo ajustado
  bn_fit(fitted)
})

# Output oculto para usar en conditionalPanel
  output$data_type_hidden <- renderText({
    data_type_r()
  })
  outputOptions(output, "data_type_hidden", suspendWhenHidden = FALSE)
}