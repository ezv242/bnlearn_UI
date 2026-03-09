# server/server_inference.R
server_inference <- function(input, output, session) {

  inference_result <- eventReactive(input$run_inference, {
    # req detiene la ejecución hasta que haya valor
    fitted <- session$userData$bn_fitted
    if (is.null(fitted)) return("Error: el modelo aún no se ha ajustado")

    # Ejemplo de evidencia y nodo de consulta
    # Ajusta según tus inputs reales
    event_expr <- tryCatch(parse(text = input$event)[[1]], 
                           error = function(e) NULL)
    evidence_expr <- tryCatch(parse(text = input$evidence)[[1]], 
                              error = function(e) NULL)

    # Validar expresiones
    if (is.null(event_expr)) return("Error: expresión de evento inválida")
    if (is.null(evidence_expr)) return("Error: expresión de evidencia inválida")

    method_inference <- input$method_inference

    # Construir lista de argumentos dinámicamente
    args <- list(fitted = fitted, 
                 event = event_expr, 
                 evidence = evidence_expr, 
                 method = method_inference)

    # Si método exact, agregar n_samples
    if (method_inference == "lw") {
      args$n <- input$n_samples
    } 

    # Ejecutar bnlearn::cpquery con argumentos dinámicos
    result <- tryCatch({
      do.call(cpquery, args)
    }, error = function(e) {
      paste("Error en la inferencia: ", e$message)
    })
    result

  })

output$inference_output <- renderText({
  res <- inference_result()
  
  if (is.numeric(res)) {
    paste("Resultado de la inferencia:", round(res, 3))
  } else {
    # Si es texto (error), mostrar tal cual
    res
  }
})

}
