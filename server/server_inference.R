# server/server_inference.R
server_inference <- function(input, output, session) {

  inference_result <- eventReactive(input$run_inference, {
    req(session$userData$bn_fitted)
    fitted <- session$userData$bn_fitted

    # Ejemplo de evidencia y nodo de consulta
    # Ajusta según tus inputs reales
    event_expr <- tryCatch(parse(text = input$event)[[1]], error = function(e) NULL)
    evidence_expr <- tryCatch(parse(text = input$evidence)[[1]], error = function(e) NULL)
    req(event_expr, evidence_expr)  # detiene si son inválidos

    method_inference <- input$method_inference

    # Construir lista de argumentos dinámicamente
    args <- list(fitted = fitted, event = event_expr, evidence = evidence_expr, 
                 method = method_inference)

    # Si método exact, agregar n_samples
    if (method_inference == "lw") {
      args$n <- input$n_samples
    } 

    # Ejecutar bnlearn::cpquery con argumentos dinámicos
    result <- tryCatch({
      do.call(cpquery, args)
    }, error = function(e) {
      return("Error en la inferencia: " + e$message)
    })
    result

  })

  output$inference_output <- renderText({
    req(inference_result())
    paste("Resultado de la inferencia:", round(inference_result(), 3))
  })
}
