# server/server_inference.R
server_inference <- function(input, output, session) {

  inference_result <- eventReactive(input$run_inference, {
    req(session$userData$bn_model)
    bn <- session$userData$bn_model

    # Ejemplo de evidencia y nodo de consulta
    # Ajusta según tus inputs reales
    evidence <- list()  # por ejemplo: list(X1 = 1)
    query_node <- "X2"  # ejemplo

    cpquery(bn, event = (get(query_node) == 1), evidence = evidence)
  })

  output$inference_output <- renderText({
    req(inference_result())
    paste("Resultado de la inferencia:", round(inference_result(), 3))
  })
}
