# server/server_inference.R

#' Servidor para inferencia en la BN
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
server_inference <- function(input, output, session) {
  
  # Reactive para realizar inferencia cuando el usuario lo pida
  inference_result <- eventReactive(input$run_inference, {  # botón específico
    req(session$userData$bn_model)  # asegura que hay modelo
    bn <- session$userData$bn_model
    
    # Ejemplo: calcular probabilidad condicional
    evidence <- list(
      X1 = input$X1_value,
      X2 = input$X2_value
    )
    
    query <- input$query_node
    
    cpquery(bn, event = (get(query) == 1), evidence = evidence)
  })
  
  # Output de la inferencia
  output$inference_output <- renderText({
    req(inference_result())
    paste("Resultado de la inferencia:", round(inference_result(), 3))
  })
}
