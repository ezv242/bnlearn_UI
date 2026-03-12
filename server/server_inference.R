server_inference <- function(input, output, session, shared_data) {

  inference_result <- eventReactive(input$run_inference, {
    fitted <- shared_data$bn_fitted
    if (is.null(fitted)) return("Error: el modelo aún no se ha ajustado")

    # ✅ Definir los strings originales del input
    event_str    <- input$event
    evidence_str <- input$evidence

    event_expr    <- tryCatch(parse(text = event_str)[[1]],    error = function(e) NULL)
    evidence_expr <- tryCatch(parse(text = evidence_str)[[1]], error = function(e) NULL)

    if (is.null(event_expr))    return("Error: expresión de evento inválida")
    if (is.null(evidence_expr)) return("Error: expresión de evidencia inválida")

    method_inference <- input$method_inference

    result <- tryCatch({
  
      callr::r(
        func = function(fitted, event_node, event_val, evidence_node, evidence_val, method_inference, n_samples) {
          library(bnlearn)
          
          # cpdist genera muestras condicionadas a la evidencia
          # aqui SÍ acepta listas nombradas correctamente
          evidence_list <- setNames(list(evidence_val), evidence_node)
          
          if (method_inference == "lw") {
            samples <- cpdist(fitted, 
                              nodes   = event_node,
                              evidence = evidence_list,
                              method  = method_inference,
                              n       = n_samples)
          } else {
            samples <- cpdist(fitted,
                              nodes    = event_node,
                              evidence = evidence_list,
                              method   = method_inference)
          }
          
          # Calcular P(event_node == event_val | evidence)
          mean(samples[[event_node]] == event_val)
        },
        args = list(
          fitted         = fitted,
          event_node     = trimws(strsplit(event_str,    "==")[[1]][1]),
          event_val      = trimws(gsub('["\']', '', strsplit(event_str,    "==")[[1]][2])),
          evidence_node  = trimws(strsplit(evidence_str, "==")[[1]][1]),
          evidence_val   = trimws(gsub('["\']', '', strsplit(evidence_str, "==")[[1]][2])),
          method_inference = method_inference,
          n_samples      = if (method_inference == "lw") input$n_samples else NULL
        )
      )
      
    }, error = function(e) {
      paste("Error completo:", conditionMessage(e))
    })

    result
  })

  output$inference_output <- renderText({
    res <- inference_result()
    if (is.numeric(res)) {
      paste("Resultado de la inferencia:", round(res, 3))
    } else {
      res
    }
  })
}