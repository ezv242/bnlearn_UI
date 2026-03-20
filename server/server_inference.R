server_inference <- function(input, output, session, shared_data) {


  # Actualizar selectizeInput dinámicamente
  observe({
    updateSelectizeInput(
      session,
      "tags",
      choices = names(shared_data$dataset),
      server = TRUE  # recomendable para datasets grandes
    )
  })
##############################################################################
# Reactive para ejecutar CPQUERY
  inference_result <- eventReactive(input$run_inference, {
    fitted <- shared_data$bn_fitted
    if (is.null(fitted)) return("Error: el modelo aún no se ha ajustado")

    # Definir los strings originales del input
    event_str    <- trimws(input$event)
    evidence_str <- trimws(input$evidence)

    # Reemplazar comas por &
    event_str <- gsub(",", "&", event_str)
    evidence_str <- gsub(",", "&", evidence_str)

    event_expr    <- tryCatch(parse(text = event_str)[[1]],    error = function(e) NULL)
    evidence_expr <- tryCatch(parse(text = evidence_str)[[1]], error = function(e) NULL)

    #if (is.null(event_expr))    return("Error: expresión de evento inválida")
    #if (is.null(evidence_expr)) return("Error: expresión de evidencia inválida")

    method_inference <- input$method_inference

    # Construir lista de argumentos base
    args <- list(
      fitted   = fitted,
      event    = event_expr,
      evidence = evidence_expr,
      method   = method_inference
    )

    result <- tryCatch({ 

      if(method_inference == "lw") {
        # do.call(bnlearn::cpquery, args)
        # Para el caso de lw se debe pasar como evidencia variables del entorno tipo dataset
        # enviar datos al JS y convertir input
        # Suponiendo que tu dataset es shared_data$dataset
        session$sendCustomMessage(
          type = "convertirInputDatasetSimple",
          message = list(
            inputId = "evidencia_lw",
            nFilas = nrow(shared_data$dataset)
          )
        )
        #Las columnas a ser extraidas (eventos)
        columnasText <- getStringNodes(event_str)
        #Seleccionar solo columnas que estén en el dataset AND que sean parte del evento
        columnas <- match(columnasText, names(shared_data$dataset))
        #Las filas seleccionadas por el usuario como evidencia
        filas <- input$evidencia_lw + 1
        do.call(bnlearn::cpquery,
          list(
            fitted = fitted,
            event = event_expr,
            evidence = as.list(shared_data$dataset[filas, -columnas]),
            method = method_inference,
          )
        )

      }else{
        do.call(bnlearn::cpquery, args)
      }
    }, error = function(e) {
      paste("Error completo:", conditionMessage(e))
    })

    result
  })

##############################################################################
# Reactive para ejecutar CPDIST
  cpdist <- eventReactive(input$run_cpdist, {
    fitted <- shared_data$bn_fitted
    if (is.null(fitted)) return("Error: el modelo aún no se ha ajustado")

    nodos_evento <- input$tags
    evidence_str <- trimws(input$evidence_cpdist)

    evidence_str <- gsub(",", "&", evidence_str)

    evidence_expr <- tryCatch(parse(text = evidence_str)[[1]], error = function(e) NULL)

    method_inference <- input$method_inference_cpdist

    args <- list(
      fitted   = fitted,
      event    = nodos_evento,
      evidence = evidence_expr,
      method   = method_inference
    )

    result <- tryCatch({ 
      
      if(method_inference == "lw") {
        # do.call(bnlearn::cpquery, args)
        # Para el caso de lw se debe pasar como evidencia variables del entorno tipo dataset
        # enviar datos al JS y convertir input
        session$sendCustomMessage(
          type = "convertirInputDatasetSimple",
          message = list(
            inputId = "evidence_cpdist_lw",
            nFilas = nrow(shared_data$dataset)
          )
        )

        #Seleccionar solo columnas que estén en el dataset AND que sean parte del evento
        columnas <- match(nodos_evento, names(shared_data$dataset))
        #Las filas seleccionadas por el usuario como evidencia
        filas <- input$evidencia_lw + 1
        do.call(bnlearn::cpquery,
          list(
            fitted = fitted,
            event = event_expr,
            evidence = as.list(shared_data$dataset[filas, -columnas]),
            method = method_inference,
          )
        )

      }else{
        do.call(bnlearn::cpquery, args)
      }

    }, error = function(e) {
      paste("Error completo:", conditionMessage(e))
    })

    result
  })

##############################################################################
# Renderizar resultados
  output$inference_output <- renderText({
    res <- inference_result()
    if (is.numeric(res)) {
      paste("Resultado de la inferencia:", round(res, 3))
    } else {
      res
    }
  })

  output$selectedRows <- renderPrint({
    # input$evidencia_lw es un vector de índices de fila (empezando en 0)
    req(input$evidencia_lw)
    
    # Si quieres usarlo directamente en R (dataset indexa desde 1)
    filas <- input$evidencia_lw + 1
    filas
  })
  
  output$datosFiltrados <- renderTable({
    req(input$evidencia_lw)
    filas <- input$evidencia_lw + 1
    shared_data$dataset[filas, ]
  })
}

getStringNodes <- function(event_str) {
  # Separar por comas
  parts <- strsplit(event_str, ",|&")[[1]]
  parts <- trimws(parts)

  # Extraer solo el nombre antes de "==", limpio
  nodes <- sapply(parts, function(p) {
    nodo <- strsplit(p, "==")[[1]][1]  # tomar lado izquierdo
    trimws(nodo)                        # quitar espacios
  }, USE.NAMES = FALSE)

  as.list(nodes)  # devolver como lista de nodos
}