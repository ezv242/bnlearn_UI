server_inference <- function(input, output, session, shared_data) {

  output_result <- reactiveVal(NULL)

  # Actualizar selectizeInput dinámicamente
  observe({
    req(shared_data$dataset)
    updateSelectizeInput(
      session,
      "tags",
      choices = names(shared_data$dataset),
      server = TRUE  # recomendable para datasets grandes
    )
  })

  # Configurar input de evidencia para lw cuando se selecciona el método
  observe({
    if (!is.null(input$method_inference_cpquery) && input$filas_cpquery == TRUE && !is.null(shared_data$dataset)) {
      session$sendCustomMessage(
        type = "convertirInputDatasetSimple",
        message = list(
          inputId = "evidencia_lw",
          nFilas = nrow(shared_data$dataset)
        )
      )
    }
  })

  # Configurar input de evidencia para cpdist lw
  observe({
    if (!is.null(input$method_inference_cpdist) && input$filas_cpdist == TRUE && !is.null(shared_data$dataset)) {
      session$sendCustomMessage(
        type = "convertirInputDatasetSimple",
        message = list(
          inputId = "evidence_cpdist_lw",
          nFilas = nrow(shared_data$dataset)
        )
      )
    }
  })

##############################################################################
# Reactive para ejecutar CPQUERY
  cpquery <- eventReactive(input$run_inference, {

    fitted <- shared_data$bn_fitted
    if (is.null(fitted)) return("Error: el modelo aún no se ha ajustado")

    filas_cpquery <- input$filas_cpquery
    method_inference <- input$method_inference_cpquery
    # Definir los strings originales del input
    event_str    <- input$event
    evidence_str <- input$evidence

    # Se obtiene el evento y la evidencia en forma de expresion
    event_expr <- tryCatch(getExpressionFromText(event_str), error = function(e) NULL)
    evidence_expr <- tryCatch(getExpressionFromText(evidence_str), error = function(e) NULL)

    if(is.null(event_expr)) return("Error: Expresión del evento inválida.")
    if(!filas_cpquery && is.null(evidence_expr)) return("Error: Expresión de evidencia inválida.")

    # Construir lista de argumentos base
    args <- list(
      fitted   = fitted,
      event    = event_expr,
      method   = method_inference
    )

    # Añadir parametros generales
    if(!is.null(input$n_cpquery)) args$n <- input$n_cpquery
    if(!is.null(input$batch_cpquery)) args$batch <- input$batch_cpquery

    # Si se usan las filas del dataset como evidencia
    if(filas_cpquery){
      #Las columnas a ser extraidas (eventos)
      columnas_text <- getStringNodes(event_str)
      #Seleccionar columnas que del dataset AND que son parte del evento
      columnas <- match(columnas_text, names(shared_data$dataset))
      #Las filas seleccionadas por el usuario como evidencia
      filas <- input$evidencia_lw + 1 # Se obtienen las filas seleccionadas
      evidencia_dataset <- shared_data$dataset[filas, -columnas]
      if(method_inference == "lw"){
        args$evidence <- as.list(evidencia_dataset)#Formato lista
      }else{
        args$evidence <- getExpressionFromDataset(evidencia_dataset)#Formato expresión
      }
    # Si se usa el cuadro de texto de expresión como evidencia
    }else{
      if(method_inference == "lw"){
        args$evidence <- getListFormat(evidence_str)#Formato lista
      }else{
        args$evidence <- evidence_expr#Formato expresión
      }
    }
    str(evidence_expr)
    # Se ejecuta la inferencia
    result <- tryCatch({ 
      do.call(bnlearn::cpquery, args)
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

    filas_cpdist <- input$filas_cpdist
    method_inference_cpdist <- input$method_inference_cpdist
    # Nodos seleccionados para el evento
    nodos_evento <- input$tags
    if(is.null(nodos_evento) || length(nodos_evento) == 0) return("Error: Selecciona al menos un nodo para el evento.")
    str(nodos_evento)

    # Cadena de evidencia
    evidence_str <- input$evidence_cpdist
    evidence_expr <- tryCatch(getExpressionFromText(evidence_str), error = function(e) NULL)

    args <- list(
      fitted   = fitted,
      nodes    = nodos_evento,
      method   = method_inference_cpdist
    )

    # Añadir parametros generales
    if(!is.null(input$n_cpdist)) args$n <- input$n_cpdist
    if(!is.null(input$batch_cpdist)) args$batch <- input$batch_cpdist

    if(filas_cpdist){
      columnas <- match(nodos_evento, names(shared_data$dataset))
      #Las filas seleccionadas por el usuario como evidencia
      filas <- input$evidence_cpdist_lw + 1 # Se obtienen las filas seleccionadas
      evidencia_dataset <- shared_data$dataset[filas, -columnas]
      if(method_inference_cpdist == "lw"){
        args$evidence <- as.list(evidencia_dataset)#Formato lista
      }else{
        args$evidence <- getExpressionFromDataset(evidencia_dataset)#Formato expresión
      }
    }else{
      if(method_inference_cpdist == "lw"){
        args$evidence <- getListFormat(evidence_str)#Formato lista
      }else{
        args$evidence <- evidence_expr#Formato expresión
      }
    }

    # Se ejecuta la inferencia
    result <- tryCatch({ 
      do.call(bnlearn::cpdist, args)
    }, error = function(e) {
      paste("Error completo:", conditionMessage(e))
    })
    result
  })

##############################################################################
# Renderizar resultados

  observeEvent(input$run_inference, {
    output_result("cpquery")
  })

  observeEvent(input$run_cpdist, {
    output_result("cpdist")
  })

  output$render_cpquery_text <- renderPrint({
    cpquery() # Evaluamos el eventReactive de cpquery
  })

  output$render_cpdist_text <- renderPrint({
    cpdist() # Evaluamos el eventReactive de cpdist
  })

  # 4. El renderizado dinámico: SOLO muestra el último que cambió
  output$output_inference <- renderUI({
    req(output_result())

    if (output_result() == "cpquery") {
      # Devolvemos el texto de validación cruzada
      verbatimTextOutput("render_cpquery_text")

    } else if (output_result() == "cpdist") {
      # Devolvemos, por ejemplo, una tabla u otro texto
      verbatimTextOutput("render_cpdist_text")
    }
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

# Función para transformar input de texto al formato de evidencia
getListFormat <- function(text_str) {

  parts <- strsplit(text_str, ",|&")[[1]]
  parts <- trimws(parts)
  lista_final <- list() # Aquí iremos guardando todo

  # 2. Bucle iterativo
  for (p in parts) {
    # Dividir por el "=="
    # Usamos un if por si acaso una parte no tiene "=="
    if (grepl("==", p)) {
      res <- strsplit(p, "==")[[1]]

      # Limpiar el nombre (Clave)
      nodo <- as.character(trimws(res[1]))
      valor <- as.character(trimws(res[2]))

      num <- suppressWarnings(as.numeric(valor))
      if (!is.na(num)) {
        valor <- num
      }
      # Limpiar el valor y quitarle las comillas
      #valor <- gsub("['\"]", "", valor)

      # Aquí usamos los corchetes dobles para crear la clave con el nombre
      # que está guardado en la variable 'nodo'
      lista_final[[nodo]] <- valor
    }
  }
  lista_final
}

getExpressionFromText <- function(text_str){
  # Eliminar especios vacios
  text_str <- trimws(text_str)
  # Reemplazar comas por &
  text_str <- gsub(",", "&", text_str)
  # Convertir el texto en una expresion de R
  #text_str    <- tryCatch(parse(text = text_str)[[1]],    error = function(e) NULL)
  text_str    <- parse(text = text_str)[[1]]
  text_str
}

getExpressionFromText2 <- function(text_str){
  parts <- strsplit(text_str, ",|&")[[1]]
  parts <- trimws(parts)
  text_final <- ""
  text_operacion <- ""

  # Bucle iterativo
  for (p in parts) {
    if (text_final != "") text_final <- paste0(text_final, " & ")
    # Dividir por el "=="
    # Se usa un if por si acaso una parte no tiene "=="
    if (grepl("==|>=|<=|>|<", p)) {
      # Se extrae el operador exacto y luego se parte por ese operador
      operador <- regmatches(p, regexpr("==|>=|<=|>|<", p))
      res <- strsplit(p, "==|>=|<=|>|<")[[1]]

      # Limpiar el nombre (Clave)
      nodo <- as.character(trimws(res[1]))
      valor <- as.character(trimws(res[2]))

      num <- suppressWarnings(as.numeric(valor)) # Se comprueba si es numérico
      if (!is.na(num)) {
        valor <- num
      }

      text_operacion <- paste0("(", nodo, " ", operador, " ", valor, ")")
    }
  }
}


getExpressionFromDataset <- function(dataset){
  # Asegurarnos de trabajar con una sola fila
  fila <- as.list(dataset)
  nombres <- names(fila)

  # Construir cada fragmento de la expresión
  fragmentos <- sapply(nombres, function(nom) {
    valor <- fila[[nom]]

    # Si es factor, extraemos su etiqueta de texto y ponemos comillas
    if (is.factor(valor) || is.character(valor)) {
      return(paste0("(", nom, " == '", as.character(valor), "')"))
    }
    # Si es numérico o lógico, lo ponemos tal cual (sin comillas)
    else {
      return(paste0("(", nom, " == ", valor, ")"))
    }
  })

  # Unir todos los fragmentos con &
  texto_final <- paste(fragmentos, collapse = " & ")

  # Convertir a expresión de R
  parse(text = texto_final)[[1]]
}