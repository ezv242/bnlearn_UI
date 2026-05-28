source("server/server_manipulationGraph.R", local = TRUE)
server_graphs <- function(input, output, session, shared_data) {
  # Reactive para almacenar nodos y aristas del grafo
  nodes <- reactiveVal(NULL)
  edges <- reactiveVal(NULL)
  nodo_nuevo <- reactiveVal(NULL)

  # Actualizar nodos y aristas cada vez que se carga o actualiza la red bayesiana
  graph_results <- observeEvent(shared_data$network, {
    req(shared_data$network)
    bn <- shared_data$network

    nodes(data.frame(
      id = bnlearn::nodes(bn),
      label = bnlearn::nodes(bn)
    ))

    arcs_df <- as.data.frame(bnlearn::arcs(bn))
    if (nrow(arcs_df) == 0) {
      arcs_df <- data.frame(
        from = character(0), to = character(0), stringsAsFactors = FALSE
      )
    }else {
      arcs_df$id <- paste0(arcs_df$from, "-", arcs_df$to)
    }
    edges(arcs_df)
  }, ignoreNULL = TRUE)

  # Render del grafo
  output$graph <- renderVisNetwork({
    req(nodes(), edges())

    visNetwork(nodes(), edges(), height = "50%", width = "100%") %>%
      visNodes(shape = "dot", size = 20) %>%
      visEdges(arrows = "to") %>%
      visLayout(randomSeed = 123) %>%
      visOptions(
        manipulation = list(
          enabled    = TRUE,
          addNode    = js_add_node,
          addEdge    = js_add_edge,
          deleteNode = js_delete_node,
          deleteEdge = js_delete_edge,
          editNode   = FALSE,
          editEdge   = FALSE
        )
      ) %>%
      visEvents(
        beforeDrawing = js_idioma_es,
        selectNode    = js_select_node,
        deselectNode  = js_deselect_node
      )
  })

  observeEvent(input$nodo_seleccionado_info, {
    info <- input$nodo_seleccionado_info
    req(info)
    nodo <- info$id

    # Extraer el objeto dinámico desde tu red bayesiana
    texto_nodo <- shared_data$bn_fitted[[nodo]]

    # Si el nodo seleccionado no tiene datos asociados
    if (is.null(texto_nodo)) {
      texto_nodo <- "Sin datos disponibles"
    } else {
      # Atrapar el texto tal cual sale en la terminal
      salida_consola <- capture.output(print(texto_nodo))
      # Juntar las líneas respetando los saltos de línea originales
      texto_plano <- paste(salida_consola, collapse = "\n")
      # Envolver en HTML con estilo de consola monoespaciada
      texto_nodo <- paste0(
        "<pre style='",
        "font-family: monospace, Courier, monospace-all; ",
        "font-size: 11px; ",
        "white-space: pre; ",      # Obliga a respetar los espacios múltiples de alineación
        "margin: 0; ",
        "background-color: #f8f9fa; ", # Gris sutil de fondo
        "padding: 8px; ",
        "border-radius: 4px; ",
        "overflow-x: auto; ",      # Por si la tabla es muy ancha, añade scroll horizontal interno
        "color: #333;",
        "'>", 
        texto_plano, 
        "</pre>"
      )
    }
    # Se envía el HTML formateado y la posición de vuelta al navegador
    session$sendCustomMessage(type = "mostrar_mini_popup", message = list(
      texto = texto_nodo,
      x = info$x,
      y = info$y,
      id = info$id
    ))
  })

  # Sincronizar cambios visuales con el modelo bnlearn
  observeEvent(input$graph_change, {

    req(shared_data$network)
    req(shared_data$dataset)
    res <- input$graph_change
    nodo_nuevo(res$data$label)
    bn <- shared_data$network
    data <- shared_data$dataset

    print("Cambio en el grafo detectado:")
    print(res)

    tryCatch({

      # Se añade un nuevo nodo al modelo bnlearn y al dataset
      if (res$type == "addNode") {
        show_modal("modal_añadir_nodo")
      }

      if (res$type == "addEdge") {
        bn_new <- bnlearn::set.arc(
          bn,
          from = res$data$from,
          to = res$data$to,
          check.cycles = TRUE
        )
        shared_data$network <- bn_new
      }

      if (res$type == "deleteNode") {

        # Contamos cuántos IDs vienen en el vector de nodos
        num_borrados <- length(res$data$nodes)

        # Si solo es uno, lo borramos directamente
        if (num_borrados == 1) {
          id_a_borrar <- res$data$nodes[[1]]
          bn_new <- bnlearn::remove.node(bn, id_a_borrar)
          data[[id_a_borrar]] <- NULL

        } else if (num_borrados > 1) {
          # Si el usuario seleccionó varios y los borró de golpe
          for (id in res$data$nodes) {
            bn <- bnlearn::remove.node(bn, id)
            data[[id]] <- NULL
          }
        }
        shared_data$dataset <- data
        shared_data$network <- bn_new
      }

      if (res$type == "deleteEdge") {
        # El id de la arista es en formato from-to, por ejemplo "A-B"
        edge_id <- unlist(res$data$edges)[[1]]
        nodos_arista <- unlist(strsplit(edge_id, "-"))# vector con los nodos [from, to]
        args <- list(x = bn, from = nodos_arista[1], to = nodos_arista[2])
        bn_new <- do.call(bnlearn::drop.arc, args) #bnlearn::drop.arc(bn, from = from, to = to)
        shared_data$network <- bn_new
      }
    }, error = function(e) {
      showNotification(paste(
        "Error al modificar el grafo:",
        e$message
      ), type = "error")
    })
  })

  # Configuración del nuevo nodo añadido
  observeEvent(input$btn_añadir_nodo, {

    tipo_nodo <- input$new_node_type
    niveles <- input$new_node_levels
    new_data <- shared_data$dataset
    bn <- shared_data$network
    nombre_nodo <- input$new_node_name

    tryCatch({
      bn_new <- bnlearn::add.node(bn, nombre_nodo)
      shared_data$network <- bn_new
      tipo_dataset <- shared_data$data_type

      if (tipo_nodo == "cualitativo") {
        # Se crea la columna con NA y le asignamos niveles
        new_data[[nombre_nodo]] <- factor(
          rep(NA, nrow(new_data)), 
          levels = unlist(strsplit(niveles, ","))
        )
      } else {
        # Se crea la columna como numérica
        new_data[[nombre_nodo]] <- as.numeric(rep(NA, nrow(new_data)))
        # Actualizar información del dataset en shared_data
      }
      # Actualizar información del dataset en shared_data
      if(tipo_nodo != tipo_dataset){
        shared_data$data_type <- "mixtos"
        shared_data$data_continuous <- FALSE
        shared_data$data_discrete <- FALSE
      }
      shared_data$dataset_NAs <- TRUE
      shared_data$dataset <- new_data

      # Cerrar el modal automáticamente al terminar
      shiny.semantic::hide_modal(
        id = "modal_añadir_nodo",
        session = shiny::getDefaultReactiveDomain(),
        asis = TRUE
      )
    }, error = function(e) {
      showNotification(paste(
        "Error al añadir nodo:",
        e$message
      ), type = "error")
    })
  })

  # Mostrar el grafo (red bayesiana) con graphviz
  output$bn_plot <- renderPlot({
    req(shared_data$network)
    graphviz.plot(shared_data$network)
  })

  output$bn_debug <- renderPrint({
    req(shared_data$network)
    shared_data$network
  })

}
