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
    visOptions(manipulation = list(
      enabled = TRUE,
      addNode = htmlwidgets::JS("function(data, callback) { 
        Shiny.setInputValue('graph_change', {type: 'addNode', data: data}, {priority: 'event'});
        callback(null); 
      }"),
      addEdge = htmlwidgets::JS("function(data, callback) { 
        Shiny.setInputValue('graph_change', {type: 'addEdge', data: data}, {priority: 'event'});
        callback(data); 
      }"),
      deleteNode = htmlwidgets::JS("function(data, callback) { 
        Shiny.setInputValue('graph_change', {type: 'deleteNode', data: data}, {priority: 'event'});
        callback(data); 
      }"),
      deleteEdge = htmlwidgets::JS("function(data, callback) { 
        Shiny.setInputValue('graph_change', {type: 'deleteEdge', data: data}, {priority: 'event'});
        callback(data); 
      }")
      #editNode = FALSE,
      #editEdge = FALSE
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

    cat("\n--- FEEDBACK DEL GRAFO ---\n")
    print(paste("Evento detectado:", res$type))

    # Se añade un nuevo nodo al modelo bnlearn y al dataset
    if (res$type == "addNode") {
      show_modal("modal_añadir_nodo")
    }

    if (res$type == "addEdge") {
      tryCatch({
        bn_new <- bnlearn::set.arc(
          bn,
          from = res$data$from,
          to = res$data$to,
          check.cycles = TRUE
        )
        shared_data$network <- bn_new
      }, error = function(e) {
        showNotification(paste(
          "Error al añadir arista:",
          e$message
        ), type = "error")
      })
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
      bn_new <- bnlearn::drop.arc(bn, from = res$data$from, to = res$data$to)
      shared_data$network <- bn_new
    }
    cat("--------------------------\n")
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
        # CORRECTO: Creamos la columna con NA y le asignamos niveles
        new_data[[nombre_nodo]] <- factor(
          rep(NA, nrow(new_data)), 
          levels = unlist(strsplit(niveles, ","))
        )
      } else {
        # CORRECTO: Creamos la columna como numérica
        new_data[[nombre_nodo]] <- as.numeric(rep(NA, nrow(new_data)))
        # Actualizar información del dataset en shared_data
      }
      # Actualizar información del dataset en shared_data
      if(tipo_nodo != tipo_dataset){
        shared_data$data_type <- "mixtos"
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


  output$bn_plot <- renderPlot({
    req(shared_data$network)
    graphviz.plot(shared_data$network)
  })

  output$bn_debug <- renderPrint({
    req(shared_data$network)
    shared_data$network
  })

}
