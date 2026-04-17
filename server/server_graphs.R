server_graphs <- function(input, output, session, shared_data) {

  # Reactive para almacenar nodos y aristas del grafo
  nodes <- reactiveVal(NULL)
  edges <- reactiveVal(NULL)

  graph_results <- observeEvent(shared_data$network, {
    req(shared_data$network)
    bn <- shared_data$network

    nodes(data.frame(
      id = bnlearn::nodes(bn),
      label = bnlearn::nodes(bn)
    ))

    arcs_df <- as.data.frame(bnlearn::arcs(bn))
    if (nrow(arcs_df) == 0) {
      arcs_df <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    }
    edges(arcs_df)
  }, ignoreNULL = TRUE)

  # Render del grafo
  output$graph <- renderVisNetwork({
    req(nodes(), edges())
    visNetwork(nodes(), edges(), height = "50%", width = "100%") %>%
    visNodes(shape = "dot", size = 20) %>%
    visEdges(arrows = "to") %>%
    visOptions(manipulation = list(
      enabled = TRUE,
      addNode = htmlwidgets::JS("function(data, callback) { 
        Shiny.setInputValue('graph_change', {type: 'addNode', data: data}, {priority: 'event'});
        callback(data); 
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
    ))
  })

  # Sincronizar cambios visuales con el modelo bnlearn
  observeEvent(input$graph_change, {
    res <- input$graph_change
    nuevo_nodo <- res$data
    req(shared_data$network)
    req(shared_data$dataset)
    bn <- shared_data$network
    data <- shared_data$dataset

    cat("\n--- FEEDBACK DEL GRAFO ---\n")
    print(paste("Evento detectado:", res$type))

    if (res$type == "addNode") {
      bn_new <- bnlearn::add.node(bn, nuevo_nodo$label) # Agrega el nodo al modelo bnlearn
      data[[nuevo_nodo$label]] <- NA_real_  # Agrega una columna vacía al dataset para el nuevo nodo
      #print(res$data) # Verás ID, etiqueta, etc.
    }

    if (res$type == "addEdge") {
      bn_new <- bn
      # En visNetwork, las aristas nuevas traen 'from' y 'to'
      print(paste("Origen:", res$data$from, "-> Destino:", res$data$to))
    }

    if (res$type == "deleteNode") {
      bn_new <- bnlearn::remove.node(bn, nuevo_nodo$label) # Elimina el nodo del modelo bnlearn
      data[[nuevo_nodo$label]] <- NULL  # Elimina la columna del dataset
      #print(res$data$nodes)
    }
    cat("--------------------------\n")

    shared_data$network <- bn_new
    shared_data$dataset <- data
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
