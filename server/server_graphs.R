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
    visOptions(manipulation = TRUE)
  })

  # Sincronizar cambios visuales con el modelo bnlearn
  observeEvent(list(input$graph_nodes, input$graph_edges), {
    req(input$graph_nodes)

    nodes_df <- input$graph_nodes
    edges_df <- input$graph_edges
    if (is.null(edges_df)) {
      edges_df <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
    }

    # Crear grafo vacío con los nodos seleccionados
    bn_new <- bnlearn::empty.graph(nodes_df$id)

    # Añadir aristas si hay
    if (nrow(edges_df) > 0) {
      arcs_matrix <- as.matrix(edges_df[, c("from", "to")])
      bnlearn::arcs(bn_new) <- arcs_matrix
    }

    # Guardar en el shared_data
    shared_data$network <- bn_new

    # Actualizar nodos y edges visibles para mantener coherencia
    nodes(nodes_df)
    edges(edges_df)
  })

  output$bn_debug <- renderPrint({
    req(shared_data$network)

    cat("Nodos:\n")
    print(bnlearn::nodes(shared_data$network))

    cat("\nArcos:\n")
    print(bnlearn::arcs(shared_data$network))
  })
}
