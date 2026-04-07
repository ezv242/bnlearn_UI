server_graphs <- function(input, output, session, shared_data) {

  nodes <- reactiveVal(NULL)
  edges <- reactiveVal(NULL)

  graph_results <- observeEvent(input$run_bnlearn, {
    req(shared_data$network)
    bn <- shared_data$network

    nodes(data.frame(
      id = bnlearn::nodes(bn),
      label = bnlearn::nodes(bn)
    ))
    edges(as.data.frame(bnlearn::arcs(bn)))
  })

  # Render del grafo
  output$graph <- renderVisNetwork({
    req(nodes(), edges())
    visNetwork(nodes(), edges(), height = "100%", width = "100%") %>%
    visNodes(shape = "dot", size = 20) %>%
    visEdges(arrows = "to") %>%
    visOptions(manipulation = TRUE)
  })

  # Agregar nodo
  observeEvent(input$add_node, {
    new_id <- nrow(nodes()) + 1
    nodes(rbind(nodes(), data.frame(id = new_id, label = input$new_node)))
  })

  # Eliminar último nodo
  observeEvent(input$remove_node, {
    current_nodes <- nodes()
    if(nrow(current_nodes) > 0) {
      nodes(current_nodes[-nrow(current_nodes),])
    }
  })
}
