server_graphs <- function(input, output, session, shared_data) {

# =========================================================================
# INYECTAR EL SCRIPT RECEPTOR DE JAVASCRIPT 
# PARA MOSTRAR CONTENIDO DEL POPUP
# =========================================================================
insertUI(
  selector = "body",
  where = "afterEnd",
  ui = tags$script(HTML("
    Shiny.addCustomMessageHandler('mostrar_mini_popup', function(data) {
      // Buscamos el contenedor interno del widget de visNetwork
      var graphContainer = $('.vis-network');
      
      // Eliminamos cualquier popup anterior para que no se dupliquen
      $('#nodo-custom-popup').remove();
      
      // Construimos la tarjeta sutil de Semantic UI con el texto real que viene de R
      var popupHtml = `
        <div id='nodo-custom-popup' class='ui fluid card' style='
          position: absolute; 
          z-index: 999; 
          width: 250px; 
          box-shadow: 0px 4px 10px rgba(0,0,0,0.15);
          left: ${(data.x + 15)}px; 
          top: ${(data.y - 40)}px;
        '>
          <div class='content' style='padding: 10px;'>
            <div class='header' style='font-size: 1em; color: #2185d0; margin-bottom: 5px;'>
              Nodo: ${data.id}
            </div>
            <div class='description' style='font-size: 0.9em; font-weight: normal; color: #333; max-height: 150px; overflow-y: auto;'>
              ${data.texto}
            </div>
          </div>
        </div>
      `;
      
      // Lo añadimos al lienzo
      graphContainer.append(popupHtml);
    });
  ")),
  immediate = TRUE
)
# =========================================================================

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
    )) %>%
    visEvents(
      selectNode = htmlwidgets::JS("function(properties) {
        var nodeId = properties.nodes[0];
        var canvasPosition = this.canvasToDOM(this.getPositions([nodeId])[nodeId]);
        var graphContainer = $(this.container);
        
        $('#nodo-custom-popup').remove();
        
        var popupHtml = `
          <div id='nodo-custom-popup' class='ui fluid card' style='
            position: absolute; 
            z-index: 999; 
            width: 300px; 
            box-shadow: 0px 4px 10px rgba(0,0,0,0.15);
            left: ${canvasPosition.x + 15}px; 
            top: ${canvasPosition.y - 40}px;
          '>
            <div class='content' style='padding: 10px;'>
              <div class='header' style='font-size: 1.1em;'>Nodo: ${nodeId}</div>
              <div class='description' style='font-size: 0.9em; margin-top: 5px; color: rgba(0,0,0,0.6);'>
                Coordenadas:<br>
                X: ${Math.round(canvasPosition.x)} | Y: ${Math.round(canvasPosition.y)}
              </div>
            </div>
          </div>
        `;
        
        graphContainer.append(popupHtml);
        
        Shiny.setInputValue('nodo_seleccionado_info', {
          id: nodeId,
          x: canvasPosition.x,
          y: canvasPosition.y
        }, {priority: 'event'});
      }"),

      deselectNode = htmlwidgets::JS("function(properties) {
        $('#nodo-custom-popup').remove();
      }")
    )
  })

observeEvent(input$nodo_seleccionado_info, {
  info <- input$nodo_seleccionado_info
  req(info)
  nodo <- info$id

  #cat("Fitted - ID del nodo:\n")
  #print(shared_data$bn_fitted[[nodo]])

  # Extraer el objeto dinámico desde tu red bayesiana
  texto_nodo <- shared_data$bn_fitted[[nodo]]

  # Si el nodo seleccionado no tiene datos asociados
  if (is.null(texto_nodo)) {
    texto_nodo <- "Sin datos disponibles"
  } else {
    # 1. Atrapamos el print formateado tal cual sale en la terminal
    salida_consola <- capture.output(print(texto_nodo))

    # 2. Juntamos las líneas respetando los saltos de línea originales
    texto_plano <- paste(salida_consola, collapse = "\n")

    # 3. Lo envolvemos en HTML con estilo de consola monoespaciada
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
  # Nota: Eliminé 'as.character()' para que no rompa nuestra estructura HTML armada
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
