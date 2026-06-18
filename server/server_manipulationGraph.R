# Funciones de manipulación para Shiny
js_add_node    <- htmlwidgets::JS("function(d, cb) { Shiny.setInputValue('graph_change', {type: 'addNode', data: d}, {priority: 'event'}); cb(null); }")
js_add_edge    <- htmlwidgets::JS("function(d, cb) { Shiny.setInputValue('graph_change', {type: 'addEdge', data: d}, {priority: 'event'}); cb(d); }")
js_delete_node <- htmlwidgets::JS("function(d, cb) { Shiny.setInputValue('graph_change', {type: 'deleteNode', data: d}, {priority: 'event'}); cb(d); }")
js_delete_edge <- htmlwidgets::JS("function(d, cb) { Shiny.setInputValue('graph_change', {type: 'deleteEdge', data: d}, {priority: 'event'}); cb(d); }")

# Traducción al Español
js_idioma_es <- htmlwidgets::JS("function() {
  if(!this.idiomaConfigurado) {
    this.setOptions({
      locale: 'es',
      locales: {
        es: {
          edit: 'Editar', del: 'Eliminar seleccionado', back: 'Atrás',
          addNode: 'Añadir Nodo', addEdge: 'Añadir Arista',
          editNode: 'Editar Nodo', editEdge: 'Editar Arista',
          addDescription: 'Haz clic en un espacio vacío para colocar un nuevo nodo.',
          edgeDescription: 'Haz clic en un nodo y arrastra el enlace a otro para conectarlos.',
          editEdgeDescription: 'Arrastra los extremos a otro nodo para conectarlos.',
          createEdgeError: 'No se pueden conectar enlaces a un grupo vacío.',
          deleteClusterError: 'Los clusters no se pueden eliminar.',
          editClusterError: 'Los clusters no se pueden editar.'
        }
      }
    });
    this.idiomaConfigurado = true;
  }
  // Fuerza bruta para traducir el botón principal 'Edit'
  $(this.container).find('.vis-manipulation .vis-edit .vis-label').text('Editar');
}")

# Comportamiento del Popup Personalizado al seleccionar nodo
js_select_node <- htmlwidgets::JS("
  function(properties) {

    var nodeId = properties.nodes[0];

    var canvasPosition =
      this.canvasToDOM(
        this.getPositions([nodeId])[nodeId]
      );

    Shiny.setInputValue(
      'nodo_seleccionado_info',
      {
        id: nodeId,
        x: canvasPosition.x,
        y: canvasPosition.y
      },
      {
        priority: 'event'
      }
    );

  }
")

# Quitar popup al deseleccionar
js_deselect_node <- htmlwidgets::JS("function(properties) { $('#nodo-custom-popup').remove(); }")