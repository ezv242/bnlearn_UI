# JS: Funciones de manipulación para Shiny
js_add_node    <- htmlwidgets::JS("function(d, cb) { Shiny.setInputValue('graph_change', {type: 'addNode', data: d}, {priority: 'event'}); cb(null); }")
js_add_edge    <- htmlwidgets::JS("function(d, cb) { Shiny.setInputValue('graph_change', {type: 'addEdge', data: d}, {priority: 'event'}); cb(d); }")
js_delete_node <- htmlwidgets::JS("function(d, cb) { Shiny.setInputValue('graph_change', {type: 'deleteNode', data: d}, {priority: 'event'}); cb(d); }")
js_delete_edge <- htmlwidgets::JS("function(d, cb) { Shiny.setInputValue('graph_change', {type: 'deleteEdge', data: d}, {priority: 'event'}); cb(d); }")

# JS: Traducción al Español + Fix del botón Edit
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

# JS: Comportamiento del Popup Personalizado al seleccionar nodo
js_select_node <- htmlwidgets::JS("function(properties) {
  var nodeId = properties.nodes[0];
  var canvasPosition = this.canvasToDOM(this.getPositions([nodeId])[nodeId]);
  var graphContainer = $(this.container);
  
  $('#nodo-custom-popup').remove();
  
  var popupHtml = `
    <div id='nodo-custom-popup' class='ui fluid card' style='
      position: absolute; z-index: 999; width: 300px; box-shadow: 0px 4px 10px rgba(0,0,0,0.15);
      left: ${canvasPosition.x + 15}px; top: ${canvasPosition.y - 40}px;
    '>
      <div class='content' style='padding: 10px;'>
        <div class='header' style='font-size: 1.1em;'>Nodo: ${nodeId}</div>
        <div class='description' style='font-size: 0.9em; margin-top: 5px; color: rgba(0,0,0,0.6);'>
          Coordenadas:<br>X: ${Math.round(canvasPosition.x)} | Y: ${Math.round(canvasPosition.y)}
        </div>
      </div>
    </div>
  `;
  
  graphContainer.append(popupHtml);
  Shiny.setInputValue('nodo_seleccionado_info', {id: nodeId, x: canvasPosition.x, y: canvasPosition.y}, {priority: 'event'});
}")

# JS: Quitar popup al deseleccionar
js_deselect_node <- htmlwidgets::JS("function(properties) { $('#nodo-custom-popup').remove(); }")