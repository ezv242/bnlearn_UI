Shiny.addCustomMessageHandler(
  'mostrar_mini_popup',
  function(data) {

    // Buscar el contenedor del gráfico correctamente
    const graphContainer = $('#graph-container');
    
    // Validar que el contenedor existe
    if (graphContainer.length === 0) {
      console.error('Contenedor #graph-container no encontrado');
      return;
    }

    // Remover popup anterior si existe
    $('#nodo-custom-popup').remove();

    const popup = `
      <div
        id="nodo-custom-popup"
        class="node-popup ui fluid card"
        style="
          position: absolute;
          left:${data.x + 15}px;
          top:${data.y - 40}px;
        "
      >
        <div class="content">

          <div class="header node-popup-header">
            Nodo: ${data.id}
          </div>

          <div class="description node-popup-description">
            ${data.texto}
          </div>

        </div>
      </div>
    `;

    graphContainer.append(popup);
  }
);