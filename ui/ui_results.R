# ui/ui_results.R
ui_results <- function() {
  div(
    # PREPROCESADO: Modal para preprocesar datos, con inputs para tipo de datos,
    ui_preprocess(),

    tabset(
      tabs = list(
        list(
          menu_item = "Red Bayesiana",
          content = segment(
            # Representación interactiva de la red
            div(
              #class = "ui segment",
              #style = "display: flex; flex-direction: column; gap: 10px;",

              # Contenedor del grafo interactivo
              div(
                id = "graph-container",
                style = "height: 800px;",
                visNetworkOutput("graph", height = "100%")
              )
            )
          )
        ),
        list(
          menu_item = "Información del Modelo",
          content = segment(
            # Debug del modelo
            div(
              style = "display: flex; flex-direction: column; gap: 5px;",
              h4("Red bayesiana actual:"),
              verbatimTextOutput("bn_debug")
            ),

            # Gráfico estático BN
            div(
              style = "display: flex; flex-direction: column; gap: 5px;",
              h4("Gráfico BN:"),
              plotOutput("bn_plot")
            )
          )
        ),
        list(
          menu_item = "Evaluación del Modelo",
          content = segment(
            # Evaluación del modelo
            ui_evaluation()
          )
        )
      )
    ),

    # Modal para añadir nodo a la red interactuvamente
    ui_addNode(),

    verbatimTextOutput("inference_output"),
    verbatimTextOutput("selectedRows"),
    tableOutput("datosFiltrados")
  )
}
