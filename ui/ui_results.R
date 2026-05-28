# ui/ui_results.R
ui_results <- function() {
  div(
    # PREPROCESADO: Modal para preprocesar datos, con inputs para tipo de datos,
    ui_preprocess(),

    div(
      id = "tabset-azul",
      tabset(
        tabs = list(
          list(
            menu_item = "Red Bayesiana",
            content = segment(
              class = "ui segment",
              # Fijar altura al segmento (80% del alto de pantalla)
              style = "height: 100vh; display: flex; 
                      flex-direction: column; padding: 0; overflow: hidden;",

              # Contenedor del grafo (65%)
              div(
                id = "graph-container",
                style = "flex: 0 0 65%; position: relative; 
                        border-bottom: 1px solid #ddd;",
                visNetworkOutput("graph", height = "100%")
              ),

              # PASO 3: Bloque inferior (35%)
              div(
                style = "flex: 0 0 35%; overflow-y: auto; 
                        background-color: #f9f9f9; padding: 10px;",
                tags$b("Salida de Inferencia:"),
                uiOutput("output_inference")
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
          ),
          list(
            menu_item = "Datos",
            content = segment(
              mainPanel(
                DTOutput("tabla_datos")
              )
            )
          )
        )
      ),

      # Modal para añadir nodo a la red interactuvamente
      ui_addNode(),
    )
  )
}
