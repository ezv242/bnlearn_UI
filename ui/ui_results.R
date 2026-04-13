# ui/ui_results.R
ui_results <- function() {
  tagList(
    #verbatimTextOutput("network"),
    #plotOutput("bn_plot"),
    #titlePanel("Manipulación interactiva de red"),
    sidebarLayout(
      sidebarPanel(
        textInput("new_node", "Nuevo nodo"),
        #actionButton("add_node", "Agregar nodo"),
        #actionButton("remove_node", "Eliminar último nodo")
      ),
      mainPanel(
        #visNetworkOutput("graph")
        div(style = "height: 800px;", visNetworkOutput("graph", height = "50%")),
        h4("Red bayesiana actual:"),
        verbatimTextOutput("bn_debug")   # <- Aquí se mostrará la red
      )
    ),
    verbatimTextOutput("inference_output"),
    verbatimTextOutput("selectedRows"),
    tableOutput("datosFiltrados")
  )
}
