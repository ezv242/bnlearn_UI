# ui/ui_results.R
ui_results <- function() {
  tagList(
    modal(
      id = "modal_preprocesado",
      header = div(icon("magic"), "Preprocesado de Datos"),
      content = div(class = "ui  form",
        style = "display: flex; flex-direction: column; gap: 10px;",
        "Tipo de datos:",
        div(style = "color: red;",
          "El tipo de datos debe ser correctamente 
          identificado para que bnlearn funcione correctamente"
        ),
        dropdown_input("tipo_datos",
          choices = c("discretos", "continuos", "mixtos"),
        ),
        checkboxInput("to_factor", "Convertir a factor", value = FALSE),
        #checkboxInput("NA_predict", "Predecir valores NA", value = FALSE),
        #conditionalPanel(
        #  condition = "input.NA_predict == true",
        #  textInput("struct_network", "Definir la estructura de la red",
        #    placeholder = "[A][T|A][S][L|S][B|S][E|L:T][X|E][D|B:E]"
        #  )
        #),
        checkboxInput("discretizacion", "Discretizar datos", value = FALSE),
        conditionalPanel(
          condition = "input.discretizacion == true",
          dropdown_input("discretization_method",
            choices = c("interval", "quantile", "hartemink"),
            value = "quantile"
          ),
          numericInput("breaks", "Número de intervalos para discretización", value = 3, min = 2),
          conditionalPanel(

            condition = "input.discretization_method == 'hartemink'",
            "idisc",
            dropdown_input("input_idisc",
              choices = c("interval", "quantile"),
              value = "interval"
            ),
          ),
          numericInput("ibreaks", "ibreaks", value = 15, min = 10),
          checkboxInput("ordered", "Ordenar variables discretizadas", value = FALSE),
        )

      ),
      footer = div(
      class = "actions",
      action_button("btn_preprocesar", "Continuar", class = "ui blue button")
      )
    ),
    #verbatimTextOutput("network"),
    #titlePanel("Manipulación interactiva de red"),
    sidebarLayout(
      sidebarPanel(
        textInput("new_node", "Nuevo nodo"),
        #actionButton("add_node", "Agregar nodo"),
        #actionButton("remove_node", "Eliminar último nodo")
      ),
      mainPanel(
        #visNetworkOutput("graph")
        div(id = "graph-container", style = "height: 800px;", visNetworkOutput("graph", height = "50%")),
        h4("Red bayesiana actual:"),
        verbatimTextOutput("bn_debug"),
        h4("Gráfico BN:"),
        plotOutput("bn_plot")
      )
    ),
    verbatimTextOutput("inference_output"),
    verbatimTextOutput("selectedRows"),
    tableOutput("datosFiltrados")
  )
}
