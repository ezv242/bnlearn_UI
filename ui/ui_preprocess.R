#ui/ui_preprocess.R
ui_preprocess <- function() {
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
        choices = c("cualitativos", "numericos", "mixtos"),
      ),
      div(
        style = "display: flex; gap: 20px;",
        checkboxInput("to_factor", "Convertir a factor", value = FALSE),
        checkboxInput("datos_NAs", "Contiene NAs", value = FALSE),
        checkboxInput("datos_continuo", "Datos solo continuos", value = FALSE),
        conditionalPanel(
          condition = "input.tipo_datos == 'numericos' || input.tipo_datos == 'mixtos'",
          checkboxInput("datos_discretos", "Datos solo discretos", value = FALSE)
        ),
        checkboxInput("graph_dirigido", "Grafo dirigido", value = FALSE),
        checkboxInput("discretizacion", "Discretizar datos", value = FALSE)
      ),
      conditionalPanel(
        condition = "input.discretizacion == true",
        "Metodo de discretización:",
        dropdown_input("discretization_method",
          choices = c("interval", "quantile", "hartemink"),
          value = "quantile"
        ),
        numericInput("breaks", "Número de intervalos usados para la discretización", value = 3, min = 2),
        conditionalPanel(
          condition = "input.discretization_method == 'hartemink'",
          "idisc",
          dropdown_input("input_idisc",
            choices = c("interval", "quantile"),
            value = "interval"
          ),
          numericInput("ibreaks", "ibreaks", value = 15, min = 10)
        ),
        checkboxInput("ordered", "Ordenar variables discretizadas", value = FALSE),
      )
    ),
    footer = div(
      class = "actions",
      action_button("btn_preprocesar", "Continuar", class = "ui blue button")
    )
  )
}