#ui/ui_addNode.R
ui_addNode <- function() {
# Modal para añadir nodo a la red interactuvamente
  modal(
    id = "modal_añadir_nodo",
    header = div(icon("magic"), "Añadir Nodo"),
    content = div(class = "ui  form",
      style = "display: flex; flex-direction: column; gap: 10px;",
      dropdown_input("new_node_type",
        choices = c("cualitativo", "numerico")
      ),
      conditionalPanel(
        condition = "input.new_node_type == 'cualitativo'",
        div(style = "color: red;",
          "Recuerda asignar niveles a la nueva variable 
          categórica para que bnlearn funcione correctamente"
        ),
        textInput("new_node_levels", "Niveles (separados por comas)", placeholder = "e.g. bajo,medio,alto"),
      ),
      textInput("new_node_name", "Nombre del nuevo nodo")
    ),
    footer = div(
      class = "actions",
      action_button("btn_añadir_nodo", "Continuar", class = "ui blue button")
    )
  )
}