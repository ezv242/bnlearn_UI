# ui/ui_sidebar.R
ui_sidebar <- function() {
  div(style = "display: flex; flex-direction: column; gap: 5px;",
    # CARGAR DATOS: El sidebar contiene inputs para cargar datos,
    # seleccionar algoritmos, ajustar parámetros y realizar inferencia
    div(class = "ui segment",
      #style = "display: flex; flex-direction: column; gap: 10px;",
      div(
        class = "ui small header",
        style = "margin-bottom: 15px;",
        div(
          class = "content",
          "Carga de la red"
        )
      ),
      fileInput("datafile", "Selecciona archivo CSV", accept = c(".csv", ".xlsx", "txt", "rds")),
      "Algoritmo de aprendizaje:",
      uiOutput("algorithm_selector"),
      div(
        style = "display: flex; gap: 10px; margin-top: 5px;",
        action_button("run_bnlearn", "Aprender Red"),
      )
    ),
    # AJUSTE PARÁMETROS: Parámetros de ajuste y de inferencia,
    # con visibilidad condicional según el tipo de datos
    div(class = "ui segment",
      #style = "display: flex; flex-direction: column; gap: 10px;",
      div(
        class = "ui small header",
        style = "margin-bottom: 15px;",
        div(
          class = "content",
          "Ajuste de parámetros"
        )
      ),
      "Método de ajuste de parámetros:",
      uiOutput("method_parameters_selector"),
      conditionalPanel(
        condition = "input.method_parametershm == 'bayes'",
        numericInput("iss", "Equivalent Sample Size (ISS)", value = 1, min = 0)
      ),
      conditionalPanel(
        condition = "input.method_parametershm == 'bayes-g'",
        "ISS:",
        dropdown_input("iss_bayes_g",
                       choices = c("iss.mu", "iss.w"),
                       value = "iss.mu")
      ),
      conditionalPanel(
        condition = "input.method_parametershm == 'mle' || 
                     input.method_parametershm == 'mle-g' || 
                     input.method_parametershm == 'mle-cg'",
        checkboxInput("replace_unidentifiable", 
                      "replace_unidentifiable", value = TRUE)
      ),
      checkboxInput("keep_fitted", "Keep_fitted", value = TRUE),
      checkboxInput("debug", "debug", value = FALSE),
      div(
        style = "display: flex; gap: 10px; margin-top: 5px;",
        action_button("fit_model", "Ajustar Modelo"),
      )
    ),

    # INFERENCIA: Inferencia, con inputs para evento, evidencia

    #ui_inference()
    div(
      class = "ui segment",

      div(
        class = "ui small header",
        style = "margin-bottom: 15px;",
        div(
          class = "content",
          "Inferencia"
        )
      ),
      ui_inference()
    )
  )
}