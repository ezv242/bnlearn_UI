# ui/ui_sidebar.R
ui_sidebar <- function() {
  div(style = "display: flex; flex-direction: column; gap: 5px;",
    # CARGAR DATOS: El sidebar contiene inputs para cargar datos,
    # seleccionar algoritmos, ajustar parámetros y realizar inferencia
    div(class = "ui segment",
      style = "display: flex; flex-direction: column; gap: 10px;",
      fileInput("datafile", "Selecciona archivo CSV", accept = c(".csv", ".xlsx", "txt", "rds")),
      "Algoritmo de aprendizaje:",
      uiOutput("algorithm_selector"),
      div(
        style = "display: flex; gap: 10px;",
        action_button("run_bnlearn", "Aprender Red"),
      )
    ),
    # AJUSTE PARÁMETROS: Parámetros de ajuste y de inferencia,
    # con visibilidad condicional según el tipo de datos
    div(class = "ui segment",
      style = "display: flex; flex-direction: column; gap: 10px;",
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
        style = "display: flex; gap: 10px;",
        action_button("fit_model", "Ajustar Modelo"),
      )
    ),

    # INFERENCIA: Inferencia, con inputs para evento, evidencia
    tabset(
      tabs = list(
        # ---------------------
        # TAB CPQUERY
        # ---------------------
        list(
          menu_item = "cpquery",
          content = segment(
            div(
              style = "display: flex; flex-direction: column; gap: 10px;",
              #El lw funciona con redes grandes y continuas,
              # el exact con redes pequeñas y discretas
              "Método de inferencia:",
              dropdown_input("method_inference_cpquery",
                            choices = c("lw", "ls"),
                            value = "ls"),
              checkboxInput("filas_cpquery", "Usar filas como evidencia", 
                            value = FALSE),
              numericInput("n_cpquery", "Número de muestras aleatorias", 
                           value = 1000, min = 1000),
              numericInput("batch_cpquery",
                           "Muestras aleatorias generadas de golpe", 
                           value = 1000, min = 1000),
              textInput("event", "Event (e.g. X == 'yes')"),
              conditionalPanel(
                condition = "input.filas_cpquery == true",
                textInput("evidencia_lw", "Evidencia"),
              ),
              conditionalPanel(
                condition = "input.filas_cpquery == false",
                textInput("evidence", "Evidence (e.g. E == 'yes')")
              ),
              div(
                style = "display: flex; gap: 10px;",
                action_button("run_inference", "Hacer Inferencia"),
              )
            )
          )
        ),

        # ---------------------
        # TAB CPDIST
        # ---------------------
        list(
          menu_item = "cpdist",
          content = segment(
            div(
              style = "display: flex; flex-direction: column; gap: 10px;",
              "Tabla de muestras cpdist:",
              dropdown_input("method_inference_cpdist",
                            choices = c("lw", "ls"),
                            value = "ls"),
              checkboxInput("filas_cpdist", "Usar filas como evidencia",
                            value = FALSE),
              numericInput("n_cpdist", "Número de muestras aleatorias",
                           value = 1000, min = 1000),
              numericInput("batch_cpdist",
                           "Muestras aleatorias generadas de golpe",
                           value = 1000, min = 1000),
              selectizeInput(
                "tags",
                "Selecciona nodos a simular:",
                choices = NULL,
                multiple = TRUE,
                options = list(create = TRUE),
                width = "100%"
              ),
              conditionalPanel(
                condition = "input.filas_cpdist == true",
                textInput("evidence_cpdist_lw", "Evidencia"),
              ),
              conditionalPanel(
                condition = "input.filas_cpdist == false",
                textInput("evidence_cpdist", "Evidence (e.g. E == 'yes')")
              ),
              div(
                style = "display: flex; gap: 10px;",
                action_button("run_cpdist", "Hacer CPDIST"),              )
            )
          )
        )
      )
    )
  )
}