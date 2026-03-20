# ui/ui_sidebar.R
ui_sidebar <- function() {
  div(style = "display: flex; flex-direction: column; gap: 5px;",
    # CARGAR DATOS: El sidebar contiene inputs para cargar datos,
    # seleccionar algoritmos, ajustar parámetros y realizar inferencia
    div(class = "ui segment",
      style = "display: flex; flex-direction: column; gap: 10px;",
      fileInput("datafile", "Selecciona archivo CSV", accept = ".csv"),
      "Algoritmo de aprendizaje:",
      dropdown_input("algorithm",
                     choices = c("hc", "tabu", "gs", "iamb", "mmhc"),
                     value = "hc"),
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
      dropdown_input("method",
                     choices = c("mle", "bayes", "bayes-g"),
                     value = "mle"),
      conditionalPanel(
        condition = "input.method == 'bayes'",
        numericInput("iss", "Equivalent Sample Size (ISS)", value = 1, min = 0)
      ),
      conditionalPanel(
        condition = "input.method == 'bayes-g'",
        "ISS:",
        dropdown_input("iss_bayes_g",
                       choices = c("iss.mu", "iss.w"),
                       value = "iss.mu")
      ),
      conditionalPanel(
        condition = "output.data_type_hidden == 'mixed'",
        checkboxInput("replace_unidentifiable", 
                      "replace_unidentifiable", value = TRUE)
      ),
      checkboxInput("keep_fitted", "Keep_fitted", value = TRUE),
      checkboxInput("debug", "debug", value = FALSE),
    ),

    # INFERENCIA: Inferencia, con inputs para evento, evidencia
    div(class = "ui segment",
      style = "display: flex; flex-direction: column; gap: 10px;",
      #El lw funciona con redes grandes y continuas,
      # el exact con redes pequeñas y discretas
      "Método de inferencia:",
      dropdown_input("method_inference",
                     choices = c("lw", "ls"),
                     value = "ls"),
      textInput("event", "Event (e.g. X == 'yes')"),
      conditionalPanel(
        condition = "input.method_inference == 'lw'",
        textInput("evidencia_lw", "Evidencia"),
      ),
      conditionalPanel(
        condition = "input.method_inference == 'ls'",
        textInput("evidence", "Evidence (e.g. E == 'yes')")
      ),
      div(
        style = "display: flex; gap: 10px;",
        action_button("run_inference", "Hacer Inferencia"),
      )
    ),
    # CPDIST: Inferencia de distribución condicional, con inputs para evento, evidencia
    div(class = "ui segment",
      style = "display: flex; flex-direction: column; gap: 10px;",
      "Tabla de muestras cpdist:",
      dropdown_input("method_inference_cpdist",
                     choices = c("lw", "ls"),
                     value = "ls"),
      selectizeInput(
        "tags",
        "Selecciona nodos a simular:",
        choices = NULL,
        multiple = TRUE,
        options = list(create = TRUE)
      ),
      #textInput("event_cpdist", "Event (e.g. X == 'yes')"),
      conditionalPanel(
        condition = "input.method_inference_cpdist == 'lw'",
        textInput("evidence_cpdist", "Evidencia"),
      ),
      conditionalPanel(
        condition = "input.method_inference_cpdist == 'ls'",
        textInput("evidence_cpdist_lw", "Evidence (e.g. E == 'yes')")
      ),
      div(
        style = "display: flex; gap: 10px;",
        action_button("run_cpdist", "Hacer CPDIST"),
      )
    )
  )
}