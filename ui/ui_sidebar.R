# ui/ui_sidebar.R
ui_sidebar <- function() {

  # CARGAR DATOS: El sidebar contiene inputs para cargar datos, 
  # seleccionar algoritmos, ajustar parámetros y realizar inferencia
  div(style = "display: flex; flex-direction: column; gap: 5px;",
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

      # INFERENCIA: Inferencia, con inputs para evento, evidencia, 
      # método y número de muestras
      div(class = "ui segment",
      style = "display: flex; flex-direction: column; gap: 10px;",

          textInput("event", "Event (e.g. X == 'yes' or list(X = 'yes'))"),
          textInput("evidence", "Evidence (e.g. E == 'yes' or list(E = 'yes'))"),

          #El lw funciona con redes grandes y continuas, 
          # el exact con redes pequeñas y discretas
            "Método de inferencia:",
            dropdown_input("method_inference", 
                          choices = c("lw", "exact")),
            conditionalPanel(
              condition = "input.method_inference == 'lw'",
              numericInput("n_samples", "Number of Samples", value = 1000, min = 1)
            ),
          div(
            style = "display: flex; gap: 10px;",
            action_button("run_inference", "Hacer Inferencia"),
          )
      )
  )
}