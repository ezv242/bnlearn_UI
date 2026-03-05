# ui/ui_sidebar.R
ui_sidebar <- function() {
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
            checkboxInput("replace_undentifiable", 
                          "replace_undentifiable", value = TRUE)
          ),
          checkboxInput("keep_fitted", "Keep_fitted", value = TRUE),
          checkboxInput("debug", "debug", value = FALSE),
      ),

      div(class = "ui segment",
      style = "display: flex; flex-direction: column; gap: 10px;",

          textInput("event", "Event"),
          textInput("evidence", "Evidence"),
          #El lw funciona con redes grandes y continuas, el exact con redes pequeñas y discretas
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