#ui/ui_evaluation.R
ui_evaluation <- function() {

  div(
    # Score del modelo dentro de un Acordeón Desplegable
    div(class = "ui accordion",
      id = "acordeon_score",

      # 1. El título (Gatillo para expandir/colapsar)
      div(class = "title ui dividing header", 
        style = "margin: 0; cursor: pointer;",
        icon("dropdown"), 
        "Evaluación Score"
      ),

      # 2. El contenido protegido que se ocultará/mostrará
      div(class = "content",
        div(style = "display: flex; flex-direction: column; gap: 10px; padding: 10px 5px;",

          "Metodo score del modelo:",
          uiOutput("method_score_selector"),

          checkboxInput("by.node", "by.node", value = FALSE),

          conditionalPanel(
            condition = "input.method_score == 'bde' || 
                        input.method_score == 'mbde' || 
                        input.method_score == 'bds' ||
                        input.method_score == 'bdj'",
            numericInput("iss_score", "iss", min = 0, placeholder = "Introduce un número")
          ),

          conditionalPanel(
            condition = "input.method_score == 'bge'",
            numericInput("iss.mu_score", "iss.mu", min = 0, placeholder = "Introduce un número"),
            numericInput("iss.w_score", "iss.w", min = 0, placeholder = "Introduce un número"),
            numericInput("nu_score", "nu", min = 0, placeholder = "Introduce un número")
          ),

          conditionalPanel(
            condition = "input.method_score == 'pnal' || 
                        input.method_score == 'pnal-g' || 
                        input.method_score == 'pnal-cg' ||
                        input.method_score == 'aic' ||
                        input.method_score == 'bic'",
            numericInput("k_score", "Coeficiente de penalización k", min = 0, placeholder = "Introduce un número")
          ),

          conditionalPanel(
            condition = "input.method_score == 'bde' || 
                        input.method_score == 'mbde' || 
                        input.method_score == 'bds' ||
                        input.method_score == 'bdj' ||
                        input.method_score == 'bge'",
            "Seleccionar prior para el score:",
            dropdown_input("prior_score", choices = c("uniform", "vsp", "marginal", "cs"))
          ),

          fileInput("testfile", "Introducir datos para test", accept = ".csv"),

          div(
            style = "display: flex; gap: 10px; margin-top: 5px;",
            action_button("run_score", "Calcular score", class = "ui blue button")
          ),

          div(
            style = "margin-top: 15px; overflow-y: auto; background-color: #f9f9f9; padding: 10px; border-radius: 4px;",
            tags$b("Salida de Score:"),
            verbatimTextOutput("score_output")
          )
        )
      )
    ),

    # Validación cruzada dentro de un Acordeón Desplegable
    # Acordeón usando HTML directo de Semantic UI
    div(class = "ui accordion",
      id = "acordeon_cv",

      # 1. El título
      div(class = "title ui dividing header", 
        style = "margin: 0; cursor: pointer;",
        icon("dropdown"), 
        "Validación Cruzada"
      ),

      # 2. Contenido de la validación cruzada
      div(class = "content",
        div(style = "display: flex; flex-direction: column; gap: 10px; padding: 10px 5px;",

          "Seleccionar estrategia de validación cruzada:",
          dropdown_input("cv_strategy", choices = c("k-fold", "hold-out")),

          conditionalPanel(
            condition = "input.cv_strategy == 'k-fold'",
            numericInput("k_cv_fold", "K subgrupos del dataset", min = 2, value = 2)
          ),

          conditionalPanel(
            condition = "input.cv_strategy == 'hold-out'",
            numericInput("k_cv_hold", "Dividir los datos k veces", min = 0, value = 1),
            numericInput("m_cv", "Tamaño m del conjunto de test para validación", min = 1, placeholder = "Introduce un número")
          ),

          numericInput("runs_cv", "Número de veces para ejecutar la validación, runs", min = 1, value = 1),

          "Seleccionar función de pérdida, loss:",
          dropdown_input("loss_functions", choices = c("logl", "pred", "pred-exact", "cor", "mse", "f1", "auroc")),

          "Seleccionar método de predicción para validación cruzada:",
          dropdown_input("predict_method_cv", choices = c("parents", "bayes-lw", "exact")),

          selectizeInput(
            "target_cv",
            "Selecciona nodo a inferir:",
            choices = NULL,
            multiple = FALSE,
            options = list(create = TRUE)
          ),

          div(
            style = "display: flex; gap: 10px; margin-top: 5px;",
            action_button("run_cv", "Ejecutar validación cruzada", class = "ui blue button")
          ),

          div(
            style = "margin-top: 15px; overflow-y: auto; background-color: #f9f9f9; padding: 10px; border-radius: 4px;",
            tags$b("Salida de Cross Validation:"),
            verbatimTextOutput("cv_output")
          )
        )
      ),
      tags$script(HTML("$('.ui.accordion').accordion();"))
    )
  )
}