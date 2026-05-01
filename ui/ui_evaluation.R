#ui/ui_evaluation.R
ui_evaluation <- function() {
  div(class = "ui segment",
    style = "display: flex; flex-direction: column; gap: 10px;",
    "Score del modelo:",
    uiOutput("method_score_selector"),
    checkboxInput("by.node", "by.node", value = FALSE),
    conditionalPanel(
      condition = "input.method_score == 'bde' || 
                   input.method_score == 'mbde' || 
                   input.method_score == 'bds' ||
                   input.method_score == 'bdj'",
      numericInput("iss_score", "iss", min = 0, placeholder = "Introduce un número"),
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
      dropdown_input("prior_score",
                     choices = c("uniform", "vsp", "marginal", "cs")),
    ),
    fileInput("testfile", "Introducir datos para test", accept = ".csv"),
    #"Validación cruzada:",

    
  )
}