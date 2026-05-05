server_evaluation <- function(input, output, session, shared_data) {

# Reactive para leer los datos de test
test_dataset <- reactive({
  req(input$testfile)
  df <- read.csv(input$testfile$datapath, stringsAsFactors = TRUE)
  #show_modal("modal_preprocesado")
  df
})

# Guardar dataset de test en shared_data
observe({
  shared_data$test_dataset <- test_dataset()
})

# Lista dinámica de métodos score del modelo
available_method_score <- reactive({
  all_methods <- c("loglik", "aic", "bic", "ebic", 
                   "pred-loglik", "bde", "bds", "mbde", "k2",
                   "fnml", "qnml", "nal", "pnal", "loglik-g", "aic-g",
                   "bic-g", "ebic-g", "pred-loglik-g", "bge", "nal-g",
                   "loglik-cg", "aic-cg", "bic-cg", "ebic-cg", "pred-loglik-cg", "nal-cg")

  # Si no hay preprocesado, mostrar todas las opciones
  if (is.null(shared_data$data_type)) {
    return(all_methods)
  }

  data_type <- shared_data$data_type
  data_continuous <- isTRUE(shared_data$data_continuous)
  data_discrete <- isTRUE(shared_data$data_discrete)

  if (data_type == "cualitativos" || data_discrete) {
    c("loglik", "aic", "bic", "ebic", "pred-loglik", "bde", "bds", "mbde", "k2",
      "fnml", "qnml", "nal", "pnal")
  } else if (data_continuous) {
    c("loglik-g", "aic-g", "bic-g", "ebic-g", "pred-loglik-g", "bge", "nal-g")
  } else if (!data_continuous && !data_discrete) {
    c("loglik-cg", "aic-cg", "bic-cg", "ebic-cg", "pred-loglik-cg", "nal-cg")
  } else {
    all_methods
  }
})

# Mostrar por pantalla la lista desplegable dinamica de metodos score disponibles
output$method_score_selector <- renderUI({
  choices <- available_method_score()
  selected <- if (!is.null(input$method_score) && input$method_score %in% choices) {
    input$method_score
  } else {
    choices[1]
  }
  dropdown_input("method_score", choices = choices, value = selected)
})

##################################################################################
# Reactive para ejecutar el score del modelo
##################################################################################
score_result <- eventReactive(input$run_score, {
  if (is.null(shared_data$network)) {
    return("Error: Aun no se ha aprendido una red bayesiana 
           para evaluar  el modelo con un score  significativo")
  }
  if (is.null(shared_data$dataset)) {
    return("Error: no hay datos disponibles para evaluar el modelo")
  }

  method_score <- input$method_score
  by_node <- input$by.node
  iss <- if (!is.null(input$iss_score)) input$iss_score else NULL
  iss.mu <- if (!is.null(input$iss.mu_score)) input$iss.mu_score else NULL
  iss.w <- if (!is.null(input$iss.w_score)) input$iss.w_score else NULL
  nu <- if (!is.null(input$nu_score)) input$nu_score else NULL
  k <- if (!is.null(input$k_score)) input$k_score else NULL
  prior <- if (!is.null(input$prior_score)) input$prior_score else NULL

  args <- list(
    x = shared_data$network,
    data = shared_data$dataset,
    type = method_score,
    by.node = by_node
  )

  if (method_score %in% c("bde", "mbde", "bds", "bdj")) {
    args$iss <- iss
  }

  if (method_score == "bge") {
    args$iss.mu <- iss.mu
    args$iss.w <- iss.w
    args$nu <- nu
  }

  if (method_score %in% c("pnal", "pnal-g", "pnal-cg", "aic", "bic")) {
    args$k <- k
  }

  if (method_score %in% c("bde", "mbde", "bds", "bdj", "bge")) {
    args$prior <- prior
  }

  if(!is.null(shared_data$test_dataset) && 
    (method_score %in% c("pred-loglik", "pred-loglik-g", "pred-loglik-cg"))) {
    # Utilizar el dataset de test para la evaluación
    args$newdata <- shared_data$test_dataset
  }

  result <- tryCatch({
    do.call(bnlearn::score, args)
  }, error = function(e) {
    paste("Error al calcular el score:", e$message)
  })

  result
})

# Renderizar resultados
output$score_output <- renderText({
  res <- score_result()
  if (is.numeric(res)) {
    paste("Resultado del score:", round(res, 3))
  } else {
    res
  }
})

##################################################################################
# Reactive para ejecutar validación cruzada
##################################################################################
result_cv <- eventReactive(input$run_cv, {
  if (is.null(shared_data$network)) {
    output$cv_output <- renderText("Error: Aun no se ha aprendido una red bayesiana para evaluar el modelo con validación cruzada")
    return()
  }
  if (is.null(shared_data$dataset)) {
    output$cv_output <- renderText("Error: no hay datos disponibles para evaluar el modelo con validación cruzada")
    return()
  }

  estrategia_cv <- input$cv_strategy
  k <- if (!is.null(input$k_cv)) input$k_cv else NULL
  m <- if (!is.null(input$m_cv)) input$m_cv else NULL
  runs <- if (!is.null(input$runs_cv)) input$runs_cv else NULL
  loss_functions <- input$loss_functions
  predict_method_cv <- input$predict_method_cv
  nodo_objetivo <- input$target_cv


  args <- list(
    data = shared_data$dataset,
    bn = shared_data$network,
    k = k,
    runs = runs,
    loss = loss_functions,
    loss.args = list(target = nodo_objetivo)
  )

  if (estrategia_cv == "hold-out") {
    args$m <- m
  }

  if (!is.null(predict_method_cv)){
    args$loss.args$predict <- predict_method_cv
  }

  result <- tryCatch({
    do.call(bnlearn::bn.cv, args)
  }, error = function(e) {
    paste("Error al ejecutar la validación cruzada:", e$message)
  })

  result
})

# Renderizado de los resultados de validación cruzada
output$cv_output <- renderPrint({
  result_cv()
})

# Actualizar selectizeInput dinámicamente
observe({
  updateSelectizeInput(
    session,
    "target_cv",
    choices = names(shared_data$dataset),
    server = TRUE  # recomendable para datasets grandes
  )
})

}