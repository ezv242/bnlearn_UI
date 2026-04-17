# server/server_bnlearn.R
# Esta función permite leer los datos y construir una red bayesiana
# usando uno de los algoritmos
server_bnlearn <- function(input, output, session, shared_data) {

  # Reactive para leer los datos
  dataset <- reactive({
    req(input$datafile)
    df <- read.csv(input$datafile$datapath, stringsAsFactors = TRUE)
    show_modal("modal_preprocesado")
    df
  })

  # Guardar dataset en shared_data
  observe({
    shared_data$dataset <- dataset()
  })

  #Reactive para hacer el preprocesamiento de los datos
  observeEvent(input$btn_preprocesar, {

    data <- dataset()
    # Se asigna el tipo del dataset
    tipo_datos <- input$tipo_datos
    shared_data$data_type <- tipo_datos

    # Convertir a facotor si se selecciona esa opción
    if (input$to_factor) {
      data[] <- lapply(data, as.factor)
    }

    # Predecir valores NA si se selecciona esa opción
    #NA_predict <- input$NA_predict
    #if (NA_predict == TRUE) {
    #  fitted <- bnlearn::bn.fit(model2network(NA_predict), dataset_completo())
    #  bnlearn::impute(fitted, with_missing_data)
    #}

    # Discretización si se seleccciona esa opción
    discretizacion <- input$discretizacion
    if (discretizacion) {
      discretization_method <- input$discretization_method
      breaks <- input$breaks
      ordered <- input$ordered

      # Construir lista de argumentos base
      args <- list(
        data = data,
        method = discretization_method,
        ordered = ordered,
        breaks = breaks
      )

      if (discretization_method == "hartemink") {
        idisc <- input$input_idisc
        ibreaks <- input$ibreaks

        args$ibreaks <- ibreaks
        args$idisc <- idisc
      }

      # Ejecutar discretización con argumentos dinámicos
      data <- do.call(bnlearn::discretize, args)
    }

    # Guardar el dataset preprocesado en shared_data
    shared_data$dataset <- data

    # Cerrar el modal automáticamente al terminar
    shiny.semantic::hide_modal(id = "modal_preprocesado", session = shiny::getDefaultReactiveDomain(), asis = TRUE)

    # Enviar una notificación de éxito (estilo Semantic UI)
    toast("¡Datos procesados con éxito!", class = "success")
  })

  # Reactive para construir la red al hacer clic en el botón
  bn_model <- eventReactive(input$run_bnlearn, {
    data <- dataset()
    alg <- input$algorithm
    bn <- switch(alg,
                 "hc" = hc(data),
                 "tabu" = tabu(data),
                 "gs" = gs(data),
                 "mmhc" = mmhc(data),
                 "iamb" = iamb(data))
    bn
  })

  # Guardar el modelo en shared_data
  observeEvent(bn_model(), {
    shared_data$network <- bn_model()
  })

  # Output de la red, gráfico del bn_model
  #output$bn_plot <- renderPlot({
  #  req(bn_model())
  #  graphviz.plot(bn_model())
  #})

  #output$network <- renderPrint({
  #  bn_model()
  #})
}
