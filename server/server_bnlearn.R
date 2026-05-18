# Esta función permite leer los datos y construir una red bayesiana
# usando uno de los algoritmos
server_bnlearn <- function(input, output, session, shared_data) {

  # Reactive para leer los datos
  dataset <- reactive({
    #req(input$datafile)

    ext <- file_ext(input$datafile$datapath)

    # Caso 1: archivo subido
    if (!is.null(input$datafile)) {
      #df <- read.csv(input$datafile$datapath, stringsAsFactors = TRUE)
      df <- switch(ext,
        csv  = read.csv(input$datafile$datapath, stringsAsFactors = TRUE),
        txt  = read.table(input$datafile$datapath, header = TRUE, stringsAsFactors = TRUE),
        rds  = readRDS(input$datafile$datapath),
        xlsx = as.data.frame(readxl::read_excel(input$datafile$datapath)),
        stop("Formato de archivo no soportado")
      )
      show_modal("modal_preprocesado")
      return(df)
    }

    # Caso 2: dataset predefinido
    req(input$nombre_seleccionado)
    dataset <- input$nombre_seleccionado

    df <- switch(dataset,
      Asia            = { data("asia"); asia },
      Alarm           = { data("alarm"); alarm },
      clgaussian_test = { data("clgaussian.test"); clgaussian.test },
      Coronary        = { data("coronary"); coronary },
      gaussian_test   = { data("gaussian.test"); gaussian.test },
      Hailfinder      = { data("hailfinder"); hailfinder },
      Insurance       = { data("insurance"); insurance },
      learning_test   = { data("learning.test"); learning.test },
      Lizards         = { data("lizards"); lizards },
      Marks           = { data("marks"); marks }
    )
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
    data_continous <- input$datos_continuo
    graph_dirigido <- input$graph_dirigido
    dataset_NAs <- input$datos_NAs

    if (tipo_datos == "cualitativos") {
      shared_data$data_discrete <- TRUE
    }else{
      shared_data$data_discrete <- input$datos_discretos
    }

    shared_data$data_type <- tipo_datos
    shared_data$data_continuous <- data_continous
    shared_data$graph_dirigido <- graph_dirigido
    shared_data$dataset_NAs <- dataset_NAs

    # Convertir a factor si se selecciona esa opción
    if (input$to_factor) {
      data[] <- lapply(data, as.factor)
    }

    # Discretización si se seleccciona esa opción
    discretizacion <- input$discretizacion
    if (discretizacion) {

      #TEMPORAL: Para cambiar el tipo del dataset a discreto
      shared_data$data_discrete <- TRUE
      ###########################################################

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

      tryCatch({
        data <- do.call(bnlearn::discretize, args)
      }, error = function(e) {
        showNotification(paste("Error en la discretización:", e$message), type = "error")
        return(NULL)
      })
    }

    # Guardar el dataset preprocesado en shared_data
    shared_data$dataset <- data

    # Cerrar el modal automáticamente al terminar
    shiny.semantic::hide_modal(id = "modal_preprocesado", session = shiny::getDefaultReactiveDomain(), asis = TRUE)

    # Enviar una notificación de éxito (estilo Semantic UI)
    toast("¡Datos procesados con éxito!", class = "success")
  })

  # Crear una lista desplegable dinamica de algoritmos disponibles
  available_algorithms <- reactive({
    all_algorithms <- c(
      "hc", "tabu", "mmhc", "rsmax2", "h2pc",
      "direct.lingam",
      "pc.stable", "gs", "iamb", "fast.iamb",
      "inter.iamb", "iamb.fdr", "mmpc", "si.hiton.pc", "hpc"
    )

    # Si no hay preprocesado, mostrar todas las opciones
    if (is.null(shared_data$data_type) &&
        is.null(shared_data$data_continuous) &&
        is.null(shared_data$graph_dirigido) &&
        is.null(shared_data$dataset_NAs)) {
      return(all_algorithms)
    }

    graph_dirigido <- isTRUE(shared_data$graph_dirigido)
    data_continuous <- isTRUE(shared_data$data_continuous)
    dataset_NAs <- isTRUE(shared_data$dataset_NAs)
    data_type <- shared_data$data_type

    # direct.lingam solo si los datos son continuos, sin NAs y el tipo es continuos
    valid_direct_lingam <- data_continuous && !dataset_NAs && identical(data_type, "numericos")

    if (!graph_dirigido && !valid_direct_lingam) {
      c("hc", "tabu", "mmhc", "rsmax2", "h2pc")
    } else if (!graph_dirigido && valid_direct_lingam) {
      c("hc", "tabu", "mmhc", "rsmax2", "h2pc", "direct.lingam")
    } else if (graph_dirigido && !valid_direct_lingam) {
      c("hc", "tabu", "mmhc", "rsmax2", "h2pc",
        "pc.stable", "gs", "iamb", "fast.iamb",
        "inter.iamb", "iamb.fdr", "mmpc", "si.hiton.pc", "hpc")
    } else {
      c("hc", "tabu", "mmhc", "rsmax2", "h2pc", "direct.lingam",
        "pc.stable", "gs", "iamb", "fast.iamb",
        "inter.iamb", "iamb.fdr", "mmpc", "si.hiton.pc", "hpc")
    }
  })

  # Mostrar por pantalla la lista desplegable dinamica de algoritmos disponibles
  output$algorithm_selector <- renderUI({
    choices <- available_algorithms()
    selected <- if (!is.null(input$algorithm) && input$algorithm %in% choices) {
      input$algorithm
    } else {
      choices[1]
    }
    dropdown_input("algorithm", choices = choices, value = selected)
  })

  # Reactive para construir la red al hacer clic en el botón
  bn_model <- eventReactive(input$run_bnlearn, {
    data <- dataset()
    alg <- input$algorithm

    # Colectar valores lógicos de shared_data de forma segura
    graph_dirigido <- isTRUE(shared_data$graph_dirigido)
    data_continuous <- isTRUE(shared_data$data_continuous)
    dataset_NAs <- isTRUE(shared_data$dataset_NAs)
    data_type <- shared_data$data_type

    # No se cumplen las condicones de ser numerico, continuo y sin NAs
    condicion_datos <- !data_continuous ||
      dataset_NAs || !identical(data_type, "numericos")

    if (!graph_dirigido && condicion_datos) {
      bn <- switch(alg,
        #Algoritmos de aprendizaje de estructura basados en puntuación
        "hc" = bnlearn::hc(data),
        "tabu" = bnlearn::tabu(data),
        #Algoritmo de aprendizaje de estructua hibrido
        "mmhc" = bnlearn::mmhc(data),
        "rsmax2" = bnlearn::rsmax2(data),
        "h2pc" = bnlearn::h2pc(data)
      )
    }else if (!shared_data$graph_dirigido && !condicion_datos) {
      bn <- switch(alg,
        #Algoritmos de aprendizaje de estructura basados en puntuación
        "hc" = bnlearn::hc(data),
        "tabu" = bnlearn::tabu(data),
        #Algoritmo de aprendizaje de estructua hibrido
        "mmhc" = bnlearn::mmhc(data),
        "rsmax2" = bnlearn::rsmax2(data),
        "h2pc" = bnlearn::h2pc(data),
        #Algoritmo de aprendizaje de estructura basado en causalidad (Solo funciona con datos numericos continuos y sin NAs)
        "direct.lingam" = bnlearn::direct.lingam(data)
      )
    }else if (shared_data$graph_dirigido && condicion_datos) {
      bn <- switch(alg,
        #Algoritmos de aprendizaje de estructura basados en puntuación
        "hc" = bnlearn::hc(data),
        "tabu" = bnlearn::tabu(data),
        #Algoritmo de aprendizaje de estructua hibrido
        "mmhc" = bnlearn::mmhc(data),
        "rsmax2" = bnlearn::rsmax2(data),
        "h2pc" = bnlearn::h2pc(data),
        #Metodos de aprendizaje de estructura basados en test de independencia (solo para grafos dirigidos acíclicos)
        "pc.stable" = bnlearn::pc.stable(data),
        "gs" = bnlearn::gs(data),
        "iamb" = bnlearn::iamb(data),
        "fast.iamb" = bnlearn::fast.iamb(data),
        "inter.iamb" = bnlearn::inter.iamb(data),
        "iamb.fdr" = bnlearn::iamb.fdr(data),
        "mmpc" = bnlearn::mmpc(data),
        "si.hiton.pc" = bnlearn::si.hiton.pc(data),
        "hpc" = bnlearn::hpc(data)
      )
    }else{
      #Se ejecuta el algorimto de aprendizaje de la estructura de la red
      bn <- switch(alg,
        #Algoritmos de aprendizaje de estructura basados en puntuación
        "hc" = bnlearn::hc(data),
        "tabu" = bnlearn::tabu(data),
        #Algoritmo de aprendizaje de estructua hibrido
        "mmhc" = bnlearn::mmhc(data),
        "rsmax2" = bnlearn::rsmax2(data),
        "h2pc" = bnlearn::h2pc(data),
          #Algoritmo de aprendizaje de estructura basado en causalidad (Solo funciona con datos numericos continuos y sin NAs)
        "direct.lingam" = bnlearn::direct.lingam(data),
        #Metodos de aprendizaje de estructura basados en test de independencia (solo para grafos dirigidos acíclicos)
        "pc.stable" = bnlearn::pc.stable(data),
        "gs" = bnlearn::gs(data),
        "iamb" = bnlearn::iamb(data),
        "fast.iamb" = bnlearn::fast.iamb(data),
        "inter.iamb" = bnlearn::inter.iamb(data),
        "iamb.fdr" = bnlearn::iamb.fdr(data),
        "mmpc" = bnlearn::mmpc(data),
        "si.hiton.pc" = bnlearn::si.hiton.pc(data),
        "hpc" = bnlearn::hpc(data)
      )
    }

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
