# server/server_bnlearn.R
# Esta función permite leer los datos y construir una red bayesiana
# usando uno de los algoritmos
server_bnlearn <- function(input, output, session) {

  # Reactive para leer los datos
  dataset <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath, stringsAsFactors = TRUE)
  })
  
  # Guardar dataset en session para compartirlo con server_parameters
  observe({
    session$userData$dataset <- dataset
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

  # Se llama server_parameters (el dataset ya está en session$userData)
  server_parameters(input, output, session)

  # Output de la red, gráfico del bn_model
  output$bn_plot <- renderPlot({
    req(bn_model())
    graphviz.plot(bn_model())
  })

  # Guardar el modelo en session$userData para inferencia
  observeEvent(bn_model(), {
    session$userData$bn_model <- bn_model()
  })
}
