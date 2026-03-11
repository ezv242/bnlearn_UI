# server/server_bnlearn.R
# Esta función permite leer los datos y construir una red bayesiana
# usando uno de los algoritmos
server_bnlearn <- function(input, output, session, shared_data) {

  # Reactive para leer los datos
  dataset <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath, stringsAsFactors = TRUE)
  })

  # Guardar dataset en shared_data
  observe({
    shared_data$dataset <- dataset()
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
  output$bn_plot <- renderPlot({
    req(bn_model())
    graphviz.plot(bn_model())
  })

  output$network <- renderPrint({
    bn_model()
  })
}
