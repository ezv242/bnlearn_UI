# server/server_bnlearn.R
server_bnlearn <- function(input, output, session) {

  # Reactive para leer los datos
  dataset <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath, stringsAsFactors = TRUE)
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

  # Output de la red
  output$bn_plot <- renderPlot({
    req(bn_model())
    graphviz.plot(bn_model())
  })

  # Guardar el modelo en session$userData para inferencia
  observeEvent(bn_model(), {
    session$userData$bn_model <- bn_model()
  })
}
