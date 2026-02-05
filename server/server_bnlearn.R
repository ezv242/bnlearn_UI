# server/server_bnlearn.R

#' Servidor para aprendizaje de estructura BN
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
server_bnlearn <- function(input, output, session) {
  
  # Reactive para leer los datos
  dataset <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath, stringsAsFactors = TRUE)
  })
  
  # Reactive para construir la red bayesiana
  bn_model <- eventReactive(input$run_bnlearn, {  # botón específico
    data <- dataset()
    alg <- input$algorithm
    
    bn <- switch(alg,
                 "hc" = hc(data),
                 "tabu" = tabu(data),
                 "gs" = gs(data),
                 "iamb" = iamb(data))
    
    bn
  })
  
  # Output de la red
  output$bn_plot <- renderPlot({
    req(bn_model())
    graphviz.plot(bn_model())
  })
  
  # Guardar el modelo en session$userData si quieres usarlo en inferencia
  observe({
    session$userData$bn_model <- bn_model()
  })
}
