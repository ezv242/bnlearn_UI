# server/server_main.R

#' Servidor principal de la app BNLearn
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session

server_main <- function(input, output, session) {

  #Objeto reactivo compartido
  shared_data <- reactiveValues(
    dataset = NULL,      # Los datos cargados
    network = NULL,      # La estructura de la red (bn_model)
    bn_fitted = NULL,    # El modelo ajustado
    data_type = NULL     # El tipo de datos (discreto, continuo, mixto)
  )
  
  # Llamar a la parte de aprendizaje de estructura
  server_bnlearn(input, output, session, shared_data)

  # Llamar a la parte de ajuste de parámetros
  server_parameters(input, output, session, shared_data)
  
  # Llamar a la parte de inferencia
  server_inference(input, output, session, shared_data)
}
