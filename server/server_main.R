# server/server_main.R

#' Servidor principal de la app BNLearn
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
server_main <- function(input, output, session) {
  
  # Llamar a la parte de aprendizaje de estructura
  server_bnlearn(input, output, session)
  
  # Llamar a la parte de inferencia
  server_inference(input, output, session)

  #Llamar  la parte de ajuste de parámetros
  server_parameters(input, output, session)
  
}
