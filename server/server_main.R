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
    data_type = NULL,     # El tipo de datos (discreto, continuo, mixto)
    data_continuous = NULL, # Datos continuos (si se cargan)
    data_discrete = NULL,   # Datos discretos (si se cargan)
    graph_dirigido = NULL, # Si el grafo es dirigido
    dataset_NAs = NULL # Si se indica que los datos contienen NAs
  )

  # Llamar a la parte de aprendizaje de estructura
  server_bnlearn(input, output, session, shared_data)

  # Llamar a la parte de ajuste de parámetros
  server_parameters(input, output, session, shared_data)

  # Llamar a la parte de inferencia
  server_inference(input, output, session, shared_data)

  # Llamar a la parte de visualización de gráficos
  server_graphs(input, output, session, shared_data)

  server_evaluation(input, output, session, shared_data)
}
