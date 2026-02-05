# R/learn_structure.R
learn_bn <- function(data, algorithm = "hc") {
  switch(algorithm,
         "hc" = hc(data),
         "tabu" = tabu(data),
         "gs" = gs(data),
         "iamb" = iamb(data))
}
