# ui/ui_sidebar.R
ui_sidebar <- function() {
  tagList(
    fileInput("datafile", "Selecciona archivo CSV", accept = ".csv"),
    dropdown_input("algorithm", 
                   choices = c("hc", "tabu", "gs", "iamb"), 
                   value = "hc"),
    action_button("run_bnlearn", "Aprender Red"),
    action_button("run_inference", "Hacer Inferencia")
  )
}
