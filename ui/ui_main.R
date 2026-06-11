# ui/ui_main.R
ui_main <- function() {
  semanticPage(
    includeScript("www/js/rows_selector.js"),#Llamada al js para las funciones
    includeScript("www/js/node_popup.js"),
    includeCSS("www/css/styles.css"),

    # Barra superior fija
    div(class = "ui top fixed menu",

      div(class = "item header",
      tags$img(
        src = "Logo.png", # Nombre exacto de tu archivo dentro de la carpeta www
        style = "height: 35px; margin-right: 10px; vertical-align: middle;"
      ),
      "Aplicación BNLearn"),

      div(
        style = "display: flex; gap: 10px;",
        actionLink("reset_app", "Inicio", class = "item")
      ),

      # El menú que se ve igual a "Resultados"
      div(class = "ui dropdown item",
        "Datos", # El nombre del botón en la barra
        div(class = "menu",
            div(class = "item opcion-texto", "Asia"),
            div(class = "item opcion-texto", "clgaussian_test"),
            div(class = "item opcion-texto", "Alarm"),
            div(class = "item opcion-texto", "Insurance"),
            div(class = "item opcion-texto", "Coronary"),
            div(class = "item opcion-texto", "gaussian_test"),
            div(class = "item opcion-texto", "Hailfinder"),
            div(class = "item opcion-texto", "learning_test"),
            div(class = "item opcion-texto", "Lizards"),
            div(class = "item opcion-texto", "Marks")
        )
      )
    ),

    div(class = "ui grid",
      div(class = "four wide column",
        ui_sidebar()  # sidebar ocupa 4 columnas
      ),
      div(class = "twelve wide column",
        ui_results()  # results ocupa 12-4 = 8 columnas
      )
    ),

    # Mantenemos este JS para que R sepa qué opción tocaste y ACTIVAMOS el dropdown
    tags$head(
      tags$script(HTML("
        $(document).ready(function() {
          // ACTIVACIÓN NECESARIA
          $('.ui.dropdown').dropdown();
          
          $(document).on('click', '.opcion-texto', function() {
            var valor = $(this).text().trim(); 
            Shiny.setInputValue('nombre_seleccionado', valor, {priority: 'event'});
          });
        });
      "))
    )
  )
}
