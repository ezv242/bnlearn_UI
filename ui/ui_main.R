# ui/ui_main.R
ui_main <- function() {
  semanticPage(
    includeScript("www/js/cpquery_lw.js"),#Llamada al js para las funciones

    # Barra superior fija
    div(class = "ui top fixed menu",

      div(class = "item header", "Aplicación BNLearn"),

      div(
        style = "display: flex; gap: 10px;",
        actionLink("reset_app", "Inicio", class = "item")
      ),

      div(class = "ui dropdown item",
        "Datos",
        div(class = "menu",
            lapply(list.files("data", pattern="\\.csv$"), function(f) {
              div(
                class = "item dataset-item",
                `data-file` = f,
                icon("file"),
                f
              )
            })
        )
      ),

      a(class = "item", "Resultados"),

      div(class = "right menu",
        div(class = "item", icon("user")),
        div(class = "item", icon("settings"))
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

    tags$script(HTML("
      $(document).on('click', '.dataset-item', function() {
        var file = $(this).attr('data-file');
        Shiny.setInputValue('selected_dataset', file, {priority: 'event'});
      });

      $(document).ready(function() {
        $('.ui.dropdown').dropdown();
      });
    "))
  )
}
