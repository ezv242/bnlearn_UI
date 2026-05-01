# ui/ui_main.R
ui_main <- function() {
  semanticPage(
    includeScript("www/js/cpquery_lw.js"),#Llamada al js para las funciones

    # 🔹 Barra superior fija
    div(class = "ui top fixed menu",

      div(class = "item header", "Aplicación BNLearn"),

      a(class = "item", "Inicio"),
      a(class = "item", "Datos"),
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
    )
  )
}
