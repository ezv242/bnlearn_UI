ui_inference <- function() {
# INFERENCIA: Inferencia, con inputs para evento, evidencia
tabset(
    tabs = list(
        # ---------------------
        # TAB CPQUERY
        # ---------------------
        list(
            menu_item = "cpquery",
            content = segment(
                div(
                    style = "display: flex; flex-direction: column; gap: 10px;",
                    # El lw funciona con redes grandes y continuas,
                    # el exact con redes pequeñas y discretas
                    "Método de inferencia:",
                    dropdown_input("method_inference_cpquery",
                        choices = c("lw", "ls"),
                        value = "ls"
                    ),
                    checkboxInput("filas_cpquery", "Usar filas como evidencia",
                        value = FALSE
                    ),
                    numericInput("n_cpquery", "Número de muestras aleatorias",
                        value = 1000, min = 1000
                    ),
                    numericInput("batch_cpquery",
                        "Muestras aleatorias generadas de golpe",
                        value = 1000, min = 1000
                    ),
                    textInput("event", 'Event (e.g. X == "yes")'),
                    conditionalPanel(
                        condition = "input.filas_cpquery == true",
                        textInput("evidencia_lw", "Evidencia"),
                    ),
                    conditionalPanel(
                        condition = "input.filas_cpquery == false",
                        textInput("evidence", 'Evidence (e.g. E == "yes")')
                    ),
                    div(
                        style = "display: flex; gap: 10px; margin-top: 5px;",
                        action_button("run_inference", "Hacer Inferencia"),
                    )
                )
            )
        ),

        # ---------------------
        # TAB CPDIST
        # ---------------------
        list(
            menu_item = "cpdist",
            content = segment(
                div(
                    style = "display: flex; flex-direction: column; gap: 10px;",
                    "Tabla de muestras cpdist:",
                    dropdown_input("method_inference_cpdist",
                        choices = c("lw", "ls"),
                        value = "ls"
                    ),
                    checkboxInput("filas_cpdist", "Usar filas como evidencia",
                        value = FALSE
                    ),
                    numericInput("n_cpdist", "Número de muestras aleatorias",
                        value = 1000, min = 1000
                    ),
                    numericInput("batch_cpdist",
                        "Muestras aleatorias generadas de golpe",
                        value = 1000, min = 1000
                    ),
                    selectizeInput(
                        "tags",
                        "Selecciona nodos a simular:",
                        choices = NULL,
                        multiple = TRUE,
                        options = list(create = TRUE),
                        width = "100%"
                    ),
                    conditionalPanel(
                        condition = "input.filas_cpdist == true",
                        textInput("evidence_cpdist_lw", "Evidencia"),
                    ),
                    conditionalPanel(
                        condition = "input.filas_cpdist == false",
                        textInput("evidence_cpdist", "Evidence (e.g. E == 'yes')")
                    ),
                    div(
                        style = "display: flex; gap: 10px; margin-top: 5px;",
                        action_button("run_cpdist", "Hacer CPDIST"),
                    )
                )
            )
        )
    )
)
}
