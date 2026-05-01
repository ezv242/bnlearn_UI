# -----------------------------
# 1. CARGA DE LIBRERÍAS
# -----------------------------
library(shiny)
library(shiny.semantic)
library(bnlearn)
library(Rgraphviz)
library(visNetwork)

# Cargar scripts de UI
source("ui/ui_main.R")
source("ui/ui_sidebar.R")
source("ui/ui_results.R")
source("ui/ui_preprocess.R")
source("ui/ui_addNode.R")
source("ui/ui_evaluation.R")

# Cargar scripts de Server
source("server/server_main.R")
source("server/server_bnlearn.R")
source("server/server_inference.R")
source("server/server_parameters.R")
source("server/server_graphs.R")
source("server/server_evaluation.R")


# -----------------------------
# 2. EJECUTAR LA APP
# -----------------------------
shinyApp(ui = ui_main(), server = server_main)
