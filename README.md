# bnlearn_UI
Aplicación Shiny para aprender y visualizar Redes Bayesianas
usando el paquete bnlearn y shiny.semantic.

## Características
- Aprendizaje de estructura
- Inferencia probabilística
- Visualización gráfica

## Ejecutar
```r
docker build -t tfg-bayes .
docker run -p 3838:3838 tfg-bayes
http://localhost:3838/ (desde el navegador)
