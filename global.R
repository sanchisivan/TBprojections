

options(shiny.error = function() {
  err <- geterrmessage()
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(paste0("[", timestamp, "] ", err, "\n"), 
      file = "shiny_error_log.txt", append = TRUE)
})


# global.R

# Lista de paquetes necesarios
required_packages <- c(
  "shiny", "shinythemes", "tidyverse", "pracma", "sm", "KernSmooth", 
  "readxl", "mosaic", "DT", "plotly", "rmarkdown", "lubridate", "markdown"
)

# Instalar los que falten
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Cargar todos
lapply(required_packages, library, character.only = TRUE)

# (Opcional) Definir variables, funciones, etc., que uses en server y ui
jscode <- "shinyjs.closeWindow = function() { window.close(); }"
