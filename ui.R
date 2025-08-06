#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(pracma)
library(sm)
library(KernSmooth)
library(readxl)
library(mosaic)
library(DT)
library(plotly)
library(rmarkdown)
library(lubridate)
library(markdown)
library(waiter)

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  use_waiter(),
  theme = shinytheme("darkly"),
                  navbarPage("Proyección de una serie de tiempo",
                             tabPanel("Datos",
                                      sidebarPanel(
                                          textInput('lugar', 'Indicar el lugar (país, estado, etc.)'),
                                          fileInput('tabla1', 'Cargar la serie de tiempo',
                                                    accept = c(".xlsx")
                                          ),
                                          tags$h5(tags$b("Período de la proyección:")),
                                          fluidRow(column(6,numericInput('inicio', 'Año inicial',
                                                                         min = 1900, max = 2100, step = 1,value = year(Sys.Date()))),
                                                   column(6,numericInput('final', 'Año final',
                                                                         min = 1900, max = 2100, step = 1,value = year(Sys.Date())+4))),
                                          radioButtons('pregunta',"¿Tiene la población para el período a proyectar?",
                                                       choices = c("SI","NO"),selected = "NO",inline = TRUE),
                                          uiOutput('cargar_poblacion'),
                                          radioButtons('pregunta_brecha',"¿Existe una brecha entre los casos registrados y los existentes?",
                                                       choices = c("SI","NO"),selected = "NO",inline = TRUE),
                                          uiOutput('cargar_brecha'),
                                          radioButtons('pregunta_reduccion',"¿Tiene previsto reducir la brecha en el período de la proyección?",
                                                       choices = c("SI","NO"),selected = "NO",inline = TRUE),
                                          uiOutput('cargar_reduccion'),
                                          fluidRow(
                                          actionButton("submitbutton", 
                                                       "Calcular proyección", 
                                                       class = "btn btn-primary"),
                                          downloadButton("reporte_pdf",
                                                         "Descargar reporte", 
                                                         class = "btn btn-primary")
                                          )
                                          
                                      ),
                                      mainPanel(
                                        fluidRow(
                                          column(12, 
                                                 align = "center",
                                                 div(style = "background-color: #F7F9F8; padding: 20px;", # Match this with the dashboard color
                                                     img(src='logo_combinado_correcto.png', height="100%", width="100%"
                                                     ))
                                          )
                                        ),
                                        tags$label(h3('Tendencia actual y proyección')), # Status/Output Text Box
                                          plotOutput('figura'),
                                          tags$label(h3('Tabla de casos estimados')),
                                          DTOutput('tabla')
                                      ) # mainPanel
                                      
                             ), # Navbar 1, tabPanel
                             tabPanel("Instrucciones", includeMarkdown("Instrucciones.md")),
                             tabPanel("Contacto", includeMarkdown("Contacto.md")))
                 
                  
))
