# Application R Shiny : Visualisation des Fluctuations Météo
# Auteur : Assistant IA
# Description : Application interactive pour explorer les données météorologiques d'une ville

# Installation des packages nécessaires (à exécuter une seule fois)
# install.packages(c("shiny", "shinydashboard", "DT", "plotly", "dplyr", "lubridate", "ggplot2"))

# Chargement des bibliothèques
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(lubridate)
library(ggplot2)


source("ui.R")
source("server.R")


# Lancement de l'application
shinyApp(ui = ui, server = server)