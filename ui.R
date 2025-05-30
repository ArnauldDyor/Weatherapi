# Chargement des bibliothèques
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(lubridate)
library(ggplot2)


# Liste des villes disponibles
cities <- c("Paris", "Lyon", "Marseille", "Toulouse", "Nice", "Bordeaux")


# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "📊 Analyse Météorologique Interactive"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("🏠 Accueil", tabName = "home", icon = icon("home")),
      menuItem("📈 Graphiques", tabName = "charts", icon = icon("chart-line")),
      menuItem("📋 Données", tabName = "data", icon = icon("table")),
      menuItem("📊 Statistiques", tabName = "stats", icon = icon("calculator"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      # Onglet Accueil
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "🌤️ Bienvenue dans l'Analyseur Météo", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  h3("Explorez les données météorologiques de votre ville !"),
                  p("Cette application vous permet de visualiser et d'analyser les fluctuations météorologiques 
              de différentes villes françaises sur une période donnée."),
                  br(),
                  
                  h4("🎯 Fonctionnalités :"),
                  tags$ul(
                    tags$li("📈 Graphiques interactifs des températures, précipitations, etc."),
                    tags$li("📊 Analyses statistiques par saison"),
                    tags$li("📋 Exploration détaillée des données"),
                    tags$li("🔍 Comparaisons entre différentes périodes")
                  ),
                  br(),
                  
                  h4("🚀 Comment commencer :"),
                  tags$ol(
                    tags$li("Choisissez une ville dans le menu de droite"),
                    tags$li("Sélectionnez une période d'analyse"),
                    tags$li("Explorez les différents onglets pour découvrir les analyses")
                  )
                )
              ),
              
              fluidRow(
                box(
                  title = "⚙️ Paramètres", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 4,
                  selectInput("city", 
                              "🏙️ Choisir une ville :",
                              choices = cities,
                              selected = "Paris"),
                  
                  dateRangeInput("dateRange",
                                 "📅 Période d'analyse :",
                                 start = Sys.Date() - 365,
                                 end = Sys.Date(),
                                 format = "dd/mm/yyyy",
                                 language = "fr"),
                  
                  br(),
                  actionButton("updateData", 
                               "🔄 Mettre à jour", 
                               class = "btn-primary",
                               style = "width: 100%;")
                ),
                
                box(
                  title = "📊 Aperçu rapide", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 8,
                  
                  fluidRow(
                    valueBoxOutput("avgTemp", width = 6),
                    valueBoxOutput("totalRain", width = 6)
                  ),
                  fluidRow(
                    valueBoxOutput("maxWind", width = 6),
                    valueBoxOutput("avgHumidity", width = 6)
                  )
                )
              )
      ),
      
      # Onglet Graphiques
      tabItem(tabName = "charts",
              fluidRow(
                box(
                  title = "🌡️ Évolution de la Température", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("tempPlot", height = "400px")
                )
              ),
              
              fluidRow(
                box(
                  title = "🌧️ Précipitations et Humidité", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("precipPlot", height = "350px")
                ),
                box(
                  title = "💨 Vent et Pression", 
                  status = "warning", 
                  solidHeader = TRUE,
                  width = 6,
                  plotlyOutput("windPlot", height = "350px")
                )
              ),
              
              fluidRow(
                box(
                  title = "📈 Distribution des Températures par Saison", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("seasonPlot", height = "400px")
                )
              )
      ),
      
      # Onglet Données
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "📋 Données Météorologiques Détaillées", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  p("Explorez les données brutes avec les options de filtrage et de tri ci-dessous :"),
                  br(),
                  
                  DT::dataTableOutput("weatherTable")
                )
              )
      ),
      
      # Onglet Statistiques
      tabItem(tabName = "stats",
              fluidRow(
                box(
                  title = "📊 Statistiques Descriptives", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 6,
                  
                  h4("Température (°C)"),
                  verbatimTextOutput("tempStats"),
                  
                  h4("Précipitations (mm)"),
                  verbatimTextOutput("precipStats")
                ),
                
                box(
                  title = "🔢 Analyse par Saison", 
                  status = "info", 
                  solidHeader = TRUE,
                  width = 6,
                  
                  h4("Température Moyenne par Saison"),
                  DT::dataTableOutput("seasonStats"),
                  
                  br(),
                  h4("Jours de Pluie par Saison"),
                  DT::dataTableOutput("rainDays")
                )
              ),
              
              fluidRow(
                box(
                  title = "🎯 Records et Extremes", 
                  status = "success", 
                  solidHeader = TRUE,
                  width = 12,
                  
                  fluidRow(
                    column(6,
                           h4("🔥 Températures Extrêmes"),
                           verbatimTextOutput("tempExtremes")
                    ),
                    column(6,
                           h4("💧 Précipitations Record"),
                           verbatimTextOutput("precipExtremes")
                    )
                  )
                )
              )
      )
    )
  )
)
