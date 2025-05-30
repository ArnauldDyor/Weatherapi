# Génération de données météo simulées (pour la démo)
# Dans une vraie application, vous pourriez vous connecter à une API météo
generate_weather_data <- function(city, start_date, end_date) {
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day")
  n_days <- length(dates)
  
  # Simulation de températures avec variation saisonnière
  day_of_year <- as.numeric(format(dates, "%j"))
  base_temp <- 15 + 10 * sin(2 * pi * (day_of_year - 80) / 365)
  temperature <- base_temp + rnorm(n_days, mean = 0, sd = 5)
  
  # Simulation d'autres variables météo
  humidity <- pmax(pmin(60 + rnorm(n_days, mean = 0, sd = 15), 100), 0)
  precipitation <- pmax(rexp(n_days, rate = 0.5), 0)
  wind_speed <- pmax(rnorm(n_days, mean = 15, sd = 8), 0)
  pressure <- 1013 + rnorm(n_days, mean = 0, sd = 10)
  
  data.frame(
    date = dates,
    city = city,
    temperature = round(temperature, 1),
    humidity = round(humidity, 1),
    precipitation = round(precipitation, 1),
    wind_speed = round(wind_speed, 1),
    pressure = round(pressure, 1),
    season = case_when(
      month(dates) %in% c(12, 1, 2) ~ "Hiver",
      month(dates) %in% c(3, 4, 5) ~ "Printemps",
      month(dates) %in% c(6, 7, 8) ~ "Été",
      month(dates) %in% c(9, 10, 11) ~ "Automne"
    )
  )
}


# Serveur
server <- function(input, output, session) {
  
  # Données réactives
  weather_data <- eventReactive(input$updateData, {
    generate_weather_data(input$city, input$dateRange[1], input$dateRange[2])
  }, ignoreNULL = FALSE)
  
  # Mise à jour automatique au démarrage
  observe({
    if (input$updateData == 0) {
      weather_data()
    }
  })
  
  # Value boxes
  output$avgTemp <- renderValueBox({
    data <- weather_data()
    avg_temp <- round(mean(data$temperature, na.rm = TRUE), 1)
    valueBox(
      value = paste0(avg_temp, "°C"),
      subtitle = "Température Moyenne",
      icon = icon("thermometer-half"),
      color = "blue"
    )
  })
  
  output$totalRain <- renderValueBox({
    data <- weather_data()
    total_rain <- round(sum(data$precipitation, na.rm = TRUE), 1)
    valueBox(
      value = paste0(total_rain, " mm"),
      subtitle = "Précipitations Totales",
      icon = icon("cloud-rain"),
      color = "aqua"
    )
  })
  
  output$maxWind <- renderValueBox({
    data <- weather_data()
    max_wind <- round(max(data$wind_speed, na.rm = TRUE), 1)
    valueBox(
      value = paste0(max_wind, " km/h"),
      subtitle = "Vent Maximum",
      icon = icon("wind"),
      color = "green"
    )
  })
  
  output$avgHumidity <- renderValueBox({
    data <- weather_data()
    avg_humidity <- round(mean(data$humidity, na.rm = TRUE), 1)
    valueBox(
      value = paste0(avg_humidity, "%"),
      subtitle = "Humidité Moyenne",
      icon = icon("tint"),
      color = "purple"
    )
  })
  
  # Graphique des températures
  output$tempPlot <- renderPlotly({
    data <- weather_data()
    
    p <- ggplot(data, aes(x = date, y = temperature)) +
      geom_line(color = "#3498db", size = 0.8) +
      geom_smooth(method = "loess", color = "#e74c3c", alpha = 0.3) +
      labs(title = paste("Évolution de la Température à", input$city),
           x = "Date", y = "Température (°C)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Graphique des précipitations
  output$precipPlot <- renderPlotly({
    data <- weather_data()
    
    p <- ggplot(data, aes(x = date)) +
      geom_col(aes(y = precipitation), fill = "#3498db", alpha = 0.7) +
      geom_line(aes(y = humidity/2), color = "#e74c3c", size = 1) +
      scale_y_continuous(
        name = "Précipitations (mm)",
        sec.axis = sec_axis(~ . * 2, name = "Humidité (%)")
      ) +
      labs(title = "Précipitations et Humidité", x = "Date") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Graphique du vent et pression
  output$windPlot <- renderPlotly({
    data <- weather_data()
    
    p <- ggplot(data, aes(x = date)) +
      geom_line(aes(y = wind_speed), color = "#2ecc71", size = 1) +
      geom_line(aes(y = (pressure - 1000) * 2), color = "#9b59b6", size = 1) +
      scale_y_continuous(
        name = "Vitesse du vent (km/h)",
        sec.axis = sec_axis(~ . / 2 + 1000, name = "Pression (hPa)")
      ) +
      labs(title = "Vent et Pression Atmosphérique", x = "Date") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Graphique par saison
  output$seasonPlot <- renderPlotly({
    data <- weather_data()
    
    p <- ggplot(data, aes(x = season, y = temperature, fill = season)) +
      geom_boxplot(alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      scale_fill_brewer(type = "qual", palette = "Set2") +
      labs(title = "Distribution des Températures par Saison",
           x = "Saison", y = "Température (°C)") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(size = 16, face = "bold"))
    
    ggplotly(p)
  })
  
  # Tableau des données
  output$weatherTable <- DT::renderDataTable({
    data <- weather_data()
    data$date <- format(data$date, "%d/%m/%Y")
    
    DT::datatable(
      data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = c("temperature", "humidity", "precipitation", "wind_speed", "pressure"), digits = 1)
  })
  
  # Statistiques
  output$tempStats <- renderPrint({
    data <- weather_data()
    summary(data$temperature)
  })
  
  output$precipStats <- renderPrint({
    data <- weather_data()
    summary(data$precipitation)
  })
  
  # Statistiques par saison
  output$seasonStats <- DT::renderDataTable({
    data <- weather_data()
    season_stats <- data %>%
      group_by(season) %>%
      summarise(
        `Temp. Moyenne (°C)` = round(mean(temperature), 1),
        `Temp. Min (°C)` = round(min(temperature), 1),
        `Temp. Max (°C)` = round(max(temperature), 1),
        `Écart-type` = round(sd(temperature), 1)
      )
    
    DT::datatable(season_stats, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$rainDays <- DT::renderDataTable({
    data <- weather_data()
    rain_days <- data %>%
      group_by(season) %>%
      summarise(
        `Jours avec pluie` = sum(precipitation > 1),
        `Précip. totale (mm)` = round(sum(precipitation), 1),
        `Précip. max (mm)` = round(max(precipitation), 1)
      )
    
    DT::datatable(rain_days, options = list(dom = 't'), rownames = FALSE)
  })
  
  # Extrêmes
  output$tempExtremes <- renderPrint({
    data <- weather_data()
    min_temp <- min(data$temperature)
    max_temp <- max(data$temperature)
    min_date <- data$date[which.min(data$temperature)]
    max_date <- data$date[which.max(data$temperature)]
    
    cat("🔥 Température maximale :", max_temp, "°C\n")
    cat("   Date :", format(max_date, "%d/%m/%Y"), "\n\n")
    cat("❄️  Température minimale :", min_temp, "°C\n")
    cat("   Date :", format(min_date, "%d/%m/%Y"), "\n")
  })
  
  output$precipExtremes <- renderPrint({
    data <- weather_data()
    max_precip <- max(data$precipitation)
    max_precip_date <- data$date[which.max(data$precipitation)]
    rainy_days <- sum(data$precipitation > 0.1)
    
    cat("🌧️ Précipitation max :", round(max_precip, 1), "mm\n")
    cat("   Date :", format(max_precip_date, "%d/%m/%Y"), "\n\n")
    cat("🌧️ Nombre de jours pluvieux :", rainy_days, "\n")
    cat("   (> 0.1 mm de précipitation)")
  })
}
