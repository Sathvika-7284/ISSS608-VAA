#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(fabletools)
library(lubridate)
library(plotly)
library(readr)

combined_data <- read_csv("data/combined_data.csv")

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Dengue, Electricity & Weather Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", "Select Year Range:",
                  min = 2013, max = 2025,
                  value = c(2013, 2025), step = 1,
                  sep = "", ticks = FALSE)
    ),
    mainPanel(
      column(
        width = 12,
        tabsetPanel(
          tabPanel("Time Series Overview", plotlyOutput("ts_overview_plot", height = "1000px", width = "100%")),
          tabPanel("Seasonality",
                   plotlyOutput("seasonal_plot", height = "1000px", width = "100%"),
                   plotlyOutput("subseries_plot", height = "900px"),
                   plotlyOutput("stl_weather_plot", height = "900px")),
          tabPanel("Cross-Correlation Analysis",
                   h3("Dengue vs Total Rainfall"),
                   plotlyOutput("ccf_rainfall"),
                   h3("Dengue vs Avg Temp"),
                   plotlyOutput("ccf_temperature")),
          tabPanel("Dengue Analysis",
                   h3("ACF & PACF"),
                   plotlyOutput("acf_dengue"),
                   plotlyOutput("pacf_dengue"),
                   h3("STL Decomposition"),
                   plotlyOutput("stl_dengue"),
                   h3("Classical Decomposition"),
                   plotlyOutput("classical_dengue"),
                   h3("ETS Forecast"),
                   plotlyOutput("ets_dengue"),
                   h3("ETS vs ARIMA Forecast Comparison"),
                   plotlyOutput("compare_dengue")),
          tabPanel("Temperature Analysis",
                   h3("ACF & PACF"),
                   plotlyOutput("acf_temp"),
                   plotlyOutput("pacf_temp"),
                   h3("STL Decomposition"),
                   plotlyOutput("stl_temp"),
                   h3("Classical Decomposition"),
                   plotlyOutput("classical_temp"),
                   h3("ETS Forecast"),
                   plotlyOutput("ets_temp"),
                   h3("ETS vs ARIMA Forecast Comparison"),
                   plotlyOutput("compare_temp")),
          tabPanel("Rainfall Analysis",
                   h3("ACF & PACF"),
                   plotlyOutput("acf_rain"),
                   plotlyOutput("pacf_rain"),
                   h3("STL Decomposition"),
                   plotlyOutput("stl_rain"),
                   h3("Classical Decomposition"),
                   plotlyOutput("classical_rain"),
                   h3("ETS Forecast"),
                   plotlyOutput("ets_rain"),
                   h3("ETS vs ARIMA Forecast Comparison"),
                   plotlyOutput("compare_rain")),
          tabPanel("Electricity Analysis",
                   h3("ACF & PACF"),
                   plotlyOutput("acf_elec"),
                   plotlyOutput("pacf_elec"),
                   h3("STL Decomposition"),
                   plotlyOutput("stl_elec"),
                   h3("Classical Decomposition"),
                   plotlyOutput("classical_elec"),
                   h3("ETS Forecast"),
                   plotlyOutput("ets_elec"),
                   h3("ETS vs ARIMA Forecast Comparison"),
                   plotlyOutput("compare_elec")),
          tabPanel("Weather Forecast Summary",
                   h3("ETS Forecast for Weather & Electricity (Next 12 Months)"),
                   plotlyOutput("weather_forecast_summary")),
          tabPanel("Forecast Accuracy Comparison",
                   h3("ETS vs ARIMA Forecast Accuracy: Rainfall & Electricity"),
                   plotlyOutput("forecast_accuracy_plot"))
        )
      )
    )
  ))

server <- function(input, output, session) {
  tsibble_data <- reactive({
    yr_range <- input$year_range
    
    combined_data %>%
      mutate(Date = yearmonth(MonthYear)) %>%
      filter(year(Date) >= yr_range[1], year(Date) <= yr_range[2]) %>%
      group_by(Date) %>%
      summarise(
        denguecases = sum(denguecases, na.rm = TRUE),
        Electricity_KWh = sum(Electricity_KWh, na.rm = TRUE),
        AvgMeanTemp = mean(AvgMeanTemp, na.rm = TRUE),
        TotalDailyRain = sum(TotalDailyRain, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      as_tsibble(index = Date)
  })
  
  output$ts_overview_plot <- renderPlotly({
    ts_data <- tsibble_data()
    p <- ts_data %>%
      pivot_longer(cols = c(denguecases, Electricity_KWh, AvgMeanTemp, TotalDailyRain),
                   names_to = "Variable", values_to = "Value") %>%
      ggplot(aes(x = Date, y = Value)) +
      geom_line(color = "#2c3e50") +
      facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
      labs(title = "Time Series of Dengue, Electricity, and Weather Variables",
           x = "Date", y = "Value") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$seasonal_plot <- renderPlotly({
    ts_long <- tsibble_data() %>%
      pivot_longer(cols = c(denguecases, Electricity_KWh, AvgMeanTemp, TotalDailyRain),
                   names_to = "Variable", values_to = "Value")
    
    p <- gg_season(ts_long, Value) +
      facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
      labs(title = "Seasonal Patterns of Dengue, Electricity & Weather Variables") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$subseries_plot <- renderPlotly({
    ts_longer <- tsibble_data() %>%
      pivot_longer(-Date, names_to = "Variable", values_to = "Value")
    p <- gg_subseries(ts_longer, Value) +
      facet_wrap(~ Variable, scales = "free_y", ncol = 1) +
      labs(title = "Cycle Subseries Plots") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$stl_weather_plot <- renderPlotly({
    weather_stl <- tsibble_data() %>%
      pivot_longer(cols = c(AvgMeanTemp, Electricity_KWh, TotalDailyRain),
                   names_to = "Variable", values_to = "Value") %>%
      group_by(Variable) %>%
      model(STL(Value)) %>%
      components() %>%
      autoplot() +
      labs(title = "STL Decomposition of Weather and Electricity Variables") +
      theme_minimal()
    ggplotly(weather_stl)
  })
  
  output$ccf_rainfall <- renderPlotly({
    ccf_plot <- tsibble_data() %>%
      CCF(denguecases, TotalDailyRain) %>%
      autoplot() +
      labs(title = "CCF: Dengue Cases vs Total Rainfall") +
      theme_minimal()
    ggplotly(ccf_plot)
  })
  
  output$ccf_temperature <- renderPlotly({
    ccf_plot <- tsibble_data() %>%
      CCF(denguecases, AvgMeanTemp) %>%
      autoplot() +
      labs(title = "CCF: Dengue Cases vs Avg Temp") +
      theme_minimal()
    ggplotly(ccf_plot)
  })
  
  plot_all <- function(var, id) {
    output[[paste0("acf_", id)]] <- renderPlotly({
      ggplotly(autoplot(ACF(tsibble_data(), !!sym(var))))
    })
    
    output[[paste0("pacf_", id)]] <- renderPlotly({
      ggplotly(autoplot(PACF(tsibble_data(), !!sym(var))))
    })
    
    output[[paste0("stl_", id)]] <- renderPlotly({
      model_stl <- tsibble_data() %>% model(STL(!!sym(var)))
      ggplotly(autoplot(components(model_stl)))
    })
    
    output[[paste0("classical_", id)]] <- renderPlotly({
      model_classical <- tsibble_data() %>% model(classical_decomposition(!!sym(var), type = "additive"))
      ggplotly(autoplot(components(model_classical)))
    })
    
    output[[paste0("ets_", id)]] <- renderPlotly({
      train <- tsibble_data() %>% filter(Date < yearmonth("2024 Jan"))
      model_ets <- train %>% model(ETS(!!sym(var)))
      forecast_ets <- forecast(model_ets, h = "12 months")
      fitted_df <- augment(model_ets)
      
      p <- ggplot(tsibble_data(), aes(x = Date, y = !!sym(var))) +
        geom_line(color = "grey40") +
        autolayer(forecast_ets, series = "Forecast", alpha = 0.6) +
        geom_line(data = fitted_df, aes(y = .fitted), color = "blue") +
        labs(title = paste("ETS Forecast for", var)) +
        theme_minimal()
      ggplotly(p)
    })
    
    output[[paste0("compare_", id)]] <- renderPlotly({
      train <- tsibble_data() %>% filter(Date < yearmonth("2024 Jan"))
      model_ets <- train %>% model(ETS(!!sym(var)))
      model_arima <- train %>% model(ARIMA(!!sym(var)))
      forecast_ets <- forecast(model_ets, h = "12 months")
      forecast_arima <- forecast(model_arima, h = "12 months")
      
      p <- autoplot(forecast_ets, tsibble_data(), level = NULL) +
        autolayer(forecast_arima, colour = "red", level = NULL) +
        labs(title = paste("Forecast Comparison: ETS vs ARIMA for", var), y = var, x = "Date") +
        scale_color_manual(values = c("Forecast" = "blue", "ARIMA" = "red")) +
        theme_minimal()
      ggplotly(p)
    })
  }
  
  plot_all("denguecases", "dengue")
  plot_all("AvgMeanTemp", "temp")
  plot_all("TotalDailyRain", "rain")
  plot_all("Electricity_KWh", "elec")
  
  output$weather_forecast_summary <- renderPlotly({
    train_data <- tsibble_data() %>%
      filter(Date < yearmonth("2024 Jan"))
    
    model_temp <- train_data %>% model(ETS(AvgMeanTemp))
    model_rain <- train_data %>% model(ETS(TotalDailyRain))
    model_elec <- train_data %>% model(ETS(Electricity_KWh))
    
    fc_temp <- model_temp %>% forecast(h = "12 months")
    fc_rain <- model_rain %>% forecast(h = "12 months")
    fc_elec <- model_elec %>% forecast(h = "12 months")
    
    fc_temp_df <- fc_temp %>% as_tibble() %>% select(Date, .mean) %>% mutate(Variable = "AvgMeanTemp")
    fc_rain_df <- fc_rain %>% as_tibble() %>% select(Date, .mean) %>% mutate(Variable = "TotalDailyRain")
    fc_elec_df <- fc_elec %>% as_tibble() %>% select(Date, .mean) %>% mutate(Variable = "Electricity_KWh")
    
    weather_fc <- bind_rows(fc_temp_df, fc_rain_df, fc_elec_df)
    
    p <- weather_fc %>%
      ggplot(aes(x = Date, y = .mean, color = Variable)) +
      geom_line(size = 1) +
      facet_wrap(~ Variable, scales = "free_y") +
      labs(
        title = "Forecasts: Weather & Electricity (Next 12 Months)",
        y = "Forecasted Value",
        x = "Date"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$forecast_accuracy_plot <- renderPlotly({
    train_data <- tsibble_data() %>% filter(Date < yearmonth("2024 Jan"))
    test_data <- tsibble_data() %>% filter(Date >= yearmonth("2024 Jan"))
    
    model_rain_ets <- train_data %>% model(ETS(TotalDailyRain))
    model_elec_ets <- train_data %>% model(ETS(Electricity_KWh))
    model_rain_arima <- train_data %>% model(ARIMA(TotalDailyRain))
    model_elec_arima <- train_data %>% model(ARIMA(Electricity_KWh))
    
    fc_rain_ets <- forecast(model_rain_ets, h = "12 months")
    fc_elec_ets <- forecast(model_elec_ets, h = "12 months")
    fc_rain_arima <- forecast(model_rain_arima, h = "12 months")
    fc_elec_arima <- forecast(model_elec_arima, h = "12 months")
    
    acc_rain_compare <- bind_rows(
      accuracy(fc_rain_ets, test_data %>% select(Date, TotalDailyRain)) %>% mutate(Variable = "TotalDailyRain", Model = "ETS"),
      accuracy(fc_rain_arima, test_data %>% select(Date, TotalDailyRain)) %>% mutate(Variable = "TotalDailyRain", Model = "ARIMA")
    )
    
    acc_elec_compare <- bind_rows(
      accuracy(fc_elec_ets, test_data %>% select(Date, Electricity_KWh)) %>% mutate(Variable = "Electricity_KWh", Model = "ETS"),
      accuracy(fc_elec_arima, test_data %>% select(Date, Electricity_KWh)) %>% mutate(Variable = "Electricity_KWh", Model = "ARIMA")
    )
    
    weather_compare <- bind_rows(acc_rain_compare, acc_elec_compare) %>%
      select(Variable, Model, RMSE, MAE, MAPE)
    
    p <- weather_compare %>%
      pivot_longer(cols = c(RMSE, MAE, MAPE), names_to = "Metric", values_to = "Value") %>%
      ggplot(aes(x = Model, y = Value, fill = Model)) +
      geom_col(position = "dodge") +
      facet_grid(Metric ~ Variable, scales = "free_y") +
      labs(
        title = "ETS vs ARIMA Forecast Accuracy: Rainfall & Electricity",
        y = "Error Metric Value",
        x = "Model Type"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
}

shinyApp(ui, server)