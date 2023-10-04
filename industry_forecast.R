#---- industry_forecast ----
industries <- unique(ghg_section_totals$`Section name`)

industry_forecast <- function(industry_of_interest, ghg_section_totals){
  
  # prepare data
  industry_emissions <- ghg_section_totals |>
    filter(`Section name` == as.character(industry_of_interest)) |>
    pivot_longer(cols = c("1990":"2021"), names_to = "Year", values_to = "Emissions")
  
  # time series object
  ts_industry_emissions <- ts(industry_emissions$Emissions, frequency = 1, start = 1990, end = 2021)
  
  # ARIMA
  model_industry_emissions <- auto.arima(y = ts_industry_emissions)
  
  forecast_industry_emissions <- forecast(model_industry_emissions, h = 5, level = 30)
  
  assign(paste0("forecast_", industry_of_interest, "_emissions"), forecast_industry_emissions)
}
