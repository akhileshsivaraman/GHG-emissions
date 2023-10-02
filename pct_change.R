#---- percentage change calculator ----
# function to calculate the percentage in emissions between 1990 and 2021
pct_change <- function(sector_of_interest, sector_emissions_data){
  
  x <- sector_emissions_data |>
    filter(Sector == sector_of_interest)
  
  emissions_1990 <- x |>
    filter(Year == "1990") |>
    pull(Emissions)
  
  emissions_2021 <- x |>
    filter(Year == "2021") |>
    pull(Emissions)
  
  percentage_change <- ((emissions_1990 - emissions_2021) / emissions_1990) * 100
  
  return(percentage_change)
}
