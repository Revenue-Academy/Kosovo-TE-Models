'Preparing a Dashboard for Comparing Customs Revenues'

Revenue_Impact_Excise_fun <- function(merged_Customs_BU_SIM,
                                   TypeRev_Excise_data,
                                   Chapter_Excise_data_agg,
                                   MTN_Excise_data_agg,
                               forecast_horizon) {
  # Chart 1. Excise Revenues (by year) -----------------------------------------------------------------
  Excise_RevenuesTotal_plt <- plot_ly(
                                  merged_Customs_BU_SIM,
                                    x = ~year,
                                    y = ~calc_excise_bu*1e06,
                                    name = "Baseline",
                                    type = 'scatter',
                                    mode = 'lines',
                                    line = list(width = 4, dash = "solid")
                                    ) %>%
                                        add_trace(
                                          x = ~year,
                                          y = ~calc_excise_sim*1e06,
                                          name = 'Simulation',
                                          line = list(width = 4, dash = "dot")
                                      ) %>%
                                layout(
                                  title = paste("Excise import Revenues (in EUR),", min(forecast_horizon), "-", max(forecast_horizon)),
                                                xaxis = list(title = '', tickformat = 'd'),
                                                yaxis = list(title = ' ', rangemode = 'tozero'),
                                                annotations = list(
                                                  x = -0.02,
                                                  y = -0.1,
                                                  text = "Source: WB staff estimation",
                                                  showarrow = FALSE,
                                                  xref = 'paper',
                                                  yref = 'paper',
                                                  align = 'left'
                                  )
                                )
  
  # Chart 2. Customs Revenues by Type of Products ------------------------- 
  Sections_Excise_data_long <- TypeRev_Excise_data %>%
    dplyr::filter(year==SimulationYear)%>%
    pivot_longer(cols = c(Baseline, Simulation),
                 names_to = "category",
                 values_to = "value")
  
  
  
  custom_colors <- c('#1f77b4', '#ff7f0e')  
  
  
  Sections_Excise_plt <- plot_ly(Sections_Excise_data_long, 
                                  x = ~TypeOfProducts,        
                                  y = ~value,               
                                  color = ~category,        
                                  colors = custom_colors,   
                                  text = ~round(value, 2),  
                                  hoverinfo = 'text',       
                                  hovertext = ~paste("Sector: ", TypeOfProducts, 
                                                     "<br>Category: ", TypeOfProducts, 
                                                     "<br>Value: ", round(value, 2)),  
                                  type = 'bar', 
                                  barmode = 'group') %>%    
                              layout(
                                title = paste("Import Excise Revenues by Type of Products (in EUR),", simulation_year),
                                xaxis = list(title = " ", tickmode = 'linear'),  
                                yaxis = list(title = " "),
                                annotations = list(
                                  list(
                                    x = -0.02,
                                    y = -0.1,
                                    text = "Source: WB staff estimation",
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper',
                                    align = 'left',
                                    font = list(size = 10)  
                                  )
                                ),
                                bargap = 0.6  # Adjust the gap between bars (0 = no gap, 1 = full gap)
                              )

  # Chart 3. Customs Revenues by HS  ------------------------- 
    
  Chapter_Excise_data_long <- Chapter_Excise_data_agg %>%
    dplyr::filter(year==SimulationYear)%>%
    pivot_longer(cols = c(Baseline, Simulation), 
                 names_to = "category", 
                 values_to = "value")
  
  custom_colors <- c('#1f77b4', '#ff7f0e')  
  
  
  Chapter_Excise_plt <- plot_ly(Chapter_Excise_data_long,
                                 x = ~Chapter,
                                 y = ~value,
                                 color = ~category,
                                 colors = custom_colors,
                                 text = ~round(value, 2),
                                 hoverinfo = 'text',
                                 hovertext = ~paste("Chapter: ", Chapter,
                                                    "<br>Category: ", Chapters_description,
                                                    "<br>Value: ", round(value, 2)),
                                 type = 'bar',
                                 barmode = 'group') %>%
                              layout(
                                title = paste("Import Excise Revenues by HS Chapters (in EUR),", simulation_year),
                                xaxis = list(
                                  title = list(
                                    text = " ",  # X-axis title
                                    font = list(size = 6)  # Font size for X-axis title
                                  ),
                                  tickmode = 'linear',  # Linear ticks
                                  tickangle = -90,  # Rotate x-axis values by -45 degrees
                                  tickfont = list(size = 8)  # Decrease the font size of x-axis values
                                ),
                                yaxis = list(title = " "),
                                annotations = list(
                                  list(
                                    x = -0.02,
                                    y = -0.1,
                                    text = "Source: WB staff estimation",
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper',
                                    align = 'left',
                                    font = list(size = 10)
                                  )
                                ),
                                legend = list(
                                  orientation = 'h',  # Horizontal legend
                                  yanchor = 'bottom',  # Anchor the legend to the bottom
                                  y = -0.15,  # Position the legend below the chart
                                  xanchor = 'center',  # Center the legend
                                  x = 0.5  # Align to the middle
                                )
                              )

  # Chart 4. Customs Revenues by Multilateral Trade Negotiations Categories  ------------------------- 
  
  MTN_Excise_data_agg_prep <- MTN_Excise_data_agg %>%
    filter(year==SimulationYear)%>%
    pivot_longer(cols = c(Baseline, Simulation), 
                 names_to = "category", 
                 values_to = "value")
  
  custom_colors <- c('#1f77b4', '#ff7f0e')  
  
  MTN_Excise_plt <- plot_ly(MTN_Excise_data_agg_prep, 
                                 x = ~Product_group,        
                                 y = ~value,               
                                 color = ~category,        
                                 colors = custom_colors,   
                                 text = ~round(value, 2),  
                                 hoverinfo = 'text',       
                                 hovertext = ~paste("Sector: ", Product_group, 
                                                    "<br>Category: ", category, 
                                                    "<br>Value: ", round(value, 2)),  
                                 type = 'bar', 
                                 barmode = 'group') %>%    
                          layout(
                            title = paste("Import Excise Revenues by MTN Categories (in EUR),",simulation_year ),
                            xaxis = list(title = " ", tickmode = 'linear'),  
                            yaxis = list(title = " "),
                            annotations = list(
                              list(
                                x = -0.02,
                                #y = -0.1,
                                y = -0.9,
                                text = "Source: WB staff estimation",
                                showarrow = FALSE,
                                xref = 'paper',
                                yref = 'paper',
                                align = 'left',
                                font = list(size = 10)  
                              )
                            ))
  
  
  
  
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    Excise_RevenuesTotal_plt = Excise_RevenuesTotal_plt,
    Sections_Excise_plt=Sections_Excise_plt,
    Chapter_Excise_plt=Chapter_Excise_plt,
    MTN_Excise_plt=MTN_Excise_plt

  )
}

