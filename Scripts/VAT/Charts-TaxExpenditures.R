" Dashboard-TE"

suppressMessages({

# I.Function for Dashboard ------------------------------------------------------------------
VAT_TE_fun <- function(VAT_TE_tbl,forecast_combined_te_selected,forecast_TE_tbl_combined,SimulationYear) {
  
# Chart 1. Tax Expenditures  -----------------------------------------------------------------
  
  # Define custom colors
   custom_colors <- c('#1f77b4', '#ff7f0e')

  VAT_TE_tbl<-forecast_combined_agg%>%
    filter(Descriptions=='Policy Gap')

  VAT_TE_tbl$year<-as.factor(VAT_TE_tbl$year)

  # Create the plot with a dotted line for "Simulation" and fewer markers
  vat_TE_plt <- plot_ly() %>%  # Start with an empty plot
                add_trace(
                  data = subset(VAT_TE_tbl, scenario == "Baseline"),
                  x = ~year,
                  y = ~round(value, 1),
                  name = "Baseline",
                  type = 'scatter',
                  mode = 'lines', # Keep markers but reduce visibility
                  hoverinfo = 'text+y',   # Keep hover info
                  line = list(color = custom_colors[1], width = 4, dash = "solid")
                ) %>%
                add_trace(
                  data = subset(VAT_TE_tbl, scenario == "Simulation"),
                  x = ~year,
                  y = ~round(value, 1),
                  name = "Simulation",
                  type = 'scatter',
                  mode = 'lines', # Keep markers for hover points
                  hoverinfo = 'text+y',   # Keep hover info
                  line = list(color = custom_colors[2], width = 4, dash = "dot")  # Dotted line for simulation
                ) %>%
                layout(
                  title = paste("VAT Tax Expenditures (In EUR thousand),", min(forecast_horizon), "-", max(forecast_horizon)),font = list(size = 12),
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

  # Chart 2. VAT GAP Comparison (In EUR thousand)  -----------------------------------------------------------------
  
  forecast_combined_te_selected_sim<-forecast_combined_te_selected%>%
    dplyr::filter(scenario=="Simulation")%>% 
    dplyr::select(-c("year",
                     "scenario"))%>%
    reshape2::melt()
  
 # forecast_combined_te_selected_sim$label<-"TE"
  
  vat_te_tbl_plt <-  plot_ly(
                              data = forecast_combined_te_selected_sim, 
                              type = "treemap", 
                              values = ~round(value, 0),
                              labels = ~PRODUCT_INDUSTRY_CODE,
                              parents = ~variable,  
                              name = " ",
                              text = ~PRODUCT_INDUSTRY_NAME,
                              textinfo = "label+value+percent parent"
                            ) %>%
                              layout(
                                title = paste("Breakdown of Tax Expenditures by Sector,", SimulationYear), font = list(size = 12),
                                annotations = list(
                                  x = 0.05,
                                  y = -0.01,
                                  text = "Source: WB staff estimation",
                                  showarrow = FALSE,
                                  xref = 'paper',
                                  yref = 'paper',
                                  xanchor = 'center',
                                  yanchor = 'top',
                                  font = list(size = 10)
                                )
                              )
  # Chart 3. Structure of VAT by sectors -------------------------------------------------------------------------
  
  vat_CPA_plt <- plot_ly(forecast_combined_te_selected) %>%
                         add_trace(
                                    x = ~PRODUCT_INDUSTRY_CODE, 
                                    y = ~ value, 
                                    type = 'bar', 
                                    name = ~scenario, # Use the Scenario column to label traces
                                    text = "", # Do not display text inside bars
                                    hoverinfo = 'text', # Show hover text
                                    #hovertext = ~paste(PRODUCT_INDUSTRY_NAME, "<br>scenario:", scenario), # Display PRODUCT_INDUSTRY_NAME and scenario on hover
                                    hovertext = ~paste(PRODUCT_INDUSTRY_NAME, 
                                                       "<br>VAT Value:", round(value, 1) # Round the values to 1 decimal place
                                    ), 
                                    showlegend = TRUE
                                  ) %>%
                                  layout(
                                    title = list(
                                      text = paste("Breakdown of Tax Expenditures by Sector, ", SimulationYear), font = list(size = 12),
                                      x = 0.5, # Center the title
                                      y = 0.95, # Adjust vertical position (optional)
                                      font = list(size = 12) # Adjust font size
                                    ),
                                    xaxis = list(
                                      title = '', 
                                      tickangle = -90, # Tilt x-axis labels for better readability
                                      tickfont = list(size = 8) # Set smaller font size for x-axis labels
                                    ), 
                                    yaxis = list(title = ''),
                                    legend = list(x = 0.9, y = 0.99),
                                    barmode = 'group', # Group the bars by Scenario
                                    annotations = list(
                                      list(
                                        x = -0.05,
                                        y = -0.13,
                                        text = "Source: WB staff estimation", # Add your source text
                                        showarrow = FALSE, # Ensure no arrow is shown
                                        xref = "paper",    # Position relative to the chart
                                        yref = "paper",    
                                        xanchor = "left",  # Align the text to the left
                                        yanchor = "top",   
                                        font = list(size = 8) # Adjust font size for annotation
                                      )
                                    )
                                  )
  
  
  
  # Chart 4. Breakdown of VAT by Sector ---------------------------------------------------------------
  
  vat_TE_GDP_plt <- plot_ly() %>%  # Start with an empty plot
                        add_trace(
                          data = subset(forecast_TE_tbl_combined, scenario == "Baseline"),
                          x = ~year,
                          y = ~round(`Tax Expenditures (Pct of GDP)`, 2),  # Use backticks for column names with spaces
                          name = "Baseline",
                          type = 'scatter',
                          mode = 'lines', # Keep markers but reduce visibility
                          hoverinfo = 'text+y',   # Keep hover info
                          line = list(color = custom_colors[1], width = 4, dash = "solid")
                        ) %>%
                        add_trace(
                          data = subset(forecast_TE_tbl_combined, scenario == "Simulation"),
                          x = ~year,
                          y = ~round(`Tax Expenditures (Pct of GDP)`, 2),  # Use backticks for column names with spaces
                          name = "Simulation",
                          type = 'scatter',
                          mode = 'lines', # Keep markers for hover points
                          hoverinfo = 'text+y',   # Keep hover info
                          line = list(color = custom_colors[2], width = 4, dash = "dot")  # Dotted line for simulation
                        ) %>%
                        layout(
                          title = paste("Tax Expenditures (as Pct Of GDP),", min(forecast_horizon), "-", max(forecast_horizon)),font = list(size = 12), 
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
  
  
  
  
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    vat_TE_plt=vat_TE_plt,
    vat_te_tbl_plt=vat_te_tbl_plt,
    vat_CPA_plt=vat_CPA_plt,
    vat_TE_GDP_plt=vat_TE_GDP_plt
    
  )
}

})         


