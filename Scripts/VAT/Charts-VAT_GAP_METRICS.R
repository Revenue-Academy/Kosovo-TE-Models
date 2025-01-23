" Dashboard-VAT metrics"

suppressMessages({

# I.Function for Dashboard ------------------------------------------------------------------
VAT_metrics_fun <- function(vat_gap_metrics_tbl,vat_gap_metrics_tbl_treemap,vat_sectors_pie_tbl,vat_sectors_normalized,forecast_combined_agg,SimulationYear) {

  # Chart 1. VAT Revenue Comparison -----------------------------------------------------------------
  

  vat_rev_tbl_plt <- plot_ly(
                                vat_gap_metrics_tbl,
                                y = ~ reorder(Descriptions, value),
                                x = ~ value*1e03,
                                type = 'bar',
                                text = ' ',
                                hoverinfo = 'text',  # Use 'text' to display custom hover text
                                hovertext = ~ paste(Descriptions, "<br>Value:", value),
                                marker = list(color = "#1f77b4")  # Set bar color
                              ) %>%
                                layout(
                                  #title = paste("VAT Revenue Comparison (In EUR thousand),", SimulationYear),
                                  title = paste("VAT Revenue Comparison (In EUR thousand) in 2022"),
                                  xaxis = list(title = ""),
                                  yaxis = list(title = ""),
                                  margin = list(b = 100),
                                  bargap = 0.6,  # Adjust this value to control spacing between bars
                                  annotations = list(
                                    x = -0.05,
                                    y = -0.1,
                                    text = "Source: WB staff estimation",
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper',
                                    xanchor = 'center',
                                    yanchor = 'top',
                                    font = list(size = 10)
                                  )
                                )
  
                          
  # Chart 2. Decomposition of Total VAT Gap (In EUR thousand)  -----------------------------------------------------------------
          
              vat_gap_tbl_plt <- plot_ly(data = vat_gap_metrics_tbl_treemap, 
                                         type = "treemap", 
                                         values = ~round(value,0),
                                         labels = ~ Descriptions,
                                         parents = ~label,  
                                         name = " ",
                                         text = ~Descriptions,
                                         textinfo = "label+value")%>%
                                    layout(
                                      title = list(
                                        #text = paste("Decomposition of Total VAT Gap (In EUR thousand),", SimulationYear)
                                        text = paste("Decomposition of Total VAT Gap (In EUR thousand) in 2022")
                                      ),
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
                                      ))
                
  # Chart 3. VAT Gap (In EUR) -------------------------------------------------------------------------
              
              
              # Define custom colors
              custom_colors <- c('#1f77b4', '#ff7f0e')

              VAT_Benchmark_tbl<-forecast_combined_agg%>%
                filter(Descriptions %in% c('Benchmark VAT','Calibrated VAT'))
              
              VAT_Benchmark_tbl$year<-as.factor(VAT_Benchmark_tbl$year)
              
               # VAT_GAP_tbl<-VAT_Benchmark_tbl%>%
               #   dplyr::group_by(year,scenario)%>%
               #   dplyr::summarise(value=sum(value,na.rm = TRUE))
              
              VAT_GAP_tbl <- VAT_Benchmark_tbl %>%
                dplyr::group_by(year, scenario) %>%
                dplyr::summarise(value = reduce(na.omit(value), `-`), .groups = "drop")
              
               
              # # Create the plot with a dotted line for "Simulation" and fewer markers
               VAT_GAP_plt <- plot_ly() %>%  # Start with an empty plot
                            add_trace(
                              data = subset(VAT_GAP_tbl, scenario == "Baseline"),
                              x = ~year,
                              y = ~round(value*1e03, 1),
                              name = "Baseline",
                              type = 'scatter',
                              mode = 'lines', # Keep markers but reduce visibility
                              hoverinfo = 'text+y',   # Keep hover info
                              line = list(color = custom_colors[1], width = 4, dash = "solid")
                            ) %>%
                            # add_trace(
                            #   data = subset(VAT_GAP_tbl, scenario == "Simulation"),
                            #   x = ~year,
                            #   y = ~round(value*1e03, 1),
                            #   name = "Simulation",
                            #   type = 'scatter',
                            #   mode = 'lines', # Keep markers for hover points
                            #   hoverinfo = 'text+y',   # Keep hover info
                            #   line = list(color = custom_colors[2], width = 4, dash = "dot")  # Dotted line for simulation
                            # ) %>%
                            layout(
                              title = paste("VAT Gap (In EUR),", min(forecast_horizon), "-", max(forecast_horizon)),
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
              
              
              
  # Chart 4. Breakdown of VAT Revenues by Institutional Sector in 2022 ---------------------------------------------------------------
  
             
              vat_str_plt <- plot_ly(vat_sectors_normalized) %>%
                add_trace(
                  x = ~PRODUCT_INDUSTRY_CODE, 
                  y = ~Businesses_pct, 
                  type = 'bar', 
                  name = 'Businesses',
                  hoverinfo = 'text',
                  hovertext = ~paste(
                    "Sector:", PRODUCT_INDUSTRY_NAME,
                    "<br>Businesses:", round(Businesses_pct, 1), "%"
                  )
                ) %>%
                add_trace(
                  x = ~PRODUCT_INDUSTRY_CODE, 
                  y = ~Households_pct, 
                  type = 'bar', 
                  name = 'Households',
                  hoverinfo = 'text',
                  hovertext = ~paste(
                    "Sector:", PRODUCT_INDUSTRY_NAME,
                    "<br>Households:", round(Households_pct, 1), "%"
                  )
                ) %>%
                add_trace(
                  x = ~PRODUCT_INDUSTRY_CODE, 
                  y = ~NPISHs_pct, 
                  type = 'bar', 
                  name = 'NPISHs',
                  hoverinfo = 'text',
                  hovertext = ~paste(
                    "Sector:", PRODUCT_INDUSTRY_NAME,
                    "<br>NPISHs:", round(NPISHs_pct, 1), "%"
                  )
                ) %>%
                add_trace(
                  x = ~PRODUCT_INDUSTRY_CODE, 
                  y = ~Government_pct, 
                  type = 'bar', 
                  name = 'Government',
                  hoverinfo = 'text',
                  hovertext = ~paste(
                    "Sector:", PRODUCT_INDUSTRY_NAME,
                    "<br>Government:", round(Government_pct, 1), "%"
                  )
                ) %>%
                layout(
                  title = list(
                    #text = paste("Breakdown of VAT Revenues by Institutional Sector,", SimulationYear),
                    text = paste("Breakdown of VAT Revenues by Institutional Sector in 2022"),
                    x = 0.5,                # Center the title horizontally
                    y = 1,               # Lower the title for better readability
                    font = list(size = 12)  # Set title font size
                  ),
                  xaxis = list(
                    title = '',
                    tickangle = -45,
                    tickfont = list(size = 6)
                  ),
                  yaxis = list(
                    title = 'Percentage',
                    tickformat = "",      # Disable auto-scaling to percentage format
                    range = c(0, 100),    # Use 0-100 range for y-axis
                    ticksuffix = "%"      # Append "%" to each tick
                  ),
                  legend = list(
                    orientation = "h",      # Horizontal legend
                    x = 0.5,                # Center the legend horizontally
                    y = -0.05,              # Move the legend closer to the chart
                    xanchor = "center",     # Align the legend at the center
                    yanchor = "top",        # Align the legend at the top of its position
                    font = list(size = 6)   # Use a smaller font for the legend
                  ),
                  barmode = 'stack',        # Stack the bars to maintain structure
                  annotations = list(
                    list(
                      # x = -0.05,
                      # y = -0.25,            # Adjust annotation position to avoid overlap
                      x = 0.0,                # Center the legend horizontally
                      y = -0.05, 
                      
                      text = "Source: WB staff estimation", 
                      showarrow = FALSE, 
                      xref = "paper", 
                      yref = "paper", 
                      xanchor = "left", 
                      yanchor = "top", 
                      font = list(size = 6)
                    )
                  )
                )
              
        
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    vat_rev_tbl_plt=vat_rev_tbl_plt,
    vat_gap_tbl_plt=vat_gap_tbl_plt,
    VAT_GAP_plt=VAT_GAP_plt,
    vat_str_plt=vat_str_plt
    
  )
}

})    
