
" Dashboard-VAT metrics"

suppressMessages({
  
# I.Function for Dashboard ------------------------------------------------------------------
VAT_aggregation_revenues_fun <- function(forecast_combined_agg,forecast_combined_agg_tbl_wide,forecast_sim_cpa,vat_sectors_pie_tbl,
                                     SimulationYear) {
  
# Chart 1. VAT Revenue Forecast (In EUR thousand) -----------------------------------------------------------------
           # Define custom colors
          custom_colors <- c('#1f77b4', '#ff7f0e')
          
          
          VAT_revenues_tbl<-forecast_combined_agg%>%
            filter(Descriptions=='Calibrated VAT')
          
          VAT_revenues_tbl$year<-as.factor(VAT_revenues_tbl$year)
          
          # Create the plot with a dotted line for "Simulation" and fewer markers
          vat_revenue_plt <- plot_ly() %>%  # Start with an empty plot
                                add_trace(
                                  data = subset(VAT_revenues_tbl, scenario == "Baseline"),
                                  x = ~year,
                                  y = ~round(value*1e03, 1),
                                  name = "Baseline",
                                  type = 'scatter',
                                  mode = 'lines', # Keep markers but reduce visibility
                                  hoverinfo = 'text+y',   # Keep hover info
                                  line = list(color = custom_colors[1], width = 4, dash = "solid")
                                ) %>%
                                add_trace(
                                  data = subset(VAT_revenues_tbl, scenario == "Simulation"),
                                  x = ~year,
                                  y = ~round(value*1e03, 1),
                                  name = "Simulation",
                                  type = 'scatter',
                                  mode = 'lines', # Keep markers for hover points
                                  hoverinfo = 'text+y',   # Keep hover info
                                  line = list(color = custom_colors[2], width = 4, dash = "dot")  # Dotted line for simulation
                                ) %>%
                                layout(
                                  title = paste("VAT Revenue Forecast (In EUR thousand),", min(forecast_horizon), "-", max(forecast_horizon)),
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
          
          
          # Render the plot
         # vat_nace_hh_plt
  
  
  # Chart 2. Aggregate VAT by CPA sectors  -----------------------------------------------------------------
  
        
          forecast_combined_agg_tbl_wide_selected<-forecast_combined_agg_tbl_wide%>%
            dplyr::select(year,`Current Law (Pct of GDP)`,`Simulation (Pct of GDP)`)

          
          # Create the plot with solid and dotted lines (no markers)
          vat_revenue_gdp_plt <- plot_ly() %>%
                            add_trace(
                              data = forecast_combined_agg_tbl_wide_selected,
                              x = ~year,
                              y = ~`Current Law (Pct of GDP)`,
                              name = "Current Law",
                              type = 'scatter',
                              mode = 'lines',  # Lines only, no markers
                              hoverinfo = 'text+y',
                              line = list(color = custom_colors[1], width = 4, dash = "solid")  # Solid line with custom color
                            ) %>%
                            add_trace(
                              data = forecast_combined_agg_tbl_wide_selected,
                              x = ~year,
                              y = ~`Simulation (Pct of GDP)`,
                              name = "Simulation",
                              type = 'scatter',
                              mode = 'lines',  # Lines only, no markers
                              hoverinfo = 'text+y',
                              line = list(color = custom_colors[2], width = 4, dash = "dot")  # Dotted line with custom color
                            ) %>%
                            layout(
                              title = "VAT Revenue Forecast (Pct of GDP)",
                              xaxis = list(title = "Year", tickformat = 'd'),
                              yaxis = list(title = "Revenue (% of GDP)", rangemode = 'tozero'),
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
                          

  
  # Chart 3.Breakdown of VAT Revenues by Sector, Generated by Households-------------------------------------------------------------------------
  
                  vat_structure_plt<- plot_ly(
                                  vat_sectors_pie_tbl,
                                  labels = ~variable,
                                  values = ~round(value,0),
                                  type = 'pie',
                                  hole = 0.6,
                                  textinfo = 'label+percent',
                                  insidetextorientation = 'radial'
                                ) %>%
                                  layout(
                                    title = paste("Structure of VAT by Institutional Sectors,", SimulationYear),
                                    showlegend = TRUE,  # Enable the legend
                                    legend = list(
                                      orientation = 'v',  # Vertical orientation (default)
                                      x = 1.05,  # Move the legend slightly to the right of the chart
                                      y = 0.5,  # Center the legend vertically
                                      xanchor = 'left',
                                      yanchor = 'middle'
                                    ),
                                    startangle = 90,  # Rotate the pie chart by 90 degrees clockwise
                                    margin = list(l = 20, r = 20, t = 50, b = 50),  # Leave space for the annotation
                                    annotations = list(
                                      list(
                                        x = 0.19,
                                        y = 0,
                                        text = "Source: WB staff estimation",
                                        showarrow = FALSE,
                                        xref = 'paper',
                                        yref = 'paper',
                                        xanchor = 'center',
                                        yanchor = 'top',
                                        font = list(size = 10)
                                      )
                                    )
                                  )
          
          
  # Chart 4. Breakdown of VAT by Sector ---------------------------------------------------------------
  
  
         
  
          vat_sectors_treemapl_tbl<-forecast_sim_cpa%>%
            dplyr::filter(year==SimulationYear)%>%
            dplyr::select(-c("year","Total_Revenues_from_Intermediate_Inputs","Final_Demand_HH","Final_Demand_NPISH","Final_Demand_Government",
                             "scenario"))%>%
            
            melt()
          
          
          vat_treemap_sectors_plt <- plot_ly(
                                          data = vat_sectors_treemapl_tbl, 
                                          type = "treemap", 
                                          values = ~round(value, 0),
                                          labels = ~PRODUCT_INDUSTRY_CODE,
                                          parents = ~variable,  
                                          name = " ",
                                          text = ~PRODUCT_INDUSTRY_NAME,
                                          textinfo = "label+value+percent parent"
                                        ) %>%
                                    layout(
                                      title = paste("Breakdown of VAT by Sector,", SimulationYear),  # Corrected title definition
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
  
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    vat_revenue_plt=vat_revenue_plt,
    vat_revenue_gdp_plt=vat_revenue_gdp_plt,
    vat_structure_plt=vat_structure_plt,
    vat_treemap_sectors_plt=vat_treemap_sectors_plt
    
  )
}

})       

