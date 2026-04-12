" Tax Expenditures Excise Dashboard "

# I.Function for Dashboard ------------------------------------------------------------------

TE_Charts_Excise_fun <- function(ProjectionTE_Excise,summary_TE_SIM_Excise,Legal_Fuels_Cars_Products,
                                     forecast_horizon,SimulationYear) {
  # Check if forecast_horizon is provided and is valid
  if (missing(forecast_horizon) || length(forecast_horizon) != 2) {
    stop("Please provide a valid 'forecast_horizon' with minimum and maximum values.")
  }
  # Chart 1. Total Tax Expenditures Forecast -----------------------------------------------------------------
                    
 
          # te_agg_excise_plt <- plot_ly(
          #                         ProjectionTE_Excise%>%filter(tax_expenditures>0),
          #                                   x = ~Year,
          #                                   y = ~round(tax_expenditures*1e06,1),
          #                                   name = "Baseline",
          #                                   type = 'scatter',
          #                                   mode = 'lines+markers+text',
          #                                   text = ~round(tax_expenditures,1),
          #                                   textposition = 'top middle',
          #                                   hoverinfo = 'text+y',
          #                                   line = list(width = 4, dash = "solid")
          #                                 ) %>%
          #                                   layout(
          #                                     title = paste("Total Tax Expenditures (EUR MIL),", min(forecast_horizon), "-", 2025),
          #                                     xaxis = list(title = '', tickformat = 'd'),
          #                                     yaxis = list(title = ' ', rangemode = 'tozero'),
          #                                     annotations = list(
          #                                       x = -0.02,
          #                                       y = -0.1,
          #                                       text = "Source: WB staff estimation",
          #                                       showarrow = FALSE,
          #                                       xref = 'paper',
          #                                       yref = 'paper',
          #                                       align = 'left'
          #                                     )
          #                                   )
                                
  ProjectionTE_Excise_plot <- ProjectionTE_Excise %>%
    filter(tax_expenditures > 0) %>%
    arrange(Year)
  
  te_agg_excise_plt <- plot_ly(
    data = ProjectionTE_Excise_plot,
    x = ~Year,
    y = ~round(tax_expenditures * 1e06, 1),
    name = "Baseline",
    type = "scatter",
    mode = "lines+markers+text",
    text = ~round(tax_expenditures, 1),
    textposition = "top middle",
    hovertemplate = paste(
      "Year: %{x}<br>",
      "Tax expenditures: %{text} EUR mil<extra></extra>"
    ),
    line = list(width = 4, dash = "solid"),
    marker = list(size = 8)
  ) %>%
    layout(
      title = paste(
        "Total Tax Expenditures (EUR MIL),",
        min(forecast_horizon),
        "-",
        2025
      ),
      xaxis = list(
        title = "",
        tickmode = "linear",
        tick0 = min(ProjectionTE_Excise_plot$Year, na.rm = TRUE),
        dtick = 1
      ),
      yaxis = list(
        title = " ",
        rangemode = "tozero"
      ),
      annotations = list(
        x = -0.02,
        y = -0.1,
        text = "Source: WB staff estimation",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        align = "left"
      )
    )
    

  
  # Chart 2. Distribution of Tax Expenditures by type of excise products -----------------------------------------------------------------
            

          
          summary_TE_SIM_Excise_AGG<-summary_TE_SIM_Excise%>%
            filter(Description %in% c("Tax Expenditures by Mineral Oils",
                                "Tax Expenditures by Tobacco Products",
                                "Tax Expenditures by Alcohol Products",
                                "Tax Expenditures by Cars")
                                  )
          
          # Replace "Tax Expenditures by" with an empty string
          summary_TE_SIM_Excise_AGG$Description <- gsub("Tax Expenditures by", "", summary_TE_SIM_Excise_AGG$Description)
          
          
          summary_TE_SIM_Excise_AGG <- summary_TE_SIM_Excise_AGG %>%
            pivot_longer(
                        cols = c(`2023`, `2024`, `2025`),
                        names_to = "Year",
                        values_to = "value"
                      )%>%
            filter(Year==SimulationYear)
          
          
          
          TE_excise_product_plt <- plot_ly(summary_TE_SIM_Excise_AGG,
                                       x = ~Description, #~reorder(iso3c, -CustomsDuties_TE),  # Reverse order for highest on left
                                       y =  ~round(value,1),
                                      # colors = custom_colors,
                                       hoverinfo = 'text',
                                 hovertext = ~value ,
                                       type = 'bar',
                                       barmode = 'group') %>%
                                  layout(
                                    title = paste("Distribution of Tax Expenditures by type of excise products,", SimulationYear),
                                    xaxis = list(title = " ", tickmode = 'linear'),
                                    yaxis = list(title = " "),
                                    bargap = 0.6,
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
                                    ))

       
  
  # Chart 3. Distribution of Tax Expenditures by type of excise products -------------------------------------------------------------------------
                

          TE_excise_product_plt1 <- plot_ly(Legal_Fuels_Cars_Products%>%
                                              filter(Year==SimulationYear),
                                 x = ~Subdataset, #~reorder(iso3c, -CustomsDuties_TE),  # Reverse order for highest on left
                                 y =  ~round(TE,1),
                                 marker = list(color = '#ff7f0e'),
                                 hoverinfo = 'text',
                                 hovertext = ~TE ,
                                 type = 'bar',
                                 barmode = 'group') %>%
                            layout(
                              title = paste("Distribution of Tax Expenditures by type of excise products,", SimulationYear),
                              xaxis = list(title = " ", tickmode = 'linear'),
                              yaxis = list(title = " "),
                              bargap = 0.6,
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
                              ))
          
          
  # Chart 4. Structure of tax expenditures ---------------------------------------------------------------
  
          
          TE_excise_pie_plt <- plot_ly(
            Legal_Fuels_Cars_Products%>%
              filter(Year==SimulationYear),
            labels = ~Subdataset,
            values = ~TE,
            type = 'pie',
            hole = 0.6,
            textinfo = 'label+percent',
            insidetextorientation = 'radial'
          ) %>%
            layout(
              title = paste("Structure of tax expenditures,", SimulationYear),
              showlegend = TRUE,  # Enable the legend
              legend = list(
                orientation = 'v',  # Vertical orientation (default)
                x = 1.05,  # Move the legend slightly to the right of the chart
                y = 0.5,  # Center the legend vertically
                xanchor = 'left',
                yanchor = 'middle',
                font = list(size = 10)  # Adjust font size
              ),
              startangle = 90,  # Rotate the pie chart by 90 degrees clockwise
              margin = list(l = 20, r = 20, t = 50, b = 50),  # Leave space for the annotation
              annotations = list(
                list(
                  x = 1.0,  # Keep the annotation centered below the chart
                  y = 0.0,  # Move the annotation further down to ensure labels don't overlap
                  text = "Source: WB staff estimation",
                  showarrow = FALSE,
                  xref = 'paper',
                  yref = 'paper',
                  xanchor = 'center',
                  yanchor = 'top',
                  font = list(size = 12)
                )
              )
            )
          
      
  
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    te_agg_excise_plt=te_agg_excise_plt,
    TE_excise_product_plt=TE_excise_product_plt,
    TE_excise_product_plt1=TE_excise_product_plt1,
    TE_excise_pie_plt=TE_excise_pie_plt
    
  )
}


