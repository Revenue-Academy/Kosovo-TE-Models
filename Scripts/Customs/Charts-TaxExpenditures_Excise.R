" Tax Expenditures Excise Dashboard "

# I.Function for Dashboard ------------------------------------------------------------------

TE_Charts_Excise_fun <- function(ProjectionTE_Excise,summary_TE_SIM_Excise,Legal_Fuels_Cars_Products,
                                     forecast_horizon,SimulationYear) {
  # Check if forecast_horizon is provided and is valid
  if (missing(forecast_horizon) || length(forecast_horizon) != 2) {
    stop("Please provide a valid 'forecast_horizon' with minimum and maximum values.")
  }
  # Chart 1. Total Tax Expenditures Forecast -----------------------------------------------------------------
                    
 
          te_agg_excise_plt <- plot_ly(
                                  ProjectionTE_Excise,
                                            x = ~Year,
                                            y = ~round(tax_expenditures*1e04,1),
                                            name = "Baseline",
                                            type = 'scatter',
                                            mode = 'lines+markers+text',
                                            text = ~round(tax_expenditures/100,1),
                                            textposition = 'top middle',
                                            hoverinfo = 'text+y',
                                            line = list(width = 4, dash = "solid")
                                          ) %>%
                                            layout(
                                              title = paste("Total Tax Expenditures Forecast (EUR MIL),", min(forecast_horizon), "-", max(forecast_horizon)),
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
                                
    

  
  # Chart 2. Distribution of Tax Expenditures by type of excise products -----------------------------------------------------------------
            

          
          summary_TE_SIM_Excise_AGG<-summary_TE_SIM_Excise%>%
            filter(Description %in% c("Tax Expenditures by Mineral Oils",
                                "Tax Expenditures by Tobacco Products",
                                "Tax Expenditures by Alcohol Products",
                                "Tax Expenditures by Cars")
                                  )
          
          # Replace "Tax Expenditures by" with an empty string
          summary_TE_SIM_Excise_AGG$Description <- gsub("Tax Expenditures by", "", summary_TE_SIM_Excise_AGG$Description)
          
          
          
          TE_excise_product_plt <- plot_ly(summary_TE_SIM_Excise_AGG,
                                       x = ~Description, #~reorder(iso3c, -CustomsDuties_TE),  # Reverse order for highest on left
                                       y =  ~round(value,1),
                                      # colors = custom_colors,
                                       hoverinfo = 'text',
                                 hovertext = ~value ,
                                       type = 'bar',
                                       barmode = 'group') %>%
                                  layout(
                                    title = paste("Distribution of Tax Expenditures by type of excise products,", base_year),
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
                

          TE_excise_product_plt1 <- plot_ly(Legal_Fuels_Cars_Products,
                                 x = ~Subdataset, #~reorder(iso3c, -CustomsDuties_TE),  # Reverse order for highest on left
                                 y =  ~round(TE,1),
                                 marker = list(color = '#ff7f0e'),
                                 hoverinfo = 'text',
                                 hovertext = ~TE ,
                                 type = 'bar',
                                 barmode = 'group') %>%
                            layout(
                              title = paste("Distribution of Tax Expenditures by type of excise products,", base_year),
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
            Legal_Fuels_Cars_Products,
            labels = ~Subdataset,
            values = ~TE,
            type = 'pie',
            hole = 0.6,
            textinfo = 'label+percent',
            insidetextorientation = 'radial'
          ) %>%
            layout(
              title = paste("Structure of tax expenditures,", base_year),
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
          
          
          
          
        # TE_excise_pie_plt <- plot_ly(
        #     Legal_Fuels_Cars_Products,
        #                   labels = ~Subdataset,
        #                   values = ~TE,
        #                   type = 'pie',
        #                   hole = 0.6,
        #                   textinfo = 'label+percent',
        #                   insidetextorientation = 'radial'
        #                 ) %>%
        #                   layout(
        #                     title = paste("Structure of tax expenditures,", simulation_year),
        #                     showlegend = FALSE,  # Turn off the legend
        #                     margin = list(l = 20, r = 20, t = 50, b = 20),
        #                     annotations = list(
        #                       x = 0.13,
        #                       y = 0.0,
        #                       text = "Source: WB staff estimation",
        #                       showarrow = FALSE,
        #                       xref = 'paper',
        #                       yref = 'paper',
        #                       xanchor = 'center',
        #                       yanchor = 'top',
        #                       font = list(size = 12)
        #                     )
        #                   )
          
          
          
          # ProductGroups_MTN <- plot_ly(
          #                             CustomsDuties_TE_MTN,
          #                             y = ~reorder(Product_group, CustomsDuties_TE),
          #                             x = ~CustomsDuties_TE,
          #                             type = 'bar',
          #                             text = ' ',
          #                             hoverinfo = 'x+text',
          #                             hovertext = ~Product_group,
          #                             marker = list(color = '#ff7f0e')
          #                           )
          # 
          # ProductGroups_MTN <- ProductGroups_MTN %>%
          #                 layout(
          #                   title = paste("Tax expenditures by MTN Categories,", base_year),
          #                   font = list(size = 11),
          #                   yaxis = list(
          #                     title = '', 
          #                     tickangle = 0,  # Keep the labels horizontal
          #                     automargin = TRUE,  # Ensure enough margin for longer labels
          #                     tickmode = 'linear',
          #                     ticks = "outside",
          #                     standoff = 10  # Add space between the axis and labels
          #                   ),
          #                   xaxis = list(title = ''),
          #                   annotations = list(
          #                     x = -0.02,
          #                     y = -0.1,
          #                     text = "Source: WB staff estimation",
          #                     showarrow = FALSE,
          #                     xref = 'paper',
          #                     yref = 'paper',
          #                     align = 'left'
          #                   )
          #                 )
          #               
              
              
              
             
  
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    te_agg_excise_plt=te_agg_excise_plt,
    TE_excise_product_plt=TE_excise_product_plt,
    TE_excise_product_plt1=TE_excise_product_plt1,
    TE_excise_pie_plt=TE_excise_pie_plt
    
  )
}


