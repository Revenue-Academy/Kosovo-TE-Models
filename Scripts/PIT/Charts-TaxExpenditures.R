" Tax Expenditures Dashboard "

# I.Function for Dashboard ------------------------------------------------------------------

Tax_Expenditures_Charts <- function(te_agg,
                                    #choropleth_data,
                                    #gender_pit_summary_te,
                                    te_agg_type,
                                    nace_pit_summary_tbl,
                                    decile_pit_summary_tbl,
                                    forecast_horizon,
                                    SimulationYear) {

 
  # Chart 1. Total Tax Expenditures -----------------------------------------------------------------
       
          te_agg_plt <- plot_ly(
                                te_agg,
                                x = ~year,
                                y = ~`tax expenditure`*1000,
                                name = "Baseline",
                                type = 'scatter',
                                mode = 'lines+markers+text',
                                text = ~`tax expenditure`,
                                textposition = 'top middle',
                                hoverinfo = 'text+y',
                                line = list(width = 4, dash = "solid"))%>%
                                layout(
                                        title = paste("Total Tax Expenditures (in LCU),", min(forecast_horizon), "-", max(forecast_horizon)),
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
  
  # Chart 2. Tax Expenditures by Type of Income-----------------------------------------------------------------
         
         
          
          # gender_pit_summary_te$Gender<-as.factor(gender_pit_summary_te$Gender)
          # 
          # 
          # 
          # 
          # te_gender_groups_plt <- plot_ly(gender_pit_summary_te, 
          #                                 x = ~Gender, 
          #                                 y = ~tax_expenditures, 
          #                                 type = 'bar', 
          #                                 barmode = 'group',
          #                                 marker = list(color = c('#ff7f0e', '#1f77b4'))) %>%
          #   layout(
          #     title = paste("Distribution of Tax Expenditures by Gender (in LCU),", SimulationYear),
          #     xaxis = list(title = " ", tickmode = 'linear'), # Show all values on the x-axis
          #     yaxis = list(title = " "),
          #     bargap = 0.7, # Adjust this value to make bars thinner
          #     annotations = list(
          #       list(
          #         x = -0.02,
          #         y = -0.1,
          #         text = "Source: WB staff estimation",
          #         showarrow = FALSE,
          #         xref = 'paper',
          #         yref = 'paper',
          #         align = 'left'
          #       )
          #     ))
          
          
          
          #te_agg_type$`tax incentive`<-as.factor(te_agg_type$`tax incentive`)
          
          # te_agg_type$year<-as.factor(te_agg_type$year)
          # 
          # 
          # 
          # te_gender_groups_plt <- plot_ly(te_agg_type, 
          #                                 x = ~`tax incentive`, 
          #                                 y = ~`tax expenditure`, 
          #                                 type = 'bar', 
          #                                 barmode = 'group',
          #                                 marker = list(color = c('#ff7f0e', '#1f77b4'))) %>%
          #   layout(
          #     title = paste("Distribution of Tax Expenditures by Gender (in LCU),", SimulationYear),
          #     xaxis = list(title = " ", tickmode = 'linear'), # Show all values on the x-axis
          #     yaxis = list(title = " "),
          #     bargap = 0.7, # Adjust this value to make bars thinner
          #     annotations = list(
          #       list(
          #         x = -0.02,
          #         y = -0.1,
          #         text = "Source: WB staff estimation",
          #         showarrow = FALSE,
          #         xref = 'paper',
          #         yref = 'paper',
          #         align = 'left'
          #       )
          #     ))
          
          # Create the plot
          te_gender_groups_plt <- plot_ly(te_agg_type, 
                                     x = ~year, 
                                     y = ~`tax expenditure`*1e03, 
                                     type = 'bar', 
                                     color = ~`tax incentive`, 
                                     colors = c('ded_rent' = '#ff7f0e', 'ded_charitable' = '#1f77b4'), 
                                     barmode = 'group') %>%
                                layout(
                                  #title = "Distribution of Tax Expenditures by Type,",
                                  title = paste("Distribution of Tax Expenditures by Type (in LCU),", min(forecast_horizon), "-", max(forecast_horizon)),
                                  
                                  xaxis = list(title = " ", tickmode = 'linear'), # Show all values on the x-axis
                                  yaxis = list(title = " "),
                                  bargap = 0.7, # Adjust this value to make bars thinner
                                  annotations = list(
                                    list(
                                      x = -0.02,
                                      y = -0.1,
                                      text = "Source: WB staff estimation",
                                      showarrow = FALSE,
                                      xref = 'paper',
                                      yref = 'paper',
                                      align = 'left'
                                    )
                                  )
                                )
          
          

  # Chart 3. Tax Expenditures by NACE Section -------------------------------------------------------------------------
  
           
            
            treemap_nace_type_plt <- plot_ly(
                                            data = nace_pit_summary_tbl, 
                                            type = "treemap", 
                                            values = ~round(tax_expenditures, 1), 
                                            labels = ~section,
                                            parents = ~value,  
                                            name = " ",
                                            text = ~value,
                                            hovertext = ~description,
                                            textinfo = "label+value+percent parent",
                                            hoverinfo = "label+value+percent parent+text"
                                          ) %>%
                                            layout(
                                              title = list(
                                                #text = "Structure of Gross income by NACE sections",
                                                text = paste("Structure of Tax Expenditures by NACE sections (in LCU),", SimulationYear),
                                                font = list(size = 14)  # Set the font size here
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
                                              )
                                            )
            
  
  # Chart 4. Tax Expenditures by Decile Groups---------------------------------------------------------------

          
          # Create a grouped bar chart with Plotly and custom colors
          te_decile_groups_plt <- plot_ly(decile_pit_summary_tbl, 
                                          x = ~decile_group, 
                                          y = ~tax_expenditures, 
                                         # color = ~as.factor(year), 
                                         # colors = custom_colors,
                                          hoverinfo = 'text+y', # Specify hover info to include text and y value
                                          type = 'bar', 
                                          barmode = 'group') %>%
                                    layout(#title = "Tax Expenditures by Decile Groups",
                                           #title = paste("Tax Expenditures by Decile Groups,", min(forecast_horizon), "-", max(forecast_horizon)),
                                      title = paste("Tax Expenditures by Decile Groups (in LCU),", SimulationYear),
                                           xaxis = list(title = "Decile", tickmode = 'linear'), # Show all values on the x-axis
                                           yaxis = list(title = " "),
                                           annotations = list(
                                             list(
                                               x = -0.02,
                                               y = -0.1,
                                               text = "Source: WB staff estimation",
                                               showarrow = FALSE,
                                               xref = 'paper',
                                               yref = 'paper',
                                               align = 'left'
                                             )
                                           ))
          
        
  
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    te_agg_plt=te_agg_plt,
    treemap_nace_type_plt=treemap_nace_type_plt,
    te_gender_groups_plt=te_gender_groups_plt,
    te_decile_groups_plt=te_decile_groups_plt
  )
}

     