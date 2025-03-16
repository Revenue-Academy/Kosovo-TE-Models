" Tax Expenditures Dashboard "

# I.Function for Dashboard ------------------------------------------------------------------

Tax_Expenditures_Charts_fun <- function(
                                    te_summary_df,
                                    company_type_cit_summary_te,
                                    nace_cit_summary_tbl,
                                    nace_cit_summary_te_deciles,
                                    forecast_horizon,
                                    SimulationYear) {

 
  # Chart 1. Total Tax Expenditures -----------------------------------------------------------------
       

          te_agg_plt <- plot_ly(
                               te_summary_df,
                                x = ~year,
                                y = ~`tax expenditure`,
                                name = "Baseline",
                                type = 'scatter',
                                mode = 'lines+markers+text',
                                text = ~`tax expenditure`,
                                textposition = 'top middle',
                                hoverinfo = 'text+y',
                                line = list(width = 4, dash = "solid"))%>%
                                layout(
                                        title = paste("Total Tax Expenditures,", min(forecast_horizon), "-", max(forecast_horizon)),
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
  
  # Chart 2. Distribution of Tax Expenditures by type of companies-----------------------------------------------------------------

          # custom_colors <- c('#1f77b4', '#ff7f0e')  
          # 
          # te_type_companies_plt <- plot_ly(company_type_cit_summary_te, 
          #                              x = ~reorder(name_en, -tax_expenditures),  # Reverse order for highest on left
          #                              y = ~tax_expenditures,               
          #                              colors = custom_colors,   
          #                              hoverinfo = 'text', 
          #                              hovertext = ~name_en ,
          #                              type = 'bar', 
          #                              barmode = 'group') %>%
          #                         layout(
          #                           title = paste("Distribution of Tax Expenditures by type of companies,", SimulationYear),
          #                           xaxis = list(title = " ", tickmode = 'linear'),  
          #                           yaxis = list(title = " "),
          #                           annotations = list(
          #                             list(
          #                               x = -0.02,
          #                               y = -0.1,
          #                               text = "Source: WB staff estimation",
          #                               showarrow = FALSE,
          #                               xref = 'paper',
          #                               yref = 'paper',
          #                               align = 'left',
          #                               font = list(size = 8)  
          #                             )
          #                           ))
          
          te_type_companies_plt <- plot_ly(
            company_type_cit_summary_te,
            y = ~reorder(name_en, tax_expenditures),
            x = ~tax_expenditures,
            type = 'bar',
            text = ' ',
            hoverinfo = 'x+text',
            hovertext = ~name_en,
            marker = list(color = '#ff7f0e')
          )
          
          te_type_companies_plt <- te_type_companies_plt %>%
            layout(
              #title = paste("Tax expenditures by MTN Categories,", SimulationYear),
              title = paste("Distribution of Tax Expenditures by type of companies,", SimulationYear),
              font = list(size = 11),
              yaxis = list(
                title = '', 
                tickangle = 0,  # Keep the labels horizontal
                automargin = TRUE,  # Ensure enough margin for longer labels
                tickmode = 'linear',
                ticks = "outside",
                standoff = 10  # Add space between the axis and labels
              ),
              xaxis = list(title = ''),
              annotations = list(
                #x = -0.02,
                x = -0.6,
                y = -0.1,
                text = "Source: WB staff estimation",
                showarrow = FALSE,
                xref = 'paper',
                yref = 'paper',
                align = 'left'
              )
            )
          
         
  # Chart 3. Tax Expenditures by NACE Section -------------------------------------------------------------------------
            
            treemap_nace_type_plt <- plot_ly(
                                            data = nace_cit_summary_tbl, 
                                            type = "treemap", 
                                            values = ~round(tax_expenditures, 0), 
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
                                                text = paste("Structure of Gross income by NACE sections,", SimulationYear),
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
          te_decile_groups_plt <- plot_ly(nace_cit_summary_te_deciles, 
                                          x = ~decile_group, 
                                          y = ~tax_expenditures, 
                                         # color = ~as.factor(year), 
                                         # colors = custom_colors,
                                          hoverinfo = 'text+y', # Specify hover info to include text and y value
                                          type = 'bar', 
                                          barmode = 'group') %>%
                                    layout(#title = "Tax Expenditures by Decile Groups",
                                           #title = paste("Tax Expenditures by Decile Groups,", min(forecast_horizon), "-", max(forecast_horizon)),
                                      title = paste("Tax Expenditures by Decile Groups,", SimulationYear),
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
    te_type_companies_plt=te_type_companies_plt,
    treemap_nace_type_plt=treemap_nace_type_plt,
    te_decile_groups_plt=te_decile_groups_plt
  )
}


        
     