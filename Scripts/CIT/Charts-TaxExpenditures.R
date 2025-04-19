" Tax Expenditures Dashboard "

# I.Function for Dashboard ------------------------------------------------------------------

Tax_Expenditures_Charts_fun <- function(
                                    te_summary_df,
                                    company_type_cit_summary_te,
                                    nace_cit_summary_tbl,
                                    te_summary_df_type,
                                    forecast_horizon,
                                    SimulationYear) {

 
  # Chart 1. Total Tax Expenditures -----------------------------------------------------------------
       

          te_agg_plt <- plot_ly(
                               te_summary_df,
                                x = ~year,
                                y = ~`tax expenditure`*1e06,
                                name = "Baseline",
                                type = 'scatter',
                               #mode = 'lines+markers',
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
  
  # Chart 2. Distribution of Tax Expenditures by type of companies-----------------------------------------------------------------

          
          # te_type_companies_plt <- plot_ly(
          #                 company_type_cit_summary_te,
          #                 y = ~reorder(name_en, tax_expenditures),
          #                 x = ~tax_expenditures,
          #                 type = 'bar',
          #                 text = ' ',
          #                 hoverinfo = 'x+text',
          #                 hovertext = ~name_en,
          #                 marker = list(color = '#1f77b4')
          #                    )
          # 
          # 
          # te_type_companies_plt <- te_type_companies_plt %>%
          #   layout(
          #     #title = paste("Tax expenditures by MTN Categories,", SimulationYear),
          #     title = paste("Distribution of Tax Expenditures by type of companies (in LCU),", SimulationYear),
          #     font = list(size = 11),
          #     yaxis = list(
          #       title = '', 
          #       tickangle = 0,  # Keep the labels horizontal
          #       automargin = TRUE,  # Ensure enough margin for longer labels
          #       tickmode = 'linear',
          #       ticks = "outside",
          #       standoff = 10  # Add space between the axis and labels
          #     ),
          #     xaxis = list(title = ''),
          #     bargap = 0.7,  # Adjust this value to make bars thinner
          #     annotations = list(
          #       #x = -0.02,
          #       #x = -0.6,
          #       x = -0.4,
          #       y = -0.1,
          #       text = "Source: WB staff estimation",
          #       showarrow = FALSE,
          #       xref = 'paper',
          #       yref = 'paper',
          #       align = 'left'
          #     )
          #   )
          # 
          ###
          
          # te_type_companies_plt <- plot_ly(
          #                               #pit_result_bins_sim_sub_plt, 
          #                                company_type_cit_summary_te,
          #                               labels = ~name_en,
          #                               values = ~tax_expenditures,
          #                               # labels = ~`gross income`, 
          #                               #  values = ~`tax liability`,
          #                               type = 'pie', 
          #                               hole = 0.6,  
          #                               textinfo = 'label+percent',
          #                               insidetextorientation = 'radial'
          #                             ) %>%
          #                               layout(
          #                                 #title = paste("Contribution to Tax Revenues by Income Groups (Simulation),", SimulationYear),
          #                                 title = paste("Distribution of Tax Expenditures by type of companies (in LCU),", SimulationYear),
          #                                 showlegend = FALSE,  # Turn off the legend
          #                                 margin = list(l = 20, r = 20, t = 50, b = 20),
          #                                 annotations = list(
          #                                   x = 0.13,
          #                                   y = 0.0,
          #                                   text = "Source: WB staff estimation",
          #                                   showarrow = FALSE,
          #                                   xref = 'paper',
          #                                   yref = 'paper',
          #                                   xanchor = 'center',
          #                                   yanchor = 'top',
          #                                   font = list(size = 12)
          #                                 )
          #                               )
          
          te_type_companies_plt <- plot_ly(
            company_type_cit_summary_te,
            labels = ~name_en,
            values = ~tax_expenditures,
            type = 'pie',
            hole = 0.6,
            textinfo = 'label+percent',
            insidetextorientation = 'radial',
            rotation = 80,
            textfont = list(size = 10)  # <- smaller font size
          ) %>%
            layout(
              title = paste("Distribution of Tax Expenditures by type of companies (in LCU),", SimulationYear),
              showlegend = FALSE,
              margin = list(l = 20, r = 20, t = 50, b = 20),
              annotations = list(
                x = 0.13,
                y = 0.0,
                text = "Source: WB staff estimation",
                showarrow = FALSE,
                xref = 'paper',
                yref = 'paper',
                xanchor = 'center',
                yanchor = 'top',
                font = list(size = 12)
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
                                                text = paste("Structure of Gross income by NACE sections (in LCU),", SimulationYear),
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
         te_type_plt <- plot_ly(te_summary_df_type, 
                                 x = ~year, 
                                 y = ~`tax expenditure` * 1e03, 
                                 type = 'bar', 
                                 color = ~type, 
                                 colors = c('Special allowance for new assets' =   '#d62728',
                                            'Charitable contribution' =  '#e377c2',
                                            'Discounts for sponsorship in the field of sports' =  '#1f77b4', 
                                            'Discounts for sponsorship in the field of culture and youth' = '#ff7f0e'
                                 )) %>%
            layout(
              title = paste("Distribution of Tax Expenditures by Type (in LCU),", min(forecast_horizon), "-", max(forecast_horizon)),
              xaxis = list(title = " ", tickmode = 'linear'),
              yaxis = list(title = " "),
              barmode = 'stack',  # <- changed from 'group' to 'stack'
              bargap = 0.7,
              legend = list(
                orientation = 'h',
                x = 0, 
                y = -0.2
              ),
              annotations = list(
                list(
                  x = -0.03,
                  y = -0.2,
                  text = "Source: WB staff estimation",
                  showarrow = FALSE,
                  xref = 'paper',
                  yref = 'paper',
                  align = 'left'
                )
              )
            )
          
          
          
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    te_agg_plt=te_agg_plt,
    te_type_companies_plt=te_type_companies_plt,
    treemap_nace_type_plt=treemap_nace_type_plt,
    te_type_plt=te_type_plt
  )
}


        
     