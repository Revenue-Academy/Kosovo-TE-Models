" Tax Expenditures Dashboard "

# I.Function for Dashboard ------------------------------------------------------------------

Tax_Expenditures_Charts <- function(te_agg,
                                    #choropleth_data,
                                    gender_pit_summary_te,
                                    nace_pit_summary_tbl,
                                    decile_pit_summary_tbl,
                                    forecast_horizon,
                                    SimulationYear) {

 
  # Chart 1. Total Tax Expenditures -----------------------------------------------------------------
       
          te_agg_plt <- plot_ly(
                                te_agg,
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
  
  # Chart 2. Tax Expenditures by Type of Income-----------------------------------------------------------------
         
          # merged_data <- merge(kosovo_regions, merged_df_regions_pit_regions, by.x = "nam", by.y = "regions", all.x = TRUE)
          # 
          # map_pit <- ggplot() +
          #   geom_sf(data = merged_data, aes(fill = TE)) +
          #   scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90") +
          #   theme_void() +
          #   labs(title = "         Distribution by regions") #+
          
          # Choropleth map 
          # choropleth_pit <- ggplot() +
          #             geom_sf(data = choropleth_data, aes(fill = tax_expenditures)) +
          #             scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90") +
          #             theme_void() +
          #             labs(title = "         Distribution by regions") #+
          
          #choropleth_pit<-ggplotly(choropleth_pit)
          
          #theme(legend.position = "bottom")
          
          
          gender_pit_summary_te$Gender<-as.factor(gender_pit_summary_te$Gender)
          
          
          # te_gender_groups_plt <- plot_ly(gender_pit_summary_te, 
          #                                 x = ~Gender , 
          #                                 y = ~tax_expenditures, 
          #                                 type = 'bar', 
          #                                 barmode = 'group',
          #                                 marker = list(color = c('#ff7f0e', '#1f77b4'))) %>%
          #                             layout(
          #                               title = paste("Distribution of Tax Expenditures by Gender,", SimulationYear),
          #                               xaxis = list(title = "Decile", tickmode = 'linear'), # Show all values on the x-axis
          #                               yaxis = list(title = " "),
          #                               annotations = list(
          #                                 list(
          #                                   x = -0.02,
          #                                   y = -0.1,
          #                                   text = "Source: WB staff estimation",
          #                                   showarrow = FALSE,
          #                                   xref = 'paper',
          #                                   yref = 'paper',
          #                                   align = 'left'
          #                                 )
          #                               ))
          
          
          te_gender_groups_plt <- plot_ly(gender_pit_summary_te, 
                                          x = ~Gender, 
                                          y = ~tax_expenditures, 
                                          type = 'bar', 
                                          barmode = 'group',
                                          marker = list(color = c('#ff7f0e', '#1f77b4'))) %>%
            layout(
              title = paste("Distribution of Tax Expenditures by Gender,", SimulationYear),
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
              ))
          
          

  # Chart 3. Tax Expenditures by NACE Section -------------------------------------------------------------------------
  
            # # Get unique years from the data
            # unique_years <- unique(nace_pit_summary_te$year)
            # 
            # # Generate a color palette with as many colors as there are unique years
            # colors <- brewer.pal(length(unique_years), "Set1")
            # 
            # # Create a named vector for colors, mapping each year to a color
            # custom_colors <- setNames(colors, unique_years)
            # 
            # 
            # # Create a grouped bar chart with Plotly and custom colors
            # te_nace_plt <- plot_ly(nace_pit_summary_te, 
            #                        x = ~section, 
            #                        y = ~tax_expenditures, 
            #                        color = ~as.factor(year), 
            #                        colors = custom_colors,
            #                        text = ~description, # Add the description text for hover info
            #                        hoverinfo = 'text+y', # Specify hover info to include text and y value
            #                        type = 'bar', 
            #                        barmode = 'group') %>%
            #                   layout(#title = "Tax Expenditures by NACE Section",
            #                          title = paste("Tax Expenditures by NACE Section,", min(forecast_horizon), "-", max(forecast_horizon)),
            #                          xaxis = list(title = "NACE Section"),
            #                          yaxis = list(title = " "),
            #                          annotations = list(
            #                            list(
            #                              x = -0.02,
            #                              y = -0.1,
            #                              text = "Source: WB staff estimation",
            #                              showarrow = FALSE,
            #                              xref = 'paper',
            #                              yref = 'paper',
            #                              align = 'left'
            #                            )
            #                          ))
            

            
            
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
    treemap_nace_type_plt=treemap_nace_type_plt,
    te_gender_groups_plt=te_gender_groups_plt,
    te_decile_groups_plt=te_decile_groups_plt
  )
}

# CHOROPLETH

# merged_data <- merge(kosovo_regions, merged_df_regions_pit_regions, by.x = "nam", by.y = "regions", all.x = TRUE)
# 
# map_pit <- ggplot() +
#   geom_sf(data = merged_data, aes(fill = TE)) +
#   scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90") +
#   theme_void() +
#   labs(title = "         Distribution by regions") #+
# 
# # Choropleth map
# choropleth_pit <- ggplot() +
#   geom_sf(data = choropleth_data, aes(fill = tax_expenditures)) +
#   scale_fill_gradient(low = "yellow", high = "red", na.value = "grey90") +
#   theme_void() +
#   labs(title = "         Distribution by regions") #+
# 
# choropleth_pit<-ggplotly(choropleth_pit)
# 
# theme(legend.position = "bottom")
# 
# view(merged_df_regions_pit_regions)
        
     