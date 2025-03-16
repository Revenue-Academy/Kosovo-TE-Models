library(plotly)

'ovde da se dode danok po tip pol ili po region'


Revenue_Charts <- function(merged_PIT_BU_SIM,pit_gender_summary,pit_nace_summary,SimulationYear, forecast_horizon) {
  
  # Chart 1. Comparison of PIT Revenues -----------------------------------------------------------------
  PIT_RevenuesTotal_plt <- plot_ly(
                                    merged_PIT_BU_SIM,
                                    x = ~year,
                                    y = ~pitax_bu,
                                    name = "Baseline",
                                    type = 'scatter',
                                    mode = 'lines',
                                    line = list(width = 4, dash = "solid")
                                    ) %>%
                                        add_trace(
                                          x = ~year,
                                          y = ~pitax_sim,
                                          name = 'Simulation',
                                          line = list(width = 4, dash = "dot")
                                      ) %>%
                              layout(
                                title = paste("Total PIT Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
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
  
 
  
  
  # Chart 2. Comparison of PIT Revenues from Capital  ------------------------- 
  # PIT_GrossIncome_plt <- plot_ly(
  #                               merged_PIT_BU_SIM,
  #                               x = ~year,
  #                               y = ~calc_total_inc_bu,
  #                               name = "Baseline",
  #                               type = 'scatter',
  #                               mode = 'lines',
  #                               line = list(width = 4, dash = "solid")
  #                             ) %>%
  #                               # add_trace(
  #                               #   x = ~year,
  #                               #   y = ~calc_total_inc_sim,
  #                               #   name = 'Simulation',
  #                               #   line = list(width = 4, dash = "dot")
  #                               # ) %>%
  #                               layout(
  #                                 title = paste("Total Gross Income,", min(forecast_horizon), "-", max(forecast_horizon)),
  #                                 xaxis = list(title = '', tickformat = 'd'),
  #                                 yaxis = list(title = ' ', rangemode = 'tozero'),
  #                                 annotations = list(
  #                                   x = -0.02,
  #                                   y = -0.1,
  #                                   text = "Source: WB staff estimation",
  #                                   showarrow = FALSE,
  #                                   xref = 'paper',
  #                                   yref = 'paper',
  #                                   align = 'left'
  #                                 )
  #                               )

  pit_gender_summary$Gender<-as.factor(pit_gender_summary$Gender)
  
  
  pit_gender_summary_plt <- plot_ly(pit_gender_summary, 
                                    x = ~Gender , 
                                    y = ~total_calc_pitax_sim, 
                                    type = 'bar', 
                                    barmode = 'group',
                                    marker = list(color = c('#ff7f0e', '#1f77b4'))) %>%
                                layout(
                                  title = paste("Distribution of Tax Revenues by Gender,", SimulationYear),
                                  xaxis = list(title = " ", tickmode = 'linear'), # Show all values on the x-axis
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
  
  
  
  # Chart 3. Comparison of PIT Revenues from Labor and capital  ------------------------- 
  
  # PIT_deduction_plt <- plot_ly(
  #                           merged_PIT_BU_SIM,
  #                           x = ~year,
  #                           y = ~calc_total_ded_bu,
  #                           name = "Baseline",
  #                           type = 'scatter',
  #                           mode = 'lines',
  #                           line = list(width = 4, dash = "solid")
  #                         ) %>%
  #                           add_trace(
  #                             x = ~year,
  #                             y = ~calc_total_ded_sim,
  #                             name = 'Simulation',
  #                             line = list(width = 4, dash = "dot")
  #                           ) %>%
  #                           layout(
  #                             title = paste("TOTAL Deduction ,", min(forecast_horizon), "-", max(forecast_horizon)),
  #                             xaxis = list(title = '', tickformat = 'd'),
  #                             yaxis = list(title = ' ', rangemode = 'tozero'),
  #                             annotations = list(
  #                               x = -0.02,
  #                               y = -0.1,
  #                               text = "Source: WB staff estimation",
  #                               showarrow = FALSE,
  #                               xref = 'paper',
  #                               yref = 'paper',
  #                               align = 'left'
  #                             )
  #                           )
  
  treemap_nace_pit_bu_type_plt <- plot_ly(
                        data = pit_nace_summary, 
                        type = "treemap", 
                        values = ~round(total_calc_pitax_bu, 0), 
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
                            text = paste("Structure of Gross income by NACE sections (Baseline),", SimulationYear),
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
  
  
  
  # 
  # PIT_RevenuesLabor_plt <- plot_ly(
  #   merged_PIT_BU_SIM,
  #   x = ~year,
  #   y = ~((pit_w_bu - (pit_tax_agr_bu))),
  #   name = "Baseline",
  #   type = 'scatter',
  #   mode = 'lines',
  #   line = list(width = 4, dash = "solid")
  # ) %>%
  #   add_trace(
  #     x = ~year,
  #     y = ~((pit_w_sim - (pit_tax_agr_sim))),
  #     name = 'Simulation',
  #     line = list(width = 4, dash = "dot")
  #   ) %>%
  #   layout(
  #     title = paste("PIT Revenues Labor,", min(forecast_horizon), "-", max(forecast_horizon)),
  #     xaxis = list(title = '', tickformat = 'd'),
  #     yaxis = list(title = ' ', rangemode = 'tozero'),
  #     annotations = list(
  #       x = -0.02,
  #       y = -0.1,
  #       text = "Source: WB staff estimation",
  #       showarrow = FALSE,
  #       xref = 'paper',
  #       yref = 'paper',
  #       align = 'left'
  #     )
  #   )
  # 
  # 
  
  
  # df_barchart<-merged_PIT_BU_SIM%>%
  #   select(year,pit_w_bu,pit_c_bu)
  # 
  # 
  # library(tidyr)
  # library(dplyr)
  # 
  # # Assuming df_barchart is your data frame
  # df_long <- df_barchart %>%
  #   pivot_longer(
  #     cols = c(pit_w_bu, pit_c_bu),
  #     names_to = "source of income",
  #     values_to = "value"
  #   ) %>%
  #   mutate(`source of income` = recode(`source of income`, 
  #                                      pit_w_bu = "PIT from labor", 
  #                                      pit_c_bu = "PIT from capital"))
  # 
  #  
  # # Define custom colors for the tax incentive categories
  # custom_colors <- c("PIT from labor" = '#ff7f0e', "PIT from capital" = '#1f77b4')
  # 
  # # Create a stacked bar chart with Plotly and custom colors
  # PIT_RevenuesLabor_plt <- plot_ly(df_long,
  #                                     x = ~year,
  #                                     y = ~round(value, 1),  # Rounding values in the y-axis
  #                                     color = ~`source of income`,  # Corrected column reference with backticks
  #                                     colors = custom_colors,
  #                                     text = ~round(value, 1),  # Rounded values for the hover text
  #                                     hoverinfo = 'text',
  #                                     type = 'bar',
  #                                     barmode = 'stack',
  #                                     textposition = 'inside',  # Position text inside the bars
  #                                     insidetextfont = list(color = 'white')  # Ensure text is readable inside bars
  # ) %>%
  #   layout(
  #     title = paste("Structure of PIT revenues by Type of Income,", min(forecast_horizon), "-", max(forecast_horizon)),
  #     xaxis = list(title = " "),
  #     yaxis = list(title = " "),
  #     barmode = 'stack',
  #     bargap = 0.6,  # Adjust this value to control the bar width
  #     annotations = list(
  #       x = -0.02,
  #       y = -0.1,
  #       text = "Source: WB staff estimation",
  #       showarrow = FALSE,
  #       xref = 'paper',
  #       yref = 'paper',
  #       align = 'left'
  #     )
  #   )
  # 
  
  

  
  # Chart 4. Comparison of PIT Revenues from Wages  ------------------------- 

  # PIT_deduction_plt <- plot_ly(
  #                                   merged_PIT_BU_SIM,
  #                                   x = ~year,
  #                                   y = ~calc_total_ded_bu,
  #                                   name = "Baseline",
  #                                   type = 'scatter',
  #                                   mode = 'lines',
  #                                   line = list(width = 4, dash = "solid")
  #                                 ) %>%
  #                                   add_trace(
  #                                     x = ~year,
  #                                     y = ~calc_total_ded_sim,
  #                                     name = 'Simulation',
  #                                     line = list(width = 4, dash = "dot")
  #                                   ) %>%
  #                                   layout(
  #                                     title = paste("TOTAL Deduction ,", min(forecast_horizon), "-", max(forecast_horizon)),
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

  treemap_nace_pit_sim_type_plt <- plot_ly(
                                data = pit_nace_summary, 
                                type = "treemap", 
                                values = ~round(total_calc_pitax_sim, 0), 
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
                                    text = paste("Structure of Gross income by NACE sections (Simulation),", SimulationYear),
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
                             
  
  
 
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    PIT_RevenuesTotal_plt = PIT_RevenuesTotal_plt,
     pit_gender_summary_plt=pit_gender_summary_plt,
     treemap_nace_pit_bu_type_plt=treemap_nace_pit_bu_type_plt,
    treemap_nace_pit_sim_type_plt=treemap_nace_pit_sim_type_plt,
    
    # Tables
    merged_PIT_BU_SIM = merged_PIT_BU_SIM
  )
}
