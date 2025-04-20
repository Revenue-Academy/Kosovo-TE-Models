" Strucuture of Gross Income "

# Define custom colors
custom_colors <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
# Infoboxes colors: # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.

# I.Function for Dashboard ------------------------------------------------------------------

Structure_GrossIncome_Charts_fun <- function(gross_income_BU_SIM,
                                         labor_capital_type,
                                         gross_nace_tbl, forecast_horizon) {

# I.Total Gross income -------------------------------------------------------------------------

  
                    # Define custom colors for the gross income categories
                   # custom_colors <- c("labor" = '#1f77b4', "capital" = '#ff7f0e')
                    
                    gross_inc_plt <- plot_ly(
                                        gross_income_BU_SIM,
                                        x = ~year,
                                        y = ~calc_total_inc_sim*1e06,
                                        name = "Baseline",
                                        type = 'scatter',
                                        mode = 'lines+markers+text',
                                        #text = ~round(calc_total_inc_sim,0),
                                        textposition = 'top middle',
                                        hoverinfo = 'text+y',
                                        line = list(width = 4, dash = "solid")
                                      ) %>%
                                        layout(
                                          title = paste("Total Gross Income (in LCU),", min(forecast_horizon), "-", max(forecast_horizon)),
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
                    
                

# II. Chart Type of Income --------------------------------

                  
                    
                    color_mapping <- c(
                      "forestgreen",       # keep for a deep green
                      "deeppink",          # replaces "chartreuse" (much more contrast)
                      "cyan",              # keep
                      "brown",             # keep
                      "purple",            # keep
                      "orange",            # keep
                      "darkturquoise",     # keep
                      "#1f77b4",           # Plotly blue
                      "red",               # keep
                      "gold",              # replaces second "chartreuse" with warmer contrast
                      "mediumslateblue"    # replaces "yellow" with a clearer purple-blue
                    )
                    
                    
                    # Create a stacked bar chart with Plotly and custom colors
                    labor_capital_type_plt <- plot_ly(labor_capital_type, 
                                                 x = ~decile_group, 
                                                 y = ~value, 
                                                 color = ~gross_income, 
                                                 colors = color_mapping,
                                                 type = 'bar', 
                                                 barmode = 'stack',
                                                 textposition = 'inside',  # Position text inside the bars
                                                 insidetextfont = list(color = 'white')  # Ensure text is readable inside bars
                                                ) %>%
                                                  layout(title = "Total Gross Income by Decile Group and Type of Income",
                                                         xaxis = list(
                                                           title = " ",
                                                           tickvals = labor_capital_type$decile_group,
                                                           ticktext = labor_capital_type$decile_group
                                                         ),
                                                         yaxis = list(title = " "),
                                                         barmode = 'stack',
                                                         annotations = list(
                                                           x = -0.02,
                                                           y = -0.1,
                                                           text = "Source: WB staff estimation",
                                                           showarrow = FALSE,
                                                           xref = 'paper',
                                                           yref = 'paper',
                                                           align = 'left'
                                                         ))

                   
                    
# III. Chart Treemap GROSS INCOME -------------------------------------------------------
    
                    
                    treemap_labor_capital_type_plt <- plot_ly(data = long_labor_capital_type, 
                                                        type = "treemap", 
                                                        values = ~round(value/1e06,1), 
                                                        labels = ~income_type,
                                                        parents = ~TypeOfIncome,  
                                                        name = " ",
                                                        text = ~TypeOfIncome,
                                                        textinfo = "label+value+percent parent")%>%
                                                    layout(
                              
                                                      title = list(
                                                        text = "Structure of Gross income by Type of Income",
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
                                                      ))
                                                  
                    #treemap_labor_capital_type_plt
                    
                    

# IV. Structure of gross income by NACE sections-------------------------------------------

                    treemap_nace_type_plt <- plot_ly(
                                                data = gross_nace_tbl, 
                                                type = "treemap", 
                                                values = ~round(total_inc/1e06, 1), 
                                                labels = ~nace_section,
                                                parents = ~TypeOfIncome,  
                                                name = " ",
                                                text = ~TypeOfIncome,
                                                hovertext = ~description,
                                                textinfo = "label+value+percent parent",
                                                hoverinfo = "label+value+percent parent+text"
                                              ) %>%
                                                layout(
                                                  title = list(
                                                    text = "Structure of Gross income by NACE sections",
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
                      gross_inc_plt=gross_inc_plt,
                      labor_capital_type_plt=labor_capital_type_plt,
                      treemap_labor_capital_type_plt=treemap_labor_capital_type_plt,
                      treemap_nace_type_plt=treemap_nace_type_plt

                    )
}      