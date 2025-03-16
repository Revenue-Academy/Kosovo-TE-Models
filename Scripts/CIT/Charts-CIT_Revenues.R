#library(plotly)


Revenue_Charts_fun <- function(merged_CIT_BU_SIM,
                                 nace_cit_big_corporations_summary_long,
                                 nace_cit_small_corporations_summary_long,
                                 SimulationYear, 
                                 forecast_horizon) {
  
  # Chart 1. Comparison of CIT Revenues (Big corporations) -----------------------------------------------------------------
  CIT_big_corporations_rev_plt <- plot_ly(
                                    merged_CIT_BU_SIM,
                                    x = ~year,
                                    y = ~calc_cit_bu,
                                    name = "Baseline",
                                    type = 'scatter',
                                    mode = 'lines',
                                    line = list(width = 4, dash = "solid")
                                    ) %>%
                                        add_trace(
                                          x = ~year,
                                          y = ~calc_cit_sim,
                                          name = 'Simulation',
                                          line = list(width = 4, dash = "dot")
                                      ) %>%
                              layout(
                                title = paste("Total CIT Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
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
  
 
  # Chart 2. Comparison of CIT Revenues (Small corporations)  ------------------------- 
  
  CIT_small_corporations_rev_plt <- plot_ly(
                                            merged_CIT_BU_SIM,
                                            x = ~year,
                                            y = ~totax_bu,
                                            name = "Baseline",
                                            type = 'scatter',
                                            mode = 'lines',
                                            line = list(width = 4, dash = "solid")
                                          ) %>%
                                            add_trace(
                                              x = ~year,
                                              y = ~totax_sim,
                                              name = 'Simulation',
                                              line = list(width = 4, dash = "dot")
                                            ) %>%
                                            layout(
                                              title = paste("ToT Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
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
                                          
  
  # Chart 3.  Revenues by NACE sections (Big corporations)  ------------------------- 
  
  custom_colors <- c('#1f77b4', '#ff7f0e')  
  
  CIT_nace_big_corporations_plt <- plot_ly(nace_cit_big_corporations_summary_long, 
                                      x = ~section,        
                                      y = ~value,               
                                      color = ~category,        
                                      colors = custom_colors,   
                                      text = ~round(value, 2),  
                                      hoverinfo = 'text',       
                                      hovertext = ~paste("Sector: ", description, 
                                                         "<br>Category: ", category, 
                                                         "<br>Value: ", round(value, 2)),  
                                      type = 'bar', 
                                      barmode = 'group') %>%    
                                  layout(
                                    #title = paste("Profit Tax by NACE Section (in AMD bn),", min(legal_type_cit_summary$year), "-", max(legal_type_cit_summary$year)),
                                    title = paste("CIT Revenues by NACE Sectors (in EUR),",SimulationYear ),
                                    xaxis = list(title = " ", tickmode = 'linear'),  
                                    yaxis = list(title = " "),
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
  
  
  
    # Chart 4. Revenues by NACE sections (Small corporations) ------------------------- 

  custom_colors <- c('#1f77b4', '#ff7f0e')  
  
  CIT_nace_small_corporations_plt <- plot_ly(nace_cit_small_corporations_summary_long, 
                                           x = ~section,        
                                           y = ~value,               
                                           color = ~category,        
                                           colors = custom_colors,   
                                           text = ~round(value, 2),  
                                           hoverinfo = 'text',       
                                           hovertext = ~paste("Sector: ", description, 
                                                              "<br>Category: ", category, 
                                                              "<br>Value: ", round(value, 2)),  
                                           type = 'bar', 
                                           barmode = 'group') %>%    
                                      layout(
                                        #title = paste("Profit Tax by NACE Section (in AMD bn),", min(legal_type_cit_summary$year), "-", max(legal_type_cit_summary$year)),
                                        title = paste("ToT Revenues by NACE Sectors (in EUR),",SimulationYear ),
                                        xaxis = list(title = " ", tickmode = 'linear'),  
                                        yaxis = list(title = " "),
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
  
 
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    CIT_big_corporations_rev_plt = CIT_big_corporations_rev_plt,
    CIT_small_corporations_rev_plt=CIT_small_corporations_rev_plt,
    CIT_nace_big_corporations_plt=CIT_nace_big_corporations_plt,
    CIT_nace_small_corporations_plt=CIT_nace_small_corporations_plt

  )
}
