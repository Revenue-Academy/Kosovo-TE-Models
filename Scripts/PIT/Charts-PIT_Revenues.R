library(plotly)

'ovde da se dode danok po tip pol ili po region'


Revenue_Charts <- function(merged_PIT_BU_SIM,pit_gender_summary,pit_nace_summary,SimulationYear, forecast_horizon) {
  
  # Chart 1. Comparison of PIT Revenues -----------------------------------------------------------------
  PIT_RevenuesTotal_plt <- plot_ly(
                                    merged_PIT_BU_SIM,
                                    x = ~year,
                                    y = ~pitax_bu*1e06,
                                    name = "Baseline",
                                    type = 'scatter',
                                    mode = 'lines',
                                    line = list(width = 4, dash = "solid")
                                    ) %>%
                                        add_trace(
                                          x = ~year,
                                          y = ~pitax_sim*1e06,
                                          name = 'Simulation',
                                          line = list(width = 4, dash = "dot")
                                      ) %>%
                              layout(
                                title = paste("PIT Revenues (in LCU),", min(forecast_horizon), "-", max(forecast_horizon)),
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
  
 
  
  
  # Chart 2. Distribution of Tax Revenues by Gender ------------------------- 
  
  pit_gender_summary$Gender<-as.factor(pit_gender_summary$Gender)
  
  pit_gender_summary<-pit_gender_summary%>%
    dplyr::filter(Gender %in% c("F","M"))
  
  # pit_gender_summary_plt <- plot_ly(pit_gender_summary, 
  #                                   x = ~Gender , 
  #                                   y = ~total_calc_pitax_sim, 
  #                                   type = 'bar', 
  #                                   barmode = 'group',
  #                                   marker = list(color = c('#ff7f0e', '#1f77b4'))) %>%
  #                               layout(
  #                                 title = paste("Distribution of Tax Revenues by Gender,", SimulationYear),
  #                                 xaxis = list(title = " ", tickmode = 'linear'), # Show all values on the x-axis
  #                                 yaxis = list(title = " "),
  #                                 annotations = list(
  #                                   list(
  #                                     x = -0.02,
  #                                     y = -0.1,
  #                                     text = "Source: WB staff estimation",
  #                                     showarrow = FALSE,
  #                                     xref = 'paper',
  #                                     yref = 'paper',
  #                                     align = 'left'
  #                                   )
  #                                 ))
  
  pit_gender_summary_plt <- plot_ly(
    pit_gender_summary, 
    x = ~Gender, 
    y = ~total_calc_pitax_sim, 
    type = 'bar', 
    barmode = 'group',
    marker = list(color = c('#ff7f0e', '#1f77b4')),
    width = 0.6  # Optional: makes bars narrower
  ) %>%
    layout(
      title = paste("Distribution of Tax Revenues by Gender,", SimulationYear),
      xaxis = list(title = " ", tickmode = 'linear'),
      yaxis = list(title = " "),
      bargap = 0.6,       # gap between groups (0-1)
      bargroupgap = 0.2,  # gap between bars within a group (0-1)
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
  
  
  
  # Chart 3. Comparison of PIT Revenues from Labor and capital  ------------------------- 

  treemap_nace_pit_bu_type_plt <- plot_ly(
                        data = pit_nace_summary, 
                        type = "treemap", 
                        values = ~round(total_calc_pitax_bu/1e06, 0), 
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
                            text = paste("Structure of PIT by NACE sections (Baseline),", SimulationYear),
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
  
  
  
  
  
  # Chart 4. Comparison of PIT Revenues from Wages  ------------------------- 

  
  treemap_nace_pit_sim_type_plt <- plot_ly(
                                data = pit_nace_summary, 
                                type = "treemap", 
                                values = ~round(total_calc_pitax_sim/1e06, 0), 
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
                                    text = paste("Structure of PIT by NACE sections (Simulation),", SimulationYear),
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
