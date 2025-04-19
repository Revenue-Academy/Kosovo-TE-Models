" Distribution Dashboard "
# I.Function for Dashboard ------------------------------------------------------------------
Distribution_Charts_fun <- function(cit_centile_distribution_bu_sim,
                                    cit_decile_distribution_bu_sim,
                                    cit_result_bins_bu_sub,
                                    cit_result_bins_sim_sub,
                                    SimulationYear,
                                    forecast_horizon) {

  # Chart 1. Centile Groups -----------------------------------------------------------------
       
  
  dist_centile_groups_plt <- plot_ly(cit_centile_distribution_bu_sim, x = ~centile_group, y = ~etr_bu, name = "Baseline", type = 'scatter', mode = 'lines',
                 line = list(width = 4,dash = "solid"))
  
    
  
  dist_centile_groups_plt <- dist_centile_groups_plt %>%
    add_trace(y = ~etr_sim, name = "Simulation", line = list(width = 4, dash = "dash")) %>%
    layout(
      title = paste("Effective Tax Rate by Percentile Groups,", SimulationYear),
      xaxis = list(
        title = 'Percentile',
        tickvals = seq(10, 100, by = 10),
        ticktext = seq(10, 100, by = 10)
      ),
      yaxis = list(title = ' '),
      annotations = list(
        list(
          x = -0.02,
          y = -0.13,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = 'paper',
          yref = 'paper',
          align = 'left'
        )
      )
    )
  


  
  # Chart 2. Decile Groups-----------------------------------------------------------------
  

            # Define custom colors
            custom_colors <- c('#1f77b4', '#ff7f0e')
            
            # Adapt the Plotly chart
            dist_decile_groups_plt <- plot_ly(
                                              #pit_decile_distribution_bu_sub, 
                                              cit_decile_distribution_bu_sim,
                                              #x = ~decile_group,
                                              x = ~as.numeric(`Decile groups`), 
                                              y = ~ `Total CIT liability (business as usual) in MIL`, 
                                              name = 'Baseline',
                                              marker = list(color = custom_colors[1]),
                                              hoverinfo = 'text+y', 
                                              type = 'bar', 
                                              barmode = 'group') %>%
                                      add_trace(y = ~`Total CIT liability (simulation) in MIL`, 
                                                name = 'Simulation', 
                                                marker = list(color = custom_colors[2]),
                                                hoverinfo = 'text+y') %>%
                                      layout(title = paste("Tax Revenue by Decile Groups,", SimulationYear),
                                             xaxis = list(title = "Decile", tickmode = 'linear'), 
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
            
          

  # Chart 3. Tax Revenue by Bin Groups-BU -------------------------------------------------------------------------

            cit_bins_bu_sub_plt <- plot_ly(
                                          cit_result_bins_bu_sub,
                                          labels = ~bin_group,
                                          values = ~sum_calc_citax  ,
                                          type = 'pie',
                                          hole = 0.6,
                                          textinfo = 'label+percent',
                                          insidetextorientation = 'radial'
                                        ) %>%
                                          layout(
                                            title = paste("Contribution to Tax Revenues by Income Groups (Baseline),", SimulationYear),
                                            showlegend = FALSE,  # Turn off the legend
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
            
            
            
           
  
  # Chart 4. Tax Revenue by Bin Groups-SIM ---------------------------------------------------------------

            cit_bins_sim_sub_plt <- plot_ly(
                                            #pit_result_bins_sim_sub_plt, 
                                            cit_result_bins_sim_sub,
                                            labels = ~bin_group,
                                            values = ~sum_calc_citax,
                                            # labels = ~`gross income`, 
                                            #  values = ~`tax liability`,
                                            type = 'pie', 
                                            hole = 0.6,  
                                            textinfo = 'label+percent',
                                            insidetextorientation = 'radial'
                                          ) %>%
                                            layout(
                                              title = paste("Contribution to Tax Revenues by Income Groups (Simulation),", SimulationYear),
                                              showlegend = FALSE,  # Turn off the legend
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
            
  # Export Charts -----------------------------------------------------------
  list(
    # Charts
    dist_centile_groups_plt=dist_centile_groups_plt,
    dist_decile_groups_plt=dist_decile_groups_plt,
    cit_bins_bu_sub_plt=cit_bins_bu_sub_plt,
    cit_bins_sim_sub_plt=cit_bins_sim_sub_plt
    
    
  )
}
