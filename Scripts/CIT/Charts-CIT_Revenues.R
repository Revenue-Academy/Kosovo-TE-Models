
Revenue_Charts_fun <- function(merged_CIT_BU_SIM,
                               nace_cit_big_corporations_summary_long,
                               cit_centile_distribution_bu_sim,
                               cit_decile_distribution_bu_sim,
                               SimulationYear,
                               forecast_horizon) {
  
  # Chart 1. Total CIT Revenues BU vs Simulation ----
  CIT_big_corporations_rev_plt <- plot_ly(
    merged_CIT_BU_SIM,
    x    = ~year,
    y    = ~calc_cit_bu * 1e3,
    name = "Baseline",
    type = 'scatter',
    mode = 'lines',
    line = list(width = 4, dash = "solid")
  ) %>%
    add_trace(
      x    = ~year,
      y    = ~calc_cit_sim * 1e3,
      name = 'Simulation',
      line = list(width = 4, dash = "dot")
    ) %>%
    layout(
      title  = paste("Total CIT Revenues,", min(forecast_horizon), "-", max(forecast_horizon)),
      xaxis  = list(title = '', tickformat = 'd'),
      yaxis  = list(title = ' ', rangemode = 'tozero'),
      annotations = list(
        x = -0.02, y = -0.1,
        text      = "Source: WB staff estimation",
        showarrow = FALSE,
        xref = 'paper', yref = 'paper',
        align = 'left'
      )
    )
  
  
  # Chart 2. ETR by Percentile Groups ----
  # Moved here from Distribution_Charts_fun.
  CIT_etr_centile_plt <- plot_ly(
    cit_centile_distribution_bu_sim,
    x    = ~centile_group,
    y    = ~etr_bu,
    name = "Baseline",
    type = 'scatter',
    mode = 'lines',
    line = list(width = 4, dash = "solid")
  ) %>%
    add_trace(
      y    = ~etr_sim,
      name = "Simulation",
      line = list(width = 4, dash = "dash")
    ) %>%
    layout(
      title = paste("Effective Tax Rate by Percentile Groups,", SimulationYear),
      xaxis = list(
        title    = 'Percentile',
        tickvals = seq(10, 100, by = 10),
        ticktext = seq(10, 100, by = 10)
      ),
      yaxis = list(title = ' '),
      annotations = list(
        list(
          x = -0.02, y = -0.13,
          text      = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = 'paper', yref = 'paper',
          align = 'left'
        )
      )
    )
  
  
  # Chart 3. CIT Revenues by NACE Sectors ----
  custom_colors <- c('#1f77b4', '#ff7f0e')
  
  CIT_nace_big_corporations_plt <- plot_ly(
    nace_cit_big_corporations_summary_long,
    x         = ~section,
    y         = ~value,
    color     = ~category,
    colors    = custom_colors,
    text      = ~round(value, 2),
    hoverinfo = 'text',
    hovertext = ~paste("Sector: ",   description,
                       "<br>Category: ", category,
                       "<br>Value: ",    round(value, 2)),
    type     = 'bar',
    barmode  = 'group'
  ) %>%
    layout(
      title = paste("CIT Revenues by NACE Sectors (in EUR),", SimulationYear),
      xaxis = list(title = " ", tickmode = 'linear'),
      yaxis = list(title = " "),
      annotations = list(
        list(
          x = -0.02, y = -0.1,
          text      = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = 'paper', yref = 'paper',
          align = 'left',
          font  = list(size = 10)
        )
      )
    )
  
  
  # Chart 4. Tax Revenue by Decile Groups ----
  # Moved here from Distribution_Charts_fun.
  custom_colors <- c('#1f77b4', '#ff7f0e')
  
  CIT_decile_groups_plt <- plot_ly(
    cit_decile_distribution_bu_sim,
    x         = ~as.numeric(`Decile groups`),
    y         = ~`Total CIT liability (business as usual) in MIL`,
    name      = 'Baseline',
    marker    = list(color = custom_colors[1]),
    hoverinfo = 'text+y',
    type      = 'bar',
    barmode   = 'group'
  ) %>%
    add_trace(
      y      = ~`Total CIT liability (simulation) in MIL`,
      name   = 'Simulation',
      marker = list(color = custom_colors[2]),
      hoverinfo = 'text+y'
    ) %>%
    layout(
      title = paste("Tax Revenue by Decile Groups,", SimulationYear),
      xaxis = list(title = "Decile", tickmode = 'linear'),
      yaxis = list(title = " "),
      annotations = list(
        list(
          x = -0.02, y = -0.1,
          text      = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = 'paper', yref = 'paper',
          align = 'left'
        )
      )
    )
  
  
  # Export Charts -----------------------------------------------------------
  list(
    CIT_big_corporations_rev_plt = CIT_big_corporations_rev_plt,
    CIT_etr_centile_plt          = CIT_etr_centile_plt,
    CIT_nace_big_corporations_plt = CIT_nace_big_corporations_plt,
    CIT_decile_groups_plt        = CIT_decile_groups_plt
  )
}
