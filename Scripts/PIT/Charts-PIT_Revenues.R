library(plotly)


Revenue_Charts <- function(pit_summary_df,
                           pit_section_summary,
                           pit_by_taxpayer_type,
                           pit_etr_by_section,
                           pit_by_decile,
                           SimulationYear,
                           forecast_horizon) {
  
  
  # Step0 Shared chart settings ----
  
  bottom_std  <- 70    # charts with horizontal tick labels
  bottom_nace <- 110   # NACE chart has rotated tick labels
  
  # Source note anchored to the bottom edge of the figure.
  # The note's bottom sits 6 pixels above the figure edge.
  source_note <- function(bottom_margin) {
    list(
      list(
        x         = 0,
        y         = 0,
        xref      = "paper",
        yref      = "paper",
        xanchor   = "left",
        yanchor   = "bottom",
        yshift    = -(bottom_margin - 6),
        text      = "Source: WB staff estimation",
        showarrow = FALSE,
        align     = "left",
        font      = list(size = 11, color = "grey")
      )
    )
  }
  
  chart_margin <- function(b) list(l = 60, r = 120, t = 70, b = b)
  
  
  # Step1 Chart 1 - Total PIT Revenues ----
  
  pit_revenues_total_df <- pit_summary_df[
    ,
    .(
      pit_bu  = sum(`Current law (LCU Mil)`, na.rm = TRUE),
      pit_sim = sum(`Simulation (LCU Mil)`,  na.rm = TRUE)
    ),
    by = .(year)
  ]
  
  data.table::setorder(pit_revenues_total_df, year)
  
  PIT_RevenuesTotal_plt <- plotly::plot_ly(
    data = pit_revenues_total_df,
    x    = ~year,
    y    = ~pit_bu * 1e6,
    name = "Baseline",
    type = "scatter",
    mode = "lines",
    line = list(width = 4, dash = "solid")
  ) %>%
    
    plotly::add_trace(
      x    = ~year,
      y    = ~pit_sim * 1e6,
      name = "Simulation",
      type = "scatter",
      mode = "lines",
      line = list(width = 4, dash = "dot")
    ) %>%
    
    plotly::layout(
      title = paste0(
        "Total PIT Revenues (LCU), ",
        min(pit_revenues_total_df$year),
        "-",
        max(pit_revenues_total_df$year)
      ),
      xaxis = list(
        title      = "",
        tickformat = "d"
      ),
      yaxis = list(
        title     = "LCU",
        rangemode = "tozero"
      ),
      legend = list(
        orientation = "v",
        x           = 1.02,
        y           = 1,
        xanchor     = "left",
        yanchor     = "top"
      ),
      margin      = chart_margin(bottom_std),
      annotations = source_note(bottom_std)
    )
  
  
  # Step2 Chart 2 - ETR by Income Decile (grouped bar chart, ETR in %) ----
  #
  #   - White background, light grey gridlines
  #   - X-axis = decile groups D01 to D10 (D01 = lowest income)
  #   - Blue bars = baseline, orange bars = simulation, grouped side by side
  #   - Y-axis shows ETR in percent
  #   - Legend on the right, source note just below the axis title
  #   - Groups are defined on current-law income (set in Aggregate-Results)
  
  pit_etr_decile_df <- data.table::copy(pit_by_decile)
  data.table::setorder(pit_etr_decile_df, Decile)
  
  decile_order <- pit_etr_decile_df$DecileGroup
  
  PIT_ETRbyDecile_plt <- plotly::plot_ly(
    data   = pit_etr_decile_df,
    x      = ~factor(DecileGroup, levels = decile_order),
    y      = ~`ETR - Current Law (%)`,
    name   = "Baseline",
    type   = "bar",
    marker = list(
      color = "#1f77b4",
      line  = list(width = 0)
    ),
    hovertemplate = ~paste0(
      "Decile: ",       DecileGroup,
      "<br>Taxpayers: ", format(Taxpayers, big.mark = ","),
      "<br>Gross income (LCU Mil): ",
      round(`Gross Income - Current Law (LCU Mil)`, 2),
      "<br>PIT (LCU Mil): ",
      round(`PIT - Current Law (LCU Mil)`, 2),
      "<br>ETR Baseline: ",
      round(`ETR - Current Law (%)`, 2), "%",
      "<extra></extra>"
    )
  ) %>%
    
    plotly::add_trace(
      x      = ~factor(DecileGroup, levels = decile_order),
      y      = ~`ETR - Simulation (%)`,
      name   = "Simulation",
      type   = "bar",
      marker = list(
        color = "#ff7f0e",
        line  = list(width = 0)
      ),
      hovertemplate = ~paste0(
        "Decile: ",       DecileGroup,
        "<br>Taxpayers: ", format(Taxpayers, big.mark = ","),
        "<br>Gross income (LCU Mil): ",
        round(`Gross Income - Simulation (LCU Mil)`, 2),
        "<br>PIT (LCU Mil): ",
        round(`PIT - Simulation (LCU Mil)`, 2),
        "<br>ETR Simulation: ",
        round(`ETR - Simulation (%)`, 2), "%",
        "<extra></extra>"
      )
    ) %>%
    
    plotly::layout(
      barmode = "group",
      title   = list(
        text = paste0(
          "Effective Tax Rate by Income Decile Groups, ",
          SimulationYear
        ),
        x    = 0.5,
        xref = "paper",
        font = list(size = 16)
      ),
      xaxis = list(
        title         = list(text = "Income decile", standoff = 8),
        categoryorder = "array",
        categoryarray = decile_order,
        showgrid      = FALSE,
        zeroline      = FALSE,
        showline      = FALSE
      ),
      yaxis = list(
        title      = list(text = "ETR (%)", standoff = 8),
        ticksuffix = "%",
        rangemode  = "tozero",
        showgrid   = TRUE,
        gridcolor  = "#e5e5e5",
        gridwidth  = 1,
        zeroline   = FALSE,
        showline   = FALSE
      ),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      legend = list(
        orientation = "v",
        x           = 1.02,
        y           = 1,
        xanchor     = "left",
        yanchor     = "top",
        bgcolor     = "rgba(255,255,255,0)",
        bordercolor = "rgba(0,0,0,0)"
      ),
      margin      = chart_margin(bottom_std),
      annotations = source_note(bottom_std)
    )
  
  
  # Step3 Chart 3 - PIT revenues by TaxpayerType ----
  
  pit_taxpayer_chart_df <- pit_by_taxpayer_type[
    ,
    .(
      `Current law (LCU Mil)` = sum(`Current law (LCU Mil)`, na.rm = TRUE),
      `Simulation (LCU Mil)`  = sum(`Simulation (LCU Mil)`,  na.rm = TRUE)
    ),
    by = .(TaxpayerType)
  ]
  
  data.table::setorder(pit_taxpayer_chart_df, -`Current law (LCU Mil)`)
  
  taxpayer_order <- pit_taxpayer_chart_df$TaxpayerType
  
  PIT_RevenuesByTaxpayer_plt <- plotly::plot_ly(
    data        = pit_taxpayer_chart_df,
    y           = ~factor(TaxpayerType, levels = taxpayer_order),
    x           = ~`Current law (LCU Mil)` * 1e6,
    name        = "Baseline",
    type        = "bar",
    orientation = "h",
    marker      = list(line = list(width = 0)),
    hovertemplate = ~paste0(
      "Taxpayer type: ", TaxpayerType,
      "<br>Baseline: ", format(
        round(`Current law (LCU Mil)` * 1e6, 0),
        big.mark = ",", scientific = FALSE
      ),
      "<extra></extra>"
    )
  ) %>%
    
    plotly::add_trace(
      y           = ~factor(TaxpayerType, levels = taxpayer_order),
      x           = ~`Simulation (LCU Mil)` * 1e6,
      name        = "Simulation",
      type        = "bar",
      orientation = "h",
      marker      = list(line = list(width = 0)),
      hovertemplate = ~paste0(
        "Taxpayer type: ", TaxpayerType,
        "<br>Simulation: ", format(
          round(`Simulation (LCU Mil)` * 1e6, 0),
          big.mark = ",", scientific = FALSE
        ),
        "<extra></extra>"
      )
    ) %>%
    
    plotly::layout(
      barmode = "group",
      title   = "PIT Revenues by Taxpayer Type (LCU): Baseline vs Simulation",
      xaxis   = list(
        title     = list(text = "LCU", standoff = 8),
        rangemode = "tozero"
      ),
      yaxis   = list(
        title         = "",
        categoryorder = "array",
        categoryarray = taxpayer_order,
        tickangle     = 0
      ),
      legend = list(
        orientation = "v",
        x           = 1.02,
        y           = 1,
        xanchor     = "left",
        yanchor     = "top"
      ),
      margin      = chart_margin(bottom_std),
      annotations = source_note(bottom_std)
    )
  
  
  # Step4 Chart 4 - ETR by NACE Section ----
  
  pit_etr_weighted <- merge(
    pit_etr_by_section,
    pit_section_summary[
      ,
      .(
        Module,
        section,
        description_en,
        `Current law (LCU Mil)`,
        `Simulation (LCU Mil)`
      )
    ],
    by    = c("Module", "section", "description_en"),
    all.x = TRUE
  )
  
  pit_etr_nace_chart_df <- pit_etr_weighted[
    !is.na(`ETR Current law (%)`) & !is.na(`ETR Simulation (%)`),
    .(
      `ETR Current law (%)` = sum(
        `ETR Current law (%)` * `Current law (LCU Mil)`, na.rm = TRUE
      ) / sum(`Current law (LCU Mil)`, na.rm = TRUE),
      
      `ETR Simulation (%)` = sum(
        `ETR Simulation (%)` * `Simulation (LCU Mil)`, na.rm = TRUE
      ) / sum(`Simulation (LCU Mil)`, na.rm = TRUE),
      
      description_en = first(description_en)
    ),
    by = .(section)
  ]
  
  pit_etr_nace_chart_df[, is_na_section := section == "NotAvailable"]
  data.table::setorder(pit_etr_nace_chart_df, is_na_section, section)
  pit_etr_nace_chart_df[, is_na_section := NULL]
  
  etr_section_order <- pit_etr_nace_chart_df$section
  
  PIT_ETRbyNACE_plt <- plotly::plot_ly(
    data   = pit_etr_nace_chart_df,
    x      = ~factor(section, levels = etr_section_order),
    y      = ~`ETR Current law (%)`,
    name   = "Baseline",
    type   = "bar",
    marker = list(line = list(width = 0)),
    hovertemplate = ~paste0(
      "Section: ",    section,
      "<br>Sector: ", description_en,
      "<br>ETR Baseline: ", round(`ETR Current law (%)`, 2), "%",
      "<extra></extra>"
    )
  ) %>%
    
    plotly::add_trace(
      x      = ~factor(section, levels = etr_section_order),
      y      = ~`ETR Simulation (%)`,
      name   = "Simulation",
      type   = "bar",
      marker = list(line = list(width = 0)),
      hovertemplate = ~paste0(
        "Section: ",    section,
        "<br>Sector: ", description_en,
        "<br>ETR Simulation: ", round(`ETR Simulation (%)`, 2), "%",
        "<extra></extra>"
      )
    ) %>%
    
    plotly::layout(
      barmode = "group",
      title   = "PIT Effective Tax Rate by NACE Section (%): Baseline vs Simulation",
      xaxis   = list(
        title         = "",
        categoryorder = "array",
        categoryarray = etr_section_order,
        tickangle     = -45
      ),
      yaxis = list(
        title      = list(text = "ETR (%)", standoff = 8),
        ticksuffix = "%",
        rangemode  = "tozero"
      ),
      legend = list(
        orientation = "v",
        x           = 1.02,
        y           = 1,
        xanchor     = "left",
        yanchor     = "top"
      ),
      margin      = chart_margin(bottom_nace),
      annotations = source_note(bottom_nace)
    )
  
  
  # Step5 Export charts ----
  
  return(
    list(
      PIT_RevenuesTotal_plt      = PIT_RevenuesTotal_plt,
      PIT_ETRbyDecile_plt        = PIT_ETRbyDecile_plt,
      PIT_RevenuesByTaxpayer_plt = PIT_RevenuesByTaxpayer_plt,
      PIT_ETRbyNACE_plt          = PIT_ETRbyNACE_plt
    )
  )
}
