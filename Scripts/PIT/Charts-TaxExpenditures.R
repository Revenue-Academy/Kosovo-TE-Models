# Tax Expenditures Dashboard


# Step1 Tax_Expenditures_Charts function ----


Tax_Expenditures_Charts <- function(pd_pit_results_te,
                                    forecast_horizon,
                                    SimulationYear) {
  
  
  # Step2 Chart 1 - Total Tax Expenditures line chart ----
  
  te_agg_plt <- plotly::plot_ly(
    data          = pd_pit_results_te$pit_summary_pd_te,
    x             = ~year,
    y             = ~`Tax Expenditure (LCU Mil)` * 1e6,
    name          = "Tax Expenditure",
    type          = "scatter",
    mode          = "lines+markers+text",
    text          = ~formatC(
      `Tax Expenditure (LCU Mil)` * 1e6,
      digits   = 0,
      format   = "f",
      big.mark = ","
    ),
    textposition  = "top center",
    hovertemplate = ~paste0(
      "Year: ", year,
      "<br>Tax Expenditure: ",
      formatC(
        `Tax Expenditure (LCU Mil)` * 1e6,
        digits   = 0,
        format   = "f",
        big.mark = ","
      ),
      " LCU",
      "<extra></extra>"
    ),
    line   = list(width = 4, dash = "solid"),
    marker = list(size  = 8)
  ) %>%
    
    plotly::layout(
      title = paste0(
        "Total Tax Expenditures (LCU), ",
        min(pd_pit_results_te$pit_summary_pd_te$year),
        "-",
        max(pd_pit_results_te$pit_summary_pd_te$year)
      ),
      xaxis = list(title = "", tickformat = "d"),
      yaxis = list(title = "LCU", rangemode = "tozero"),
      legend = list(orientation = "h", x = 0, y = -0.15),
      annotations = list(
        list(
          x = -0.02, y = -0.2,
          text      = "Source: WB staff estimation",
          showarrow = FALSE,
          xref      = "paper",
          yref      = "paper",
          align     = "left"
        )
      )
    )
  
  
  # Step3 Chart 2 - TE stacked bar chart by incentive type ----
  
  te_groups_plt <- plotly::plot_ly(
    data = pd_pit_results_te$te_split_combined[
      incentive == "Rent Deduction"
    ],
    x             = ~year,
    y             = ~te_lcu,
    type          = "bar",
    name          = "Rent Deduction",
    marker        = list(color = "#ff7f0e"),
    hovertemplate = ~paste0(
      "Year: ", year,
      "<br>Incentive: Rent Deduction",
      "<br>Tax Expenditure: ",
      formatC(te_lcu, digits = 0, format = "f", big.mark = ","),
      " LCU",
      "<extra></extra>"
    )
  ) %>%
    
    plotly::add_trace(
      data = pd_pit_results_te$te_split_combined[
        incentive == "Charitable Deduction"
      ],
      x             = ~year,
      y             = ~te_lcu,
      type          = "bar",
      name          = "Charitable Deduction",
      marker        = list(color = "#1f77b4"),
      hovertemplate = ~paste0(
        "Year: ", year,
        "<br>Incentive: Charitable Deduction",
        "<br>Tax Expenditure: ",
        formatC(te_lcu, digits = 0, format = "f", big.mark = ","),
        " LCU",
        "<extra></extra>"
      )
    ) %>%
    
    plotly::layout(
      barmode = "stack",
      title   = paste0(
        "Distribution of Tax Expenditures by Type (LCU), ",
        min(pd_pit_results_te$te_split_combined$year),
        " - ",
        max(pd_pit_results_te$te_split_combined$year)
      ),
      xaxis  = list(title = "", tickmode = "linear", tickformat = "d"),
      yaxis  = list(title = "LCU", rangemode = "tozero"),
      bargap = 0.5,
      legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left"),
      annotations = list(
        list(
          x = -0.02, y = -0.1,
          text      = "Source: WB staff estimation",
          showarrow = FALSE,
          xref      = "paper",
          yref      = "paper",
          align     = "left"
        )
      )
    )
  
  
  # Step4 Chart 3 - TE treemap by NACE Section ----
  
  te_nace_treemap_df <- pd_pit_results_te$nace_pit_summary_te[
    year == as.integer(SimulationYear)
  ]
  
  te_nace_treemap_df[, parent := "Total"]
  
  te_nace_treemap_df <- te_nace_treemap_df[
    tax_expenditure > 0
  ]
  
  treemap_nace_te_plt <- plotly::plot_ly(
    data      = te_nace_treemap_df,
    type      = "treemap",
    values    = ~round(tax_expenditure * 1e6, 0),
    labels    = ~section,
    parents   = ~parent,
    name      = " ",
    hovertext = ~paste0(
      "Section: ", section,
      "<br>Tax Expenditure: ",
      formatC(
        round(tax_expenditure * 1e6, 0),
        digits   = 0,
        format   = "f",
        big.mark = ","
      ),
      " LCU"
    ),
    textinfo  = "label+value+percent parent",
    hoverinfo = "text"
  ) %>%
    
    plotly::layout(
      title = list(
        text = paste0(
          "Structure of Tax Expenditures by NACE Section (LCU), ",
          SimulationYear
        ),
        font = list(size = 14)
      ),
      annotations = list(
        list(
          x = 0.05, y = -0.01,
          text      = "Source: WB staff estimation",
          showarrow = FALSE,
          xref      = "paper",
          yref      = "paper",
          xanchor   = "center",
          yanchor   = "top",
          font      = list(size = 10)
        )
      )
    )
  
  
  # Step5 Chart 4 - TE bar chart by Taxpayer Type ----
  
  taxpayer_order_te <- pd_pit_results_te$taxpayer_pit_summary_te[
    ,
    .(total_te = sum(tax_expenditure, na.rm = TRUE)),
    by = TaxpayerType
  ][order(-total_te)]$TaxpayerType
  
  te_taxpayer_plt <- plotly::plot_ly()
  
  for (tp in taxpayer_order_te) {
    
    dt_tp <- pd_pit_results_te$taxpayer_pit_summary_te[
      TaxpayerType == tp
    ]
    
    te_taxpayer_plt <- te_taxpayer_plt %>%
      plotly::add_trace(
        data          = dt_tp,
        x             = ~year,
        y             = ~tax_expenditure * 1e6,
        type          = "bar",
        name          = tp,
        hovertemplate = ~paste0(
          "Year: ", year,
          "<br>Taxpayer Type: ", tp,
          "<br>Tax Expenditure: ",
          formatC(
            tax_expenditure * 1e6,
            digits   = 0,
            format   = "f",
            big.mark = ","
          ),
          " LCU",
          "<extra></extra>"
        )
      )
  }
  
  te_taxpayer_plt <- te_taxpayer_plt %>%
    
    plotly::layout(
      barmode = "stack",
      title   = paste0(
        "Tax Expenditures by Taxpayer Type (LCU), ",
        min(pd_pit_results_te$taxpayer_pit_summary_te$year),
        " - ",
        max(pd_pit_results_te$taxpayer_pit_summary_te$year)
      ),
      xaxis  = list(title = "", tickmode = "linear", tickformat = "d"),
      yaxis  = list(title = "LCU", rangemode = "tozero"),
      bargap = 0.5,
      legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left"),
      annotations = list(
        list(
          x = -0.02, y = -0.1,
          text      = "Source: WB staff estimation",
          showarrow = FALSE,
          xref      = "paper",
          yref      = "paper",
          align     = "left"
        )
      )
    )
  
  
  # Step6 Export charts ----
  
  return(
    list(
      te_agg_plt          = te_agg_plt,
      te_groups_plt       = te_groups_plt,
      treemap_nace_te_plt = treemap_nace_te_plt,
      te_taxpayer_plt     = te_taxpayer_plt
    )
  )
}
