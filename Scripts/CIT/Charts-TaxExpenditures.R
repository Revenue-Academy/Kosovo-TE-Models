# Charts-TaxExpenditures.R ----

Tax_Expenditures_Charts_fun <- function(
    te_summary_df,
    company_type_cit_summary_te,
    nace_cit_summary_tbl,
    te_summary_df_type,
    forecast_horizon,
    SimulationYear) {
  
  # Prepare chart-specific copies and convert displayed amounts to thousand LCU ----
  
  # te_summary_df is in million LCU: convert to thousand LCU.
  te_agg_data <- data.table::copy(data.table::as.data.table(te_summary_df))
  te_agg_data[, tax_expenditure_thousand := `tax expenditure` * 1000]
  
  # Company-type breakdown is in LCU: convert to thousand LCU.
  company_type_data <- data.table::copy(
    data.table::as.data.table(company_type_cit_summary_te)
  )
  company_type_data[, tax_expenditure_thousand := tax_expenditures / 1000]
  
  # NACE breakdown is in LCU: convert to thousand LCU.
  nace_data <- data.table::copy(
    data.table::as.data.table(nace_cit_summary_tbl)
  )
  nace_data[, tax_expenditure_thousand := tax_expenditures / 1000]
  
  # te_summary_df_type is already in thousand LCU: no additional conversion.
  te_type_data <- data.table::copy(
    data.table::as.data.table(te_summary_df_type)
  )
  
  
  # Chart 1. Total tax expenditures by year ----
  
  te_agg_plt <- plotly::plot_ly(
    data = te_agg_data,
    x = ~year,
    y = ~`tax expenditure`,
    type = "scatter",
    mode = "lines+markers+text",
    name = "Tax expenditure",
    text = ~format(round(`tax expenditure`, 1), big.mark = ","),
    textposition = "top center",
    hovertemplate = paste(
      "Year: %{x}",
      "<br>Tax expenditure: %{y:,.1f} thousand LCU",
      "<extra></extra>"
    ),
    line = list(width = 4, dash = "solid")
  ) %>%
    plotly::layout(
      title = paste(
        "Total Tax Expenditures (thousand LCU),",
        min(forecast_horizon), "-", max(forecast_horizon)
      ),
      xaxis = list(title = "", tickformat = "d"),
      yaxis = list(title = "Thousand LCU", rangemode = "tozero"),
      annotations = list(
        list(
          x = -0.02, y = -0.12,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = "paper", yref = "paper",
          align = "left"
        )
      )
    )
  
  
  
  # Chart 2. Tax expenditures by company type ----
  
  te_type_companies_plt <- plotly::plot_ly(
    data = company_type_data,
    labels = ~name_en,
    values = ~tax_expenditure_thousand,
    type = "pie",
    hole = 0.6,
    textinfo = "label+percent",
    textposition = "outside",
    insidetextorientation = "radial",
    rotation = 200,
    textfont = list(size = 10),
    hovertext = ~paste0(
      name_en,
      "<br>Tax expenditure: ",
      format(round(tax_expenditure_thousand, 1), big.mark = ","),
      " thousand LCU"
    ),
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      title = paste(
        "Tax Expenditures by Company Type (thousand LCU),",
        SimulationYear
      ),
      showlegend = FALSE,
      margin = list(l = 60, r = 60, t = 80, b = 40),
      annotations = list(
        list(
          x = 0.13,
          y = -0.05,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          font = list(size = 12)
        )
      )
    )
  
  
  # Chart 3. Tax Expenditures by NACE Section -------------------------------------------------------------------------
  
  nace_plot_dt <- data.table::copy(data.table::as.data.table(nace_cit_summary_tbl))
  # Keep values in thousands rounded to 1 decimal place
  nace_plot_dt[, te_val := round(tax_expenditures / 1e3, 1)]
  nace_plot_dt <- nace_plot_dt[te_val > 0]
  
  treemap_nace_type_plt <- plot_ly(
    data      = nace_plot_dt,
    type      = "treemap",
    values    = ~te_val,
    labels    = ~section,
    parents   = ~value,
    name      = " ",
    text      = ~paste0(description, "<br>", format(te_val, big.mark = ",", nsmall = 1), " th. LCU"),
    hovertext = ~paste0(
      "Section: ",      section,       "<br>",
      "Description: ",  description,   "<br>",
      "Tax Expenditure: ", format(te_val, big.mark = ",", nsmall = 1), " thousand LCU"
    ),
    textinfo  = "label+value+percent parent",
    hoverinfo = "text"
  ) %>%
    layout(
      title = list(
        text = paste("Structure of Tax Expenditures by NACE Sections (thousand LCU),", SimulationYear),
        font = list(size = 14)
      ),
      annotations = list(
        x         = 0.05,
        y         = -0.01,
        text      = "Source: WB staff estimation",
        showarrow = FALSE,
        xref      = "paper",
        yref      = "paper",
        xanchor   = "center",
        yanchor   = "top",
        font      = list(size = 10)
      )
    )
  
  
  # Chart 4. Tax expenditures by provision type and year ----
  
  te_type_plt <- plotly::plot_ly(
    data = te_type_data,
    x = ~year,
    y = ~`tax expenditure`,
    type = "bar",
    color = ~type,
    colors = c(
      "Special allowance for new assets" = "#d62728",
      "Charitable contribution" = "#e377c2",
      "Discounts for sponsorship in the field of sports" = "#1f77b4",
      "Discounts for sponsorship in the field of culture and youth" = "#ff7f0e"
    ),
    hovertemplate = paste(
      "Year: %{x}",
      "<br>Tax expenditure: %{y:,.1f} thousand LCU",
      "<extra></extra>"
    )
  ) %>%
    plotly::layout(
      title = paste(
        "Tax Expenditures by Provision (thousand LCU),",
        min(forecast_horizon), "-", max(forecast_horizon)
      ),
      xaxis = list(title = "", tickmode = "linear"),
      yaxis = list(title = "Thousand LCU", rangemode = "tozero"),
      barmode = "stack",
      bargap = 0.7,
      legend = list(
        orientation = "h",
        x = 0,
        y = -0.2
      ),
      annotations = list(
        list(
          x = -0.03,
          y = -0.2,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          align = "left"
        )
      )
    )
  
  
  # Return charts ----
  
  list(
    te_agg_plt            = te_agg_plt,
    te_type_companies_plt = te_type_companies_plt,
    treemap_nace_type_plt = treemap_nace_type_plt,
    te_type_plt           = te_type_plt
  )
}
