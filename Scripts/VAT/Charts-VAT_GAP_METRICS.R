"Dashboard-VAT metrics"

suppressMessages({

VAT_metrics_fun <- function(
  vat_gap_metrics_tbl,
  vat_gap_metrics_tbl_treemap,
  vat_sectors_pie_tbl,
  vat_sectors_normalized,
  forecast_combined_agg,
  SimulationYear
) {

  chart_base_year <- if (exists("base_year_VAT", envir = .GlobalEnv)) {
    suppressWarnings(as.integer(get("base_year_VAT", envir = .GlobalEnv)))
  } else {
    suppressWarnings(as.integer(min(forecast_combined_agg$year, na.rm = TRUE)))
  }

  forecast_combined_agg$year <- suppressWarnings(as.integer(as.character(forecast_combined_agg$year)))
  forecast_combined_agg <- forecast_combined_agg |>
    dplyr::filter(!is.na(year), year >= chart_base_year)

  forecast_horizon <- sort(unique(forecast_combined_agg$year))

  # Reporting copies in EUR million ----

  vat_gap_metrics_tbl_display <- vat_gap_metrics_tbl |>
    dplyr::mutate(value = value / 1000)

  vat_gap_metrics_tbl_treemap_display <- vat_gap_metrics_tbl_treemap |>
    dplyr::mutate(value = value / 1000)

  forecast_combined_agg_display <- forecast_combined_agg |>
    dplyr::mutate(value = value / 1000)

  # 1. VAT Revenue Comparison ----

  vat_rev_tbl_plt <- plotly::plot_ly(
    vat_gap_metrics_tbl_display,
    y = ~reorder(Descriptions, value),
    x = ~value,
    type = "bar",
    text = " ",
    hoverinfo = "text",
    hovertext = ~paste0(
      Descriptions,
      "<br>Value: ",
      format(round(value, 1), big.mark = ",", nsmall = 1),
      " EUR million"
    )
  ) |>
    plotly::layout(
      title = paste("VAT Revenue Comparison (EUR million) in", SimulationYear),
      xaxis = list(title = "EUR million"),
      yaxis = list(title = ""),
      margin = list(b = 100),
      bargap = 0.6,
      annotations = list(
        x = -0.05,
        y = -0.1,
        text = "Source: WB staff estimation",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "top",
        font = list(size = 10)
      )
    )

  # 2. Decomposition of Total VAT Gap ----

  vat_gap_tbl_plt <- plotly::plot_ly(
    data = vat_gap_metrics_tbl_treemap_display,
    type = "treemap",
    values = ~round(value, 1),
    labels = ~Descriptions,
    parents = ~label,
    name = " ",
    text = ~Descriptions,
    textinfo = "label+value",
    hoverinfo = "text",
    hovertext = ~paste0(
      Descriptions,
      "<br>Value: ",
      format(round(value, 1), big.mark = ",", nsmall = 1),
      " EUR million"
    )
  ) |>
    plotly::layout(
      title = list(
        text = paste("Decomposition of Total VAT Gap (EUR million) in", SimulationYear)
      ),
      annotations = list(
        x = 0.05,
        y = -0.01,
        text = "Source: WB staff estimation",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        xanchor = "center",
        yanchor = "top",
        font = list(size = 10)
      )
    )

  # 3. VAT Gap Forecast ----

  VAT_Benchmark_tbl <- forecast_combined_agg_display |>
    dplyr::filter(Descriptions %in% c("Benchmark VAT", "Calibrated VAT"))

  VAT_Benchmark_tbl$year <- as.factor(VAT_Benchmark_tbl$year)

  VAT_GAP_tbl <- VAT_Benchmark_tbl |>
    dplyr::group_by(year, scenario) |>
    dplyr::summarise(
      value = purrr::reduce(stats::na.omit(value), `-`),
      .groups = "drop"
    )

  VAT_GAP_plt <- plotly::plot_ly() |>
    plotly::add_trace(
      data = subset(VAT_GAP_tbl, scenario == "Baseline"),
      x = ~year,
      y = ~round(value, 1),
      name = "Baseline",
      type = "scatter",
      mode = "lines",
      hoverinfo = "text",
      text = ~paste0(
        "Year: ", year,
        "<br>VAT Gap: ",
        format(round(value, 1), big.mark = ",", nsmall = 1),
        " EUR million"
      ),
      line = list(width = 4, dash = "solid")
    ) |>
    plotly::layout(
      title = paste(
        "VAT Gap (EUR million),",
        min(forecast_horizon),
        "-",
        max(forecast_horizon)
      ),
      xaxis = list(title = "", tickformat = "d"),
      yaxis = list(title = "EUR million", rangemode = "tozero"),
      annotations = list(
        x = -0.02,
        y = -0.1,
        text = "Source: WB staff estimation",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        align = "left"
      )
    )

  # 4. Institutional-sector composition ----

  vat_str_plt <- plotly::plot_ly(vat_sectors_normalized) |>
    plotly::add_trace(
      x = ~PRODUCT_INDUSTRY_CODE,
      y = ~Businesses_pct,
      type = "bar",
      name = "Businesses",
      hoverinfo = "text",
      hovertext = ~paste(
        "Sector:", PRODUCT_INDUSTRY_NAME,
        "<br>Businesses:", round(Businesses_pct, 1), "%"
      )
    ) |>
    plotly::add_trace(
      x = ~PRODUCT_INDUSTRY_CODE,
      y = ~Households_pct,
      type = "bar",
      name = "Households",
      hoverinfo = "text",
      hovertext = ~paste(
        "Sector:", PRODUCT_INDUSTRY_NAME,
        "<br>Households:", round(Households_pct, 1), "%"
      )
    ) |>
    plotly::add_trace(
      x = ~PRODUCT_INDUSTRY_CODE,
      y = ~NPISHs_pct,
      type = "bar",
      name = "NPISHs",
      hoverinfo = "text",
      hovertext = ~paste(
        "Sector:", PRODUCT_INDUSTRY_NAME,
        "<br>NPISHs:", round(NPISHs_pct, 1), "%"
      )
    ) |>
    plotly::add_trace(
      x = ~PRODUCT_INDUSTRY_CODE,
      y = ~Government_pct,
      type = "bar",
      name = "Government",
      hoverinfo = "text",
      hovertext = ~paste(
        "Sector:", PRODUCT_INDUSTRY_NAME,
        "<br>Government:", round(Government_pct, 1), "%"
      )
    ) |>
    plotly::layout(
      title = list(
        text = paste("Breakdown of VAT Revenues by Institutional Sector in", SimulationYear),
        x = 0.5,
        y = 1,
        font = list(size = 12)
      ),
      xaxis = list(
        title = "",
        tickangle = -45,
        tickfont = list(size = 6)
      ),
      yaxis = list(
        title = "Percentage",
        tickformat = "",
        range = c(0, 100),
        ticksuffix = "%"
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        y = -0.05,
        xanchor = "center",
        yanchor = "top",
        font = list(size = 6)
      ),
      barmode = "stack",
      annotations = list(
        list(
          x = 0.0,
          y = -0.05,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "top",
          font = list(size = 6)
        )
      )
    )

  list(
    vat_rev_tbl_plt = vat_rev_tbl_plt,
    vat_gap_tbl_plt = vat_gap_tbl_plt,
    VAT_GAP_plt = VAT_GAP_plt,
    vat_str_plt = vat_str_plt
  )
}

})
