"Dashboard-VAT Aggregate Revenues"

suppressMessages({

VAT_aggregation_revenues_fun <- function(
  forecast_combined_agg,
  forecast_combined_agg_tbl_wide,
  forecast_sim_cpa,
  vat_sectors_pie_tbl,
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

  forecast_combined_agg_tbl_wide$year <- suppressWarnings(as.integer(as.character(forecast_combined_agg_tbl_wide$year)))
  forecast_combined_agg_tbl_wide <- forecast_combined_agg_tbl_wide |>
    dplyr::filter(!is.na(year), year >= chart_base_year)

  # 1. VAT Revenue Forecast ----

  VAT_revenues_tbl <- forecast_combined_agg |>
    dplyr::filter(Descriptions == "Calibrated VAT") |>
    dplyr::mutate(value = value / 1000) |>
    dplyr::arrange(year, scenario)

  vat_revenue_plt <- plotly::plot_ly() |>
    plotly::add_trace(
      data = VAT_revenues_tbl |> dplyr::filter(scenario == "Baseline"),
      x = ~year,
      y = ~round(value, 1),
      name = "Baseline",
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "Year: ", year,
        "<br>Baseline: ", format(round(value, 1), big.mark = ",", nsmall = 1),
        " EUR million"
      ),
      line = list(width = 4, dash = "solid")
    ) |>
    plotly::add_trace(
      data = VAT_revenues_tbl |> dplyr::filter(scenario == "Simulation"),
      x = ~year,
      y = ~round(value, 1),
      name = "Simulation",
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "Year: ", year,
        "<br>Simulation: ", format(round(value, 1), big.mark = ",", nsmall = 1),
        " EUR million"
      ),
      line = list(width = 4, dash = "dot")
    ) |>
    plotly::layout(
      title = paste(
        "VAT Revenue Forecast (EUR million),",
        min(VAT_revenues_tbl$year, na.rm = TRUE),
        "-",
        max(VAT_revenues_tbl$year, na.rm = TRUE)
      ),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = sort(unique(VAT_revenues_tbl$year)),
        ticktext = sort(unique(VAT_revenues_tbl$year))
      ),
      yaxis = list(
        title = "VAT Revenue (EUR million)",
        rangemode = "tozero"
      ),
      annotations = list(
        list(
          x = -0.02,
          y = -0.1,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          align = "left"
        )
      )
    )

  # 2. VAT Revenue Forecast - Pct of GDP ----

  forecast_combined_agg_tbl_wide_selected <- forecast_combined_agg_tbl_wide |>
    dplyr::select(
      year,
      `Current Law (Pct of GDP)`,
      `Simulation (Pct of GDP)`
    ) |>
    dplyr::arrange(year)

  vat_revenue_gdp_plt <- plotly::plot_ly() |>
    plotly::add_trace(
      data = forecast_combined_agg_tbl_wide_selected,
      x = ~year,
      y = ~`Current Law (Pct of GDP)`,
      name = "Current Law",
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "Year: ", year,
        "<br>Current Law: ", `Current Law (Pct of GDP)`, "% of GDP"
      ),
      line = list(width = 4, dash = "solid")
    ) |>
    plotly::add_trace(
      data = forecast_combined_agg_tbl_wide_selected,
      x = ~year,
      y = ~`Simulation (Pct of GDP)`,
      name = "Simulation",
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "Year: ", year,
        "<br>Simulation: ", `Simulation (Pct of GDP)`, "% of GDP"
      ),
      line = list(width = 4, dash = "dot")
    ) |>
    plotly::layout(
      title = paste(
        "VAT Revenue Forecast (Pct of GDP),",
        min(forecast_combined_agg_tbl_wide_selected$year, na.rm = TRUE),
        "-",
        max(forecast_combined_agg_tbl_wide_selected$year, na.rm = TRUE)
      ),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = sort(unique(forecast_combined_agg_tbl_wide_selected$year)),
        ticktext = sort(unique(forecast_combined_agg_tbl_wide_selected$year))
      ),
      yaxis = list(
        title = "Revenue (% of GDP)",
        rangemode = "tozero"
      ),
      annotations = list(
        list(
          x = -0.02,
          y = -0.1,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          align = "left"
        )
      )
    )

  # 3. Structure of VAT by institutional sectors ----
  
  vat_sectors_pie_tbl_display <- vat_sectors_pie_tbl |>
    dplyr::mutate(
      value = value / 1000
    )
  
  vat_structure_plt <- plotly::plot_ly(
    vat_sectors_pie_tbl_display,
    labels = ~variable,
    values = ~value,
    type = "pie",
    hole = 0.6,
    rotation = 90,
    direction = "clockwise",
    textinfo = "label+percent",
    insidetextorientation = "radial",
    hoverinfo = "text",
    text = ~paste0(
      variable,
      "<br>",
      format(
        round(
          value,
          1
        ),
        big.mark = ",",
        nsmall = 1
      ),
      " EUR million"
    )
  ) |>
    plotly::layout(
      title = paste(
        "Structure of VAT by Institutional Sectors,",
        SimulationYear
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "v",
        x = 1.05,
        y = 0.5,
        xanchor = "left",
        yanchor = "middle"
      ),
      margin = list(
        l = 20,
        r = 20,
        t = 50,
        b = 50
      ),
      annotations = list(
        list(
          x = 0.19,
          y = 0,
          text = "Source: WB staff estimation",
          showarrow = FALSE,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          font = list(
            size = 10
          )
        )
      )
    )

  # 4. Breakdown of VAT by sector ----

  vat_sectors_treemapl_tbl <- forecast_sim_cpa |>
    dplyr::filter(year == SimulationYear) |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      value
    ) |>
    dplyr::mutate(value = value / 1000)

  vat_sectors_treemapl_tbl <- data.table::melt(
    data.table::as.data.table(vat_sectors_treemapl_tbl),
    id.vars = c("PRODUCT_INDUSTRY_CODE", "PRODUCT_INDUSTRY_NAME")
  )

  vat_treemap_sectors_plt <- plotly::plot_ly(
    data = vat_sectors_treemapl_tbl,
    type = "treemap",
    values = ~round(value, 1),
    labels = ~PRODUCT_INDUSTRY_CODE,
    parents = ~variable,
    name = " ",
    text = ~PRODUCT_INDUSTRY_NAME,
    textinfo = "label+value+percent parent",
    hoverinfo = "text",
    hovertext = ~paste0(
      PRODUCT_INDUSTRY_NAME,
      "<br>",
      format(round(value, 1), big.mark = ",", nsmall = 1),
      " EUR million"
    )
  ) |>
    plotly::layout(
      title = paste("Breakdown of VAT by Sector (EUR million),", SimulationYear),
      annotations = list(
        list(
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
    )

  list(
    vat_revenue_plt = vat_revenue_plt,
    vat_revenue_gdp_plt = vat_revenue_gdp_plt,
    vat_structure_plt = vat_structure_plt,
    vat_treemap_sectors_plt = vat_treemap_sectors_plt
  )
}

})
