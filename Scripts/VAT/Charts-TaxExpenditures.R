"Dashboard - VAT Tax Expenditures"

suppressMessages({

# 1. Function ----

VAT_TE_fun <- function(
  forecast_combined_te_selected,
  forecast_TE_tbl_combined,
  SimulationYear
) {


# 2. Aggregate tax expenditure forecast ----

  if (!all(c("year", "scenario") %in% names(forecast_TE_tbl_combined))) {
    stop("forecast_TE_tbl_combined must contain year and scenario.")
  }

  chart_base_year <- if (exists("base_year_VAT", envir = .GlobalEnv)) {
    suppressWarnings(as.integer(get("base_year_VAT", envir = .GlobalEnv)))
  } else {
    suppressWarnings(as.integer(min(forecast_TE_tbl_combined$year, na.rm = TRUE)))
  }

  forecast_TE_tbl_combined$year <- suppressWarnings(as.integer(as.character(forecast_TE_tbl_combined$year)))
  forecast_TE_tbl_combined <- forecast_TE_tbl_combined |>
    dplyr::filter(!is.na(year), year >= chart_base_year)

  if ("Tax Expenditures (In EUR thousand)" %in% names(forecast_TE_tbl_combined)) {
    forecast_TE_tbl_combined$`Tax Expenditures (In EUR million)` <- round(forecast_TE_tbl_combined$`Tax Expenditures (In EUR thousand)` / 1000, 1)
  } else if (!"Tax Expenditures (In EUR million)" %in% names(forecast_TE_tbl_combined)) {
    stop("forecast_TE_tbl_combined must contain a Tax Expenditures monetary column.")
  }

  forecast_TE_tbl_combined <- forecast_TE_tbl_combined |>
    dplyr::arrange(year, scenario)

  vat_TE_plt <- plotly::plot_ly() |>
    plotly::add_trace(
      data = forecast_TE_tbl_combined |>
        dplyr::filter(scenario == "Baseline"),
      x = ~year,
      y = ~`Tax Expenditures (In EUR million)`,
      name = "Baseline",
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = paste(
        "Year: %{x}",
        "<br>Tax expenditures: %{y:.1f} EUR million",
        "<extra></extra>"
      )
    ) |>
    plotly::add_trace(
      data = forecast_TE_tbl_combined |>
        dplyr::filter(scenario == "Simulation"),
      x = ~year,
      y = ~`Tax Expenditures (In EUR million)`,
      name = "Simulation",
      type = "scatter",
      mode = "lines+markers",
      line = list(dash = "dot"),
      hovertemplate = paste(
        "Year: %{x}",
        "<br>Tax expenditures: %{y:.1f} EUR million",
        "<extra></extra>"
      )
    ) |>
    plotly::layout(
      title = paste(
        "Tax Expenditure Forecast (EUR million),",
        min(forecast_TE_tbl_combined$year, na.rm = TRUE),
        "-",
        max(forecast_TE_tbl_combined$year, na.rm = TRUE)
      ),
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = sort(unique(forecast_TE_tbl_combined$year))
      ),
      yaxis = list(
        title = "EUR million",
        rangemode = "tozero"
      )
    )


# 3. Selected-year tax expenditures ----

  forecast_TE_selected <- forecast_TE_tbl_combined |>
    dplyr::filter(year == SimulationYear)

  vat_te_tbl_plt <- plotly::plot_ly(
    data = forecast_TE_selected,
    x = ~scenario,
    y = ~`Tax Expenditures (In EUR million)`,
    type = "bar",
    hovertemplate = paste(
      "%{x}",
      "<br>Tax expenditures: %{y:.1f} EUR million",
      "<extra></extra>"
    )
  ) |>
    plotly::layout(
      title = paste("Tax Expenditures (EUR million) in", SimulationYear),
      xaxis = list(title = ""),
      yaxis = list(title = "EUR million", rangemode = "tozero"),
      showlegend = FALSE
    )


# 4. Tax expenditures by sector ----

  if (!all(c("PRODUCT_INDUSTRY_CODE", "PRODUCT_INDUSTRY_NAME", "value", "scenario") %in% names(forecast_combined_te_selected))) {
    stop("forecast_combined_te_selected is missing required sector tax-expenditure columns.")
  }

  forecast_combined_te_selected$TE_EUR_million <- round(forecast_combined_te_selected$value / 1000, 1)

  vat_CPA_plt <- plotly::plot_ly(
    data = forecast_combined_te_selected,
    x = ~PRODUCT_INDUSTRY_CODE,
    y = ~TE_EUR_million,
    name = ~scenario,
    type = "bar",
    hovertemplate = paste(
      "%{customdata}",
      "<br>Tax expenditures: %{y:.1f} EUR million",
      "<extra></extra>"
    ),
    customdata = ~PRODUCT_INDUSTRY_NAME
  ) |>
    plotly::layout(
      title = paste("Tax Expenditures by Sector (EUR million) in", SimulationYear),
      xaxis = list(title = "", tickangle = -90),
      yaxis = list(title = "EUR million"),
      barmode = "group"
    )


# 5. Tax expenditures as pct of GDP ----

  if (
    !"Tax Expenditures (Pct of GDP)" %in% names(forecast_TE_tbl_combined) ||
    all(is.na(forecast_TE_tbl_combined$`Tax Expenditures (Pct of GDP)`))
  ) {
    if (!exists("nominal_gdp_vat", envir = .GlobalEnv)) {
      stop("nominal_gdp_vat is required to calculate Tax Expenditures (Pct of GDP).")
    }

    nominal_gdp_vat <- get("nominal_gdp_vat", envir = .GlobalEnv)
    nominal_gdp_vat$year <- as.integer(nominal_gdp_vat$year)
    forecast_TE_tbl_combined$year <- as.integer(forecast_TE_tbl_combined$year)

    forecast_TE_tbl_combined <- forecast_TE_tbl_combined |>
      dplyr::select(-dplyr::any_of(c("Tax Expenditures (Pct of GDP)", "gdp"))) |>
      dplyr::left_join(
        nominal_gdp_vat |> dplyr::select(year, gdp),
        by = "year"
      )

    if ("Tax Expenditures (In EUR million)" %in% names(forecast_TE_tbl_combined)) {
      forecast_TE_tbl_combined <- forecast_TE_tbl_combined |>
        dplyr::mutate(`Tax Expenditures (Pct of GDP)` = round((`Tax Expenditures (In EUR million)` / gdp) * 100, 2))
    } else {
      forecast_TE_tbl_combined <- forecast_TE_tbl_combined |>
        dplyr::mutate(`Tax Expenditures (Pct of GDP)` = round((`Tax Expenditures (In EUR thousand)` / (gdp * 1000)) * 100, 2))
    }

    forecast_TE_tbl_combined <- forecast_TE_tbl_combined |> dplyr::select(-gdp)
  }

  vat_TE_GDP_plt <- plotly::plot_ly() |>
    plotly::add_trace(
      data = forecast_TE_tbl_combined |>
        dplyr::filter(scenario == "Baseline"),
      x = ~year,
      y = ~`Tax Expenditures (Pct of GDP)`,
      name = "Baseline",
      type = "scatter",
      mode = "lines+markers",
      hovertemplate = paste(
        "Year: %{x}",
        "<br>Tax expenditures: %{y:.2f}% of GDP",
        "<extra></extra>"
      )
    ) |>
    plotly::add_trace(
      data = forecast_TE_tbl_combined |>
        dplyr::filter(scenario == "Simulation"),
      x = ~year,
      y = ~`Tax Expenditures (Pct of GDP)`,
      name = "Simulation",
      type = "scatter",
      mode = "lines+markers",
      line = list(dash = "dot"),
      hovertemplate = paste(
        "Year: %{x}",
        "<br>Tax expenditures: %{y:.2f}% of GDP",
        "<extra></extra>"
      )
    ) |>
    plotly::layout(
      title = "Tax Expenditures (Pct of GDP)",
      xaxis = list(
        title = "",
        tickmode = "array",
        tickvals = sort(unique(forecast_TE_tbl_combined$year))
      ),
      yaxis = list(
        title = "% of GDP",
        rangemode = "tozero"
      )
    )


# 6. Export charts ----

  list(
    vat_TE_plt = vat_TE_plt,
    vat_te_tbl_plt = vat_te_tbl_plt,
    vat_CPA_plt = vat_CPA_plt,
    vat_TE_GDP_plt = vat_TE_GDP_plt
  )
}

})
