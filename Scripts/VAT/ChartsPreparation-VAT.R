# CHARTS PREPARATION - VAT

suppressMessages({
  
  
  # 1. VAT revenue comparison ----
  
  vat_gap_metrics_tbl <-
    forecast_combined_agg |>
    dplyr::filter(
      year == SimulationYear,
      scenario == "Baseline",
      Descriptions %in% c(
        "Benchmark VAT",
        "Uncalibrated VAT",
        "Calibrated VAT"
      )
    ) |>
    dplyr::select(
      -scenario,
      -year
    )
  
  
  # 2. VAT gap decomposition ----
  
  vat_gap_metrics_tbl_treemap <-
    forecast_combined_agg |>
    dplyr::filter(
      year == SimulationYear,
      scenario == "Baseline",
      Descriptions %in% c(
        "Policy Gap",
        "Compliance Gap"
      )
    ) |>
    dplyr::select(
      -scenario,
      -year
    )
  
  vat_gap_metrics_tbl_treemap$label <-
    "VAT Gap Comparison"
  
  
  # 3. VAT structure by institutional sector ----
  
  vat_sectors_pie_tbl <-
    forecast_sim_cpa |>
    dplyr::filter(
      year == SimulationYear
    ) |>
    dplyr::select(
      -PRODUCT_INDUSTRY_CODE,
      -PRODUCT_INDUSTRY_NAME,
      -value,
      -scenario
    ) |>
    dplyr::summarise(
      Businesses = sum(
        Total_Revenues_from_Intermediate_Inputs,
        na.rm = TRUE
      ),
      Households = sum(
        Final_Demand_HH,
        na.rm = TRUE
      ),
      NPISHs = sum(
        Final_Demand_NPISH,
        na.rm = TRUE
      ),
      Government = sum(
        Final_Demand_Government,
        na.rm = TRUE
      )
    )
  
  vat_sectors_pie_tbl <-
    data.table::melt(
      data.table::as.data.table(
        vat_sectors_pie_tbl
      )
    )
  
  
  # 4. VAT structure by product ----
  
  vat_sectors_structure_tbl <-
    forecast_combined_cpa_selected |>
    dplyr::filter(
      year == SimulationYear,
      scenario == "Simulation"
    ) |>
    dplyr::select(
      -scenario
    )
  
  
  # 5. Normalize VAT structure ----
  
  vat_sectors_normalized <-
    vat_sectors_structure_tbl |>
    dplyr::mutate(
      Businesses_pct =
        (
          Businesses_VAT /
            Total_VAT
        ) *
        100,
      
      Households_pct =
        (
          Households_VAT /
            Total_VAT
        ) *
        100,
      
      NPISHs_pct =
        (
          NPISH_VAT /
            Total_VAT
        ) *
        100,
      
      Government_pct =
        (
          Goverment_VAT /
            Total_VAT
        ) *
        100
    )
  
  vat_sectors_normalized <-
    vat_sectors_normalized |>
    dplyr::mutate(
      Percentage_Check =
        Businesses_pct +
        Households_pct +
        NPISHs_pct +
        Government_pct
    )
  
  vat_sectors_normalized <-
    vat_sectors_normalized |>
    dplyr::mutate(
      Adjusted_Factor =
        100 /
        (
          Businesses_pct +
            Households_pct +
            NPISHs_pct +
            Government_pct
        ),
      
      Businesses_pct =
        Businesses_pct *
        Adjusted_Factor,
      
      Households_pct =
        Households_pct *
        Adjusted_Factor,
      
      NPISHs_pct =
        NPISHs_pct *
        Adjusted_Factor,
      
      Government_pct =
        Government_pct *
        Adjusted_Factor
    )
  
  vat_sectors_normalized$Businesses_pct[
    !is.finite(
      vat_sectors_normalized$Businesses_pct
    )
  ] <- 0
  
  vat_sectors_normalized$Households_pct[
    !is.finite(
      vat_sectors_normalized$Households_pct
    )
  ] <- 0
  
  vat_sectors_normalized$NPISHs_pct[
    !is.finite(
      vat_sectors_normalized$NPISHs_pct
    )
  ] <- 0
  
  vat_sectors_normalized$Government_pct[
    !is.finite(
      vat_sectors_normalized$Government_pct
    )
  ] <- 0
  
  vat_sectors_normalized$Percentage_Check <-
    vat_sectors_normalized$Businesses_pct +
    vat_sectors_normalized$Households_pct +
    vat_sectors_normalized$NPISHs_pct +
    vat_sectors_normalized$Government_pct
  
  
  # 6. Total VAT by product - Baseline ----
  
  vat_nace_tbl_total_bu <-
    CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Final_Demand_Total
    ) |>
    dplyr::mutate(
      Scenario = "Baseline"
    )
  
  
  # 7. Total VAT by product - Simulation ----
  
  vat_nace_tbl_total_sim <-
    CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Final_Demand_Total
    ) |>
    dplyr::mutate(
      Scenario = "Simulation"
    )
  
  
  # 8. Total VAT by product - Combined ----
  
  vat_nace_tbl_total_combined <-
    dplyr::bind_rows(
      vat_nace_tbl_total_bu,
      vat_nace_tbl_total_sim
    )
  
  
  # 9. Business VAT - Baseline ----
  
  vat_nace_tbl_businesses_bu <-
    CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Total_Revenues_from_Intermediate_Inputs
    ) |>
    dplyr::mutate(
      Scenario = "Baseline"
    )
  
  
  # 10. Business VAT - Simulation ----
  
  vat_nace_tbl_businesses_sim <-
    CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Total_Revenues_from_Intermediate_Inputs
    ) |>
    dplyr::mutate(
      Scenario = "Simulation"
    )
  
  
  # 11. Business VAT - Combined ----
  
  vat_nace_tbl_businesses_combined <-
    dplyr::bind_rows(
      vat_nace_tbl_businesses_bu,
      vat_nace_tbl_businesses_sim
    )
  
  
  # 12. Household VAT - Baseline ----
  
  vat_nace_tbl_hh_bu <-
    CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Final_Demand_HH
    ) |>
    dplyr::mutate(
      Scenario = "Baseline"
    )
  
  
  # 13. Household VAT - Simulation ----
  
  vat_nace_tbl_hh_sim <-
    CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Final_Demand_HH
    ) |>
    dplyr::mutate(
      Scenario = "Simulation"
    )
  
  
  # 14. Household VAT - Combined ----
  
  vat_nace_tbl_hh_combined <-
    dplyr::bind_rows(
      vat_nace_tbl_hh_bu,
      vat_nace_tbl_hh_sim
    )
  
  
  # 15. Government VAT - Baseline ----
  
  vat_nace_tbl_gov_bu <-
    CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Final_Demand_Government
    ) |>
    dplyr::mutate(
      Scenario = "Baseline"
    )
  
  
  # 16. Government VAT - Simulation ----
  
  vat_nace_tbl_gov_sim <-
    CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Final_Demand_Government
    ) |>
    dplyr::mutate(
      Scenario = "Simulation"
    )
  
  
  # 17. Government VAT - Combined ----
  
  vat_nace_tbl_gov_combined <-
    dplyr::bind_rows(
      vat_nace_tbl_gov_bu,
      vat_nace_tbl_gov_sim
    )
  
  
})


print(
  "Script Charts Preparation Done !"
)