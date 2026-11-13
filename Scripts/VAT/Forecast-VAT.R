'Revenue forecast'

suppressMessages({
  
  
  # 1. Forecast settings ----
  
  if (!exists("growfactors_vat", envir = .GlobalEnv)) {
    stop(
      "growfactors_vat not found. Import the VAT growth-factor file before running Forecast-VAT.R."
    )
  }
  
  if (
    !all(
      c(
        "year",
        "growth_factor"
      ) %in% names(growfactors_vat)
    )
  ) {
    stop(
      "growfactors_vat must contain columns: year and growth_factor."
    )
  }
  
  growfactors_vat$year <- as.integer(
    growfactors_vat$year
  )
  
  growfactors_vat$growth_factor <- as.numeric(
    growfactors_vat$growth_factor
  )
  
  growfactors_vat <- growfactors_vat |>
    dplyr::arrange(
      year
    )
  
  if (
    any(
      is.na(growfactors_vat$year)
    ) ||
    any(
      !is.finite(growfactors_vat$growth_factor)
    )
  ) {
    stop(
      "growfactors_vat contains invalid year or growth_factor values."
    )
  }
  
  if (
    any(
      duplicated(
        growfactors_vat$year
      )
    )
  ) {
    stop(
      "growfactors_vat contains duplicate years."
    )
  }
  
  base_year_VAT <- min(
    growfactors_vat$year,
    na.rm = TRUE
  )
  
  end_year <- max(
    growfactors_vat$year,
    na.rm = TRUE
  )
  
  forecast_horizon <- seq(
    base_year_VAT,
    end_year
  )
  
  if (
    !SimulationYear %in%
    forecast_horizon
  ) {
    stop(
      paste0(
        "SimulationYear ",
        SimulationYear,
        " is outside the available forecast horizon ",
        base_year_VAT,
        "-",
        end_year,
        "."
      )
    )
  }
  
  
  # 2. CPA revenues - Baseline ----
  
  vat_nace_cpa_tbl_bu <-
    CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev
  
  forecast_bu_cpa <- expand.grid(
    year = forecast_horizon,
    PRODUCT_INDUSTRY_CODE =
      vat_nace_cpa_tbl_bu$PRODUCT_INDUSTRY_CODE,
    stringsAsFactors = FALSE
  )
  
  forecast_bu_cpa <-
    forecast_bu_cpa |>
    dplyr::left_join(
      growfactors_vat,
      by = "year"
    ) |>
    dplyr::left_join(
      vat_nace_cpa_tbl_bu |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          PRODUCT_INDUSTRY_NAME,
          Total_Revenues_from_Intermediate_Inputs,
          Final_Demand_HH,
          Final_Demand_NPISH,
          Final_Demand_Government,
          Final_Demand_Total
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      year
    )
  
  forecast_bu_cpa$Grossed_Values <- NA_real_
  
  forecast_bu_cpa$Businesses_Grossed <- NA_real_
  
  forecast_bu_cpa$Households_Grossed <- NA_real_
  
  forecast_bu_cpa$NPISH_Grossed <- NA_real_
  
  forecast_bu_cpa$Government_Grossed <- NA_real_
  
  
  for (
    code in unique(
      forecast_bu_cpa$PRODUCT_INDUSTRY_CODE
    )
  ) {
    
    indices <- which(
      forecast_bu_cpa$PRODUCT_INDUSTRY_CODE ==
        code
    )
    
    for (
      i in seq_along(
        indices
      )
    ) {
      
      current_index <- indices[i]
      
      if (
        forecast_bu_cpa$year[current_index] ==
        base_year_VAT
      ) {
        
        forecast_bu_cpa$Grossed_Values[current_index] <-
          forecast_bu_cpa$Final_Demand_Total[current_index]
        
        forecast_bu_cpa$Businesses_Grossed[current_index] <-
          forecast_bu_cpa$Total_Revenues_from_Intermediate_Inputs[current_index]
        
        forecast_bu_cpa$Households_Grossed[current_index] <-
          forecast_bu_cpa$Final_Demand_HH[current_index]
        
        forecast_bu_cpa$NPISH_Grossed[current_index] <-
          forecast_bu_cpa$Final_Demand_NPISH[current_index]
        
        forecast_bu_cpa$Government_Grossed[current_index] <-
          forecast_bu_cpa$Final_Demand_Government[current_index]
        
      } else {
        
        previous_index <- indices[i - 1]
        
        forecast_bu_cpa$Grossed_Values[current_index] <-
          forecast_bu_cpa$Grossed_Values[previous_index] *
          forecast_bu_cpa$growth_factor[current_index]
        
        forecast_bu_cpa$Businesses_Grossed[current_index] <-
          forecast_bu_cpa$Businesses_Grossed[previous_index] *
          forecast_bu_cpa$growth_factor[current_index]
        
        forecast_bu_cpa$Households_Grossed[current_index] <-
          forecast_bu_cpa$Households_Grossed[previous_index] *
          forecast_bu_cpa$growth_factor[current_index]
        
        forecast_bu_cpa$NPISH_Grossed[current_index] <-
          forecast_bu_cpa$NPISH_Grossed[previous_index] *
          forecast_bu_cpa$growth_factor[current_index]
        
        forecast_bu_cpa$Government_Grossed[current_index] <-
          forecast_bu_cpa$Government_Grossed[previous_index] *
          forecast_bu_cpa$growth_factor[current_index]
        
      }
      
    }
    
  }
  
  forecast_bu_cpa <- forecast_bu_cpa |>
    dplyr::transmute(
      year,
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Total_Revenues_from_Intermediate_Inputs =
        Businesses_Grossed,
      Final_Demand_HH =
        Households_Grossed,
      Final_Demand_NPISH =
        NPISH_Grossed,
      Final_Demand_Government =
        Government_Grossed,
      value =
        Grossed_Values,
      scenario =
        "Baseline"
    )
  
  
  # 3. CPA revenues - Simulation ----
  
  vat_nace_cpa_tbl_sim <-
    CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev
  
  forecast_sim_cpa <- expand.grid(
    year = forecast_horizon,
    PRODUCT_INDUSTRY_CODE =
      vat_nace_cpa_tbl_sim$PRODUCT_INDUSTRY_CODE,
    stringsAsFactors = FALSE
  )
  
  forecast_sim_cpa <-
    forecast_sim_cpa |>
    dplyr::left_join(
      growfactors_vat,
      by = "year"
    ) |>
    dplyr::left_join(
      vat_nace_cpa_tbl_sim |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          PRODUCT_INDUSTRY_NAME,
          Total_Revenues_from_Intermediate_Inputs,
          Final_Demand_HH,
          Final_Demand_NPISH,
          Final_Demand_Government,
          Final_Demand_Total
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      year
    )
  
  forecast_sim_cpa$Grossed_Values <- NA_real_
  
  forecast_sim_cpa$Businesses_Grossed <- NA_real_
  
  forecast_sim_cpa$Households_Grossed <- NA_real_
  
  forecast_sim_cpa$NPISH_Grossed <- NA_real_
  
  forecast_sim_cpa$Government_Grossed <- NA_real_
  
  
  for (
    code in unique(
      forecast_sim_cpa$PRODUCT_INDUSTRY_CODE
    )
  ) {
    
    indices <- which(
      forecast_sim_cpa$PRODUCT_INDUSTRY_CODE ==
        code
    )
    
    for (
      i in seq_along(
        indices
      )
    ) {
      
      current_index <- indices[i]
      
      if (
        forecast_sim_cpa$year[current_index] ==
        base_year_VAT
      ) {
        
        forecast_sim_cpa$Grossed_Values[current_index] <-
          forecast_sim_cpa$Final_Demand_Total[current_index]
        
        forecast_sim_cpa$Businesses_Grossed[current_index] <-
          forecast_sim_cpa$Total_Revenues_from_Intermediate_Inputs[current_index]
        
        forecast_sim_cpa$Households_Grossed[current_index] <-
          forecast_sim_cpa$Final_Demand_HH[current_index]
        
        forecast_sim_cpa$NPISH_Grossed[current_index] <-
          forecast_sim_cpa$Final_Demand_NPISH[current_index]
        
        forecast_sim_cpa$Government_Grossed[current_index] <-
          forecast_sim_cpa$Final_Demand_Government[current_index]
        
      } else {
        
        previous_index <- indices[i - 1]
        
        forecast_sim_cpa$Grossed_Values[current_index] <-
          forecast_sim_cpa$Grossed_Values[previous_index] *
          forecast_sim_cpa$growth_factor[current_index]
        
        forecast_sim_cpa$Businesses_Grossed[current_index] <-
          forecast_sim_cpa$Businesses_Grossed[previous_index] *
          forecast_sim_cpa$growth_factor[current_index]
        
        forecast_sim_cpa$Households_Grossed[current_index] <-
          forecast_sim_cpa$Households_Grossed[previous_index] *
          forecast_sim_cpa$growth_factor[current_index]
        
        forecast_sim_cpa$NPISH_Grossed[current_index] <-
          forecast_sim_cpa$NPISH_Grossed[previous_index] *
          forecast_sim_cpa$growth_factor[current_index]
        
        forecast_sim_cpa$Government_Grossed[current_index] <-
          forecast_sim_cpa$Government_Grossed[previous_index] *
          forecast_sim_cpa$growth_factor[current_index]
        
      }
      
    }
    
  }
  
  forecast_sim_cpa <- forecast_sim_cpa |>
    dplyr::transmute(
      year,
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Total_Revenues_from_Intermediate_Inputs =
        Businesses_Grossed,
      Final_Demand_HH =
        Households_Grossed,
      Final_Demand_NPISH =
        NPISH_Grossed,
      Final_Demand_Government =
        Government_Grossed,
      value =
        Grossed_Values,
      scenario =
        "Simulation"
    )
  
  
  # 4. CPA revenues - Combined ----
  
  forecast_combined_cpa <-
    dplyr::bind_rows(
      forecast_bu_cpa,
      forecast_sim_cpa
    ) |>
    dplyr::rename(
      Businesses_VAT =
        Total_Revenues_from_Intermediate_Inputs,
      Households_VAT =
        Final_Demand_HH,
      NPISH_VAT =
        Final_Demand_NPISH,
      Goverment_VAT =
        Final_Demand_Government,
      Total_VAT =
        value
    )
  
  
  # 5. Tax expenditures - Baseline ----
  
  te_benchmark_tbl_bu <-
    CPA_PRODUCTS_EST_TE_BU$BM_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Final_Demand_Total
    ) |>
    dplyr::rename(
      BenchmarkRevenue =
        Final_Demand_Total
    )
  
  te_est_tbl_bu <-
    CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      Final_Demand_Total
    ) |>
    dplyr::rename(
      EstimationRevenue =
        Final_Demand_Total
    )
  
  te_merged_bu <-
    te_benchmark_tbl_bu |>
    dplyr::left_join(
      te_est_tbl_bu,
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      te_bu =
        BenchmarkRevenue -
        EstimationRevenue
    )
  
  forecast_bu_cpa_te <- expand.grid(
    year = forecast_horizon,
    PRODUCT_INDUSTRY_CODE =
      te_merged_bu$PRODUCT_INDUSTRY_CODE,
    stringsAsFactors = FALSE
  )
  
  forecast_bu_cpa_te <-
    forecast_bu_cpa_te |>
    dplyr::left_join(
      growfactors_vat,
      by = "year"
    ) |>
    dplyr::left_join(
      te_merged_bu |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          PRODUCT_INDUSTRY_NAME,
          te_bu
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      year
    )
  
  forecast_bu_cpa_te$Grossed_Values <- NA_real_
  
  for (
    code in unique(
      forecast_bu_cpa_te$PRODUCT_INDUSTRY_CODE
    )
  ) {
    
    indices <- which(
      forecast_bu_cpa_te$PRODUCT_INDUSTRY_CODE ==
        code
    )
    
    for (
      i in seq_along(
        indices
      )
    ) {
      
      current_index <- indices[i]
      
      if (
        forecast_bu_cpa_te$year[current_index] ==
        base_year_VAT
      ) {
        
        forecast_bu_cpa_te$Grossed_Values[current_index] <-
          forecast_bu_cpa_te$te_bu[current_index]
        
      } else {
        
        previous_index <- indices[i - 1]
        
        forecast_bu_cpa_te$Grossed_Values[current_index] <-
          forecast_bu_cpa_te$Grossed_Values[previous_index] *
          forecast_bu_cpa_te$growth_factor[current_index]
        
      }
      
    }
    
  }
  
  forecast_bu_cpa_te <-
    forecast_bu_cpa_te |>
    dplyr::transmute(
      year,
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      value =
        Grossed_Values,
      scenario =
        "Baseline"
    )
  
  
  # 6. Tax expenditures - Simulation ----
  
  te_benchmark_tbl_sim <-
    CPA_PRODUCTS_EST_TE_SIM$BM_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Final_Demand_Total
    ) |>
    dplyr::rename(
      BenchmarkRevenue =
        Final_Demand_Total
    )
  
  te_est_tbl_sim <-
    CPA_PRODUCTS_EST_CAL_FACTOR_SIM$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      Final_Demand_Total
    ) |>
    dplyr::rename(
      EstimationRevenue =
        Final_Demand_Total
    )
  
  te_merged_sim <-
    te_benchmark_tbl_sim |>
    dplyr::left_join(
      te_est_tbl_sim,
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      te_sim =
        BenchmarkRevenue -
        EstimationRevenue
    )
  
  forecast_sim_cpa_te <- expand.grid(
    year = forecast_horizon,
    PRODUCT_INDUSTRY_CODE =
      te_merged_sim$PRODUCT_INDUSTRY_CODE,
    stringsAsFactors = FALSE
  )
  
  forecast_sim_cpa_te <-
    forecast_sim_cpa_te |>
    dplyr::left_join(
      growfactors_vat,
      by = "year"
    ) |>
    dplyr::left_join(
      te_merged_sim |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          PRODUCT_INDUSTRY_NAME,
          te_sim
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      year
    )
  
  forecast_sim_cpa_te$Grossed_Values <- NA_real_
  
  for (
    code in unique(
      forecast_sim_cpa_te$PRODUCT_INDUSTRY_CODE
    )
  ) {
    
    indices <- which(
      forecast_sim_cpa_te$PRODUCT_INDUSTRY_CODE ==
        code
    )
    
    for (
      i in seq_along(
        indices
      )
    ) {
      
      current_index <- indices[i]
      
      if (
        forecast_sim_cpa_te$year[current_index] ==
        base_year_VAT
      ) {
        
        forecast_sim_cpa_te$Grossed_Values[current_index] <-
          forecast_sim_cpa_te$te_sim[current_index]
        
      } else {
        
        previous_index <- indices[i - 1]
        
        forecast_sim_cpa_te$Grossed_Values[current_index] <-
          forecast_sim_cpa_te$Grossed_Values[previous_index] *
          forecast_sim_cpa_te$growth_factor[current_index]
        
      }
      
    }
    
  }
  
  forecast_sim_cpa_te <-
    forecast_sim_cpa_te |>
    dplyr::transmute(
      year,
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      value =
        Grossed_Values,
      scenario =
        "Simulation"
    )
  
  
  # 7. Tax expenditures - Combined ----
  
  forecast_combined_cpa_te <-
    dplyr::bind_rows(
      forecast_bu_cpa_te,
      forecast_sim_cpa_te
    )
  
  
  # 8. Aggregate results - Baseline ----
  
  MainResultsVATFinal_BU_forecast <-
    MainResultsVATFinal_BU |>
    dplyr::filter(
      Descriptions %in%
        c(
          "Benchmark VAT",
          "Uncalibrated VAT",
          "Calibrated VAT",
          "Total VAT Gap",
          "Policy Gap",
          "Compliance Gap"
        )
    )
  
  forecast_bu <- expand.grid(
    year = forecast_horizon,
    Descriptions =
      MainResultsVATFinal_BU_forecast$Descriptions,
    stringsAsFactors = FALSE
  )
  
  forecast_bu <-
    forecast_bu |>
    dplyr::left_join(
      growfactors_vat,
      by = "year"
    ) |>
    dplyr::left_join(
      MainResultsVATFinal_BU_forecast |>
        dplyr::select(
          Descriptions,
          Values
        ),
      by = "Descriptions"
    ) |>
    dplyr::arrange(
      factor(
        Descriptions,
        levels = c(
          "Benchmark VAT",
          "Uncalibrated VAT",
          "Calibrated VAT",
          "Total VAT Gap",
          "Policy Gap",
          "Compliance Gap"
        )
      ),
      year
    )
  
  forecast_bu$Grossed_Values <- NA_real_
  
  for (
    desc in unique(
      forecast_bu$Descriptions
    )
  ) {
    
    indices <- which(
      forecast_bu$Descriptions ==
        desc
    )
    
    for (
      i in seq_along(
        indices
      )
    ) {
      
      current_index <- indices[i]
      
      if (
        forecast_bu$year[current_index] ==
        base_year_VAT
      ) {
        
        forecast_bu$Grossed_Values[current_index] <-
          forecast_bu$Values[current_index]
        
      } else {
        
        previous_index <- indices[i - 1]
        
        forecast_bu$Grossed_Values[current_index] <-
          forecast_bu$Grossed_Values[previous_index] *
          forecast_bu$growth_factor[current_index]
        
      }
      
    }
    
  }
  
  forecast_bu <-
    forecast_bu |>
    dplyr::transmute(
      year,
      Descriptions,
      value =
        Grossed_Values,
      scenario =
        "Baseline"
    )
  
  
  # 9. Aggregate results - Simulation ----
  
  MainResultsVATFinal_SIM_forecast <-
    MainResultsVATFinal_SIM |>
    dplyr::filter(
      Descriptions %in%
        c(
          "Benchmark VAT",
          "Uncalibrated VAT",
          "Calibrated VAT",
          "Total VAT Gap",
          "Policy Gap",
          "Compliance Gap"
        )
    )
  
  forecast_sim <- expand.grid(
    year = forecast_horizon,
    Descriptions =
      MainResultsVATFinal_SIM_forecast$Descriptions,
    stringsAsFactors = FALSE
  )
  
  forecast_sim <-
    forecast_sim |>
    dplyr::left_join(
      growfactors_vat,
      by = "year"
    ) |>
    dplyr::left_join(
      MainResultsVATFinal_SIM_forecast |>
        dplyr::select(
          Descriptions,
          Values
        ),
      by = "Descriptions"
    ) |>
    dplyr::arrange(
      factor(
        Descriptions,
        levels = c(
          "Benchmark VAT",
          "Uncalibrated VAT",
          "Calibrated VAT",
          "Total VAT Gap",
          "Policy Gap",
          "Compliance Gap"
        )
      ),
      year
    )
  
  forecast_sim$Grossed_Values <- NA_real_
  
  for (
    desc in unique(
      forecast_sim$Descriptions
    )
  ) {
    
    indices <- which(
      forecast_sim$Descriptions ==
        desc
    )
    
    for (
      i in seq_along(
        indices
      )
    ) {
      
      current_index <- indices[i]
      
      if (
        forecast_sim$year[current_index] ==
        base_year_VAT
      ) {
        
        forecast_sim$Grossed_Values[current_index] <-
          forecast_sim$Values[current_index]
        
      } else {
        
        previous_index <- indices[i - 1]
        
        forecast_sim$Grossed_Values[current_index] <-
          forecast_sim$Grossed_Values[previous_index] *
          forecast_sim$growth_factor[current_index]
        
      }
      
    }
    
  }
  
  forecast_sim <-
    forecast_sim |>
    dplyr::transmute(
      year,
      Descriptions,
      value =
        Grossed_Values,
      scenario =
        "Simulation"
    )
  
  
  # 10. Aggregate results - Combined ----
  
  forecast_combined_agg <-
    dplyr::bind_rows(
      forecast_bu,
      forecast_sim
    )
  
  baseline_data <-
    forecast_combined_agg |>
    dplyr::filter(
      scenario ==
        "Baseline"
    )
  
  simulation_data <-
    forecast_combined_agg |>
    dplyr::filter(
      scenario ==
        "Simulation"
    ) |>
    dplyr::left_join(
      baseline_data |>
        dplyr::select(
          year,
          Descriptions,
          baseline_value =
            value
        ),
      by = c(
        "year",
        "Descriptions"
      )
    ) |>
    dplyr::mutate(
      value =
        ifelse(
          year < SimulationYear,
          baseline_value,
          value
        )
    ) |>
    dplyr::select(
      -baseline_value
    )
  
  forecast_combined_agg <-
    dplyr::bind_rows(
      baseline_data,
      simulation_data
    ) |>
    dplyr::arrange(
      Descriptions,
      scenario,
      year
    )
  
  
  # 11. VAT revenue table ----
  
  forecast_combined_agg_tbl <-
    forecast_combined_agg |>
    dplyr::filter(
      Descriptions ==
        "Calibrated VAT"
    )
  
  forecast_combined_agg_tbl_wide_raw <-
    forecast_combined_agg_tbl |>
    tidyr::pivot_wider(
      id_cols =
        year,
      names_from =
        c(
          scenario,
          Descriptions
        ),
      values_from =
        value
    ) |>
    dplyr::rename(
      `Current Law (In EUR thousand)` =
        `Baseline_Calibrated VAT`,
      `Simulation (In EUR thousand)` =
        `Simulation_Calibrated VAT`
    ) |>
    dplyr::mutate(
      `Fiscal Impact (In EUR thousand)` =
        `Simulation (In EUR thousand)` -
        `Current Law (In EUR thousand)`
    ) |>
    dplyr::mutate(
      dplyr::across(
        -year,
        ~ round(
          .x,
          1
        )
      )
    )
  
  
  # # 12. VAT revenue table - GDP ----
  # 
  # if (
  #   exists(
  #     "MACRO_FISCAL_INDICATORS",
  #     envir = .GlobalEnv
  #   )
  # ) {
  #   
  #   forecast_combined_agg_tbl_wide <-
  #     forecast_combined_agg_tbl_wide_raw |>
  #     dplyr::left_join(
  #       MACRO_FISCAL_INDICATORS,
  #       by = c(
  #         "year" =
  #           "Year"
  #       )
  #     )
  #   
  #   if (
  #     "Nominal_GDP" %in%
  #     names(
  #       forecast_combined_agg_tbl_wide
  #     )
  #   ) {
  #     
  #     forecast_combined_agg_tbl_wide <-
  #       forecast_combined_agg_tbl_wide |>
  #       dplyr::mutate(
  #         `Current Law (Pct of GDP)` =
  #           round(
  #             (
  #               `Current Law (In EUR thousand)` /
  #                 (
  #                   Nominal_GDP *
  #                     1000
  #                 )
  #             ) *
  #               100,
  #             2
  #           ),
  #         `Simulation (Pct of GDP)` =
  #           round(
  #             (
  #               `Simulation (In EUR thousand)` /
  #                 (
  #                   Nominal_GDP *
  #                     1000
  #                 )
  #             ) *
  #               100,
  #             2
  #           ),
  #         `Fiscal Impact (Pct of GDP)` =
  #           round(
  #             (
  #               `Fiscal Impact (In EUR thousand)` /
  #                 (
  #                   Nominal_GDP *
  #                     1000
  #                 )
  #             ) *
  #               100,
  #             2
  #           )
  #       ) |>
  #       dplyr::select(
  #         -Nominal_GDP
  #       )
  #     
  #   } else {
  #     
  #     forecast_combined_agg_tbl_wide <-
  #       forecast_combined_agg_tbl_wide |>
  #       dplyr::mutate(
  #         `Current Law (Pct of GDP)` =
  #           NA_real_,
  #         `Simulation (Pct of GDP)` =
  #           NA_real_,
  #         `Fiscal Impact (Pct of GDP)` =
  #           NA_real_
  #       )
  #     
  #   }
  #   
  # } else {
  #   
  #   forecast_combined_agg_tbl_wide <-
  #     forecast_combined_agg_tbl_wide_raw |>
  #     dplyr::mutate(
  #       `Current Law (Pct of GDP)` =
  #         NA_real_,
  #       `Simulation (Pct of GDP)` =
  #         NA_real_,
  #       `Fiscal Impact (Pct of GDP)` =
  #         NA_real_
  #     )
  #   
  # }
  # 
  # forecast_combined_agg_tbl_wide <-
  #   data.table::as.data.table(
  #     forecast_combined_agg_tbl_wide
  #   )
  # 
  # 
  # # 13. Tax expenditure table ----
  # 
  # forecast_TE_raw <-
  #   forecast_combined_agg |>
  #   dplyr::filter(
  #     Descriptions ==
  #       "Policy Gap",
  #     scenario ==
  #       "Simulation"
  #   )
  # 
  # if (
  #   exists(
  #     "MACRO_FISCAL_INDICATORS",
  #     envir = .GlobalEnv
  #   )
  # ) {
  #   
  #   forecast_TE_tbl <-
  #     forecast_TE_raw |>
  #     dplyr::left_join(
  #       MACRO_FISCAL_INDICATORS,
  #       by = c(
  #         "year" =
  #           "Year"
  #       )
  #     )
  #   
  #   if (
  #     "Nominal_GDP" %in%
  #     names(
  #       forecast_TE_tbl
  #     )
  #   ) {
  #     
  #     forecast_TE_tbl <-
  #       forecast_TE_tbl |>
  #       dplyr::mutate(
  #         `Tax Expenditures (Pct of GDP)` =
  #           round(
  #             (
  #               value /
  #                 (
  #                   Nominal_GDP *
  #                     1000
  #                 )
  #             ) *
  #               100,
  #             2
  #           )
  #       ) |>
  #       dplyr::select(
  #         -Nominal_GDP,
  #         -Descriptions,
  #         -scenario
  #       )
  #     
  #   } else {
  #     
  #     forecast_TE_tbl <-
  #       forecast_TE_tbl |>
  #       dplyr::mutate(
  #         `Tax Expenditures (Pct of GDP)` =
  #           NA_real_
  #       ) |>
  #       dplyr::select(
  #         -Descriptions,
  #         -scenario
  #       )
  #     
  #   }
  #   
  # } else {
  #   
  #   forecast_TE_tbl <-
  #     forecast_TE_raw |>
  #     dplyr::mutate(
  #       `Tax Expenditures (Pct of GDP)` =
  #         NA_real_
  #     ) |>
  #     dplyr::select(
  #       -Descriptions,
  #       -scenario
  #     )
  #   
  # }
  # 
  # forecast_TE_tbl <-
  #   forecast_TE_tbl |>
  #   dplyr::rename(
  #     `Tax Expenditures (In EUR thousand)` =
  #       value
  #   ) |>
  #   dplyr::mutate(
  #     `Tax Expenditures (In EUR thousand)` =
  #       round(
  #         `Tax Expenditures (In EUR thousand)`,
  #         1
  #       )
  #   )
  # 
  # forecast_TE_tbl <-
  #   data.table::as.data.table(
  #     forecast_TE_tbl
  #   )
  # 
  
  
  
  # 12. Validate nominal GDP data ----
  
  if (
    !exists(
      "nominal_gdp_vat",
      envir = .GlobalEnv
    )
  ) {
    stop(
      "nominal_gdp_vat not found."
    )
  }
  
  if (
    !all(
      c(
        "year",
        "gdp"
      ) %in%
      names(
        nominal_gdp_vat
      )
    )
  ) {
    stop(
      "nominal_gdp_vat must contain columns: year and gdp."
    )
  }
  
  nominal_gdp_vat$year <-
    as.integer(
      nominal_gdp_vat$year
    )
  
  nominal_gdp_vat$gdp <-
    as.numeric(
      nominal_gdp_vat$gdp
    )
  
  if (
    any(
      duplicated(
        nominal_gdp_vat$year
      )
    )
  ) {
    stop(
      "nominal_gdp_vat contains duplicate years."
    )
  }
  
  if (
    any(
      is.na(
        nominal_gdp_vat$year
      )
    ) ||
    any(
      !is.finite(
        nominal_gdp_vat$gdp
      )
    ) ||
    any(
      nominal_gdp_vat$gdp <= 0
    )
  ) {
    stop(
      "nominal_gdp_vat contains invalid year or GDP values."
    )
  }
  
  missing_gdp_years <-
    setdiff(
      forecast_horizon,
      nominal_gdp_vat$year
    )
  
  if (
    length(
      missing_gdp_years
    ) > 0
  ) {
    stop(
      paste0(
        "nominal_gdp_vat is missing GDP for year(s): ",
        paste(
          missing_gdp_years,
          collapse = ", "
        )
      )
    )
  }
  
  
  # 13. VAT revenue table - GDP ----
  
  forecast_combined_agg_tbl_wide <-
    forecast_combined_agg_tbl_wide_raw |>
    dplyr::left_join(
      nominal_gdp_vat,
      by = "year"
    ) |>
    dplyr::mutate(
      `Current Law (Pct of GDP)` =
        round(
          (
            `Current Law (In EUR thousand)` /
              (
                gdp *
                  1000
              )
          ) *
            100,
          2
        ),
      
      `Simulation (Pct of GDP)` =
        round(
          (
            `Simulation (In EUR thousand)` /
              (
                gdp *
                  1000
              )
          ) *
            100,
          2
        ),
      
      `Fiscal Impact (Pct of GDP)` =
        round(
          (
            `Fiscal Impact (In EUR thousand)` /
              (
                gdp *
                  1000
              )
          ) *
            100,
          2
        )
    ) |>
    dplyr::select(
      -gdp
    )
  
  forecast_combined_agg_tbl_wide <-
    data.table::as.data.table(
      forecast_combined_agg_tbl_wide
    )
  
  
  # 14. Tax expenditure table ----
  
  forecast_TE_raw <-
    forecast_combined_agg |>
    dplyr::filter(
      Descriptions ==
        "Policy Gap",
      scenario ==
        "Simulation"
    )
  
  forecast_TE_tbl <-
    forecast_TE_raw |>
    dplyr::left_join(
      nominal_gdp_vat,
      by = "year"
    ) |>
    dplyr::mutate(
      `Tax Expenditures (Pct of GDP)` =
        round(
          (
            value /
              (
                gdp *
                  1000
              )
          ) *
            100,
          2
        )
    ) |>
    dplyr::select(
      -gdp,
      -Descriptions,
      -scenario
    ) |>
    dplyr::rename(
      `Tax Expenditures (In EUR thousand)` =
        value
    ) |>
    dplyr::mutate(
      `Tax Expenditures (In EUR thousand)` =
        round(
          `Tax Expenditures (In EUR thousand)`,
          1
        )
    )
  
  forecast_TE_tbl <-
    data.table::as.data.table(
      forecast_TE_tbl
    )
  
  
  # 15. Combined tax expenditure table ----
  
  forecast_TE_raw_combined <-
    forecast_combined_agg |>
    dplyr::filter(
      Descriptions ==
        "Policy Gap"
    )
  
  forecast_TE_tbl_combined <-
    forecast_TE_raw_combined |>
    dplyr::left_join(
      nominal_gdp_vat,
      by = "year"
    ) |>
    dplyr::mutate(
      `Tax Expenditures (Pct of GDP)` =
        round(
          (
            value /
              (
                gdp *
                  1000
              )
          ) *
            100,
          2
        )
    ) |>
    dplyr::select(
      -gdp,
      -Descriptions
    ) |>
    dplyr::rename(
      `Tax Expenditures (In EUR thousand)` =
        value
    ) |>
    dplyr::mutate(
      `Tax Expenditures (In EUR thousand)` =
        round(
          `Tax Expenditures (In EUR thousand)`,
          1
        )
    )
  
  forecast_TE_tbl_combined <-
    data.table::as.data.table(
      forecast_TE_tbl_combined
    )
  
  # 14. Combined tax expenditure table ----
  
  forecast_TE_raw_combined <-
    forecast_combined_agg |>
    dplyr::filter(
      Descriptions ==
        "Policy Gap"
    )
  
  if (
    exists(
      "MACRO_FISCAL_INDICATORS",
      envir = .GlobalEnv
    )
  ) {
    
    forecast_TE_tbl_combined <-
      forecast_TE_raw_combined |>
      dplyr::left_join(
        MACRO_FISCAL_INDICATORS,
        by = c(
          "year" =
            "Year"
        )
      )
    
    if (
      "Nominal_GDP" %in%
      names(
        forecast_TE_tbl_combined
      )
    ) {
      
      forecast_TE_tbl_combined <-
        forecast_TE_tbl_combined |>
        dplyr::mutate(
          `Tax Expenditures (Pct of GDP)` =
            round(
              (
                value /
                  (
                    Nominal_GDP *
                      1000
                  )
              ) *
                100,
              2
            )
        ) |>
        dplyr::select(
          -Nominal_GDP,
          -Descriptions
        )
      
    } else {
      
      forecast_TE_tbl_combined <-
        forecast_TE_tbl_combined |>
        dplyr::mutate(
          `Tax Expenditures (Pct of GDP)` =
            NA_real_
        ) |>
        dplyr::select(
          -Descriptions
        )
      
    }
    
  } else {
    
    forecast_TE_tbl_combined <-
      forecast_TE_raw_combined |>
      dplyr::mutate(
        `Tax Expenditures (Pct of GDP)` =
          NA_real_
      ) |>
      dplyr::select(
        -Descriptions
      )
    
  }
  
  forecast_TE_tbl_combined <-
    forecast_TE_tbl_combined |>
    dplyr::rename(
      `Tax Expenditures (In EUR thousand)` =
        value
    ) |>
    dplyr::mutate(
      `Tax Expenditures (In EUR thousand)` =
        round(
          `Tax Expenditures (In EUR thousand)`,
          1
        )
    )
  
  forecast_TE_tbl_combined <-
    data.table::as.data.table(
      forecast_TE_tbl_combined
    )
  
  
  # 15. Selected CPA data ----
  
  custom_order <- LETTERS[1:21]
  
  forecast_combined_cpa_selected <-
    forecast_combined_cpa |>
    dplyr::filter(
      year ==
        SimulationYear
    ) |>
    dplyr::mutate(
      PRODUCT_INDUSTRY_CODE =
        factor(
          PRODUCT_INDUSTRY_CODE,
          levels =
            custom_order
        )
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      scenario
    )
  
  
  # 16. Selected TE data ----
  
  forecast_combined_te_selected <-
    forecast_combined_cpa_te |>
    dplyr::filter(
      year ==
        SimulationYear
    ) |>
    dplyr::mutate(
      PRODUCT_INDUSTRY_CODE =
        factor(
          PRODUCT_INDUSTRY_CODE,
          levels =
            custom_order
        )
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      scenario
    )
  
  
})


print(
  "Script Forecast is Done !"
)