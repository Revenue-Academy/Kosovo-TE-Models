suppressMessages({
  
  
  # 1. Baseline policy scenario ----
  
  SIM_SCENARIO_EST_CAL_FACTOR_BU <- CPA_TAXABLE_PROPORTIONS_BU
  
  SIM_SCENARIO_EST_CAL_FACTOR_BU <- SIM_SCENARIO_EST_CAL_FACTOR_BU |>
    dplyr::mutate(
      Simulated_Policy_Exempt = ifelse(
        is.na(ProportionExempted),
        Current_Policy_Exempt,
        ProportionExempted
      ),
      Simulated_Policy_Reduced_Rate = ifelse(
        is.na(ProportionPreferentialRate1),
        Current_Policy_Reduced_Rate,
        ProportionPreferentialRate1
      ),
      Simulated_Policy_Fully_Taxable =
        1 -
        Simulated_Policy_Exempt -
        Simulated_Policy_Reduced_Rate
    )
  
  
  # 2. Tax expenditure scenario ----
  
  SIM_SCENARIO_EST_TE <- CPA_TAXABLE_PROPORTIONS_BU
  
  SIM_SCENARIO_EST_TE$Simulated_Policy_Fully_Taxable <- 1
  SIM_SCENARIO_EST_TE$Simulated_Policy_Reduced_Rate <- 0
  SIM_SCENARIO_EST_TE$Simulated_Policy_Exempt <- 0
  
  
  # 3. Effective VAT rate scenario ----
  
  SIM_SCENARIO_EFFECTIVE_VAT_RATE <- SIM_SCENARIO_EST_CAL_FACTOR_BU
  
  SIM_SCENARIO_EFFECTIVE_VAT_RATE$ProportionPreferentialRate1 <- TE_EXEMPT
  
  SIM_SCENARIO_EFFECTIVE_VAT_RATE <- SIM_SCENARIO_EFFECTIVE_VAT_RATE |>
    dplyr::mutate(
      Simulated_Policy_Exempt = ifelse(
        is.na(ProportionExempted),
        Current_Policy_Exempt,
        ProportionExempted
      ),
      Simulated_Policy_Reduced_Rate = ifelse(
        is.na(ProportionPreferentialRate1),
        Current_Policy_Reduced_Rate,
        ProportionPreferentialRate1
      ),
      Simulated_Policy_Fully_Taxable =
        1 -
        Simulated_Policy_Exempt -
        Simulated_Policy_Reduced_Rate
    )
  
  
  # 4. Benchmark revenues ----
  
  CPA_PRODUCTS_BU <- data.table::copy(
    CPA_PRODUCTS
  )
  
  CPA_PRODUCTS_BU$BM_Rev <- CPA_PRODUCTS_BU$Use_K_NetPurch |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Final_consumption_expenditure_by_households
    ) |>
    dplyr::mutate(
      Final_Demand_HH =
        dplyr::if_else(
          PRODUCT_INDUSTRY_CODE == "L",
          Final_consumption_expenditure_by_households - imputed_rents,
          Final_consumption_expenditure_by_households
        ) *
        benchmark_tax_rate_bu
    ) |>
    dplyr::select(
      -Final_consumption_expenditure_by_households
    )
  
  CPA_PRODUCTS_BU$BM_Rev$Final_Demand_HH[
    is.na(CPA_PRODUCTS_BU$BM_Rev$Final_Demand_HH)
  ] <- 0
  
  CPA_PRODUCTS_BU$BM_Rev$Final_Demand_HH[
    grepl(
      "Imputed rent",
      CPA_PRODUCTS_BU$BM_Rev$PRODUCT_INDUSTRY_NAME
    )
  ] <- 0
  
  CPA_PRODUCTS_BU$BM_Rev$Final_Demand_NPISH <-
    CPA_PRODUCTS_BU$Use_K_NetPurch$Final_consumption_expenditure_NPISH *
    benchmark_tax_rate_bu
  
  CPA_PRODUCTS_BU$BM_Rev$Final_Demand_NPISH[
    is.na(CPA_PRODUCTS_BU$BM_Rev$Final_Demand_NPISH)
  ] <- 0
  
  CPA_PRODUCTS_BU$BM_Rev$Final_Demand_NPISH[
    grepl(
      "Imputed rent",
      CPA_PRODUCTS_BU$BM_Rev$PRODUCT_INDUSTRY_NAME
    )
  ] <- 0
  
  CPA_PRODUCTS_BU$BM_Rev$Final_Demand_Government <-
    CPA_PRODUCTS_BU$Use_K_NetPurch$Final_consumption_expenditure_by_government *
    benchmark_tax_rate_bu
  
  CPA_PRODUCTS_BU$BM_Rev$Final_Demand_Government[
    is.na(CPA_PRODUCTS_BU$BM_Rev$Final_Demand_Government)
  ] <- 0
  
  CPA_PRODUCTS_BU$BM_Rev$Final_Demand_Government[
    grepl(
      "Imputed rent",
      CPA_PRODUCTS_BU$BM_Rev$PRODUCT_INDUSTRY_NAME
    )
  ] <- 0
  
  
  # Residential construction - product F
  
  CPA_PRODUCTS_BU$BM_Rev$Final_Demand_HH[
    CPA_PRODUCTS_BU$BM_Rev$PRODUCT_INDUSTRY_CODE == "F"
  ] <-
    CPA_PRODUCTS_BU$Use_K_NetPurch$Final_consumption_expenditure_by_households[
      CPA_PRODUCTS_BU$Use_K_NetPurch$PRODUCT_INDUSTRY_CODE == "F"
    ] *
    vat_rate_on_residential_construction
  
  
  CPA_PRODUCTS_BU$BM_Rev$Final_Demand_Total <- rowSums(
    cbind(
      CPA_PRODUCTS_BU$BM_Rev$Final_Demand_HH,
      CPA_PRODUCTS_BU$BM_Rev$Final_Demand_NPISH,
      CPA_PRODUCTS_BU$BM_Rev$Final_Demand_Government
    ),
    na.rm = TRUE
  )
  
  Sum_of_Final_Demand_Total <- sum(
    CPA_PRODUCTS_BU$BM_Rev$Final_Demand_Total,
    na.rm = TRUE
  )
  
  
  # 5. Industry share - calibration scenario ----
  
  SUPPLY_DOM_EST_CAL_FACTOR <- data.table::copy(
    SUPPLY_DOM
  )
  
  NACE_INDUSTRIES_EST_CAL_FACTOR <- data.table::copy(
    NACE_INDUSTRIES
  )
  
  EST.IS_EST_CAL_FACTOR <- SUPPLY_DOM_EST_CAL_FACTOR |>
    dplyr::left_join(
      SIM_SCENARIO_EST_CAL_FACTOR_BU |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      value =
        value *
        Simulated_Policy_Exempt
    ) |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      INDUSTRY_CODE,
      INDUSTRY_NAME,
      value
    )
  
  EST.IS_EST_CAL_FACTOR$value[
    is.na(EST.IS_EST_CAL_FACTOR$value)
  ] <- 0
  
  NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS <- EST.IS_EST_CAL_FACTOR |>
    dplyr::group_by(
      INDUSTRY_CODE,
      INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Industry_Share = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      NACE_INDUSTRIES_EST_CAL_FACTOR$Supply_Dom |>
        dplyr::select(
          INDUSTRY_CODE,
          Total_output_by_industries_at_basic_prices
        ),
      by = "INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      Industry_Share =
        Industry_Share /
        Total_output_by_industries_at_basic_prices
    ) |>
    dplyr::select(
      INDUSTRY_CODE,
      INDUSTRY_NAME,
      Industry_Share
    ) |>
    dplyr::arrange(
      INDUSTRY_CODE
    )
  
  NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS$Industry_Share[
    !is.finite(
      NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS$Industry_Share
    )
  ] <- 0
  
  
  # 6. Industry share - baseline ----
  
  EST.IS <- SUPPLY_DOM |>
    dplyr::left_join(
      SIM_SCENARIO_EST_CAL_FACTOR_BU |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      value =
        value *
        Simulated_Policy_Exempt
    ) |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      INDUSTRY_CODE,
      INDUSTRY_NAME,
      value
    )
  
  EST.IS$value[
    is.na(EST.IS$value)
  ] <- 0
  
  NACE_INDUSTRIES$Est.IS <- EST.IS |>
    dplyr::group_by(
      INDUSTRY_CODE,
      INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Industry_Share = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      NACE_INDUSTRIES$Supply_Dom |>
        dplyr::select(
          INDUSTRY_CODE,
          Total_output_by_industries_at_basic_prices
        ),
      by = "INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      Industry_Share =
        Industry_Share /
        Total_output_by_industries_at_basic_prices
    ) |>
    dplyr::select(
      INDUSTRY_CODE,
      INDUSTRY_NAME,
      Industry_Share
    ) |>
    dplyr::arrange(
      INDUSTRY_CODE
    )
  
  NACE_INDUSTRIES$Est.IS$Industry_Share[
    !is.finite(
      NACE_INDUSTRIES$Est.IS$Industry_Share
    )
  ] <- 0
  
  
  # 7. Industry share - TE ----
  
  SUPPLY_DOM_EST_TE <- data.table::copy(
    SUPPLY_DOM
  )
  
  NACE_INDUSTRIES_EST_TE <- data.table::copy(
    NACE_INDUSTRIES
  )
  
  EST.IS_EST_TE <- SUPPLY_DOM_EST_TE |>
    dplyr::left_join(
      SIM_SCENARIO_EST_TE |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      value =
        value *
        Simulated_Policy_Exempt
    ) |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      INDUSTRY_CODE,
      INDUSTRY_NAME,
      value
    )
  
  EST.IS_EST_TE$value[
    is.na(EST.IS_EST_TE$value)
  ] <- 0
  
  NACE_INDUSTRIES_EST_TE$Est.IS <- EST.IS_EST_TE |>
    dplyr::group_by(
      INDUSTRY_CODE,
      INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Industry_Share = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      NACE_INDUSTRIES_EST_TE$Supply_Dom |>
        dplyr::select(
          INDUSTRY_CODE,
          Total_output_by_industries_at_basic_prices
        ),
      by = "INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      Industry_Share =
        Industry_Share /
        Total_output_by_industries_at_basic_prices
    ) |>
    dplyr::select(
      INDUSTRY_CODE,
      INDUSTRY_NAME,
      Industry_Share
    ) |>
    dplyr::arrange(
      INDUSTRY_CODE
    )
  
  NACE_INDUSTRIES_EST_TE$Est.IS$Industry_Share[
    !is.finite(
      NACE_INDUSTRIES_EST_TE$Est.IS$Industry_Share
    )
  ] <- 0
  
  
  # 8. Industry share - effective VAT rate ----
  
  SUPPLY_DOM_EFFECTIVE_VAT_RATE <- data.table::copy(
    SUPPLY_DOM
  )
  
  NACE_INDUSTRIES_EFFECTIVE_VAT_RATE <- data.table::copy(
    NACE_INDUSTRIES
  )
  
  EST.IS_EFFECTIVE_VAT_RATE <- SUPPLY_DOM_EFFECTIVE_VAT_RATE |>
    dplyr::left_join(
      SIM_SCENARIO_EFFECTIVE_VAT_RATE |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      value =
        value *
        Simulated_Policy_Exempt
    ) |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      INDUSTRY_CODE,
      INDUSTRY_NAME,
      value
    )
  
  EST.IS_EFFECTIVE_VAT_RATE$value[
    is.na(EST.IS_EFFECTIVE_VAT_RATE$value)
  ] <- 0
  
  NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS <- EST.IS_EFFECTIVE_VAT_RATE |>
    dplyr::group_by(
      INDUSTRY_CODE,
      INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Industry_Share = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Supply_Dom |>
        dplyr::select(
          INDUSTRY_CODE,
          Total_output_by_industries_at_basic_prices
        ),
      by = "INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      Industry_Share =
        Industry_Share /
        Total_output_by_industries_at_basic_prices
    ) |>
    dplyr::select(
      INDUSTRY_CODE,
      INDUSTRY_NAME,
      Industry_Share
    ) |>
    dplyr::arrange(
      INDUSTRY_CODE
    )
  
  NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS$Industry_Share[
    !is.finite(
      NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS$Industry_Share
    )
  ] <- 0
  
  
  # 9. Revenue - calibration scenario ----
  
  USE_K_DOM_NETPURCH_EST_CAL_FACTOR <- data.table::copy(
    USE_K_DOM_NETPURCH
  )
  
  CPA_PRODUCTS_EST_CAL_FACTOR_BU <- data.table::copy(
    CPA_PRODUCTS_BU
  )
  
  EST_REV_EST_CAL_FACTOR <- USE_K_DOM_NETPURCH_EST_CAL_FACTOR |>
    dplyr::left_join(
      NACE_INDUSTRIES_EST_CAL_FACTOR$Est.IS |>
        dplyr::select(
          INDUSTRY_CODE,
          Industry_Share
        ),
      by = "INDUSTRY_CODE"
    ) |>
    dplyr::left_join(
      SIM_SCENARIO_EST_CAL_FACTOR_BU |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt,
          Simulated_Policy_Reduced_Rate,
          Simulated_Policy_Fully_Taxable,
          PreferentialVATRate_1,
          StandardVATRate
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      value =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Industry_Share *
        value
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev <- EST_REV_EST_CAL_FACTOR |>
    dplyr::group_by(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_Revenues_from_Intermediate_Inputs = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev <-
    CPA_PRODUCTS_EST_CAL_FACTOR_BU$Use_K_NetPurch |>
    dplyr::left_join(
      SIM_SCENARIO_EST_CAL_FACTOR_BU |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt,
          Simulated_Policy_Reduced_Rate,
          Simulated_Policy_Fully_Taxable,
          PreferentialVATRate_1,
          StandardVATRate
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      Final_Demand_HH =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        dplyr::if_else(
          PRODUCT_INDUSTRY_CODE == "L",
          Final_consumption_expenditure_by_households - imputed_rents,
          Final_consumption_expenditure_by_households
        ),
      Final_Demand_NPISH =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Final_consumption_expenditure_NPISH,
      Final_Demand_Government =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Final_consumption_expenditure_by_government
    ) |>
    dplyr::left_join(
      CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Total_Revenues_from_Intermediate_Inputs
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Total_Revenues_from_Intermediate_Inputs,
      Final_Demand_HH,
      Final_Demand_NPISH,
      Final_Demand_Government
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE
    )
  
  
  # Residential construction - F
  
  CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev$Final_Demand_HH[
    CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev$PRODUCT_INDUSTRY_CODE == "F"
  ] <-
    CPA_PRODUCTS_EST_CAL_FACTOR_BU$Use_K_NetPurch$Final_consumption_expenditure_by_households[
      CPA_PRODUCTS_EST_CAL_FACTOR_BU$Use_K_NetPurch$PRODUCT_INDUSTRY_CODE == "F"
    ] *
    (
      1 -
        SIM_SCENARIO_EST_CAL_FACTOR_BU$Simulated_Policy_Exempt[
          match(
            "F",
            SIM_SCENARIO_EST_CAL_FACTOR_BU$PRODUCT_INDUSTRY_CODE
          )
        ]
    ) *
    vat_rate_on_residential_construction
  
  
  CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev$Final_Demand_Total <- rowSums(
    cbind(
      CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev$Total_Revenues_from_Intermediate_Inputs,
      CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev$Final_Demand_HH,
      CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev$Final_Demand_NPISH,
      CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev$Final_Demand_Government
    ),
    na.rm = TRUE
  )
  
  
  # 10. Revenue - baseline ----
  
  EST_REV <- USE_K_DOM_NETPURCH |>
    dplyr::left_join(
      NACE_INDUSTRIES$Est.IS |>
        dplyr::select(
          INDUSTRY_CODE,
          Industry_Share
        ),
      by = "INDUSTRY_CODE"
    ) |>
    dplyr::left_join(
      SIM_SCENARIO_EST_CAL_FACTOR_BU |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt,
          Simulated_Policy_Reduced_Rate,
          Simulated_Policy_Fully_Taxable,
          PreferentialVATRate_1,
          StandardVATRate
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      value =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Industry_Share *
        value
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_BU$Est_Rev <- EST_REV |>
    dplyr::group_by(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_Revenues_from_Intermediate_Inputs = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_BU$Est_Rev <- CPA_PRODUCTS_BU$Use_K_NetPurch |>
    dplyr::left_join(
      SIM_SCENARIO_EST_CAL_FACTOR_BU |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt,
          Simulated_Policy_Reduced_Rate,
          Simulated_Policy_Fully_Taxable,
          PreferentialVATRate_1,
          StandardVATRate
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      Final_Demand_HH =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        dplyr::if_else(
          PRODUCT_INDUSTRY_CODE == "L",
          Final_consumption_expenditure_by_households - imputed_rents,
          Final_consumption_expenditure_by_households
        ),
      Final_Demand_NPISH =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Final_consumption_expenditure_NPISH,
      Final_Demand_Government =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Final_consumption_expenditure_by_government
    ) |>
    dplyr::left_join(
      CPA_PRODUCTS_BU$Est_Rev |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Total_Revenues_from_Intermediate_Inputs
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Total_Revenues_from_Intermediate_Inputs,
      Final_Demand_HH,
      Final_Demand_NPISH,
      Final_Demand_Government
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_BU$Est_Rev$Final_Demand_HH[
    CPA_PRODUCTS_BU$Est_Rev$PRODUCT_INDUSTRY_CODE == "F"
  ] <-
    CPA_PRODUCTS_BU$Use_K_NetPurch$Final_consumption_expenditure_by_households[
      CPA_PRODUCTS_BU$Use_K_NetPurch$PRODUCT_INDUSTRY_CODE == "F"
    ] *
    (
      1 -
        SIM_SCENARIO_EST_CAL_FACTOR_BU$Simulated_Policy_Exempt[
          match(
            "F",
            SIM_SCENARIO_EST_CAL_FACTOR_BU$PRODUCT_INDUSTRY_CODE
          )
        ]
    ) *
    vat_rate_on_residential_construction
  
  CPA_PRODUCTS_BU$Est_Rev$Final_Demand_Total <- rowSums(
    cbind(
      CPA_PRODUCTS_BU$Est_Rev$Total_Revenues_from_Intermediate_Inputs,
      CPA_PRODUCTS_BU$Est_Rev$Final_Demand_HH,
      CPA_PRODUCTS_BU$Est_Rev$Final_Demand_NPISH,
      CPA_PRODUCTS_BU$Est_Rev$Final_Demand_Government
    ),
    na.rm = TRUE
  )
  
  NACE_NAMES <- CPA_PRODUCTS_BU$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    )
  
  
  # 11. Revenue - TE ----
  
  USE_K_DOM_NETPURCH_EST_TE <- data.table::copy(
    USE_K_DOM_NETPURCH
  )
  
  CPA_PRODUCTS_EST_TE_BU <- data.table::copy(
    CPA_PRODUCTS_BU
  )
  
  EST_REV_EST_TE <- USE_K_DOM_NETPURCH_EST_TE |>
    dplyr::left_join(
      NACE_INDUSTRIES_EST_TE$Est.IS |>
        dplyr::select(
          INDUSTRY_CODE,
          Industry_Share
        ),
      by = "INDUSTRY_CODE"
    ) |>
    dplyr::left_join(
      SIM_SCENARIO_EST_TE |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt,
          Simulated_Policy_Reduced_Rate,
          Simulated_Policy_Fully_Taxable,
          PreferentialVATRate_1,
          StandardVATRate
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      value =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Industry_Share *
        value
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_EST_TE_BU$Est_Rev <- EST_REV_EST_TE |>
    dplyr::group_by(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_Revenues_from_Intermediate_Inputs = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_EST_TE_BU$Est_Rev <-
    CPA_PRODUCTS_EST_TE_BU$Use_K_NetPurch |>
    dplyr::left_join(
      SIM_SCENARIO_EST_TE |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt,
          Simulated_Policy_Reduced_Rate,
          Simulated_Policy_Fully_Taxable,
          PreferentialVATRate_1,
          StandardVATRate
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      Final_Demand_HH =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        dplyr::if_else(
          PRODUCT_INDUSTRY_CODE == "L",
          Final_consumption_expenditure_by_households - imputed_rents,
          Final_consumption_expenditure_by_households
        ),
      Final_Demand_NPISH =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Final_consumption_expenditure_NPISH,
      Final_Demand_Government =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Final_consumption_expenditure_by_government
    ) |>
    dplyr::left_join(
      CPA_PRODUCTS_EST_TE_BU$Est_Rev |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Total_Revenues_from_Intermediate_Inputs
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Total_Revenues_from_Intermediate_Inputs,
      Final_Demand_HH,
      Final_Demand_NPISH,
      Final_Demand_Government
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_EST_TE_BU$Est_Rev$Final_Demand_HH[
    CPA_PRODUCTS_EST_TE_BU$Est_Rev$PRODUCT_INDUSTRY_CODE == "F"
  ] <-
    CPA_PRODUCTS_EST_TE_BU$Use_K_NetPurch$Final_consumption_expenditure_by_households[
      CPA_PRODUCTS_EST_TE_BU$Use_K_NetPurch$PRODUCT_INDUSTRY_CODE == "F"
    ] *
    (
      1 -
        SIM_SCENARIO_EST_TE$Simulated_Policy_Exempt[
          match(
            "F",
            SIM_SCENARIO_EST_TE$PRODUCT_INDUSTRY_CODE
          )
        ]
    ) *
    vat_rate_on_residential_construction
  
  CPA_PRODUCTS_EST_TE_BU$Est_Rev$Final_Demand_Total <- rowSums(
    cbind(
      CPA_PRODUCTS_EST_TE_BU$Est_Rev$Total_Revenues_from_Intermediate_Inputs,
      CPA_PRODUCTS_EST_TE_BU$Est_Rev$Final_Demand_HH,
      CPA_PRODUCTS_EST_TE_BU$Est_Rev$Final_Demand_NPISH,
      CPA_PRODUCTS_EST_TE_BU$Est_Rev$Final_Demand_Government
    ),
    na.rm = TRUE
  )
  
  
  # 12. Revenue - effective VAT rate ----
  
  USE_K_DOM_NETPURCH_EFFECTIVE_VAT_RATE <- data.table::copy(
    USE_K_DOM_NETPURCH
  )
  
  CPA_PRODUCTS_EFFECTIVE_VAT_RATE <- data.table::copy(
    CPA_PRODUCTS_BU
  )
  
  EST_REV_EFFECTIVE_VAT_RATE <- USE_K_DOM_NETPURCH_EFFECTIVE_VAT_RATE |>
    dplyr::left_join(
      NACE_INDUSTRIES_EFFECTIVE_VAT_RATE$Est.IS |>
        dplyr::select(
          INDUSTRY_CODE,
          Industry_Share
        ),
      by = "INDUSTRY_CODE"
    ) |>
    dplyr::left_join(
      SIM_SCENARIO_EFFECTIVE_VAT_RATE |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt,
          Simulated_Policy_Reduced_Rate,
          Simulated_Policy_Fully_Taxable,
          PreferentialVATRate_1,
          StandardVATRate
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      value =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Industry_Share *
        value
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE,
      INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev <- EST_REV_EFFECTIVE_VAT_RATE |>
    dplyr::group_by(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME
    ) |>
    dplyr::summarise(
      Total_Revenues_from_Intermediate_Inputs = sum(
        value,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev <-
    CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Use_K_NetPurch |>
    dplyr::left_join(
      SIM_SCENARIO_EFFECTIVE_VAT_RATE |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Simulated_Policy_Exempt,
          Simulated_Policy_Reduced_Rate,
          Simulated_Policy_Fully_Taxable,
          PreferentialVATRate_1,
          StandardVATRate
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::mutate(
      Final_Demand_HH =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        dplyr::if_else(
          PRODUCT_INDUSTRY_CODE == "L",
          Final_consumption_expenditure_by_households - imputed_rents,
          Final_consumption_expenditure_by_households
        ),
      Final_Demand_NPISH =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Final_consumption_expenditure_NPISH,
      Final_Demand_Government =
        (
          Simulated_Policy_Reduced_Rate *
            PreferentialVATRate_1 +
            Simulated_Policy_Fully_Taxable *
            StandardVATRate
        ) *
        Final_consumption_expenditure_by_government
    ) |>
    dplyr::left_join(
      CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev |>
        dplyr::select(
          PRODUCT_INDUSTRY_CODE,
          Total_Revenues_from_Intermediate_Inputs
        ),
      by = "PRODUCT_INDUSTRY_CODE"
    ) |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Total_Revenues_from_Intermediate_Inputs,
      Final_Demand_HH,
      Final_Demand_NPISH,
      Final_Demand_Government
    ) |>
    dplyr::arrange(
      PRODUCT_INDUSTRY_CODE
    )
  
  CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Final_Demand_HH[
    CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$PRODUCT_INDUSTRY_CODE == "F"
  ] <-
    CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Use_K_NetPurch$Final_consumption_expenditure_by_households[
      CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Use_K_NetPurch$PRODUCT_INDUSTRY_CODE == "F"
    ] *
    (
      1 -
        SIM_SCENARIO_EST_CAL_FACTOR_BU$Simulated_Policy_Exempt[
          match(
            "F",
            SIM_SCENARIO_EST_CAL_FACTOR_BU$PRODUCT_INDUSTRY_CODE
          )
        ]
    ) *
    vat_rate_on_residential_construction
  
  CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Final_Demand_Total <- rowSums(
    cbind(
      CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Total_Revenues_from_Intermediate_Inputs,
      CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Final_Demand_HH,
      CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Final_Demand_NPISH,
      CPA_PRODUCTS_EFFECTIVE_VAT_RATE$Est_Rev$Final_Demand_Government
    ),
    na.rm = TRUE
  )
  
  
  # 13. Calibration factor ----
  
  Results_EST_CAL_FACTOR <- as.list(
    c(
      "VAT_Gap",
      "Simulation"
    )
  )
  
  names(
    Results_EST_CAL_FACTOR
  ) <- c(
    "VAT_Gap",
    "Simulation"
  )
  
  Results_EST_CAL_FACTOR$VAT_Gap <- as.data.frame(
    sum(
      CPA_PRODUCTS_EST_CAL_FACTOR_BU$BM_Rev$Final_Demand_Total,
      na.rm = TRUE
    )
  )
  
  colnames(
    Results_EST_CAL_FACTOR$VAT_Gap
  ) <- "Benchmark_VAT_LCU"
  
  Uncalibrated_VAT <- sum(
    CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev$Final_Demand_Total,
    na.rm = TRUE
  )
  
  VAT_control <-
    sum(
      CPA_PRODUCTS_EST_CAL_FACTOR_BU$Use_VAT$Total_use_at_basic_prices,
      na.rm = TRUE
    ) -
    sum(
      CPA_PRODUCTS_EST_CAL_FACTOR_BU$Use_VAT$Exports_FOB,
      na.rm = TRUE
    )
  
  Locked_Calibration_Factor_TEST <-
    VAT_control /
    Uncalibrated_VAT
  
  Locked_Calibration_Factor_BU <-
    Locked_Calibration_Factor_TEST
  
  Results_EST_CAL_FACTOR$VAT_Gap$Calibrated_VAT_Est.LCU <-
    sum(
      CPA_PRODUCTS_EST_CAL_FACTOR_BU$Est_Rev$Final_Demand_Total,
      na.rm = TRUE
    ) *
    Locked_Calibration_Factor_BU
  
  Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU <-
    VAT_control
  
  Results_EST_CAL_FACTOR$VAT_Gap$Total_VAT_Gap.LCU <-
    Results_EST_CAL_FACTOR$VAT_Gap$Benchmark_VAT_LCU -
    Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU
  
  Results_EST_CAL_FACTOR$VAT_Gap$Total_VAT_Gap.Prc <-
    Results_EST_CAL_FACTOR$VAT_Gap$Total_VAT_Gap.LCU /
    Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU
  
  
  # 14. Baseline results ----
  
  Results <- as.list(
    c(
      "VAT_Gap",
      "Simulation"
    )
  )
  
  names(
    Results
  ) <- c(
    "VAT_Gap",
    "Simulation"
  )
  
  Results$VAT_Gap <- as.data.frame(
    sum(
      CPA_PRODUCTS_BU$BM_Rev$Final_Demand_Total,
      na.rm = TRUE
    )
  )
  
  colnames(
    Results$VAT_Gap
  ) <- "Benchmark_VAT_LCU"
  
  Results$VAT_Gap$Uncalibrated_VAT_Est.LCU <- sum(
    CPA_PRODUCTS_BU$Est_Rev$Final_Demand_Total,
    na.rm = TRUE
  )
  
  Results$VAT_Gap$Calibrated_VAT_Est.LCU <-
    Results$VAT_Gap$Uncalibrated_VAT_Est.LCU *
    Locked_Calibration_Factor_BU
  
  Results$VAT_Gap$VAT_Control_Total.LCU <-
    VAT_control
  
  Results$VAT_Gap$Total_VAT_Gap.LCU <-
    Results$VAT_Gap$Benchmark_VAT_LCU -
    Results$VAT_Gap$VAT_Control_Total.LCU
  
  Results$VAT_Gap$Total_VAT_Gap.Prc <-
    Results$VAT_Gap$Total_VAT_Gap.LCU /
    Results$VAT_Gap$VAT_Control_Total.LCU
  
  Results$Simulation <- as.data.frame(
    as.integer(
      Results$VAT_Gap$Calibrated_VAT_Est.LCU -
        Results$VAT_Gap$VAT_Control_Total.LCU
    )
  )
  
  colnames(
    Results$Simulation
  ) <- "Simulated_Change_in_Revenues.LCU"
  
  Results$Simulation$Simulated_Change_in_Revenues.Prc <-
    (
      Results$Simulation$Simulated_Change_in_Revenues.LCU /
        Results$VAT_Gap$VAT_Control_Total.LCU
    ) *
    100
  
  
  # 15. TE results ----
  
  Results_EST_TE <- as.list(
    c(
      "VAT_Gap",
      "Simulation"
    )
  )
  
  names(
    Results_EST_TE
  ) <- c(
    "VAT_Gap",
    "Simulation"
  )
  
  Results_EST_TE$VAT_Gap <- as.data.frame(
    sum(
      CPA_PRODUCTS_EST_TE_BU$BM_Rev$Final_Demand_Total,
      na.rm = TRUE
    )
  )
  
  colnames(
    Results_EST_TE$VAT_Gap
  ) <- "Benchmark_VAT_LCU"
  
  Results_EST_TE$VAT_Gap$Uncalibrated_VAT_Est.LCU <- sum(
    CPA_PRODUCTS_EST_TE_BU$Est_Rev$Final_Demand_Total,
    na.rm = TRUE
  )
  
  Results_EST_TE$VAT_Gap$Calibrated_VAT_Est.LCU <-
    Results_EST_TE$VAT_Gap$Uncalibrated_VAT_Est.LCU *
    Locked_Calibration_Factor_BU
  
  Results_EST_TE$VAT_Gap$VAT_Control_Total.LCU <-
    VAT_control
  
  Results_EST_TE$VAT_Gap$Total_VAT_Gap.LCU <-
    Results_EST_TE$VAT_Gap$Benchmark_VAT_LCU -
    Results_EST_TE$VAT_Gap$VAT_Control_Total.LCU
  
  Results_EST_TE$VAT_Gap$Total_VAT_Gap.Prc <-
    Results_EST_TE$VAT_Gap$Total_VAT_Gap.LCU /
    Results_EST_TE$VAT_Gap$VAT_Control_Total.LCU
  
  Results_EST_TE$Simulation <- as.data.frame(
    as.integer(
      Results_EST_TE$VAT_Gap$Calibrated_VAT_Est.LCU -
        Results_EST_TE$VAT_Gap$VAT_Control_Total.LCU
    )
  )
  
  colnames(
    Results_EST_TE$Simulation
  ) <- "Simulated_Change_in_Revenues.LCU"
  
  Results_EST_TE$Simulation$Simulated_Change_in_Revenues.Prc <-
    (
      Results_EST_TE$Simulation$Simulated_Change_in_Revenues.LCU /
        Results_EST_TE$VAT_Gap$VAT_Control_Total.LCU
    ) *
    100
  
  
  # 16. VAT gaps ----
  
  Results$VAT_Gap$Policy_Gap.LCU <-
    Results_EST_TE$Simulation$Simulated_Change_in_Revenues.LCU
  
  Results$VAT_Gap$Policy_Gap.Prc <-
    Results$VAT_Gap$Policy_Gap.LCU /
    Results$VAT_Gap$VAT_Control_Total.LCU
  
  Results$VAT_Gap$Compliance_Gap.LCU <-
    Results$VAT_Gap$Total_VAT_Gap.LCU -
    Results$VAT_Gap$Policy_Gap.LCU
  
  Results$VAT_Gap$Compliance_Gap.Prc <-
    Results$VAT_Gap$Compliance_Gap.LCU /
    Results$VAT_Gap$VAT_Control_Total.LCU
  
  Results$VAT_Gap$Calibration_Factor <-
    Locked_Calibration_Factor_BU
  
  
  Results_EST_CAL_FACTOR$VAT_Gap$Policy_Gap.LCU <-
    Results_EST_TE$Simulation$Simulated_Change_in_Revenues.LCU
  
  Results_EST_CAL_FACTOR$VAT_Gap$Policy_Gap.Prc <-
    Results_EST_CAL_FACTOR$VAT_Gap$Policy_Gap.LCU /
    Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU
  
  Results_EST_CAL_FACTOR$VAT_Gap$Compliance_Gap.LCU <-
    Results_EST_CAL_FACTOR$VAT_Gap$Total_VAT_Gap.LCU -
    Results_EST_CAL_FACTOR$VAT_Gap$Policy_Gap.LCU
  
  Results_EST_CAL_FACTOR$VAT_Gap$Compliance_Gap.Prc <-
    Results_EST_CAL_FACTOR$VAT_Gap$Compliance_Gap.LCU /
    Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU
  
  Results_EST_CAL_FACTOR$VAT_Gap$Calibration_Factor <-
    Locked_Calibration_Factor_BU
  
  Results_EST_CAL_FACTOR$Simulation <- as.data.frame(
    as.integer(
      Results_EST_CAL_FACTOR$VAT_Gap$Calibrated_VAT_Est.LCU -
        Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU
    )
  )
  
  colnames(
    Results_EST_CAL_FACTOR$Simulation
  ) <- "Simulated_Change_in_Revenues.LCU"
  
  Results_EST_CAL_FACTOR$Simulation$Simulated_Change_in_Revenues.Prc <-
    (
      Results_EST_CAL_FACTOR$Simulation$Simulated_Change_in_Revenues.LCU /
        Results_EST_CAL_FACTOR$VAT_Gap$VAT_Control_Total.LCU
    ) *
    100
  
  
  # 17. Original output objects ----
  
  Export_Main_Results <- Results$VAT_Gap
  
  Simulation_Results <- Results$Simulation
  
  Revenue_VAT_TOTAL_BU <- CPA_PRODUCTS_BU$Est_Rev |>
    dplyr::select(
      PRODUCT_INDUSTRY_CODE,
      PRODUCT_INDUSTRY_NAME,
      Total_Revenues_from_Intermediate_Inputs,
      Final_Demand_HH,
      Final_Demand_NPISH,
      Final_Demand_Government,
      Final_Demand_Total
    )
  
  Revenue_VAT_TOTAL_BU[3:7] <-
    Revenue_VAT_TOTAL_BU[3:7] *
    Locked_Calibration_Factor_BU
  
  Simulation_Results_EST_TE <-
    Results_EST_TE$Simulation
  
  Simulation_Results_EST_TE_te <-
    Results_EST_TE$Simulation$Simulated_Change_in_Revenues.LCU
  
  
  # 18. Main results ----
  
  MainResultsVATFinal_BU <- data.frame(
    Descriptions = c(
      "Benchmark VAT rate",
      "Benchmark VAT",
      "Uncalibrated VAT",
      "Calibrated VAT",
      "Total VAT Gap",
      "Policy Gap",
      "Compliance Gap",
      "Total VAT Gap Pct",
      "Policy Gap Pct",
      "Compliance Gap Pct"
    ),
    Values = c(
      benchmark_tax_rate_bu,
      round(
        Results$VAT_Gap$Benchmark_VAT_LCU,
        0
      ),
      round(
        Results$VAT_Gap$Uncalibrated_VAT_Est.LCU,
        0
      ),
      round(
        Results$VAT_Gap$Calibrated_VAT_Est.LCU,
        0
      ),
      round(
        Results$VAT_Gap$Total_VAT_Gap.LCU,
        0
      ),
      round(
        Results$VAT_Gap$Policy_Gap.LCU,
        0
      ),
      round(
        Results$VAT_Gap$Compliance_Gap.LCU,
        0
      ),
      round(
        Results$VAT_Gap$Total_VAT_Gap.Prc *
          100,
        1
      ),
      round(
        Results$VAT_Gap$Policy_Gap.Prc *
          100,
        1
      ),
      round(
        Results$VAT_Gap$Compliance_Gap.Prc *
          100,
        1
      )
    )
  )
  
  MainResultsVATFinal_BU <- data.table::as.data.table(
    MainResultsVATFinal_BU
  )
  
})


print(
  "Script TAX CALCULATOR-MODULE BU Done !"
)

print(
  MainResultsVATFinal_BU 
)