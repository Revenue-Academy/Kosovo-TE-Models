###############################################################################
# PIT COMMERCIAL, SERVICES AND RENT MODULE 
#
# RUN THIS SCRIPT AFTER THE MAIN PIT SCRIPT.
#
# Reuses:
#   get_param_fun()
#   validate_parameter_table()
#   get_growth_factor_row()
#   apply_growth_factors()
#   summarize_PIT_fun_dt()
#
# No weights are used.

###############################################################################


#==============================================================================
# 0. CHECK SHARED FUNCTIONS
#==============================================================================

required_shared_functions_qs <- c(
  "get_param_fun",
  "validate_parameter_table",
  "get_growth_factor_row",
  "apply_growth_factors",
  "summarize_PIT_fun_dt"
)


available_shared_functions_qs <- vapply(
  required_shared_functions_qs,
  exists,
  logical(1),
  mode = "function",
  inherits = TRUE
)


missing_shared_functions_qs <- required_shared_functions_qs[
  !available_shared_functions_qs
]


if (length(missing_shared_functions_qs) > 0L) {
  stop(
    paste0(
      "Run the main PIT script first. ",
      "The following shared functions are missing: ",
      paste(
        missing_shared_functions_qs,
        collapse = ", "
      )
    )
  )
}


if (!"package:data.table" %in% search()) {
  stop(
    paste0(
      "The data.table package is not attached. ",
      "Run the main PIT script first."
    )
  )
}


#==============================================================================
# 1. QS MODULE FUNCTION
#==============================================================================

run_qs_pit_module <- function(
    raw_data,
    growth_data,
    parameters_raw,
    parameters_updated,
    simulation_year
) {
  
  module_start_time_qs <- proc.time()
  
  
  #----------------------------------------------------------------------------
  # 1.1 Create local copies
  #----------------------------------------------------------------------------
  
  raw_data <- as.data.table(
    copy(raw_data)
  )
  
  growth_data <- as.data.table(
    copy(growth_data)
  )
  
  parameters_raw <- as.data.table(
    copy(parameters_raw)
  )
  
  parameters_updated <- as.data.table(
    copy(parameters_updated)
  )
  
  
  #----------------------------------------------------------------------------
  # 1.2 Validate growth-factor table
  #----------------------------------------------------------------------------
  
  required_growth_columns_qs <- c(
    "Year",
    "scenarios",
    "grossIncomeCommercial",
    "grossIncomeServices",
    "gross_rent_income"
  )
  
  
  missing_growth_columns_qs <- setdiff(
    required_growth_columns_qs,
    names(growth_data)
  )
  
  
  if (length(missing_growth_columns_qs) > 0L) {
    stop(
      "pit_growth_factors_qs is missing columns: ",
      paste(
        missing_growth_columns_qs,
        collapse = ", "
      )
    )
  }
  
  
  setorder(
    growth_data,
    Year
  )
  
  
  if (anyNA(growth_data$Year)) {
    stop(
      "pit_growth_factors_qs contains missing Year values."
    )
  }
  
  
  if (anyNA(growth_data$scenarios)) {
    stop(
      "pit_growth_factors_qs contains missing scenario labels."
    )
  }
  
  
  if (anyDuplicated(growth_data$Year)) {
    stop(
      "Each year must appear exactly once in pit_growth_factors_qs."
    )
  }
  
  
  if (anyDuplicated(growth_data$scenarios)) {
    stop(
      "Each scenario must appear exactly once in pit_growth_factors_qs."
    )
  }
  
  
  #----------------------------------------------------------------------------
  # 1.3 Dynamic years and scenarios
  #----------------------------------------------------------------------------
  
  scenario_years_qs <- growth_data$Year
  scenarios_qs <- growth_data$scenarios
  
  forecast_horizon_qs <- scenario_years_qs
  
  base_year_qs <- min(
    scenario_years_qs
  )
  
  end_year_qs <- max(
    scenario_years_qs
  )
  
  
  #----------------------------------------------------------------------------
  # Determine where the simulation begins
  #----------------------------------------------------------------------------
  
  if (simulation_year <= base_year_qs) {
    
    start_index_qs <- 1L
    
    if (simulation_year < base_year_qs) {
      message(
        sprintf(
          paste0(
            "SimulationYear %s is before the first QS forecast year %s. ",
            "Updated parameters will apply from %s."
          ),
          simulation_year,
          base_year_qs,
          base_year_qs
        )
      )
    }
    
  } else if (simulation_year > end_year_qs) {
    
    stop(
      sprintf(
        paste0(
          "SimulationYear %s is after the QS forecast horizon. ",
          "The final available year is %s."
        ),
        simulation_year,
        end_year_qs
      )
    )
    
  } else {
    
    start_index_qs <- match(
      simulation_year,
      scenario_years_qs
    )
  }
  
  
  scenario_lookup_qs <- data.table(
    scenarios = scenarios_qs,
    year = scenario_years_qs
  )
  
  
  #----------------------------------------------------------------------------
  # 1.4 Validate raw QS data
  #----------------------------------------------------------------------------
  
  required_raw_columns_qs <- c(
    "id_n",
    "Year",
    "grossIncomeCommercial",
    "grossIncomeServices",
    "taxCommercial",
    "taxServices",
    "taxPaymentTotal",
    "gross_rent_income",
    "rent_tax_9pct",
    "rent_tax_withheld_others",
    "rent_tax_net",
    "total_amount_due"
  )
  
  
  missing_raw_columns_qs <- setdiff(
    required_raw_columns_qs,
    names(raw_data)
  )
  
  
  if (length(missing_raw_columns_qs) > 0L) {
    stop(
      "pit_raw_qs is missing columns: ",
      paste(
        missing_raw_columns_qs,
        collapse = ", "
      )
    )
  }
  
  
  numeric_raw_columns_qs <- c(
    "grossIncomeCommercial",
    "grossIncomeServices",
    "taxCommercial",
    "taxServices",
    "taxPaymentTotal",
    "gross_rent_income",
    "rent_tax_9pct",
    "rent_tax_withheld_others",
    "rent_tax_net",
    "total_amount_due"
  )
  
  
  nonnumeric_raw_columns_qs <- numeric_raw_columns_qs[
    !vapply(
      raw_data[, ..numeric_raw_columns_qs],
      is.numeric,
      logical(1)
    )
  ]
  
  
  if (length(nonnumeric_raw_columns_qs) > 0L) {
    stop(
      "The following QS columns must be numeric: ",
      paste(
        nonnumeric_raw_columns_qs,
        collapse = ", "
      )
    )
  }
  
  
  raw_years_qs <- unique(
    raw_data[
      !is.na(Year),
      Year
    ]
  )
  
  
 
  
  if (length(raw_years_qs) == 0L) {
    stop("pit_raw_qs contains no valid Year values.")
  }
  
  if (length(raw_years_qs) > 1L) {
    stop("pit_raw_qs contains multiple years: ",
         paste(raw_years_qs, collapse = ", "),
         ". Only one baseline year is allowed.")
  }
  
  if (raw_years_qs[1L] != base_year_qs) {
    message("QS data year (", raw_years_qs[1L], ") differs from growth factor ",
            "start year (", base_year_qs, "). ",
            "Overwriting QS Year with ", base_year_qs, ".")
    raw_data[, Year := base_year_qs]
  }
  
  
  #----------------------------------------------------------------------------
  # 1.5 Variables receiving cumulative growth factors

  #----------------------------------------------------------------------------
  
  vars_to_grow_qs <- c(
    "grossIncomeCommercial",
    "grossIncomeServices",
    "gross_rent_income"
  )
  
  
  missing_raw_growth_vars_qs <- setdiff(
    vars_to_grow_qs,
    names(raw_data)
  )
  
  
  if (length(missing_raw_growth_vars_qs) > 0L) {
    stop(
      "Missing QS growth variables in pit_raw_qs: ",
      paste(
        missing_raw_growth_vars_qs,
        collapse = ", "
      )
    )
  }
  
  
  missing_growth_factor_vars_qs <- setdiff(
    vars_to_grow_qs,
    names(growth_data)
  )
  
  
  if (length(missing_growth_factor_vars_qs) > 0L) {
    warning(
      paste0(
        "The following variables do not have growth-factor columns. ",
        "A factor of 1 will be used: ",
        paste(
          missing_growth_factor_vars_qs,
          collapse = ", "
        )
      )
    )
  }
  
  
  #----------------------------------------------------------------------------
  # 1.6 Validate QS policy parameters
  #
  # Exact parameter names:
  #   rate_commerical_qs
  #   rate_services_qs
  #   min_payment_small_qs
  #   rate_rent_qs
  #----------------------------------------------------------------------------
  
  required_parameters_qs <- c(
    "rate_commerical_qs",
    "rate_services_qs",
    "min_payment_small_qs",
    "rate_rent_qs"
  )
  
  
  validate_parameter_table(
    params_dt = parameters_raw,
    table_name = "pit_simulation_parameters_raw",
    required_years = scenario_years_qs,
    required_parameters = required_parameters_qs
  )
  
  
  updated_parameter_years_qs <- scenario_years_qs[
    seq.int(
      from = start_index_qs,
      to = length(scenario_years_qs)
    )
  ]
  
  
  validate_parameter_table(
    params_dt = parameters_updated,
    table_name = "pit_simulation_parameters_updated",
    required_years = updated_parameter_years_qs,
    required_parameters = required_parameters_qs
  )
  
  
  #----------------------------------------------------------------------------
  # 1.7 QS tax calculation
  #
  # This function is local to the QS module.
  #
  # All calculated columns end in "_calc" so that they are aggregated by
  # summarize_PIT_fun_dt().
  #----------------------------------------------------------------------------
  
  qs_tax_calc_fun <- function(
    dt_scn,
    params_dt,
    year
  ) {
    
    #--------------------------------------------------------------------------
    # Retrieve year-specific parameters
    #--------------------------------------------------------------------------
    
    rate_commerical_qs <- get_param_fun(
      params_dt,
      "rate_commerical_qs",
      year
    )
    
    
    rate_services_qs <- get_param_fun(
      params_dt,
      "rate_services_qs",
      year
    )
    
    
    min_payment_small_qs <- get_param_fun(
      params_dt,
      "min_payment_small_qs",
      year
    )
    
    
    rate_rent_qs <- get_param_fun(
      params_dt,
      "rate_rent_qs",
      year
    )
    
    
    #--------------------------------------------------------------------------
    # Validate parameters
    #--------------------------------------------------------------------------
    
    qs_rates <- c(
      rate_commerical_qs = rate_commerical_qs,
      rate_services_qs = rate_services_qs,
      rate_rent_qs = rate_rent_qs
    )
    
    
    invalid_rate_names_qs <- names(qs_rates)[
      !is.finite(qs_rates) |
        qs_rates < 0 |
        qs_rates > 1
    ]
    
    
    if (length(invalid_rate_names_qs) > 0L) {
      stop(
        sprintf(
          paste0(
            "The following QS rates must be between 0 and 1 ",
            "for year %s: %s."
          ),
          year,
          paste(
            invalid_rate_names_qs,
            collapse = ", "
          )
        )
      )
    }
    
    
    if (
      !is.finite(min_payment_small_qs) ||
      min_payment_small_qs < 0
    ) {
      stop(
        sprintf(
          paste0(
            "min_payment_small_qs must be nonnegative ",
            "for year %s."
          ),
          year
        )
      )
    }
    
    
    #==========================================================================
    # A. COMMERCIAL INCOME TAX
    #==========================================================================
    
    dt_scn[
      ,
      pit_commercial_qs_calc :=
        fcoalesce(
          grossIncomeCommercial,
          0
        ) *
        rate_commerical_qs
    ]
    
    
    #==========================================================================
    # B. SERVICES INCOME TAX
    #==========================================================================
    
    dt_scn[
      ,
      pit_services_qs_calc :=
        fcoalesce(
          grossIncomeServices,
          0
        ) *
        rate_services_qs
    ]
    
    
    #==========================================================================
    # C. COMMERCIAL AND SERVICES TAX BEFORE MINIMUM PAYMENT
    #==========================================================================
    
    dt_scn[
      ,
      pit_before_minimum_qs_calc :=
        pit_commercial_qs_calc +
        pit_services_qs_calc
    ]
    
    
    #==========================================================================
    # D. FINAL COMMERCIAL AND SERVICES PIT
    #==========================================================================
    
    dt_scn[
      ,
      pit_commercial_services_qs_calc := fifelse(
        pit_before_minimum_qs_calc > 0,
        pmax(
          min_payment_small_qs,
          pit_before_minimum_qs_calc
        ),
        0
      )
    ]
    
    
    #==========================================================================
    # E. MINIMUM-PAYMENT ADJUSTMENT
    #==========================================================================
    
    dt_scn[
      ,
      minimum_payment_adjustment_qs_calc :=
        pit_commercial_services_qs_calc -
        pit_before_minimum_qs_calc
    ]
    
    
    #==========================================================================
    # F. GROSS RENT PIT
    #
    # calc_rent_qs =
    #   gross_rent_income * rate_rent_qs
    #==========================================================================
    
    dt_scn[
      ,
      rent_qs_calc :=
        fcoalesce(
          gross_rent_income,
          0
        ) *
        rate_rent_qs
    ]
    
    
    #==========================================================================
    # G. NET RENT PIT
    #
    # calc_rent_qs_net =
    #   calc_rent_qs - rent_tax_net
    #
    # This formula is implemented exactly as provided.
    # Negative values are not automatically changed to zero.
    #==========================================================================
    
    dt_scn[
      ,
      rent_qs_net_calc :=
        rent_qs_calc -
        fcoalesce(
          rent_tax_net,
          0
        )
    ]
    
    
    #==========================================================================
    # H. TOTAL QS PIT
    #
    # calc_total_pit_qs =
    #   commercial/services PIT + net rent PIT
    #==========================================================================
    
    dt_scn[
      ,
      total_pit_qs_calc :=
        pit_commercial_services_qs_calc +
        rent_qs_net_calc
    ]
    
    
    invisible(dt_scn)
  }
  
  
  #============================================================================
  # 2. BUSINESS-AS-USUAL PATH
  #============================================================================
  
  PIT_BU_list_qs <- list()
  
  
  dt_scn_BU_qs <- copy(
    raw_data
  )
  
  
  for (i in seq_along(scenarios_qs)) {
    
    scenario_name <- scenarios_qs[i]
    scenario_year <- scenario_years_qs[i]
    
    
    growth_values <- get_growth_factor_row(
      scenario_name = scenario_name,
      growth_dt = growth_data,
      variables = vars_to_grow_qs
    )
    
    
    apply_growth_factors(
      dt = dt_scn_BU_qs,
      growth_values = growth_values
    )
    
    
    qs_tax_calc_fun(
      dt_scn = dt_scn_BU_qs,
      params_dt = parameters_raw,
      year = scenario_year
    )
    
    
    dt_scn_BU_qs[
      ,
      `:=`(
        scenario = scenario_name,
        simulation_data_year = scenario_year
      )
    ]
    
    
    PIT_BU_list_qs[[scenario_name]] <- copy(
      dt_scn_BU_qs
    )
  }
  
  
  #============================================================================
  # 3. SIMULATION PATH
  #============================================================================
  
  PIT_SIM_list_qs <- list()
  
  
  #----------------------------------------------------------------------------
  # 3.1 Years before SimulationYear equal BU
  #----------------------------------------------------------------------------
  
  if (start_index_qs > 1L) {
    
    for (i in seq_len(start_index_qs - 1L)) {
      
      scenario_name <- scenarios_qs[i]
      
      PIT_SIM_list_qs[[scenario_name]] <- copy(
        PIT_BU_list_qs[[scenario_name]]
      )
    }
  }
  
  
  #----------------------------------------------------------------------------
  # 3.2 Starting dataset for simulation
  #----------------------------------------------------------------------------
  
  if (start_index_qs == 1L) {
    
    dt_scn_SIM_qs <- copy(
      raw_data
    )
    
  } else {
    
    previous_scenario <- scenarios_qs[
      start_index_qs - 1L
    ]
    
    dt_scn_SIM_qs <- copy(
      PIT_BU_list_qs[[previous_scenario]]
    )
  }
  
  
  #----------------------------------------------------------------------------
  # 3.3 Updated policy from SimulationYear onward
  #----------------------------------------------------------------------------
  
  for (
    i in seq.int(
      from = start_index_qs,
      to = length(scenarios_qs)
    )
  ) {
    
    scenario_name <- scenarios_qs[i]
    scenario_year <- scenario_years_qs[i]
    
    
    growth_values <- get_growth_factor_row(
      scenario_name = scenario_name,
      growth_dt = growth_data,
      variables = vars_to_grow_qs
    )
    
    
    apply_growth_factors(
      dt = dt_scn_SIM_qs,
      growth_values = growth_values
    )
    
    
    qs_tax_calc_fun(
      dt_scn = dt_scn_SIM_qs,
      params_dt = parameters_updated,
      year = scenario_year
    )
    
    
    dt_scn_SIM_qs[
      ,
      `:=`(
        scenario = scenario_name,
        simulation_data_year = scenario_year
      )
    ]
    
    
    PIT_SIM_list_qs[[scenario_name]] <- copy(
      dt_scn_SIM_qs
    )
  }
  
  
  #============================================================================
  # 4. AGGREGATE BU AND SIMULATION RESULTS
  #============================================================================
  
  summary_BU_qs <- summarize_PIT_fun_dt(
    PIT_list = PIT_BU_list_qs,
    suffix = "_bu"
  )
  
  
  summary_SIM_qs <- summarize_PIT_fun_dt(
    PIT_list = PIT_SIM_list_qs,
    suffix = "_sim"
  )
  
  
  merged_PIT_BU_SIM_qs <- merge(
    summary_BU_qs,
    summary_SIM_qs,
    by = "scenarios",
    all = TRUE
  )
  
  
  merged_PIT_BU_SIM_qs <- merge(
    scenario_lookup_qs,
    merged_PIT_BU_SIM_qs,
    by = "scenarios",
    all.x = TRUE,
    sort = FALSE
  )
  
  
  setorder(
    merged_PIT_BU_SIM_qs,
    year
  )
  
  
  setcolorder(
    merged_PIT_BU_SIM_qs,
    c(
      "year",
      "scenarios",
      setdiff(
        names(merged_PIT_BU_SIM_qs),
        c(
          "year",
          "scenarios"
        )
      )
    )
  )
  
  
  #============================================================================
  # 5. CONVERT AGGREGATES TO LCU MILLIONS
  #============================================================================
  
  aggregate_columns_qs <- setdiff(
    names(merged_PIT_BU_SIM_qs),
    c(
      "year",
      "scenarios"
    )
  )
  
  
  numeric_aggregate_columns_qs <- aggregate_columns_qs[
    vapply(
      merged_PIT_BU_SIM_qs[, ..aggregate_columns_qs],
      is.numeric,
      logical(1)
    )
  ]
  
  
  merged_PIT_BU_SIM_qs[
    ,
    (numeric_aggregate_columns_qs) :=
      lapply(
        .SD,
        function(value) {
          value / 1e6
        }
      ),
    .SDcols = numeric_aggregate_columns_qs
  ]
  
  
  #============================================================================
  # 6. MAIN FISCAL-IMPACT SUMMARY
  #============================================================================
  
  required_summary_columns_qs <- c(
    "total_pit_qs_calc_bu",
    "total_pit_qs_calc_sim"
  )
  
  
  missing_summary_columns_qs <- setdiff(
    required_summary_columns_qs,
    names(merged_PIT_BU_SIM_qs)
  )
  
  
  if (length(missing_summary_columns_qs) > 0L) {
    stop(
      "Missing QS summary columns: ",
      paste(
        missing_summary_columns_qs,
        collapse = ", "
      )
    )
  }
  
  
  pit_summary_qs <- merged_PIT_BU_SIM_qs[
    ,
    .(
      year,
      
      `Current law QS PIT (LCU Mil)` =
        round(
          total_pit_qs_calc_bu,
          1
        ),
      
      `Simulation QS PIT (LCU Mil)` =
        round(
          total_pit_qs_calc_sim,
          1
        ),
      
      `Fiscal impact QS PIT (LCU Mil)` =
        round(
          total_pit_qs_calc_sim -
            total_pit_qs_calc_bu,
          1
        )
    )
  ]
  
  
  #============================================================================
  # 7. COMPONENT SUMMARY
  #============================================================================
  
  qs_component_summary <- merged_PIT_BU_SIM_qs[
    ,
    .(
      year,
      
      `Commercial PIT - current law` =
        round(
          pit_commercial_qs_calc_bu,
          1
        ),
      
      `Commercial PIT - simulation` =
        round(
          pit_commercial_qs_calc_sim,
          1
        ),
      
      `Commercial PIT - impact` =
        round(
          pit_commercial_qs_calc_sim -
            pit_commercial_qs_calc_bu,
          1
        ),
      
      `Services PIT - current law` =
        round(
          pit_services_qs_calc_bu,
          1
        ),
      
      `Services PIT - simulation` =
        round(
          pit_services_qs_calc_sim,
          1
        ),
      
      `Services PIT - impact` =
        round(
          pit_services_qs_calc_sim -
            pit_services_qs_calc_bu,
          1
        ),
      
      `Commercial/services PIT - current law` =
        round(
          pit_commercial_services_qs_calc_bu,
          1
        ),
      
      `Commercial/services PIT - simulation` =
        round(
          pit_commercial_services_qs_calc_sim,
          1
        ),
      
      `Commercial/services PIT - impact` =
        round(
          pit_commercial_services_qs_calc_sim -
            pit_commercial_services_qs_calc_bu,
          1
        ),
      
      `Gross rent PIT - current law` =
        round(
          rent_qs_calc_bu,
          1
        ),
      
      `Gross rent PIT - simulation` =
        round(
          rent_qs_calc_sim,
          1
        ),
      
      `Gross rent PIT - impact` =
        round(
          rent_qs_calc_sim -
            rent_qs_calc_bu,
          1
        ),
      
      `Net rent PIT - current law` =
        round(
          rent_qs_net_calc_bu,
          1
        ),
      
      `Net rent PIT - simulation` =
        round(
          rent_qs_net_calc_sim,
          1
        ),
      
      `Net rent PIT - impact` =
        round(
          rent_qs_net_calc_sim -
            rent_qs_net_calc_bu,
          1
        ),
      
      `Total QS PIT - current law` =
        round(
          total_pit_qs_calc_bu,
          1
        ),
      
      `Total QS PIT - simulation` =
        round(
          total_pit_qs_calc_sim,
          1
        ),
      
      `Total QS fiscal impact` =
        round(
          total_pit_qs_calc_sim -
            total_pit_qs_calc_bu,
          1
        )
    )
  ]
  
  
  #============================================================================
  # 8. BASELINE VALIDATION
  #============================================================================
  
  baseline_scenario_qs <- scenarios_qs[1L]
  
  baseline_dt_qs <- PIT_BU_list_qs[[baseline_scenario_qs]]
  
  
  baseline_validation_qs <- baseline_dt_qs[
    ,
    .(
      reported_gross_commercial_income =
        sum(
          grossIncomeCommercial,
          na.rm = TRUE
        ) / 1e6,
      
      reported_gross_services_income =
        sum(
          grossIncomeServices,
          na.rm = TRUE
        ) / 1e6,
      
      reported_commercial_tax =
        sum(
          taxCommercial,
          na.rm = TRUE
        ) / 1e6,
      
      calculated_commercial_tax =
        sum(
          pit_commercial_qs_calc,
          na.rm = TRUE
        ) / 1e6,
      
      reported_services_tax =
        sum(
          taxServices,
          na.rm = TRUE
        ) / 1e6,
      
      calculated_services_tax =
        sum(
          pit_services_qs_calc,
          na.rm = TRUE
        ) / 1e6,
      
      reported_commercial_services_total =
        sum(
          taxPaymentTotal,
          na.rm = TRUE
        ) / 1e6,
      
      calculated_commercial_services_total =
        sum(
          pit_commercial_services_qs_calc,
          na.rm = TRUE
        ) / 1e6,
      
      reported_gross_rent_income =
        sum(
          gross_rent_income,
          na.rm = TRUE
        ) / 1e6,
      
      reported_rent_tax_9pct =
        sum(
          rent_tax_9pct,
          na.rm = TRUE
        ) / 1e6,
      
      reported_rent_tax_withheld_others =
        sum(
          rent_tax_withheld_others,
          na.rm = TRUE
        ) / 1e6,
      
      reported_rent_tax_net =
        sum(
          rent_tax_net,
          na.rm = TRUE
        ) / 1e6,
      
      calculated_gross_rent_tax =
        sum(
          rent_qs_calc,
          na.rm = TRUE
        ) / 1e6,
      
      calculated_net_rent_tax =
        sum(
          rent_qs_net_calc,
          na.rm = TRUE
        ) / 1e6,
      
      reported_total_amount_due =
        sum(
          total_amount_due,
          na.rm = TRUE
        ) / 1e6,
      
      calculated_total_qs_pit =
        sum(
          total_pit_qs_calc,
          na.rm = TRUE
        ) / 1e6
    )
  ]
  
  
  baseline_validation_qs[
    ,
    `:=`(
      commercial_tax_difference =
        calculated_commercial_tax -
        reported_commercial_tax,
      
      services_tax_difference =
        calculated_services_tax -
        reported_services_tax,
      
      commercial_services_difference =
        calculated_commercial_services_total -
        reported_commercial_services_total,
      
      gross_rent_tax_difference =
        calculated_gross_rent_tax -
        reported_rent_tax_9pct,
      
      net_rent_tax_difference =
        calculated_net_rent_tax -
        reported_rent_tax_net,
      
      total_qs_difference =
        calculated_total_qs_pit -
        reported_total_amount_due
    )
  ]
  
  
  #============================================================================
  # 9. GROWTH-FACTOR CHECK
  #============================================================================
  
  growth_factor_check_qs <- rbindlist(
    lapply(
      scenarios_qs,
      function(scenario_name) {
        
        scenario_growth_values <- get_growth_factor_row(
          scenario_name = scenario_name,
          growth_dt = growth_data,
          variables = vars_to_grow_qs
        )
        
        scenario_year <- scenario_lookup_qs[
          scenarios == scenario_name,
          year
        ]
        
        data.table(
          scenarios = scenario_name,
          year = scenario_year,
          
          minimum_growth_factor =
            min(
              scenario_growth_values,
              na.rm = TRUE
            ),
          
          maximum_growth_factor =
            max(
              scenario_growth_values,
              na.rm = TRUE
            ),
          
          number_of_growth_variables =
            length(
              scenario_growth_values
            )
        )
      }
    )
  )
  
  
  #============================================================================
  # 10. RUNTIME
  #============================================================================
  
  module_end_time_qs <- proc.time()
  
  runtime_minutes_qs <- (
    module_end_time_qs[3L] -
      module_start_time_qs[3L]
  ) / 60
  
  
  #============================================================================
  # 11. RETURN RESULTS
  #============================================================================
  
  return(
    list(
      scenario_years_qs = scenario_years_qs,
      scenarios_qs = scenarios_qs,
      forecast_horizon_qs = forecast_horizon_qs,
      base_year_qs = base_year_qs,
      end_year_qs = end_year_qs,
      scenario_lookup_qs = scenario_lookup_qs,
      
      PIT_BU_list_qs = PIT_BU_list_qs,
      PIT_SIM_list_qs = PIT_SIM_list_qs,
      
      summary_BU_qs = summary_BU_qs,
      summary_SIM_qs = summary_SIM_qs,
      
      merged_PIT_BU_SIM_qs = merged_PIT_BU_SIM_qs,
      
      pit_summary_qs = pit_summary_qs,
      qs_component_summary = qs_component_summary,
      
      baseline_validation_qs = baseline_validation_qs,
      growth_factor_check_qs = growth_factor_check_qs,
      
      runtime_minutes_qs = runtime_minutes_qs
    )
  )
}


#==============================================================================
# 2. RUN QS MODULE
#==============================================================================

qs_pit_results <- run_qs_pit_module(
  raw_data = pit_raw_qs,
  growth_data = pit_growth_factors_qs,
  parameters_raw = pit_simulation_parameters_raw,
  parameters_updated = pit_simulation_parameters_updated,
  simulation_year = SimulationYear
)


#==============================================================================
# 3. KEEP ONLY THE SELECTED SIMULATION YEAR INSIDE qs_pit_results
#==============================================================================

selected_scenario_qs <-
  qs_pit_results$scenario_lookup_qs[
    year == as.integer(SimulationYear),
    scenarios
  ]


if (length(selected_scenario_qs) != 1L) {
  stop(
    paste0(
      "Exactly one QS scenario must correspond to SimulationYear ",
      SimulationYear,
      ". Matches found: ",
      length(selected_scenario_qs)
    )
  )
}


# Store the selected scenario name inside the main results list
qs_pit_results$selected_scenario_qs <-
  selected_scenario_qs


# Keep only the selected BU scenario as a one-element named list
qs_pit_results$PIT_BU_list_qs_selected <-
  qs_pit_results$PIT_BU_list_qs[
    selected_scenario_qs
  ]


# Keep only the selected SIM scenario as a one-element named list
qs_pit_results$PIT_SIM_list_qs_selected <-
  qs_pit_results$PIT_SIM_list_qs[
    selected_scenario_qs
  ]


#==============================================================================
# 4. REMOVE THE COMPLETE t0-t5 LISTS FROM qs_pit_results
#==============================================================================

qs_pit_results$PIT_BU_list_qs <- NULL
qs_pit_results$PIT_SIM_list_qs <- NULL


# Remove the temporary standalone helper object
rm(
  selected_scenario_qs
)


gc(
  verbose = FALSE
)


message(
  paste0(
    "Selected QS scenario retained: ",
    qs_pit_results$selected_scenario_qs,
    " / ",
    SimulationYear,
    ". Complete QS BU and SIM lists were removed."
  )
)


#==============================================================================
# 5. VERIFY THE SELECTED QS LISTS
#==============================================================================

print(
  names(
    qs_pit_results$PIT_BU_list_qs_selected
  )
)


print(
  names(
    qs_pit_results$PIT_SIM_list_qs_selected
  )
)


# Confirm that the complete lists were removed
print(
  "PIT_BU_list_qs" %in% names(qs_pit_results)
)


print(
  "PIT_SIM_list_qs" %in% names(qs_pit_results)
)


#==============================================================================
# 6. PRINT RESULTS FROM THE MAIN QS RESULTS LIST
#==============================================================================

print(
  qs_pit_results$scenario_lookup_qs
)


print(
  qs_pit_results$growth_factor_check_qs
)


print(
  qs_pit_results$merged_PIT_BU_SIM_qs
)


print(
  qs_pit_results$pit_summary_qs
)


print(
  qs_pit_results$qs_component_summary
)


print(
  qs_pit_results$baseline_validation_qs
)


cat(
  "\nQS module runtime in minutes:",
  qs_pit_results$runtime_minutes_qs,
  "\n\n"
)


# qs_pit_results

