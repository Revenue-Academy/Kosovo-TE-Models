###############################################################################
# PIT COMMERCIAL AND SERVICES MODULE - IS
#
# RUN THIS SCRIPT AFTER THE MAIN PIT SCRIPT.
#
# Reuses these shared functions:
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

required_shared_functions_is <- c(
  "get_param_fun",
  "validate_parameter_table",
  "get_growth_factor_row",
  "apply_growth_factors",
  "summarize_PIT_fun_dt"
)

available_shared_functions_is <- vapply(
  required_shared_functions_is,
  exists,
  logical(1),
  mode     = "function",
  inherits = TRUE
)

missing_shared_functions_is <- required_shared_functions_is[
  !available_shared_functions_is
]

if (length(missing_shared_functions_is) > 0L) {
  stop(
    paste0(
      "Run the main PIT script first. ",
      "The following shared functions are missing: ",
      paste(missing_shared_functions_is, collapse = ", ")
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
# 1. IS MODULE FUNCTION
#==============================================================================

run_is_pit_module <- function(
    raw_data,
    growth_data,
    parameters_raw,
    parameters_updated,
    simulation_year
) {
  
  module_start_time_is <- proc.time()
  
  
  #----------------------------------------------------------------------------
  # 1.1 Create local copies
  #----------------------------------------------------------------------------
  
  raw_data           <- as.data.table(copy(raw_data))
  growth_data        <- as.data.table(copy(growth_data))
  parameters_raw     <- as.data.table(copy(parameters_raw))
  parameters_updated <- as.data.table(copy(parameters_updated))
  
  
  #----------------------------------------------------------------------------
  # 1.2 Validate growth-factor table
  #----------------------------------------------------------------------------
  
  required_growth_columns_is <- c(
    "Year",
    "scenarios",
    "grossIncomeCommercial",
    "grossIncomeServices"
  )
  
  missing_growth_columns_is <- setdiff(
    required_growth_columns_is,
    names(growth_data)
  )
  
  if (length(missing_growth_columns_is) > 0L) {
    stop(
      "pit_growth_factors_is is missing columns: ",
      paste(missing_growth_columns_is, collapse = ", ")
    )
  }
  
  setorder(growth_data, Year)
  
  if (anyNA(growth_data$Year)) {
    stop("pit_growth_factors_is contains missing Year values.")
  }
  
  if (anyNA(growth_data$scenarios)) {
    stop("pit_growth_factors_is contains missing scenario labels.")
  }
  
  if (anyDuplicated(growth_data$Year)) {
    stop("Each year must appear exactly once in pit_growth_factors_is.")
  }
  
  if (anyDuplicated(growth_data$scenarios)) {
    stop("Each scenario must appear exactly once in pit_growth_factors_is.")
  }
  
  
  #----------------------------------------------------------------------------
  # 1.3 Dynamic years and scenarios
  #----------------------------------------------------------------------------
  
  scenario_years_is   <- growth_data$Year
  scenarios_is        <- growth_data$scenarios
  forecast_horizon_is <- scenario_years_is
  base_year_is        <- min(scenario_years_is)
  end_year_is         <- max(scenario_years_is)
  
  
  #----------------------------------------------------------------------------
  # Determine where the simulation begins
  #
  # If SimulationYear is earlier than the first forecast year,
  # updated parameters apply from the first available scenario.
  #----------------------------------------------------------------------------
  
  if (simulation_year <= base_year_is) {
    
    start_index_is <- 1L
    
    if (simulation_year < base_year_is) {
      message(
        sprintf(
          paste0(
            "SimulationYear %s is before the first IS forecast year %s. ",
            "Updated parameters will apply from %s."
          ),
          simulation_year,
          base_year_is,
          base_year_is
        )
      )
    }
    
  } else if (simulation_year > end_year_is) {
    
    stop(
      sprintf(
        paste0(
          "SimulationYear %s is after the IS forecast horizon. ",
          "The final available year is %s."
        ),
        simulation_year,
        end_year_is
      )
    )
    
  } else {
    
    start_index_is <- match(simulation_year, scenario_years_is)
  }
  
  scenario_lookup_is <- data.table(
    scenarios = scenarios_is,
    year      = scenario_years_is
  )
  
  
  #----------------------------------------------------------------------------
  # 1.4 Validate raw IS data
  #----------------------------------------------------------------------------
  
  required_raw_columns_is <- c(
    "id_n",
    "Year",
    "grossIncomeCommercial",
    "grossIncomeServices",
    "taxCommercial",
    "taxServices",
    "taxPaymentTotal"
  )
  
  missing_raw_columns_is <- setdiff(
    required_raw_columns_is,
    names(raw_data)
  )
  
  if (length(missing_raw_columns_is) > 0L) {
    stop(
      "pit_raw_is is missing columns: ",
      paste(missing_raw_columns_is, collapse = ", ")
    )
  }
  
  raw_years_is <- unique(raw_data[!is.na(Year), Year])
  
  if (length(raw_years_is) != 1L) {
    stop(
      paste0(
        "pit_raw_is must contain exactly one baseline year. Years found: ",
        paste(raw_years_is, collapse = ", ")
      )
    )
  }
  
  if (raw_years_is[1L] != base_year_is) {
    stop(
      sprintf(
        paste0(
          "The baseline year in pit_raw_is is %s, ",
          "but the first growth-factor year is %s."
        ),
        raw_years_is[1L],
        base_year_is
      )
    )
  }
  
  
  #----------------------------------------------------------------------------
  # 1.5 Variables receiving growth factors
  #
  # Only income bases are grown.
  # taxCommercial, taxServices and taxPaymentTotal remain as reported values
  # and are used only for baseline validation.
  #----------------------------------------------------------------------------
  
  vars_to_grow_is <- c(
    "grossIncomeCommercial",
    "grossIncomeServices"
  )
  
  missing_raw_growth_vars_is <- setdiff(vars_to_grow_is, names(raw_data))
  
  if (length(missing_raw_growth_vars_is) > 0L) {
    stop(
      "Missing IS growth variables in pit_raw_is: ",
      paste(missing_raw_growth_vars_is, collapse = ", ")
    )
  }
  
  missing_growth_factor_vars_is <- setdiff(vars_to_grow_is, names(growth_data))
  
  if (length(missing_growth_factor_vars_is) > 0L) {
    warning(
      paste0(
        "The following variables do not have growth-factor columns. ",
        "A factor of 1 will be used: ",
        paste(missing_growth_factor_vars_is, collapse = ", ")
      )
    )
  }
  
  
  #----------------------------------------------------------------------------
  # 1.6 Validate policy parameters
  #
  # Three parameters required:
  #   rate_commerical_is     - flat rate on commercial income
  #   rate_services_is       - flat rate on services income
  #   min_payment_small_is   - minimum annual tax payment
  #----------------------------------------------------------------------------
  
  required_parameters_is <- c(
    "rate_commerical_is",
    "rate_services_is",
    "min_payment_small_is"
  )
  
  validate_parameter_table(
    params_dt           = parameters_raw,
    table_name          = "pit_simulation_parameters_raw",
    required_years      = scenario_years_is,
    required_parameters = required_parameters_is
  )
  
  updated_parameter_years_is <- scenario_years_is[
    seq.int(from = start_index_is, to = length(scenario_years_is))
  ]
  
  validate_parameter_table(
    params_dt           = parameters_updated,
    table_name          = "pit_simulation_parameters_updated",
    required_years      = updated_parameter_years_is,
    required_parameters = required_parameters_is
  )
  
  
  #----------------------------------------------------------------------------
  # 1.7 IS tax calculation function
  #
  # Exists only inside run_is_pit_module().
  # All calculated columns end in "_calc" so they are picked up
  # automatically by summarize_PIT_fun_dt().
  #
  # Formula:
  #   A. pit_commercial_calc    = grossIncomeCommercial * rate_commerical_is
  #   B. pit_services_calc      = grossIncomeServices   * rate_services_is
  #   C. pit_before_minimum     = A + B
  #   D. pit_final              = max(min_payment_small_is, C) if C > 0
  #                               else 0
  #   E. minimum_payment_adjustment = D - C
  #----------------------------------------------------------------------------
  
  is_tax_calc_fun <- function(dt_scn, params_dt, year) {
    
    # Read parameters for this year
    rate_commerical_is   <- get_param_fun(params_dt, "rate_commerical_is",   year)
    rate_services_is     <- get_param_fun(params_dt, "rate_services_is",     year)
    min_payment_small_is <- get_param_fun(params_dt, "min_payment_small_is", year)
    
    # Validate parameters
    if (
      rate_commerical_is < 0 ||
      rate_commerical_is > 1 ||
      !is.finite(rate_commerical_is)
    ) {
      stop(
        sprintf(
          "rate_commerical_is must be between 0 and 1 for year %s.",
          year
        )
      )
    }
    
    if (
      rate_services_is < 0 ||
      rate_services_is > 1 ||
      !is.finite(rate_services_is)
    ) {
      stop(
        sprintf(
          "rate_services_is must be between 0 and 1 for year %s.",
          year
        )
      )
    }
    
    if (
      min_payment_small_is < 0 ||
      !is.finite(min_payment_small_is)
    ) {
      stop(
        sprintf(
          "min_payment_small_is must be nonnegative for year %s.",
          year
        )
      )
    }
    
    # A. Tax on commercial income
    dt_scn[
      ,
      pit_commercial_calc :=
        fcoalesce(grossIncomeCommercial, 0) * rate_commerical_is
    ]
    
    # B. Tax on services income
    dt_scn[
      ,
      pit_services_calc :=
        fcoalesce(grossIncomeServices, 0) * rate_services_is
    ]
    
    # C. Tax before minimum payment
    dt_scn[
      ,
      pit_before_minimum_calc :=
        pit_commercial_calc + pit_services_calc
    ]
    
    # D. Final IS PIT - apply minimum payment where positive tax arises
    dt_scn[
      ,
      pit_commercial_services_calc := fifelse(
        pit_before_minimum_calc > 0,
        pmax(min_payment_small_is, pit_before_minimum_calc),
        0
      )
    ]
    
    # E. Minimum-payment adjustment
    dt_scn[
      ,
      minimum_payment_adjustment_calc :=
        pit_commercial_services_calc - pit_before_minimum_calc
    ]
    
    invisible(dt_scn)
  }
  
  
  #============================================================================
  # 2. BUSINESS-AS-USUAL PATH
  #============================================================================
  
  PIT_BU_list_is <- list()
  dt_scn_BU_is   <- copy(raw_data)
  
  for (i in seq_along(scenarios_is)) {
    
    scenario_name <- scenarios_is[i]
    scenario_year <- scenario_years_is[i]
    
    growth_values <- get_growth_factor_row(
      scenario_name = scenario_name,
      growth_dt     = growth_data,
      variables     = vars_to_grow_is
    )
    
    apply_growth_factors(dt = dt_scn_BU_is, growth_values = growth_values)
    
    is_tax_calc_fun(
      dt_scn    = dt_scn_BU_is,
      params_dt = parameters_raw,
      year      = scenario_year
    )
    
    dt_scn_BU_is[
      ,
      `:=`(
        scenario             = scenario_name,
        simulation_data_year = scenario_year
      )
    ]
    
    PIT_BU_list_is[[scenario_name]] <- copy(dt_scn_BU_is)
  }
  
  
  #============================================================================
  # 3. SIMULATION PATH
  #============================================================================
  
  PIT_SIM_list_is <- list()
  
  # Years before SimulationYear equal BU
  if (start_index_is > 1L) {
    for (i in seq_len(start_index_is - 1L)) {
      scenario_name <- scenarios_is[i]
      PIT_SIM_list_is[[scenario_name]] <- copy(PIT_BU_list_is[[scenario_name]])
    }
  }
  
  # Starting dataset for the simulation path
  if (start_index_is == 1L) {
    dt_scn_SIM_is <- copy(raw_data)
  } else {
    previous_scenario <- scenarios_is[start_index_is - 1L]
    dt_scn_SIM_is    <- copy(PIT_BU_list_is[[previous_scenario]])
  }
  
  # Updated policy from SimulationYear onward
  for (i in seq.int(from = start_index_is, to = length(scenarios_is))) {
    
    scenario_name <- scenarios_is[i]
    scenario_year <- scenario_years_is[i]
    
    growth_values <- get_growth_factor_row(
      scenario_name = scenario_name,
      growth_dt     = growth_data,
      variables     = vars_to_grow_is
    )
    
    apply_growth_factors(dt = dt_scn_SIM_is, growth_values = growth_values)
    
    is_tax_calc_fun(
      dt_scn    = dt_scn_SIM_is,
      params_dt = parameters_updated,
      year      = scenario_year
    )
    
    dt_scn_SIM_is[
      ,
      `:=`(
        scenario             = scenario_name,
        simulation_data_year = scenario_year
      )
    ]
    
    PIT_SIM_list_is[[scenario_name]] <- copy(dt_scn_SIM_is)
  }
  
  
  #============================================================================
  # 4. AGGREGATE BU AND SIMULATION RESULTS
  #============================================================================
  
  summary_BU_is  <- summarize_PIT_fun_dt(PIT_list = PIT_BU_list_is,  suffix = "_bu")
  summary_SIM_is <- summarize_PIT_fun_dt(PIT_list = PIT_SIM_list_is, suffix = "_sim")
  
  merged_PIT_BU_SIM_is <- merge(
    summary_BU_is,
    summary_SIM_is,
    by  = "scenarios",
    all = TRUE
  )
  
  merged_PIT_BU_SIM_is <- merge(
    scenario_lookup_is,
    merged_PIT_BU_SIM_is,
    by    = "scenarios",
    all.x = TRUE,
    sort  = FALSE
  )
  
  setorder(merged_PIT_BU_SIM_is, year)
  
  setcolorder(
    merged_PIT_BU_SIM_is,
    c(
      "year",
      "scenarios",
      setdiff(names(merged_PIT_BU_SIM_is), c("year", "scenarios"))
    )
  )
  
  
  #============================================================================
  # 5. CONVERT AGGREGATES TO LCU MILLIONS
  #============================================================================
  
  aggregate_columns_is <- setdiff(
    names(merged_PIT_BU_SIM_is),
    c("year", "scenarios")
  )
  
  numeric_aggregate_columns_is <- aggregate_columns_is[
    vapply(
      merged_PIT_BU_SIM_is[, ..aggregate_columns_is],
      is.numeric,
      logical(1)
    )
  ]
  
  merged_PIT_BU_SIM_is[
    ,
    (numeric_aggregate_columns_is) :=
      lapply(.SD, function(v) v / 1e6),
    .SDcols = numeric_aggregate_columns_is
  ]
  
  
  #============================================================================
  # 6. MAIN FISCAL-IMPACT SUMMARY
  #============================================================================
  
  required_summary_columns_is <- c(
    "pit_commercial_services_calc_bu",
    "pit_commercial_services_calc_sim"
  )
  
  missing_summary_columns_is <- setdiff(
    required_summary_columns_is,
    names(merged_PIT_BU_SIM_is)
  )
  
  if (length(missing_summary_columns_is) > 0L) {
    stop(
      "Missing IS summary columns: ",
      paste(missing_summary_columns_is, collapse = ", ")
    )
  }
  
  pit_summary_is <- merged_PIT_BU_SIM_is[
    ,
    .(
      year,
      `Current law commercial/services PIT (LCU Mil)`  =
        round(pit_commercial_services_calc_bu,  1),
      `Simulation commercial/services PIT (LCU Mil)`   =
        round(pit_commercial_services_calc_sim, 1),
      `Fiscal impact commercial/services PIT (LCU Mil)` =
        round(pit_commercial_services_calc_sim - pit_commercial_services_calc_bu, 1)
    )
  ]
  
  
  #============================================================================
  # 7. COMPONENT SUMMARY
  #============================================================================
  
  is_component_summary <- merged_PIT_BU_SIM_is[
    ,
    .(
      year,
      
      `Commercial PIT - current law`           = round(pit_commercial_calc_bu,  1),
      `Commercial PIT - simulation`            = round(pit_commercial_calc_sim, 1),
      `Commercial PIT - impact`                = round(pit_commercial_calc_sim - pit_commercial_calc_bu, 1),
      
      `Services PIT - current law`             = round(pit_services_calc_bu,  1),
      `Services PIT - simulation`              = round(pit_services_calc_sim, 1),
      `Services PIT - impact`                  = round(pit_services_calc_sim - pit_services_calc_bu, 1),
      
      `Tax before minimum - current law`       = round(pit_before_minimum_calc_bu,  1),
      `Tax before minimum - simulation`        = round(pit_before_minimum_calc_sim, 1),
      `Tax before minimum - impact`            = round(pit_before_minimum_calc_sim - pit_before_minimum_calc_bu, 1),
      
      `Min-payment adjustment - current law`   = round(minimum_payment_adjustment_calc_bu,  1),
      `Min-payment adjustment - simulation`    = round(minimum_payment_adjustment_calc_sim, 1),
      `Min-payment adjustment - impact`        = round(minimum_payment_adjustment_calc_sim - minimum_payment_adjustment_calc_bu, 1),
      
      `Total PIT - current law`                = round(pit_commercial_services_calc_bu,  1),
      `Total PIT - simulation`                 = round(pit_commercial_services_calc_sim, 1),
      `Total fiscal impact`                    = round(pit_commercial_services_calc_sim - pit_commercial_services_calc_bu, 1)
    )
  ]
  
  
  #============================================================================
  # 8. BASELINE VALIDATION
  #============================================================================
  
  baseline_scenario_is <- scenarios_is[1L]
  baseline_dt_is       <- PIT_BU_list_is[[baseline_scenario_is]]
  
  baseline_validation_is <- baseline_dt_is[
    ,
    .(
      reported_gross_commercial_income_MIL  = sum(grossIncomeCommercial,           na.rm = TRUE) / 1e6,
      reported_gross_services_income_MIL    = sum(grossIncomeServices,             na.rm = TRUE) / 1e6,
      reported_commercial_tax_MIL           = sum(taxCommercial,                   na.rm = TRUE) / 1e6,
      calculated_commercial_tax_MIL         = sum(pit_commercial_calc,             na.rm = TRUE) / 1e6,
      reported_services_tax_MIL             = sum(taxServices,                     na.rm = TRUE) / 1e6,
      calculated_services_tax_MIL           = sum(pit_services_calc,               na.rm = TRUE) / 1e6,
      reported_total_tax_payment_MIL        = sum(taxPaymentTotal,                 na.rm = TRUE) / 1e6,
      calculated_tax_before_minimum_MIL     = sum(pit_before_minimum_calc,         na.rm = TRUE) / 1e6,
      calculated_min_payment_adjustment_MIL = sum(minimum_payment_adjustment_calc, na.rm = TRUE) / 1e6,
      calculated_total_tax_payment_MIL      = sum(pit_commercial_services_calc,    na.rm = TRUE) / 1e6
    )
  ]
  
  baseline_validation_is[
    ,
    `:=`(
      commercial_tax_difference_MIL =
        calculated_commercial_tax_MIL - reported_commercial_tax_MIL,
      services_tax_difference_MIL   =
        calculated_services_tax_MIL   - reported_services_tax_MIL,
      total_tax_difference_MIL      =
        calculated_total_tax_payment_MIL - reported_total_tax_payment_MIL
    )
  ]
  
  
  #============================================================================
  # 9. GROWTH-FACTOR CHECK
  #============================================================================
  
  growth_factor_check_is <- rbindlist(
    lapply(
      scenarios_is,
      function(scenario_name) {
        
        scenario_growth_values <- get_growth_factor_row(
          scenario_name = scenario_name,
          growth_dt     = growth_data,
          variables     = vars_to_grow_is
        )
        
        scenario_year <- scenario_lookup_is[
          scenarios == scenario_name,
          year
        ]
        
        data.table(
          scenarios                  = scenario_name,
          year                       = scenario_year,
          minimum_growth_factor      = min(scenario_growth_values, na.rm = TRUE),
          maximum_growth_factor      = max(scenario_growth_values, na.rm = TRUE),
          number_of_growth_variables = length(scenario_growth_values)
        )
      }
    )
  )
  
  
  #============================================================================
  # 10. RUNTIME
  #============================================================================
  
  module_end_time_is <- proc.time()
  
  runtime_minutes_is <- (
    module_end_time_is[3L] - module_start_time_is[3L]
  ) / 60
  
  
  #============================================================================
  # 11. RETURN RESULTS
  #============================================================================
  
  return(
    list(
      scenario_years_is      = scenario_years_is,
      scenarios_is           = scenarios_is,
      forecast_horizon_is    = forecast_horizon_is,
      base_year_is           = base_year_is,
      end_year_is            = end_year_is,
      scenario_lookup_is     = scenario_lookup_is,
      
      PIT_BU_list_is         = PIT_BU_list_is,
      PIT_SIM_list_is        = PIT_SIM_list_is,
      
      summary_BU_is          = summary_BU_is,
      summary_SIM_is         = summary_SIM_is,
      
      merged_PIT_BU_SIM_is   = merged_PIT_BU_SIM_is,
      
      pit_summary_is         = pit_summary_is,
      is_component_summary   = is_component_summary,
      
      baseline_validation_is = baseline_validation_is,
      growth_factor_check_is = growth_factor_check_is,
      
      runtime_minutes_is     = runtime_minutes_is
    )
  )
}


#==============================================================================
# 2. RUN IS MODULE
#==============================================================================

is_pit_results <- run_is_pit_module(
  raw_data           = pit_raw_is,
  growth_data        = pit_growth_factors_is,
  parameters_raw     = pit_simulation_parameters_raw,
  parameters_updated = pit_simulation_parameters_updated,
  simulation_year    = SimulationYear
)


#==============================================================================
# 3. SELECT SIMULATION YEAR AND TRIM FULL LISTS
#==============================================================================

selected_scenario_is <- is_pit_results$scenario_lookup_is[
  year == as.integer(SimulationYear),
  scenarios
]

if (length(selected_scenario_is) != 1L) {
  stop(
    paste0(
      "Exactly one IS scenario must correspond to SimulationYear ",
      SimulationYear,
      ". Matches found: ",
      length(selected_scenario_is)
    )
  )
}

# Keep only the selected simulation year - discard full t0-t5 lists
is_pit_results$PIT_BU_list_is_selected  <- is_pit_results$PIT_BU_list_is[selected_scenario_is]
is_pit_results$PIT_SIM_list_is_selected <- is_pit_results$PIT_SIM_list_is[selected_scenario_is]
is_pit_results$selected_scenario_is     <- selected_scenario_is

is_pit_results$PIT_BU_list_is  <- NULL
is_pit_results$PIT_SIM_list_is <- NULL

rm(selected_scenario_is)
gc(verbose = FALSE)


#==============================================================================
# 4. VERIFY STRUCTURE
#==============================================================================

cat("\n--- Selected BU scenario ---\n")
print(names(is_pit_results$PIT_BU_list_is_selected))

cat("\n--- Selected SIM scenario ---\n")
print(names(is_pit_results$PIT_SIM_list_is_selected))

cat(
  "\nFull BU list removed  :",
  !"PIT_BU_list_is"  %in% names(is_pit_results), "\n"
)
cat(
  "Full SIM list removed :",
  !"PIT_SIM_list_is" %in% names(is_pit_results), "\n"
)


#==============================================================================
# 5. PRINT RESULTS
#==============================================================================

cat("\n--- Scenario Lookup ---\n")
print(is_pit_results$scenario_lookup_is)

cat("\n--- Growth Factor Check ---\n")
print(is_pit_results$growth_factor_check_is)

cat("\n--- Merged BU / SIM (LCU Millions) ---\n")
print(is_pit_results$merged_PIT_BU_SIM_is)

cat("\n--- IS PIT Summary (LCU Millions) ---\n")
print(is_pit_results$pit_summary_is)

cat("\n--- IS Component Summary (LCU Millions) ---\n")
print(is_pit_results$is_component_summary)

cat("\n--- Baseline Validation (LCU Millions) ---\n")
print(is_pit_results$baseline_validation_is)

cat(
  "\nIS module runtime in minutes:",
  is_pit_results$runtime_minutes_is,
  "\n\n"
)
