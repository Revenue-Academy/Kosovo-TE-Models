###################################################################################################################################################
#                                                                                                                                                 #
#                                                       TAX CALCULATOR - CUSTOMS & Excise                                                         #
#                                                        Data set (customs_data)                                                                  #
#                                                                                                                                                 #
###################################################################################################################################################

'Defining Functions'

# I. General functions for data manipulation --------------------------------------------------------

# 1.1 Filtering columns ---------------------------------------------------
filter_columns <- function(data) {
  col_names <- colnames(data)
  include_pattern_adjusted <- "_adjusted$"
  include_pattern_calc <- "^calc_"
  cols_to_keep <- c("sample_row_id", "weight")
  adjusted_cols <- col_names[grepl(include_pattern_adjusted, col_names)]
  calc_cols <- col_names[grepl(include_pattern_calc, col_names)]
  cols_to_keep <- c(cols_to_keep, adjusted_cols, calc_cols)
  cols_to_keep <- cols_to_keep[cols_to_keep %in% col_names]
  filtered_data <- data[, ..cols_to_keep, with = FALSE]
  return(filtered_data)
}

# 1.2 Sequential weighting Function ---------------------------------------
# NEW LOGIC:
# - t0 uses original sample values * weight_t0 * growth_t0
# - t1 uses already adjusted t0 result * weight_t1 * growth_t1
# - t2 uses already adjusted t1 result * weight_t2 * growth_t2
# - ...
cal_weighting_fun <- function(sample_data,
                              weights,
                              growth_factors,
                              columns_to_process,
                              previous_adjusted = NULL) {
  
  print(paste("Number of rows in sample_data:", nrow(sample_data)))
  print(paste("Length of weights vector:", length(weights)))
  print(paste("Is weights numeric?:", is.numeric(weights)))
  
  if (length(weights) != nrow(sample_data)) {
    stop("Weights must be a numeric vector with the same length as the number of rows in sample_data.")
  }
  
  sample_data[, weight := weights]
  
  for (col in columns_to_process) {
    
    adjusted_col <- paste0(col, "_adjusted")
    
    # If previous adjusted results exist, continue from previous scenario result
    if (!is.null(previous_adjusted) && adjusted_col %in% names(previous_adjusted)) {
      base_vector <- previous_adjusted[[adjusted_col]]
    } else {
      base_vector <- sample_data[[col]]
    }
    
    if (is.null(base_vector)) {
      next
    }
    
    if (col == "Value") {
      sample_data[, (adjusted_col) := base_vector * weights * growth_factors$Value]
    } else if (col == "Quantity") {
      sample_data[, (adjusted_col) := base_vector * weights * growth_factors$Quantity]
    } else if (is.numeric(sample_data[[col]])) {
      sample_data[, (adjusted_col) := base_vector * weights]
    } else {
      warning(paste("Skipping non-numeric column:", col))
    }
  }
  
  return(sample_data)
}

# Function to determine which dataset to use based on simulation_year
get_sim_dataset <- function(current_year, simulation_year) {
  if (current_year < simulation_year) {
    return(customs_simulation_parameters_raw)
  } else {
    return(customs_simulation_parameters_updated)
  }
}

# II. Function for 'What if analysis' --------------------------------------

# 1. Function to calculate customs duties
vec_calc_customs_duties_fun <- function(Value, Effective_Customs_rate) {
  Value <- ifelse(is.na(Value), 0, Value)
  Effective_Customs_rate <- ifelse(is.na(Effective_Customs_rate), 0, Effective_Customs_rate)
  calc_customs_duties <- Value * (Effective_Customs_rate / 100)
  return(calc_customs_duties)
}

# 2. Function to calculate excise duties
vec_calc_excise_fun <- function(Quantity, Effective_Excise_rate) {
  Quantity <- ifelse(is.na(Quantity), 0, Quantity)
  Effective_Excise_rate <- ifelse(is.na(Effective_Excise_rate), 0, Effective_Excise_rate)
  calc_excise <- Quantity * Effective_Excise_rate
  return(calc_excise)
}

# 3. Function to calculate VAT
vec_calc_vat_fun <- function(Value, calc_customs_duties, calc_excise, Effective_VAT_rate) {
  Value <- ifelse(is.na(Value), 0, Value)
  Effective_VAT_rate <- ifelse(is.na(Effective_VAT_rate), 0, Effective_VAT_rate)
  calc_vat <- (Value + calc_customs_duties + calc_excise) * (Effective_VAT_rate / 100)
  return(calc_vat)
}

# Start timing
start.time <- proc.time()

# I. Preparation ----------------------------------------------------------------

# Convert to data.table
customs_data$sample <- as.data.table(customs_data$sample)
customs_data$growth_factors <- as.data.table(customs_data$growth_factors)

customs_simulation_parameters_raw <- as.data.table(customs_simulation_parameters_raw)
customs_simulation_parameters_updated <- as.data.table(customs_simulation_parameters_updated)

# Weights source: support both old and new structure
# Priority:
# 1. customs_data_weights  (wide table with columns t0:t5)
# 2. customs_data$weights  (can be either wide table or old list structure)
if (exists("customs_data_weights")) {
  weights_source <- as.data.table(customs_data_weights)
} else if ("weights" %in% names(customs_data)) {
  if (is.list(customs_data$weights) && !is.data.frame(customs_data$weights)) {
    # old structure: list of scenario-specific tables
    # convert to wide table t0:t5
    weights_list_tmp <- customs_data$weights
    weights_source <- data.table(
      t0 = as.numeric(weights_list_tmp[[1]][[1]]),
      t1 = as.numeric(weights_list_tmp[[2]][[1]]),
      t2 = as.numeric(weights_list_tmp[[3]][[1]]),
      t3 = as.numeric(weights_list_tmp[[4]][[1]]),
      t4 = as.numeric(weights_list_tmp[[5]][[1]]),
      t5 = as.numeric(weights_list_tmp[[6]][[1]])
    )
  } else {
    weights_source <- as.data.table(customs_data$weights)
  }
} else {
  stop("No weights source found. Please provide either customs_data_weights or customs_data$weights.")
}

# Ensure consistent types
customs_data$sample[, HS_code := as.character(HS_code)]
customs_simulation_parameters_raw[, HS_code := as.character(HS_code)]
customs_simulation_parameters_updated[, HS_code := as.character(HS_code)]

customs_data$sample[, Year := as.integer(Year)]
customs_data$growth_factors[, Year := as.integer(Year)]
customs_simulation_parameters_raw[, Year := as.integer(Year)]
customs_simulation_parameters_updated[, Year := as.integer(Year)]

# Replace NA only in numeric columns
replace_na_numeric_dt <- function(dt) {
  num_cols <- names(dt)[sapply(dt, is.numeric)]
  for (j in num_cols) {
    set(dt, which(is.na(dt[[j]])), j, 0)
  }
  invisible(dt)
}

replace_na_numeric_dt(customs_data$sample)
replace_na_numeric_dt(customs_data$growth_factors)
replace_na_numeric_dt(customs_simulation_parameters_raw)
replace_na_numeric_dt(customs_simulation_parameters_updated)
replace_na_numeric_dt(weights_source)

# Keep only needed columns and enforce one row per Year-HS_code
customs_simulation_parameters_raw_unique <- unique(
  customs_simulation_parameters_raw[, .(
    Year,
    HS_code,
    Effective_Customs_rate,
    Effective_Excise_rate,
    Effective_VAT_rate
  )],
  by = c("Year", "HS_code")
)

customs_simulation_parameters_updated_unique <- unique(
  customs_simulation_parameters_updated[, .(
    Year,
    HS_code,
    Effective_Customs_rate,
    Effective_Excise_rate,
    Effective_VAT_rate
  )],
  by = c("Year", "HS_code")
)

# II. Years and scenarios --------------------------------------------------------

base_year <- min(customs_simulation_parameters_raw_unique$Year, na.rm = TRUE)
end_year <- base_year + 5
simulation_year <- as.integer(SimulationYear)

forecast_horizon <- seq(base_year, end_year)
scenarios <- paste0("t", seq_along(forecast_horizon) - 1)

Customs_BU_list <- list()
Customs_SIM_list <- list()

# Add row id to align weights with sample rows
customs_data$sample[, sample_row_id := .I]

# Available sample years
sample_years <- sort(unique(customs_data$sample$Year))
last_sample_year <- max(sample_years, na.rm = TRUE)

# III. Local helper functions ----------------------------------------------------

get_last_available_year <- function(target_year, available_years) {
  available_years <- sort(unique(as.integer(available_years)))
  valid_years <- available_years[available_years <= target_year]
  
  if (length(valid_years) == 0) {
    stop(paste("No available parameter year found for target year", target_year))
  }
  
  return(max(valid_years))
}

prepare_param_table <- function(dt) {
  dt <- as.data.table(copy(dt))
  dt[, HS_code := as.character(HS_code)]
  dt[, Year := as.integer(Year)]
  
  dt <- unique(
    dt[, .(
      Year,
      HS_code,
      Effective_Customs_rate,
      Effective_Excise_rate,
      Effective_VAT_rate
    )],
    by = c("Year", "HS_code")
  )
  
  return(dt)
}

get_year_slice_safe <- function(target_year, param_dt) {
  
  param_dt <- prepare_param_table(param_dt)
  
  available_years <- sort(unique(param_dt$Year))
  use_year <- get_last_available_year(target_year, available_years)
  
  out <- copy(param_dt[Year == use_year])
  out[, Year := target_year]
  out <- unique(out, by = c("Year", "HS_code"))
  
  return(out)
}

get_baseline_dataset_safe <- function(current_year, raw_dt) {
  out <- get_year_slice_safe(
    target_year = current_year,
    param_dt = raw_dt
  )
  return(out)
}

get_sim_dataset_safe <- function(current_year, simulation_year, raw_dt, updated_dt) {
  
  # 1. Always start from full baseline for the current year
  baseline_dt <- get_year_slice_safe(
    target_year = current_year,
    param_dt = raw_dt
  )
  
  # 2. Before simulation year, simulation = baseline
  if (current_year < simulation_year) {
    return(baseline_dt)
  }
  
  # 3. If no updates, also return baseline
  updated_dt <- prepare_param_table(updated_dt)
  
  if (nrow(updated_dt) == 0) {
    return(baseline_dt)
  }
  
  # 4. Get last available updated slice for current year
  updated_slice <- get_year_slice_safe(
    target_year = current_year,
    param_dt = updated_dt
  )
  
  # 5. Overlay updated rows on top of baseline by HS_code
  out <- merge(
    baseline_dt,
    updated_slice,
    by = c("Year", "HS_code"),
    all.x = TRUE,
    suffixes = c("_raw", "_upd"),
    allow.cartesian = FALSE
  )
  
  out[, Effective_Customs_rate := fifelse(
    !is.na(Effective_Customs_rate_upd),
    Effective_Customs_rate_upd,
    Effective_Customs_rate_raw
  )]
  
  out[, Effective_Excise_rate := fifelse(
    !is.na(Effective_Excise_rate_upd),
    Effective_Excise_rate_upd,
    Effective_Excise_rate_raw
  )]
  
  out[, Effective_VAT_rate := fifelse(
    !is.na(Effective_VAT_rate_upd),
    Effective_VAT_rate_upd,
    Effective_VAT_rate_raw
  )]
  
  out <- out[, .(
    Year,
    HS_code,
    Effective_Customs_rate,
    Effective_Excise_rate,
    Effective_VAT_rate
  )]
  
  out <- unique(out, by = c("Year", "HS_code"))
  
  return(out)
}

# NEW:
# Base sample is taken once from base_year and then carried forward sequentially
get_base_sample_for_projection <- function(base_year, sample_dt) {
  available_years <- sort(unique(sample_dt$Year))
  use_year <- get_last_available_year(base_year, available_years)
  
  out <- copy(sample_dt[Year == use_year])
  out[, Year := base_year]
  
  return(out)
}

# NEW:
# weights table is wide: columns t0, t1, t2, t3, t4, t5
get_weights_for_scenario <- function(scenario, weights_dt, sample_current, full_sample_n) {
  
  if (!(scenario %in% names(weights_dt))) {
    stop(paste("Scenario column", scenario, "not found in weights table."))
  }
  
  weights_full <- as.numeric(weights_dt[[scenario]])
  
  if (length(weights_full) == nrow(sample_current)) {
    return(weights_full)
  }
  
  if (length(weights_full) != full_sample_n) {
    stop(paste("Weights length is not compatible for scenario", scenario))
  }
  
  weights_current <- weights_full[sample_current$sample_row_id]
  
  if (length(weights_current) != nrow(sample_current)) {
    stop(paste("Subsetted weights do not match the number of rows for scenario", scenario))
  }
  
  return(weights_current)
}

get_growth_factors_for_year <- function(current_year, scenario, growth_dt) {
  
  growth_dt <- as.data.table(copy(growth_dt))
  growth_dt[, Year := as.integer(Year)]
  
  if ("scenarios" %in% names(growth_dt)) {
    gf_row <- growth_dt[Year == current_year & scenarios == scenario]
    
    if (nrow(gf_row) == 0) {
      available_years <- sort(unique(growth_dt$Year))
      use_year <- get_last_available_year(current_year, available_years)
      gf_row <- growth_dt[Year == use_year & scenarios == scenario]
    }
    
    if (nrow(gf_row) == 0) {
      available_years <- sort(unique(growth_dt$Year))
      use_year <- get_last_available_year(current_year, available_years)
      gf_row <- growth_dt[Year == use_year]
    }
  } else {
    available_years <- sort(unique(growth_dt$Year))
    use_year <- get_last_available_year(current_year, available_years)
    gf_row <- growth_dt[Year == use_year]
  }
  
  if (nrow(gf_row) == 0) {
    stop(paste("No growth factors found for Year", current_year, "and scenario", scenario))
  }
  
  gf_row <- gf_row[1]
  
  return(list(
    Value = as.numeric(gf_row[["Value"]]),
    Quantity = as.numeric(gf_row[["Quantity"]])
  ))
}

# IV. Sequential calculation -----------------------------------------------------

results <- vector("list", length(scenarios))

# NEW:
# one base sample only, carried forward from t0 to t5
base_sample_projection <- get_base_sample_for_projection(
  base_year = base_year,
  sample_dt = customs_data$sample
)

# carry adjusted data from one scenario to the next
previous_adjusted_BU <- NULL
previous_adjusted_SIM <- NULL

for (scenario_index in seq_along(scenarios)) {
  
  scenario <- scenarios[scenario_index]
  current_year <- forecast_horizon[scenario_index]
  
  cat("Running scenario:", scenario, "for year:", current_year, "\n")
  
  # 1. Start from the same projection base sample and update only Year
  sample_current <- copy(base_sample_projection)
  sample_current[, Year := current_year]
  sample_current[, HS_code := as.character(HS_code)]
  
  if (nrow(sample_current) == 0) {
    stop(paste("No sample rows found for projection year", current_year))
  }
  
  # 2. Weights for current scenario
  weights_current <- get_weights_for_scenario(
    scenario = scenario,
    weights_dt = weights_source,
    sample_current = sample_current,
    full_sample_n = nrow(customs_data$sample)
  )
  
  # 3. Growth factors for current year/scenario
  growth_factors <- get_growth_factors_for_year(
    current_year = current_year,
    scenario = scenario,
    growth_dt = customs_data$growth_factors
  )
  
  # 4. Baseline parameters
  selected_parameters_BU <- get_baseline_dataset_safe(
    current_year = current_year,
    raw_dt = customs_simulation_parameters_raw_unique
  )
  
  merged_data_BU <- merge(
    sample_current,
    selected_parameters_BU,
    by = c("HS_code", "Year"),
    all.x = TRUE,
    allow.cartesian = FALSE
  )
  
  merged_data_BU[is.na(Effective_Customs_rate), Effective_Customs_rate := 0]
  merged_data_BU[is.na(Effective_Excise_rate), Effective_Excise_rate := 0]
  merged_data_BU[is.na(Effective_VAT_rate), Effective_VAT_rate := 0]
  
  columns_to_process_BU <- setdiff(
    colnames(merged_data_BU),
    c(
      "sample_row_id",
      "weight",
      "HS_code", "Year",
      "Effective_Customs_rate", "Effective_Excise_rate", "Effective_VAT_rate",
      "Value_adjusted", "Quantity_adjusted",
      "CustomsRevenue", "ExciseRevenue", "VAT_Revenue"
    )
  )
  
  Customs_weighted_BU <- cal_weighting_fun(
    sample_data = merged_data_BU,
    weights = weights_current,
    growth_factors = growth_factors,
    columns_to_process = columns_to_process_BU,
    previous_adjusted = previous_adjusted_BU
  )
  
  Customs_BU <- copy(Customs_weighted_BU)
  Customs_BU[, calc_customs_duties := vec_calc_customs_duties_fun(Value_adjusted, Effective_Customs_rate)]
  Customs_BU[, calc_excise := vec_calc_excise_fun(Quantity_adjusted, Effective_Excise_rate)]
  Customs_BU[, calc_vat := vec_calc_vat_fun(Value_adjusted, calc_customs_duties, calc_excise, Effective_VAT_rate)]
  
  # Save adjusted baseline result for next scenario
  previous_adjusted_BU <- filter_columns(Customs_BU)
  
  # 5. Simulation parameters
  selected_parameters_SIM <- get_sim_dataset_safe(
    current_year = current_year,
    simulation_year = simulation_year,
    raw_dt = customs_simulation_parameters_raw_unique,
    updated_dt = customs_simulation_parameters_updated_unique
  )
  
  merged_data_SIM <- merge(
    sample_current,
    selected_parameters_SIM,
    by = c("HS_code", "Year"),
    all.x = TRUE,
    allow.cartesian = FALSE
  )
  
  merged_data_SIM[is.na(Effective_Customs_rate), Effective_Customs_rate := 0]
  merged_data_SIM[is.na(Effective_Excise_rate), Effective_Excise_rate := 0]
  merged_data_SIM[is.na(Effective_VAT_rate), Effective_VAT_rate := 0]
  
  columns_to_process_SIM <- setdiff(
    colnames(merged_data_SIM),
    c(
      "sample_row_id",
      "weight",
      "HS_code", "Year",
      "Effective_Customs_rate", "Effective_Excise_rate", "Effective_VAT_rate",
      "Value_adjusted", "Quantity_adjusted",
      "CustomsRevenue", "ExciseRevenue", "VAT_Revenue"
    )
  )
  
  Customs_weighted_SIM <- cal_weighting_fun(
    sample_data = merged_data_SIM,
    weights = weights_current,
    growth_factors = growth_factors,
    columns_to_process = columns_to_process_SIM,
    previous_adjusted = previous_adjusted_SIM
  )
  
  Customs_SIM <- copy(Customs_weighted_SIM)
  Customs_SIM[, calc_customs_duties := vec_calc_customs_duties_fun(Value_adjusted, Effective_Customs_rate)]
  Customs_SIM[, calc_excise := vec_calc_excise_fun(Quantity_adjusted, Effective_Excise_rate)]
  Customs_SIM[, calc_vat := vec_calc_vat_fun(Value_adjusted, calc_customs_duties, calc_excise, Effective_VAT_rate)]
  
  # Save adjusted simulation result for next scenario
  previous_adjusted_SIM <- filter_columns(Customs_SIM)
  
  results[[scenario_index]] <- list(
    year = current_year,
    scenario = scenario,
    Customs_BU = Customs_BU,
    Customs_SIM = Customs_SIM
  )
}

# V. Compile results -------------------------------------------------------------

Customs_BU_list <- lapply(results, function(res) res$Customs_BU)
names(Customs_BU_list) <- sapply(results, function(res) as.character(res$scenario))

Customs_SIM_list <- lapply(results, function(res) res$Customs_SIM)
names(Customs_SIM_list) <- sapply(results, function(res) as.character(res$scenario))

# VI. Summaries ------------------------------------------------------------------

summarize_Customs_fun <- function(results_obj, which_dataset = c("Customs_BU", "Customs_SIM"), suffix) {
  
  which_dataset <- match.arg(which_dataset)
  result_list <- list()
  
  for (i in seq_along(results_obj)) {
    
    df <- as.data.table(copy(results_obj[[i]][[which_dataset]]))
    current_year <- results_obj[[i]]$year
    current_scenario <- results_obj[[i]]$scenario
    
    if (is.list(df$calc_customs_duties)) {
      df[, calc_customs_duties := sapply(
        calc_customs_duties,
        function(x) if (is.null(x)) NA_real_ else as.numeric(x)
      )]
    }
    
    calc_columns <- names(df)[grepl("^calc", names(df))]
    calc_sums <- df[, lapply(.SD, sum, na.rm = TRUE), .SDcols = calc_columns]
    
    scenario_result <- data.table(
      year = as.integer(current_year),
      scenarios = current_scenario
    )
    
    scenario_result <- cbind(scenario_result, calc_sums)
    result_list[[i]] <- scenario_result
  }
  
  result <- rbindlist(result_list, fill = TRUE)
  setnames(result, old = calc_columns, new = paste0(calc_columns, suffix))
  return(result)
}

summary_BU <- summarize_Customs_fun(results, which_dataset = "Customs_BU", suffix = "_bu")
summary_SIM <- summarize_Customs_fun(results, which_dataset = "Customs_SIM", suffix = "_sim")

merged_Customs_BU_SIM <- merge(summary_BU, summary_SIM, by = c("year", "scenarios"), all = TRUE)

numeric_columns <- names(merged_Customs_BU_SIM)[sapply(merged_Customs_BU_SIM, is.numeric)]
numeric_columns <- setdiff(numeric_columns, "year")
merged_Customs_BU_SIM[, (numeric_columns) := lapply(.SD, function(x) x / 1e06), .SDcols = numeric_columns]

setorder(merged_Customs_BU_SIM, year)

print(merged_Customs_BU_SIM)

# VII. GUI tables ---------------------------------------------------------------

MacroFiscalData <- as.data.table(MacroFiscalData)
MacroFiscalData[, Year := as.integer(as.character(Year))]

# 1. Customs summary
Customs_summary <- merged_Customs_BU_SIM %>%
  tidyr::pivot_longer(
    cols = -c(year, scenarios),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(bu|sim)"
  ) %>%
  dplyr::mutate(difference = sim - bu) %>%
  dplyr::mutate(dplyr::across(c(bu, sim, difference), ~ round(., 1))) %>%
  dplyr::filter(variable == "calc_customs_duties") %>%
  dplyr::select(year, scenarios, bu, sim, difference) %>%
  dplyr::rename(
    "Current law (EUR Mil)" = bu,
    "Simulation (EUR Mil)" = sim,
    "Fiscal impact (EUR Mil)" = difference
  )

Customs_summary <- Customs_summary %>%
  dplyr::left_join(MacroFiscalData %>% dplyr::select(Year, GDP), by = c("year" = "Year")) %>%
  dplyr::select(year, scenarios, "Current law (EUR Mil)", "Simulation (EUR Mil)", "Fiscal impact (EUR Mil)", GDP) %>%
  dplyr::mutate(
    `Current law (Pct of GDP)` = round(ifelse(GDP == 0, 0, `Current law (EUR Mil)` / GDP * 100), 2),
    `Simulation (Pct of GDP)` = round(ifelse(GDP == 0, 0, `Simulation (EUR Mil)` / GDP * 100), 2),
    `Fiscal impact (Pct of GDP)` = round(ifelse(GDP == 0, 0, `Fiscal impact (EUR Mil)` / GDP * 100), 2)
  ) %>%
  dplyr::select(-GDP)

Customs_summary <- as.data.table(Customs_summary)
Customs_summary$scenarios <- NULL

# 2. Excise summary
Excise_summary <- merged_Customs_BU_SIM %>%
  tidyr::pivot_longer(
    cols = -c(year, scenarios),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(bu|sim)"
  ) %>%
  dplyr::mutate(difference = sim - bu) %>%
  dplyr::mutate(dplyr::across(c(bu, sim, difference), ~ round(., 1))) %>%
  dplyr::filter(variable == "calc_excise") %>%
  dplyr::select(year, scenarios, bu, sim, difference) %>%
  dplyr::rename(
    "Current law (EUR Mil)" = bu,
    "Simulation (EUR Mil)" = sim,
    "Fiscal impact (EUR Mil)" = difference
  )

Excise_summary <- Excise_summary %>%
  dplyr::left_join(MacroFiscalData %>% dplyr::select(Year, GDP), by = c("year" = "Year")) %>%
  dplyr::select(year, scenarios, "Current law (EUR Mil)", "Simulation (EUR Mil)", "Fiscal impact (EUR Mil)", GDP) %>%
  dplyr::mutate(
    `Current law (Pct of GDP)` = round(ifelse(GDP == 0, 0, `Current law (EUR Mil)` / GDP * 100), 2),
    `Simulation (Pct of GDP)` = round(ifelse(GDP == 0, 0, `Simulation (EUR Mil)` / GDP * 100), 2),
    `Fiscal impact (Pct of GDP)` = round(ifelse(GDP == 0, 0, `Fiscal impact (EUR Mil)` / GDP * 100), 2)
  ) %>%
  dplyr::select(-GDP)

Excise_summary <- as.data.table(Excise_summary)
Excise_summary$scenarios <- NULL

# 3. VAT summary
VAT_summary <- merged_Customs_BU_SIM %>%
  tidyr::pivot_longer(
    cols = -c(year, scenarios),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(bu|sim)"
  ) %>%
  dplyr::mutate(difference = sim - bu) %>%
  dplyr::mutate(dplyr::across(c(bu, sim, difference), ~ round(., 1))) %>%
  dplyr::filter(variable == "calc_vat") %>%
  dplyr::select(year, scenarios, bu, sim, difference) %>%
  dplyr::rename(
    "Current law (EUR Mil)" = bu,
    "Simulation (EUR Mil)" = sim,
    "Fiscal impact (EUR Mil)" = difference
  )

VAT_summary <- VAT_summary %>%
  dplyr::left_join(MacroFiscalData %>% dplyr::select(Year, GDP), by = c("year" = "Year")) %>%
  dplyr::select(year, scenarios, "Current law (EUR Mil)", "Simulation (EUR Mil)", "Fiscal impact (EUR Mil)", GDP) %>%
  dplyr::mutate(
    `Current law (Pct of GDP)` = round(ifelse(GDP == 0, 0, `Current law (EUR Mil)` / GDP * 100), 2),
    `Simulation (Pct of GDP)` = round(ifelse(GDP == 0, 0, `Simulation (EUR Mil)` / GDP * 100), 2),
    `Fiscal impact (Pct of GDP)` = round(ifelse(GDP == 0, 0, `Fiscal impact (EUR Mil)` / GDP * 100), 2)
  ) %>%
  dplyr::select(-GDP)

VAT_summary <- as.data.table(VAT_summary)
VAT_summary$scenarios <- NULL

# 4. Total import summary
Total_import_summary <- dplyr::bind_rows(Customs_summary, Excise_summary, VAT_summary) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(
    `Current law (EUR Mil)` = sum(`Current law (EUR Mil)`, na.rm = TRUE),
    `Simulation (EUR Mil)` = sum(`Simulation (EUR Mil)`, na.rm = TRUE),
    `Fiscal impact (EUR Mil)` = sum(`Fiscal impact (EUR Mil)`, na.rm = TRUE),
    `Current law (Pct of GDP)` = sum(`Current law (Pct of GDP)`, na.rm = TRUE),
    `Simulation (Pct of GDP)` = sum(`Simulation (Pct of GDP)`, na.rm = TRUE),
    `Fiscal impact (Pct of GDP)` = sum(`Fiscal impact (Pct of GDP)`, na.rm = TRUE),
    .groups = "drop"
  )

Total_import_summary <- as.data.table(Total_import_summary)

# VIII. Timing ------------------------------------------------------------------

end.time <- proc.time()
save.time <- end.time - start.time
cat("\n Number of minutes running-Tax Calculator:", save.time[3] / 60, "\n\n")