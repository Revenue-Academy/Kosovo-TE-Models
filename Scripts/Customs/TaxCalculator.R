# Start parallel processing
start.time <- proc.time()


# MAIN CALCULATIONS --------------------------------------------------------

            # Define base and simulation years
            base_year <- customs_simulation_parameters_raw$Year[1]
            end_year <- base_year + 4
            simulation_year <- SimulationYear  # Set this to the appropriate year dynamically as needed
            
            # Define the forecast horizon and scenarios
            forecast_horizon <- seq(base_year, end_year)
            scenarios <- c("t0", "t1", "t2", "t3", "t4")
            
            # Initialize lists for results
            Customs_BU_list <- list()
            Customs_SIM_list <- list()
            
            # Convert data to data.table and handle missing values
            customs_data$sample <- as.data.table(customs_data$sample)
            customs_data$sample[is.na(customs_data$sample)] <- 0
            
            customs_data$weights <- lapply(customs_data$weights, as.data.table)
            customs_data$growth_factors <- as.data.table(customs_data$growth_factors)
            
            customs_simulation_parameters_raw <- as.data.table(customs_simulation_parameters_raw)
            customs_simulation_parameters_updated <- as.data.table(customs_simulation_parameters_updated)
            
            # Replace NA with 0 in parameter datasets
            customs_simulation_parameters_raw[is.na(customs_simulation_parameters_raw)] <- 0
            customs_simulation_parameters_updated[is.na(customs_simulation_parameters_updated)] <- 0

# Setup parallel backend to use multiple processors
cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(data.table)
  library(dplyr)
})

# Export necessary variables and functions to cluster nodes
clusterExport(cl, c(
  "customs_data", "scenarios", "base_year", "simulation_year",
  "get_sim_dataset", "customs_simulation_parameters_raw", "customs_simulation_parameters_updated",
  "cal_weighting_fun", "vec_calc_customs_duties_fun", "vec_calc_excise_fun", "vec_calc_vat_fun"
))

# Parallel computation for each scenario
results <- parLapply(cl, seq_along(scenarios), function(scenario_index) {
  library(data.table)
  library(dplyr)
  
  scenario <- scenarios[scenario_index]
  current_year <- base_year + (scenario_index - 1)
  
  # Extract weights
  weights_table <- customs_data$weights[[scenario_index]]
  weights <- as.numeric(weights_table$V1)
  
  if (length(weights) != nrow(customs_data$sample)) {
    stop(paste("Weights must be a numeric vector with the same length as the number of rows in sample_data for scenario", scenario))
  }
  
  # Extract growth factors
  gf_row <- customs_data$growth_factors[Year == current_year & scenarios == scenario]
  if (nrow(gf_row) == 0) {
    stop(paste("No growth factors found for Year", current_year, "and scenario", scenario))
  }
  growth_factors <- list(
    Value = as.numeric(gf_row[["Value"]]),
    Quantity = as.numeric(gf_row[["Quantity"]])
  )
  
  # Customs_BU calculations
  selected_parameters_BU <- customs_simulation_parameters_raw
  merged_data_BU <- merge(customs_data$sample, selected_parameters_BU[, .(HS_code, Effective_Customs_rate, Effective_Excise_rate, Effective_VAT_rate)], by = "HS_code", all.x = TRUE)
  columns_to_process_BU <- setdiff(colnames(merged_data_BU), c("HS_code", "Effective_Customs_rate", "Value_adjusted", "CustomsRevenue", "ExciseRevenue", "Quantity_adjusted"))
  Customs_weighted_BU <- cal_weighting_fun(merged_data_BU, weights, growth_factors, columns_to_process_BU)
  Customs_BU <- Customs_weighted_BU
  Customs_BU[, calc_customs_duties := vec_calc_customs_duties_fun(Value_adjusted, Effective_Customs_rate)]
  Customs_BU[, calc_excise := vec_calc_excise_fun(Quantity_adjusted, Effective_Excise_rate)]
  Customs_BU[, calc_vat := vec_calc_vat_fun(Value_adjusted, calc_customs_duties, calc_excise, Effective_VAT_rate)]
  
  # Customs_SIM calculations
  selected_parameters_SIM <- get_sim_dataset(current_year, simulation_year)
  merged_data_SIM <- merge(customs_data$sample, selected_parameters_SIM[, .(HS_code, Effective_Customs_rate, Effective_Excise_rate, Effective_VAT_rate)], by = "HS_code", all.x = TRUE)
  columns_to_process_SIM <- setdiff(colnames(merged_data_SIM), c("HS_code", "Effective_Customs_rate", "Value_adjusted", "CustomsRevenue", "ExciseRevenue"))
  Customs_weighted_SIM <- cal_weighting_fun(merged_data_SIM, weights, growth_factors, columns_to_process_SIM)
  Customs_SIM <- Customs_weighted_SIM
  Customs_SIM[, calc_customs_duties := vec_calc_customs_duties_fun(Value_adjusted, Effective_Customs_rate)]
  Customs_SIM[, calc_excise := vec_calc_excise_fun(Quantity_adjusted, Effective_Excise_rate)]
  Customs_SIM[, calc_vat := vec_calc_vat_fun(Value_adjusted, calc_customs_duties, calc_excise, Effective_VAT_rate)]
  
  list(Customs_BU = Customs_BU, Customs_SIM = Customs_SIM, scenario = scenario)
})

# Compile results
Customs_BU_list <- lapply(results, function(res) res$Customs_BU)
names(Customs_BU_list) <- sapply(results, function(res) res$scenario)

Customs_SIM_list <- lapply(results, function(res) res$Customs_SIM)
names(Customs_SIM_list) <- sapply(results, function(res) res$scenario)

# Stop cluster
stopCluster(cl)


# Summarize function for each list
summarize_Customs_fun <- function(Customs_list, suffix) {
  result_list <- list()  # Initialize an empty list for each scenario's results
  
  for (scenario in names(Customs_list)) {
    df <- Customs_list[[scenario]]
    
    # Convert 'calc_customs_duties' from list to numeric
    if (is.list(df$calc_customs_duties)) {
      df[, calc_customs_duties := sapply(calc_customs_duties, function(x) if (is.null(x)) NA_real_ else as.numeric(x))]
    }
    
    # Sum the specified columns
    calc_columns <- names(df)[grepl("^(calc)", names(df))]
    calc_sums <- df[, lapply(.SD, sum, na.rm = TRUE), .SDcols = calc_columns]
    scenario_result <- data.table(scenarios = scenario, calc_sums)
    
    result_list[[scenario]] <- scenario_result
  }
  
  result <- rbindlist(result_list, fill = TRUE)
  setnames(result, old = names(result)[-1], new = paste0(names(result)[-1], suffix))
  
  return(result)
}

# Summarize and merge results
summary_SIM <- summarize_Customs_fun(Customs_SIM_list, "_sim")
summary_BU <- summarize_Customs_fun(Customs_BU_list, "_bu")

merged_Customs_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)

# Add year and convert numeric columns to millions
merged_Customs_BU_SIM[, year := as.character(forecast_horizon)]
setcolorder(merged_Customs_BU_SIM, c("year", setdiff(names(merged_Customs_BU_SIM), "year")))

numeric_columns <- sapply(merged_Customs_BU_SIM, is.numeric)
merged_Customs_BU_SIM[, (names(merged_Customs_BU_SIM)[numeric_columns]) := lapply(.SD, function(x) x / 1e06), .SDcols = numeric_columns]

# Display final results
print(merged_Customs_BU_SIM)

# Clean up the environment
rm(results)


# 1. Aggregate Customs revenues by years  (Customs_summary_df) ---------------------------------------------

# Convert data for presentation in GUI
Customs_summary <- merged_Customs_BU_SIM %>%
  pivot_longer(cols = -year, 
               names_to = c("variable", ".value"), 
               names_pattern = "(.*)_(bu|sim)")

# Calculate the difference between _sim and _bu columns
Customs_summary <- Customs_summary %>%
  mutate(difference = sim - bu)


Customs_summary <- Customs_summary %>%
  mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
  filter(variable=='calc_customs_duties')

# Arrange the columns
Customs_summary <- Customs_summary %>%
  select(year, bu, sim, difference)%>%
  dplyr::rename(
    "Current law (EUR Mil)"="bu",
    "Simulation (EUR Mil)"="sim",
    "Fiscal impact (EUR Mil)"="difference",
  )

# Preparing table for GUI
Customs_summary <- left_join(Customs_summary, MacroFiscalData, by = c("year" = "Year")) %>%
  select(year, "Current law (EUR Mil)", "Simulation (EUR Mil)", "Fiscal impact (EUR Mil)", GDP) %>%
  dplyr::mutate(
    `Current law (Pct of GDP)` = round(`Current law (EUR Mil)` / GDP * 100, 2),
    `Simulation (Pct of GDP)` = round(`Simulation (EUR Mil)` / GDP * 100, 2),
    `Fiscal impact (Pct of GDP)` = round(`Fiscal impact (EUR Mil)` / GDP * 100, 2)
  ) %>%
  dplyr::select(-c(GDP))

Customs_summary <- as.data.table(Customs_summary)


# 2.Aggregate Excise revenues by years ----------------------------------

# Convert data for presentation in GUI
Excise_summary <- merged_Customs_BU_SIM %>%
  pivot_longer(cols = -year, 
               names_to = c("variable", ".value"), 
               names_pattern = "(.*)_(bu|sim)")

# Calculate the difference between _sim and _bu columns
Excise_summary <- Excise_summary %>%
  mutate(difference = sim - bu)


Excise_summary <- Excise_summary %>%
  mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
  filter(variable=='calc_excise')

# Arrange the columns
Excise_summary <- Excise_summary %>%
  select(year, bu, sim, difference)%>%
  dplyr::rename(
    "Current law (EUR Mil)"="bu",
    "Simulation (EUR Mil)"="sim",
    "Fiscal impact (EUR Mil)"="difference",
  )

# Preparing table for GUI
Excise_summary <- left_join(Excise_summary, MacroFiscalData, by = c("year" = "Year")) %>%
  select(year, "Current law (EUR Mil)", "Simulation (EUR Mil)", "Fiscal impact (EUR Mil)", GDP) %>%
  dplyr::mutate(
    `Current law (Pct of GDP)` = round(`Current law (EUR Mil)` / GDP * 100, 2),
    `Simulation (Pct of GDP)` = round(`Simulation (EUR Mil)` / GDP * 100, 2),
    `Fiscal impact (Pct of GDP)` = round(`Fiscal impact (EUR Mil)` / GDP * 100, 2)
  ) %>%
  dplyr::select(-c(GDP))

Excise_summary <- as.data.table(Excise_summary)


# 3. Aggregate VAT revenues by years------------------------------------

# Convert data for presentation in GUI
VAT_summary <- merged_Customs_BU_SIM %>%
  pivot_longer(cols = -year, 
               names_to = c("variable", ".value"), 
               names_pattern = "(.*)_(bu|sim)")

# Calculate the difference between _sim and _bu columns
VAT_summary <- VAT_summary %>%
  mutate(difference = sim - bu)


VAT_summary <- VAT_summary %>%
  mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
  filter(variable=='calc_vat')

# Arrange the columns
VAT_summary <- VAT_summary %>%
  select(year, bu, sim, difference)%>%
  dplyr::rename(
    "Current law (EUR Mil)"="bu",
    "Simulation (EUR Mil)"="sim",
    "Fiscal impact (EUR Mil)"="difference",
  )

# Preparing table for GUI
VAT_summary <- left_join(VAT_summary, MacroFiscalData, by = c("year" = "Year")) %>%
  select(year, "Current law (EUR Mil)", "Simulation (EUR Mil)", "Fiscal impact (EUR Mil)", GDP) %>%
  dplyr::mutate(
    `Current law (Pct of GDP)` = round(`Current law (EUR Mil)` / GDP * 100, 2),
    `Simulation (Pct of GDP)` = round(`Simulation (EUR Mil)` / GDP * 100, 2),
    `Fiscal impact (Pct of GDP)` = round(`Fiscal impact (EUR Mil)` / GDP * 100, 2)
  ) %>%
  dplyr::select(-c(GDP))

VAT_summary <- as.data.table(VAT_summary)


# 4. Aggregate Total import revenues by years----------------------------------------



Total_import_summary<-rbind(Customs_summary,Excise_summary,VAT_summary)


Total_import_summary<-Total_import_summary%>%
  group_by(year) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))


Total_import_summary<-as.data.table(Total_import_summary)



# Timing the process
end.time <- proc.time()
save.time <- end.time - start.time
cat("\n Number of minutes running-Tax Calculator:", save.time[3] / 60, "\n \n")
