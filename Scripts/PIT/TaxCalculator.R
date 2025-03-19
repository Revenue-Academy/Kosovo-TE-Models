base_year <- unique(dt$Year)[1]
end_year <- base_year + 4
simulation_year <- SimulationYear  # Year from slider
forecast_horizon <- seq(base_year, end_year)


# Define the scenarios
scenarios <- c("t0", "t1", "t2", "t3", "t4")

# Simulation parameters must be in data.table
pit_simulation_parameters_raw <- pit_simulation_parameters_raw %>% data.table()
pit_simulation_parameters_updated <- pit_simulation_parameters_updated %>% data.table()

# 1. Tax Calculation Function -------------------------------------------------------

start.time <- proc.time()

tax_calc_fun <- function(dt_scn, params_dt) {
  rate1 <- get_param_fun(params_dt, "rate1")
  rate2 <- get_param_fun(params_dt, "rate2")
  rate3 <- get_param_fun(params_dt, "rate3")
  rate4 <- get_param_fun(params_dt, "rate4")
  tbrk1 <- get_param_fun(params_dt, "tbrk1")
  tbrk2 <- get_param_fun(params_dt, "tbrk2")
  tbrk3 <- get_param_fun(params_dt, "tbrk3")
  tbrk4 <- get_param_fun(params_dt, "tbrk4")
  rate_ded_rent <- get_param_fun(params_dt, "rate_ded_rent")
  rate_ded_charitable <- get_param_fun(params_dt, "rate_ded_charitable")
  
  'Income'
  
  # 1. D19 Total Income (add 8 to 18)----------------------------------------------------
  
  # Replace net_income_business with net_income_business_adjusted with 
  
  dt_scn[, calc_total_inc := gross_wage + net_income_business + net_income_partnership +
                             gross_rents + gross_i_interest_pen_pay + gross_i_interest +
                             gross_i_inta_prop + capital_gain + foreign_s_inc +
                             other_inc_gifts]
  
  # 2. D24 Total deductions (add 20 to 23) ------------------------------------------------------------------------
  
  dt_scn[, calc_total_ded := pmax(ded_rents_expen_rents10pct, (rate_ded_rent * gross_rents)) +
           ded_pen_cont + ded_exp_int_prop + other_allowed_ded]
  
  # 3. D25 Taxable amount (19-24) ------------------------------------------
  
  dt_scn[, calc_gti := calc_total_inc - calc_total_ded]
  
  # 4. D27 Deduction for Charitable Contributions (max 5% of taxable amount) not claimed on FS -------------------------------------
  
  #dt_scn[, calc_charity_contribution := min(dis_charity_contribution, max(calc_gti * rate_ded_charitable, 0))]
  dt_scn[, calc_charity_contribution := pmin(dis_charity_contribution, pmax(calc_gti * rate_ded_charitable, 0))]
  
  # 5. D28 Total Additional Deductions (26+27)---------------
  
  dt_scn[, calc_tot_additional_ded := calc_charity_contribution + loss_carried_for]
  
  # 6. D29 Taxable Income before tax [25]-[28] -----------------------------------
  dt_scn[, calc_taxable_inc_before_tax := calc_gti - calc_tot_additional_ded]
  
  # 7. D30 Tax on Taxable Income as per tax brackets -------------------------------
  
  dt_scn[, pitax := (rate1 * pmin(calc_taxable_inc_before_tax, tbrk1) +
                       rate2 * pmin(tbrk2 - tbrk1, pmax(0, calc_taxable_inc_before_tax - tbrk1)) +
                       rate3 * pmin(tbrk3 - tbrk2, pmax(0, calc_taxable_inc_before_tax - tbrk2)) +
                       rate4 * pmax(0, calc_taxable_inc_before_tax - tbrk3))]
}

# 2. Helper to Retrieve Growth Factors for Each Variable-------------------------------

vars_to_grow <- c(
              "gross_wage",
              "net_income_business",
              "net_income_partnership",
              "gross_rents",
              "gross_i_interest_pen_pay",
              "gross_i_interest",
              "gross_i_inta_prop",
              "capital_gain",
              "foreign_s_inc",
              "other_inc_gifts",
              "ded_rents_expen_rents10pct",
              "ded_pen_cont",
              "ded_exp_int_prop",
              "other_allowed_ded",
              "dis_charity_contribution",
              "loss_carried_for"
            )

get_growth_factor_row <- function(scenario) {
  gf_row <- growth_factors_pit[scenarios == scenario]
  out <- numeric(length(vars_to_grow))
  names(out) <- vars_to_grow
  
  for (v in vars_to_grow) {
    gf_col <- sub("_adjusted", "", v)  # e.g. "g_Wages_l"
    out[v] <- gf_row[[gf_col]]
  }
  return(out)
}

# 3. Business as usual  ------------------------------------------------------

# Regardless of SimulationYear, we do the entire chain t0..t4 in PIT_BU_list
# so we have final data for each scenario if we need it in Block 2.

PIT_BU_list <- list()

# Start from baseline
dt_scn_BU <- copy(dt)

for (s in scenarios) {
  
  # 1) Retrieve scenario growth factors
  gf_values <- get_growth_factor_row(s)
  
  # 2) Multiply each variable by gf_values[v] * weights[[s]]
  for (v in vars_to_grow) {
    dt_scn_BU[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
  }
  
  # 3) Row-wise tax logic
  tax_calc_fun(dt_scn_BU, pit_simulation_parameters_raw)
  
  # 4) ADD a 'weight' column that references weights_pit[[s]]
  dt_scn_BU[, weight := weights_pit[[s]]]
  
  # 5) Store in PIT_BU_list
  PIT_BU_list[[s]] <- copy(dt_scn_BU)
}

# 4. Simulation --------------------------------------------------------------
start_index <- match(SimulationYear, scenario_years) # 1..5

PIT_SIM_list <- list()

# 1) Copy "early" scenarios (before SimulationYear) directly from PIT_BU_list.
#    e.g. if start_index=4 => we copy t0, t1, t2 (indexes 1..3).
if (start_index > 1) {
  for (i in seq_len(start_index - 1)) {
    s_early <- scenarios[i]
    PIT_SIM_list[[s_early]] <- copy(PIT_BU_list[[s_early]])
  }
}

# 2) Determine the starting data for re-simulation
if (start_index == 1) {
  # SimulationYear=2021 => start from original dt
  dt_scn_SIM <- copy(dt)
} else {
  # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
  prev_scenario <- scenarios[start_index - 1]
  dt_scn_SIM <- copy(PIT_BU_list[[prev_scenario]])
}

# 3) Chain from scenario index = start_index .. 5
for (i in seq(from = start_index, to = length(scenarios))) {
  s <- scenarios[i]
  
  gf_values <- get_growth_factor_row(s)
  
  # Multiply each variable by growth factor * row-weight for scenario s
  for (v in vars_to_grow) {
    dt_scn_SIM[, (v) := get(v) * gf_values[v] * weights_pit[[s]]]
  }
  
  # Run row-wise calculations with updated parameters
  tax_calc_fun(dt_scn_SIM, pit_simulation_parameters_updated)
  
  # **Add a 'weight' column** with the row-specific weights_pit for scenario s
  dt_scn_SIM[, weight := weights_pit[[s]]]
  
  # Store final data in PIT_SIM_list
  PIT_SIM_list[[s]] <- copy(dt_scn_SIM)
}

message("Block 2 (PIT_SIM_list) done, including early years from PIT_BU_list, plus 'weight' column.\n")
message("All done!\n")

rm(dt_scn_BU, dt_scn_SIM)

# 5. Aggregation of simulated data -----------------------------------------------------

# Function to sum the specified columns in the list and store the results in a data frame
summary_SIM <- summarize_PIT_fun(PIT_SIM_list, "_sim")
summary_BU <- summarize_PIT_fun(PIT_BU_list, "_bu")

merged_PIT_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
merged_PIT_BU_SIM$year <- as.character(forecast_horizon)
merged_PIT_BU_SIM <- merged_PIT_BU_SIM[, c("year", names(merged_PIT_BU_SIM)[-length(merged_PIT_BU_SIM)])]

numeric_columns <- sapply(merged_PIT_BU_SIM, is.numeric)
merged_PIT_BU_SIM[, numeric_columns] <- merged_PIT_BU_SIM[, numeric_columns] / 1e06



# 6. Decile ------------------------------------------------------------------
            # # Define the function for weighted deciles

                #Apply the functions to each data frame in the PIT_BU_list
                for (name in names(PIT_BU_list)) {
                  df <- PIT_BU_list[[name]]
                  df$decile_group <- cal_weighted_deciles_fun(df$calc_total_inc, df$weight)
                  df$centile_group <- cal_weighted_centiles_fun(df$calc_total_inc, df$weight)
                  PIT_BU_list[[name]] <- df
                }
                
                # Apply the functions to each data frame in the PIT_SIM_list
                for (name in names(PIT_SIM_list)) {
                  df <- PIT_SIM_list[[name]]
                  df$decile_group <- cal_weighted_deciles_fun(df$calc_total_inc, df$weight)
                  df$centile_group <- cal_weighted_centiles_fun(df$calc_total_inc, df$weight)
                  PIT_SIM_list[[name]] <- df
                }
                
                
                # Convert data for presentation in GUI
                pit_summary_df <- merged_PIT_BU_SIM %>%
                  pivot_longer(cols = -year, 
                               names_to = c("variable", ".value"), 
                               names_pattern = "(.*)_(bu|sim)")
                
                # Calculate the difference between _sim and _bu columns
                pit_summary_df <- pit_summary_df %>%
                  mutate(difference = sim - bu)
                
                
                pit_summary_df <- pit_summary_df %>%
                  mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
                  filter(variable=='pitax')
                
                # Arrange the columns
                pit_summary_df <- pit_summary_df %>%
                            select(year, bu, sim, difference)%>%
                            dplyr::rename(
                              "Current law (LCU MIL)"="bu",
                              "Simulation (LCU MIL)"="sim",
                              "Fiscal impact (LCU MIL)"="difference",
                            )
                
                
                MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)
                
                pit_summary_df<-left_join(pit_summary_df,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
                  select(year,"Current law (LCU MIL)","Simulation (LCU MIL)","Fiscal impact (LCU MIL)",Nominal_GDP)%>%
                  dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU MIL)`/Nominal_GDP*100,2),
                                 `Simulation (Pct of GDP)`=round(`Simulation (LCU MIL)`/ Nominal_GDP*100,2),
                                 `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU MIL)`/ Nominal_GDP*100,2))%>%
                  dplyr::select(-c(Nominal_GDP))
                
                
                pit_summary_df <- as.data.table(pit_summary_df)
                
                
                
                print(merged_PIT_BU_SIM)
                
                end.time <- proc.time()
                save.time <- end.time - start.time
                cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")

                
              


# 7. Revenues by NACE sections----------------------------------------------------------------------
            
            # Function to extract columns and add scenario identifier
            extract_nace_rev_fun <- function(dt, scenario) {
              dt[, .(pitax, section,description, scenario = scenario)]
            }
            
            # Extract and process PIT_BU_list_TE
            extracted_tables_bu <- mapply(extract_nace_rev_fun, PIT_BU_list, scenarios, SIMPLIFY = FALSE)
            combined_data_bu <- rbindlist(extracted_tables_bu)
            result_bu <- combined_data_bu[, .(total_calc_pitax_bu = sum(pitax)), by = .(scenario,description, section)]
            
            # Extract and process PIT_SIM_list_TE
            
            extracted_tables_sim <- mapply(extract_nace_rev_fun, PIT_SIM_list, scenarios, SIMPLIFY = FALSE)
            combined_data_sim <- rbindlist(extracted_tables_sim)
            result_sim <- combined_data_sim[, .(total_calc_pitax_sim = sum(pitax)), by = .(scenario,section)]
            
            # Add year column to both results
            result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
            result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
            
            # Combine both results into one data frame
            pit_nace_summary <- merge(result_bu, result_sim, by = c("scenario", "year","section"), all = TRUE)


            pit_nace_summary<-pit_nace_summary%>%
              dplyr::filter(year==SimulationYear)

            #pit_nace_summary$value<-"Value"
            pit_nace_summary$value<-"In LCU"
            
            
# 8. Revenues by Gender------------------------------------------------------
      # Function to extract columns and add scenario identifier
      extract_gender_rev_fun <- function(dt, scenario) {
        dt[, .(pitax, Gender,scenario = scenario)]
      }
      
      # Extract and process PIT_BU_list_TE
      extracted_tables_bu <- mapply(extract_gender_rev_fun, PIT_BU_list, scenarios, SIMPLIFY = FALSE)
      combined_data_bu <- rbindlist(extracted_tables_bu)
      result_bu <- combined_data_bu[, .(total_calc_pitax_bu = sum(pitax)), by = .(scenario,Gender)]
      
      # Extract and process PIT_SIM_list_TE
      
      extracted_tables_sim <- mapply(extract_gender_rev_fun, PIT_SIM_list, scenarios, SIMPLIFY = FALSE)
      combined_data_sim <- rbindlist(extracted_tables_sim)
      result_sim <- combined_data_sim[, .(total_calc_pitax_sim = sum(pitax)), by = .(scenario,Gender)]
      
      # Add year column to both results
      result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
      result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
      
      # Combine both results into one data frame
      pit_gender_summary <- merge(result_bu, result_sim, by = c("scenario", "year","Gender"), all = TRUE)
      
      
      pit_gender_summary<-pit_gender_summary%>%
        dplyr::filter(year==SimulationYear)
