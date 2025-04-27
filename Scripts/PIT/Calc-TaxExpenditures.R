" Data Preparation for Tax Expenditures "
base_year <- unique(dt$Year)[1]
end_year <- base_year + 4
SimulationYear <- SimulationYear  # Year from slider
forecast_horizon <- seq(base_year, end_year)

# Define the scenarios
scenarios <- c("t0", "t1", "t2", "t3", "t4")

# Simulation parameters must be in data.table
pit_simulation_parameters_raw <- pit_simulation_parameters_raw %>% data.table()
pit_simulation_parameters_updated <- pit_simulation_parameters_updated %>% data.table()

# Adding TE

pit_simulation_parameters_updated_te<-pit_simulation_parameters_updated

# I.Tax Expenditures (rate_ded_rent) ---------------------------------------
    pit_simulation_parameters_updated_te<-pit_simulation_parameters_raw
    pit_simulation_parameters_updated_te[9,7]<-0
      # 1.Tax Calculation Function -------------------------------------------------------
      
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
      
      # 2.Helper to Retrieve Growth Factors for Each Variable-------------------------------
      
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
      
      # 3.Business as usual  ------------------------------------------------------
      
      # Regardless of SimulationYear, we do the entire chain t0..t4 in PIT_BU_list_TE
      # so we have final data for each scenario if we need it in Block 2.
      
      PIT_BU_list_TE <- list()
      
      # Start from baseline
      dt_scn_BU <- copy(dt)
      #dt_scn_BU <- copy(dt)[tax_payer_group == "0"]
      
      for (s in scenarios) {
        
        # 1) Retrieve scenario growth factors
        gf_values <- get_growth_factor_row(s)
        
        # 2) Multiply each variable by gf_values[v] * weights_pit[[s]]
        for (v in vars_to_grow) {
          dt_scn_BU[, (v) := get(v) * gf_values[v] * 1]
        }
        
        # NEW
        dt_scn_BU <- copy(dt_scn_BU)[tax_payer_group == "0"]
        
        # 3) Row-wise tax logic
        tax_calc_fun(dt_scn_BU, pit_simulation_parameters_raw)
        
        # 4) ADD a 'weight' column that references weights_pit[[s]]
        #dt_scn_BU[, weight := weights_pit[[s]]]
        dt_scn_BU[, weight := 1]
        
        # 5) Store in PIT_BU_list_TE
        PIT_BU_list_TE[[s]] <- copy(dt_scn_BU)
      }
      
      # 4.Simulation --------------------------------------------------------------
      
      start_index <- match(SimulationYear, scenario_years) # 1..5
      
      PIT_SIM_list_TE <- list()
      
      # 1) Copy "early" scenarios (before SimulationYear) directly from PIT_BU_list_TE.
      #    e.g. if start_index=4 => we copy t0, t1, t2 (indexes 1..3).
      if (start_index > 1) {
        for (i in seq_len(start_index - 1)) {
          s_early <- scenarios[i]
          PIT_SIM_list_TE[[s_early]] <- copy(PIT_BU_list_TE[[s_early]])
        }
      }
      
      # 2) Determine the starting data for re-simulation
      if (start_index == 1) {
        # SimulationYear=2021 => start from original dt
        dt_scn_SIM <- copy(dt)
      } else {
        # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
        prev_scenario <- scenarios[start_index - 1]
        dt_scn_SIM <- copy(PIT_BU_list_TE[[prev_scenario]])
      }
      
      # 3) Chain from scenario index = start_index .. 5
      for (i in seq(from = start_index, to = length(scenarios))) {
        s <- scenarios[i]
        
        gf_values <- get_growth_factor_row(s)
        
        # Multiply each variable by growth factor * row-weight for scenario s
        for (v in vars_to_grow) {
          dt_scn_SIM[, (v) := get(v) * gf_values[v] * 1]
        }
        
        # Run row-wise calculations with updated parameters
        tax_calc_fun(dt_scn_SIM, pit_simulation_parameters_updated_te)
        
        # **Add a 'weight' column** with the row-specific weights_pit for scenario s
        dt_scn_SIM[, weight := 1]
        
        # Store final data in PIT_SIM_list_TE
        PIT_SIM_list_TE[[s]] <- copy(dt_scn_SIM)
      }
      
      message("Block 2 (PIT_SIM_list_TE) done, including early years from PIT_BU_list_TE, plus 'weight' column.\n")
      message("All done!\n")
      
      rm(dt_scn_BU, dt_scn_SIM)
      
      # 5.Aggregation of simulated data -----------------------------------------------------
      
      # Function to sum the specified columns in the list and store the results in a data frame
      summary_SIM <- summarize_PIT_fun(PIT_SIM_list_TE, "_sim")
      summary_BU <- summarize_PIT_fun(PIT_BU_list_TE, "_bu")
      
      merged_PIT_BU_SIM_TE <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
      merged_PIT_BU_SIM_TE$year <- as.character(forecast_horizon)
      merged_PIT_BU_SIM_TE <- merged_PIT_BU_SIM_TE[, c("year", names(merged_PIT_BU_SIM_TE)[-length(merged_PIT_BU_SIM_TE)])]
      
      numeric_columns <- sapply(merged_PIT_BU_SIM_TE, is.numeric)
      merged_PIT_BU_SIM_TE[, numeric_columns] <- merged_PIT_BU_SIM_TE[, numeric_columns] / LCU_unit
      
      
      
      # Convert data for presentation in GUI
      pit_summary_df_TE <- merged_PIT_BU_SIM_TE %>%
        pivot_longer(cols = -year, 
                     names_to = c("variable", ".value"), 
                     names_pattern = "(.*)_(bu|sim)")
      
      # Calculate the difference between _sim and _bu columns
      pit_summary_df_TE <- pit_summary_df_TE %>%
        mutate(difference = sim - bu)
      
      
      pit_summary_df_TE <- pit_summary_df_TE %>%
        mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
        filter(variable=='calc_pitax')
      
      # Arrange the columns
      pit_summary_df_TE <- pit_summary_df_TE %>%
        select(year, bu, sim, difference)%>%
        dplyr::rename(
          "Current law (LCU Mil)"="bu",
          "Simulation (LCU Mil)"="sim",
          "Fiscal impact (LCU Mil)"="difference",
        )
      
      
      MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)
      
      pit_summary_df_TE<-left_join(pit_summary_df_TE,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
        select(year,"Current law (LCU Mil)","Simulation (LCU Mil)","Fiscal impact (LCU Mil)",Nominal_GDP)%>%
        dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU Mil)`/Nominal_GDP*100,2),
                       `Simulation (Pct of GDP)`=round(`Simulation (LCU Mil)`/ Nominal_GDP*100,2),
                       `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU Mil)`/ Nominal_GDP*100,2))%>%
        dplyr::select(-c(Nominal_GDP))
      
      
      pit_summary_df_TE <- as.data.table(pit_summary_df_TE)
      
      
      
      print(merged_PIT_BU_SIM_TE)
      
      
      # 6.Summarize data and prepare table for GUI with TE -------------------------------------------------------
      # Extract by type of TE
      patterns <- c('year',
                    "pitax_bu",
                    "pitax_sim"
      )
      
      
      # Create a select statement with all the patterns
      selected_columns <- select(merged_PIT_BU_SIM_TE, starts_with(patterns))
      
      
      # Reshape the data to long format
      te_summary_df <- selected_columns %>%
        pivot_longer(cols = -year,
                     names_to = c("variable", ".value"),
                     names_pattern = "(.*)_(bu|sim)")
      
      # Calculate the TE as difference between _sim and _bu columns
      te_summary_df <- te_summary_df %>%
        mutate(difference = sim - bu)
      
      te_summary_df <- te_summary_df %>%
        mutate(across(c(bu, sim, difference), ~ round(., 3)))
      
      # Arrange the columns
      te_summary_df <- te_summary_df %>%
        select(year, variable, bu, sim, difference)%>%
        dplyr::rename("tax incentive"="variable",
                      #"tax_incentive"="variable",
                      "current law"="bu",
                      "benchmark"="sim",
                      "tax expenditure"="difference",
        )
      
      #       
      #       # Adding name from raw table
      #      
      #       # OVA DA SE NAPRAVI
      # te_summary_df<-left_join(te_summary_df,pit_simulation_parameters_raw,by=c(`tax incentive`="LongName"))%>%
      #   select( year,`tax incentive`,`current law`,benchmark,`tax expenditure`,AdditionalInfo)%>%
      #   dplyr::rename("legal reference"="AdditionalInfo")
      
      
      #     Table for GUI
      te_summary_df <- as.data.table(te_summary_df)
      
      te_summary_df <- te_summary_df %>%
        mutate(across(where(is.numeric), ~ round(., 1)))
      
      
      # # Divide all numerical columns by 1e03 and round to 1 decimal place using dplyr
      # te_summary_df <- te_summary_df %>%
      #   mutate(across(where(is.numeric), ~ round(. / 1e03, 1)))
      
      
      
      
      # 7.Tax expenditures by years ------------------------------------------------------------------------
             
      te_agg<-te_summary_df%>%
        dplyr::select(year, `tax incentive`, `tax expenditure`)%>%
        dplyr:: filter(`tax incentive` %in% c("pitax"))
      #dplyr:: filter(`tax incentive` %in% c("calc_pitax",'pit_diplomatic_consular'))
      
      
      
      te_agg_ded_rent <- te_agg %>%
        mutate(`tax incentive` = recode(`tax incentive`,
                                        calc_pitax = "Tax_Expenditures"
        ))
      
      
      # te_agg <- te_agg %>%
      #   mutate(across(where(is.numeric), ~ round(. / 1e03, 1)))
      
      
      
      
      #view(te_agg_ded_rent)
      
  


      
# II. Tax Expenditures (rate_ded_charitable)---------------------------------------------------------------------
      pit_simulation_parameters_updated_te<-pit_simulation_parameters_raw
      pit_simulation_parameters_updated_te[10,7]<-0
      # 1.Tax Calculation Function -------------------------------------------------------
      
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
      
      # 2.Helper to Retrieve Growth Factors for Each Variable-------------------------------
      
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
      
      # 3.Business as usual  ------------------------------------------------------
      
      # Regardless of SimulationYear, we do the entire chain t0..t4 in PIT_BU_list_TE
      # so we have final data for each scenario if we need it in Block 2.
      
      PIT_BU_list_TE <- list()
      
      # Start from baseline
      dt_scn_BU <- copy(dt)
      #dt_scn_BU <- copy(dt)[tax_payer_group == "0"]
      
      for (s in scenarios) {
        
        # 1) Retrieve scenario growth factors
        gf_values <- get_growth_factor_row(s)
        
        # 2) Multiply each variable by gf_values[v] * weights_pit[[s]]
        for (v in vars_to_grow) {
          dt_scn_BU[, (v) := get(v) * gf_values[v] * 1]
        }
        
        #new
         dt_scn_BU <- copy(dt_scn_BU)[tax_payer_group == "0"]
        
        # 3) Row-wise tax logic
        tax_calc_fun(dt_scn_BU, pit_simulation_parameters_raw)
        
        # 4) ADD a 'weight' column that references weights_pit[[s]]
        dt_scn_BU[, weight := 1]
        
        # 5) Store in PIT_BU_list_TE
        PIT_BU_list_TE[[s]] <- copy(dt_scn_BU)
      }
      
      # 4.Simulation --------------------------------------------------------------
      
      start_index <- match(SimulationYear, scenario_years) # 1..5
      
      PIT_SIM_list_TE <- list()
      
      # 1) Copy "early" scenarios (before SimulationYear) directly from PIT_BU_list_TE.
      #    e.g. if start_index=4 => we copy t0, t1, t2 (indexes 1..3).
      if (start_index > 1) {
        for (i in seq_len(start_index - 1)) {
          s_early <- scenarios[i]
          PIT_SIM_list_TE[[s_early]] <- copy(PIT_BU_list_TE[[s_early]])
        }
      }
      
      # 2) Determine the starting data for re-simulation
      if (start_index == 1) {
        # SimulationYear=2021 => start from original dt
        dt_scn_SIM <- copy(dt)
      } else {
        # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
        prev_scenario <- scenarios[start_index - 1]
        dt_scn_SIM <- copy(PIT_BU_list_TE[[prev_scenario]])
      }
      
      # 3) Chain from scenario index = start_index .. 5
      for (i in seq(from = start_index, to = length(scenarios))) {
        s <- scenarios[i]
        
        gf_values <- get_growth_factor_row(s)
        
        # Multiply each variable by growth factor * row-weight for scenario s
        for (v in vars_to_grow) {
          dt_scn_SIM[, (v) := get(v) * gf_values[v] * 1]
        }
        
        # Run row-wise calculations with updated parameters
        tax_calc_fun(dt_scn_SIM, pit_simulation_parameters_updated_te)
        
        # **Add a 'weight' column** with the row-specific weights_pit for scenario s
        dt_scn_SIM[, weight := 1]
        
        # Store final data in PIT_SIM_list_TE
        PIT_SIM_list_TE[[s]] <- copy(dt_scn_SIM)
      }
      
      message("Block 2 (PIT_SIM_list_TE) done, including early years from PIT_BU_list_TE, plus 'weight' column.\n")
      message("All done!\n")
      
      rm(dt_scn_BU, dt_scn_SIM)
      
      # 5.Aggregation of simulated data -----------------------------------------------------
      
      # Function to sum the specified columns in the list and store the results in a data frame
      summary_SIM <- summarize_PIT_fun(PIT_SIM_list_TE, "_sim")
      summary_BU <- summarize_PIT_fun(PIT_BU_list_TE, "_bu")
      
      merged_PIT_BU_SIM_TE <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
      merged_PIT_BU_SIM_TE$year <- as.character(forecast_horizon)
      merged_PIT_BU_SIM_TE <- merged_PIT_BU_SIM_TE[, c("year", names(merged_PIT_BU_SIM_TE)[-length(merged_PIT_BU_SIM_TE)])]
      
      numeric_columns <- sapply(merged_PIT_BU_SIM_TE, is.numeric)
      merged_PIT_BU_SIM_TE[, numeric_columns] <- merged_PIT_BU_SIM_TE[, numeric_columns] / LCU_unit
      
      
      
      # Convert data for presentation in GUI
      pit_summary_df_TE <- merged_PIT_BU_SIM_TE %>%
        pivot_longer(cols = -year, 
                     names_to = c("variable", ".value"), 
                     names_pattern = "(.*)_(bu|sim)")
      
      # Calculate the difference between _sim and _bu columns
      pit_summary_df_TE <- pit_summary_df_TE %>%
        mutate(difference = sim - bu)
      
      
      pit_summary_df_TE <- pit_summary_df_TE %>%
        mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
        filter(variable=='calc_pitax')
      
      # Arrange the columns
      pit_summary_df_TE <- pit_summary_df_TE %>%
        select(year, bu, sim, difference)%>%
        dplyr::rename(
          "Current law (LCU Mil)"="bu",
          "Simulation (LCU Mil)"="sim",
          "Fiscal impact (LCU Mil)"="difference",
        )
      
      
      MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)
      
      pit_summary_df_TE<-left_join(pit_summary_df_TE,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
        select(year,"Current law (LCU Mil)","Simulation (LCU Mil)","Fiscal impact (LCU Mil)",Nominal_GDP)%>%
        dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU Mil)`/Nominal_GDP*100,2),
                       `Simulation (Pct of GDP)`=round(`Simulation (LCU Mil)`/ Nominal_GDP*100,2),
                       `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU Mil)`/ Nominal_GDP*100,2))%>%
        dplyr::select(-c(Nominal_GDP))
      
      
      pit_summary_df_TE <- as.data.table(pit_summary_df_TE)
      
      
      
      print(merged_PIT_BU_SIM_TE)
      
      
      # 6.Summarize data and prepare table for GUI with TE -------------------------------------------------------
      # Extract by type of TE
      patterns <- c('year',
                    "pitax_bu",
                    "pitax_sim"
      )
      
      
      # Create a select statement with all the patterns
      selected_columns <- select(merged_PIT_BU_SIM_TE, starts_with(patterns))
      
      
      # Reshape the data to long format
      te_summary_df <- selected_columns %>%
        pivot_longer(cols = -year,
                     names_to = c("variable", ".value"),
                     names_pattern = "(.*)_(bu|sim)")
      
      # Calculate the TE as difference between _sim and _bu columns
      te_summary_df <- te_summary_df %>%
        mutate(difference = sim - bu)
      
      te_summary_df <- te_summary_df %>%
        mutate(across(c(bu, sim, difference), ~ round(., 3)))
      
      # Arrange the columns
      te_summary_df <- te_summary_df %>%
        select(year, variable, bu, sim, difference)%>%
        dplyr::rename("tax incentive"="variable",
                      #"tax_incentive"="variable",
                      "current law"="bu",
                      "benchmark"="sim",
                      "tax expenditure"="difference",
        )
      
      #       
      #       # Adding name from raw table
      #      
      #       # OVA DA SE NAPRAVI
      # te_summary_df<-left_join(te_summary_df,pit_simulation_parameters_raw,by=c(`tax incentive`="LongName"))%>%
      #   select( year,`tax incentive`,`current law`,benchmark,`tax expenditure`,AdditionalInfo)%>%
      #   dplyr::rename("legal reference"="AdditionalInfo")
      
      
      #     Table for GUI
      te_summary_df <- as.data.table(te_summary_df)
      
      te_summary_df <- te_summary_df %>%
        mutate(across(where(is.numeric), ~ round(., 1)))
      
      
      # # Divide all numerical columns by 1e03 and round to 1 decimal place using dplyr
      # te_summary_df <- te_summary_df %>%
      #   mutate(across(where(is.numeric), ~ round(. / 1e03, 1)))
      
      
      
      
      # 7.Tax expenditures by years ------------------------------------------------------------------------
      
      te_agg<-te_summary_df%>%
        dplyr::select(year, `tax incentive`, `tax expenditure`)%>%
        dplyr:: filter(`tax incentive` %in% c("pitax"))
      #dplyr:: filter(`tax incentive` %in% c("calc_pitax",'pit_diplomatic_consular'))
      
      
      
      te_agg_ded_charitable <- te_agg %>%
        mutate(`tax incentive` = recode(`tax incentive`,
                                        calc_pitax = "Tax_Expenditures"
        ))
      
      
      # te_agg <- te_agg %>%
      #   mutate(across(where(is.numeric), ~ round(. / 1e03, 1)))
      
      
      
    #  view(te_agg_ded_charitable)
      

      # 8. Aggregate data ----------------------------------------------------------

      te_agg_ded_rent$`tax incentive`<-"ded_rent"
      te_agg_ded_charitable$`tax incentive`<-"ded_charitable"
    
     te_agg_type<-rbind(te_agg_ded_rent,te_agg_ded_charitable)
      
     

     
# III. Aggregate data --------------------------------------------------------------------
      
      pit_simulation_parameters_updated_te<-pit_simulation_parameters_updated
      
      pit_simulation_parameters_updated_te[9,7]<-0
      pit_simulation_parameters_updated_te[10,7]<-0
      
      
      
      # 1. Tax Calculation Function -------------------------------------------------------
      
     
      
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
      
      # Regardless of SimulationYear, we do the entire chain t0..t4 in PIT_BU_list_TE
      # so we have final data for each scenario if we need it in Block 2.
      
      PIT_BU_list_TE <- list()
      
      # Start from baseline
      dt_scn_BU <- copy(dt)
      
      for (s in scenarios) {
        
        # 1) Retrieve scenario growth factors
        gf_values <- get_growth_factor_row(s)
        
        # 2) Multiply each variable by gf_values[v] * weights_pit[[s]]
        for (v in vars_to_grow) {
          dt_scn_BU[, (v) := get(v) * gf_values[v] *1]
        }
        
        # 3) Row-wise tax logic
        tax_calc_fun(dt_scn_BU, pit_simulation_parameters_raw)
        
        # 4) ADD a 'weight' column that references weights_pit[[s]]
        dt_scn_BU[, weight := 1]
        
        # 5) Store in PIT_BU_list_TE
        PIT_BU_list_TE[[s]] <- copy(dt_scn_BU)
      }
      
      # 4. Simulation --------------------------------------------------------------
      
      start_index <- match(SimulationYear, scenario_years) # 1..5
      
      PIT_SIM_list_TE <- list()
      
      # 1) Copy "early" scenarios (before SimulationYear) directly from PIT_BU_list_TE.
      #    e.g. if start_index=4 => we copy t0, t1, t2 (indexes 1..3).
      if (start_index > 1) {
        for (i in seq_len(start_index - 1)) {
          s_early <- scenarios[i]
          PIT_SIM_list_TE[[s_early]] <- copy(PIT_BU_list_TE[[s_early]])
        }
      }
      
      # 2) Determine the starting data for re-simulation
      if (start_index == 1) {
        # SimulationYear=2021 => start from original dt
        dt_scn_SIM <- copy(dt)
      } else {
        # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
        prev_scenario <- scenarios[start_index - 1]
        dt_scn_SIM <- copy(PIT_BU_list_TE[[prev_scenario]])
      }
      
      # 3) Chain from scenario index = start_index .. 5
      for (i in seq(from = start_index, to = length(scenarios))) {
        s <- scenarios[i]
        
        gf_values <- get_growth_factor_row(s)
        
        # Multiply each variable by growth factor * row-weight for scenario s
        for (v in vars_to_grow) {
          dt_scn_SIM[, (v) := get(v) * gf_values[v] * 1]
        }
        
        # Run row-wise calculations with updated parameters
        tax_calc_fun(dt_scn_SIM, pit_simulation_parameters_updated_te)
        
        # **Add a 'weight' column** with the row-specific weights_pit for scenario s
        dt_scn_SIM[, weight := 1]
        
        # Store final data in PIT_SIM_list_TE
        PIT_SIM_list_TE[[s]] <- copy(dt_scn_SIM)
      }
      
      message("Block 2 (PIT_SIM_list_TE) done, including early years from PIT_BU_list_TE, plus 'weight' column.\n")
      message("All done!\n")
      
      rm(dt_scn_BU, dt_scn_SIM)
      
      # 5. Aggregation of simulated data -----------------------------------------------------
      
      # Function to sum the specified columns in the list and store the results in a data frame
      summary_SIM <- summarize_PIT_fun(PIT_SIM_list_TE, "_sim")
      summary_BU <- summarize_PIT_fun(PIT_BU_list_TE, "_bu")
      
      merged_PIT_BU_SIM_TE <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
      merged_PIT_BU_SIM_TE$year <- as.character(forecast_horizon)
      merged_PIT_BU_SIM_TE <- merged_PIT_BU_SIM_TE[, c("year", names(merged_PIT_BU_SIM_TE)[-length(merged_PIT_BU_SIM_TE)])]
      
      numeric_columns <- sapply(merged_PIT_BU_SIM_TE, is.numeric)
      merged_PIT_BU_SIM_TE[, numeric_columns] <- merged_PIT_BU_SIM_TE[, numeric_columns] / LCU_unit
      
      
      
      # 6. Decile ------------------------------------------------------------------
      
      
      calc_weighted_groups_in_one_pass <- function(DT, inc_col = "calc_total_inc", w_col = "weight") {
        # 1. Keep track of original row order so we can restore it after sorting
        DT[, row_id__tmp := .I]
        
        # 2. Sort by income (use setorderv for a character column name)
        setorderv(DT, inc_col)
        
        # 3. Compute the cumulative sum of weight
        #    (handle NA weights as 0, adjust if you prefer a different approach)
        DT[, w_cumsum__tmp := cumsum(fifelse(is.na(get(w_col)), 0, get(w_col)))]
        
        # 4. Get the total weight
        total_w <- DT[.N, w_cumsum__tmp]
        
        # 5. Define breakpoints for deciles (10 groups) and centiles (100 groups)
        decile_breaks  <- seq(0, total_w, length.out = 11)   # 11 points => 10 intervals
        centile_breaks <- seq(0, total_w, length.out = 101)  # 101 points => 100 intervals
        
        # 6. Assign decile_group and centile_group
        DT[, decile_group  := findInterval(w_cumsum__tmp, decile_breaks,  rightmost.closed = TRUE)]
        DT[, centile_group := findInterval(w_cumsum__tmp, centile_breaks, rightmost.closed = TRUE)]
        
        # 7. Ensure the top boundary doesn't exceed the number of groups
        DT[, decile_group  := pmin(decile_group,  10)]
        DT[, centile_group := pmin(centile_group, 100)]
        
        # 8. Restore original row order
        setorder(DT, row_id__tmp)
        
        # 9. Clean up temporary columns
        DT[, c("row_id__tmp", "w_cumsum__tmp") := NULL]
        
        # Modifies DT in place, so no return() needed
        invisible(DT)
      }
      

      for (i in seq_along(PIT_BU_list_TE)) {
        calc_weighted_groups_in_one_pass(
          DT      = PIT_BU_list_TE[[i]],
          inc_col = "calc_total_inc",
          w_col   = "weight"
        )
      }
      
      
      for (i in seq_along(PIT_SIM_list_TE)) {
        calc_weighted_groups_in_one_pass(
          DT      = PIT_SIM_list_TE[[i]],
          inc_col = "calc_total_inc",
          w_col   = "weight"
        )
      }
      
      # Convert data for presentation in GUI
      pit_summary_df_TE <- merged_PIT_BU_SIM_TE %>%
        pivot_longer(cols = -year, 
                     names_to = c("variable", ".value"), 
                     names_pattern = "(.*)_(bu|sim)")
      
      # Calculate the difference between _sim and _bu columns
      pit_summary_df_TE <- pit_summary_df_TE %>%
        mutate(difference = sim - bu)
      
      
      pit_summary_df_TE <- pit_summary_df_TE %>%
        mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
        filter(variable=='calc_pitax')
      
      # Arrange the columns
      pit_summary_df_TE <- pit_summary_df_TE %>%
        select(year, bu, sim, difference)%>%
        dplyr::rename(
          "Current law (LCU Mil)"="bu",
          "Simulation (LCU Mil)"="sim",
          "Fiscal impact (LCU Mil)"="difference",
        )
      
      
      MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)
      
      pit_summary_df_TE<-left_join(pit_summary_df_TE,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
        select(year,"Current law (LCU Mil)","Simulation (LCU Mil)","Fiscal impact (LCU Mil)",Nominal_GDP)%>%
        dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU Mil)`/Nominal_GDP*100,2),
                       `Simulation (Pct of GDP)`=round(`Simulation (LCU Mil)`/ Nominal_GDP*100,2),
                       `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU Mil)`/ Nominal_GDP*100,2))%>%
        dplyr::select(-c(Nominal_GDP))
      
      
      pit_summary_df_TE <- as.data.table(pit_summary_df_TE)
      
      
      
      print(merged_PIT_BU_SIM_TE)
      
      end.time <- proc.time()
      save.time <- end.time - start.time
      cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")
      
      
      
      # 1.Summarize data and prepare table for GUI with TE -------------------------------------------------------
      #       # Extract by type of TE
      patterns <- c('year',
                    "pitax_bu",
                    "pitax_sim"
      )
      
      
      # Create a select statement with all the patterns
      selected_columns <- select(merged_PIT_BU_SIM_TE, starts_with(patterns))
      
      
      # Reshape the data to long format
      te_summary_df <- selected_columns %>%
        pivot_longer(cols = -year,
                     names_to = c("variable", ".value"),
                     names_pattern = "(.*)_(bu|sim)")
      
      # Calculate the TE as difference between _sim and _bu columns
      te_summary_df <- te_summary_df %>%
        mutate(difference = sim - bu)
      
      te_summary_df <- te_summary_df %>%
        mutate(across(c(bu, sim, difference), ~ round(., 3)))
      
      # Arrange the columns
      te_summary_df <- te_summary_df %>%
        select(year, variable, bu, sim, difference)%>%
        dplyr::rename("tax incentive"="variable",
                      #"tax_incentive"="variable",
                      "current law"="bu",
                      "benchmark"="sim",
                      "tax expenditure"="difference",
        )
      
      
      #     Table for GUI
      te_summary_df <- as.data.table(te_summary_df)
      
      te_summary_df <- te_summary_df %>%
        mutate(across(where(is.numeric), ~ round(., 1)))
      

      
      
      # 2.Tax expenditures by years ------------------------------------------------------------------------
      #       
      te_agg<-te_summary_df%>%
        dplyr::select(year, `tax incentive`, `tax expenditure`)%>%
        dplyr:: filter(`tax incentive` %in% c("pitax"))
   
      
      
      
      te_agg <- te_agg %>%
        mutate(`tax incentive` = recode(`tax incentive`,
                                        calc_pitax = "Tax_Expenditures"
        ))
      
    
      
      
      # 3.Tax expenditures by NACE sections -----------------------------------------------------------
      
      # Function to extract columns and add scenario identifier
      extract_te_nace_fun <- function(dt, scenario) {
        dt[, .(pitax, section, scenario = scenario)]
      }
      
      # Extract and process PIT_BU_list_TE
      extracted_tables_bu <- mapply(extract_te_nace_fun, PIT_BU_list_TE, scenarios, SIMPLIFY = FALSE)
      combined_data_bu <- rbindlist(extracted_tables_bu)
      result_bu <- combined_data_bu[, .(total_calc_pitax_bu = sum(pitax)), by = .(scenario, section)]
      
      # Extract and process PIT_SIM_list_TE
      
      extracted_tables_sim <- mapply(extract_te_nace_fun, PIT_SIM_list_TE, scenarios, SIMPLIFY = FALSE)
      combined_data_sim <- rbindlist(extracted_tables_sim)
      result_sim <- combined_data_sim[, .(total_calc_pitax_sim = sum(pitax)), by = .(scenario, section)]
      
      # Add year column to both results
      result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
      result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
      
      # Combine both results into one data frame
      nace_pit_summary <- merge(result_bu, result_sim, by = c("scenario", "year","section"), all = TRUE)
      
      
      nace_pit_summary<-nace_pit_summary%>%
        dplyr::mutate(tax_expenditures=total_calc_pitax_sim-total_calc_pitax_bu)
      
      
      nace_pit_summary_te<-nace_pit_summary%>%
        select(year,section,tax_expenditures)
      
      
      # Remove rows with NA in section for the plot
      nace_pit_summary_te <- nace_pit_summary_te[!is.na(section)]
      #nace_pit_summary_te[is.na(section), section := "Other"]
      
      # Left join
      
      # Convert NACE in data.table format
      df_nace_names<-df_nace_names%>%
        data.table()
      
      # Perform the left join
      nace_pit_summary_te <- left_join(nace_pit_summary_te, df_nace_names, by = c("section" = "section"))
      
      # Preparation for Chart 
      
      nace_pit_summary_tbl<-nace_pit_summary_te%>%
        dplyr::filter(year==SimulationYear)
      
      
      nace_pit_summary_tbl$value<-"Value"
      
     
      
      
      # 4.Tax Expenditures by Decile Groups ---------------------------------------------
      
      # Function to extract columns and add scenario identifier
      extract_te_decile_fun <- function(dt, scenario) {
        #dt[, .(pitax, bin, scenario = scenario)]
        dt[, .(pitax, decile_group, scenario = scenario)]
      }
      
      
      # Extract and process PIT_BU_list_TE
      extracted_tables_bu <- mapply(extract_te_decile_fun, PIT_BU_list_TE, scenarios, SIMPLIFY = FALSE)
      combined_data_bu <- rbindlist(extracted_tables_bu)
      result_bu <- combined_data_bu[, .(total_calc_pitax_bu = sum(pitax)), by = .(scenario, decile_group)]
      
      # Extract and process PIT_SIM_list_TE
      extracted_tables_sim <- mapply(extract_te_decile_fun, PIT_SIM_list_TE, scenarios, SIMPLIFY = FALSE)
      combined_data_sim <- rbindlist(extracted_tables_sim)
      result_sim <- combined_data_sim[, .(total_calc_pitax_sim = sum(pitax)), by = .(scenario, decile_group)]
      
      # Add year column to both results
      result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
      result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
      
      # Combine both results into one data frame
      decile_pit_summary <- merge(result_bu, result_sim, by = c("scenario", "year","decile_group"), all = TRUE)
      
      
      decile_pit_summary<-decile_pit_summary%>%
        dplyr::mutate(tax_expenditures=total_calc_pitax_sim-total_calc_pitax_bu)
      
      
      decile_pit_summary<-decile_pit_summary%>%
        select(year,decile_group,tax_expenditures)
      
      decile_pit_summary$bin<-decile_pit_summary$bin+1
      
      # Preparation of chart
      
      # Get unique years from the data
      unique_years <- unique(decile_pit_summary$year)
      
      # Generate a color palette with as many colors as there are unique years
      colors <- brewer.pal(length(unique_years), "Set1")
      
      # Create a named vector for colors, mapping each year to a color
      custom_colors <- setNames(colors, unique_years)
      
      
      decile_pit_summary_tbl<-decile_pit_summary%>%
        dplyr::filter(year==SimulationYear)
      
      
      
      
      
      
      
      
      
      
      
      
      # 5.Type of taxpayer ----------------------------------------------
      
      
      # Function to extract columns and add scenario identifier
      extract_te_TaxpayerType_en_fun <- function(dt, scenario) {
        dt[, .(pitax, TaxpayerType_en, scenario = scenario)]
      }
      
      # Extract and process PIT_BU_list_TE
      extracted_tables_bu <- mapply(extract_te_TaxpayerType_en_fun, PIT_BU_list_TE, scenarios, SIMPLIFY = FALSE)
      combined_data_bu <- rbindlist(extracted_tables_bu)
      result_bu <- combined_data_bu[, .(total_calc_pitax_bu = sum(pitax)), by = .(scenario, TaxpayerType_en)]
      
      # Extract and process PIT_SIM_list_TE
      
      extracted_tables_sim <- mapply(extract_te_TaxpayerType_en_fun, PIT_SIM_list_TE, scenarios, SIMPLIFY = FALSE)
      combined_data_sim <- rbindlist(extracted_tables_sim)
      result_sim <- combined_data_sim[, .(total_calc_pitax_sim = sum(pitax)), by = .(scenario, TaxpayerType_en)]
      
      # Add year column to both results
      result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
      result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
      
      # Combine both results into one data frame
      TaxpayerType_en_pit_summary <- merge(result_bu, result_sim, by = c("scenario", "year","TaxpayerType_en"), all = TRUE)
      
      
      TaxpayerType_en_pit_summary<-TaxpayerType_en_pit_summary%>%
        dplyr::mutate(tax_expenditures=total_calc_pitax_sim-total_calc_pitax_bu)
      
      
      TaxpayerType_en_pit_summary_te<-TaxpayerType_en_pit_summary%>%
        select(year,TaxpayerType_en,tax_expenditures)
      
      
      # Remove rows with NA in TaxpayerType_en for the plot
      TaxpayerType_en_pit_summary_te <- TaxpayerType_en_pit_summary_te[!is.na(TaxpayerType_en)]
      #TaxpayerType_en_pit_summary_te[is.na(TaxpayerType_en), TaxpayerType_en := "Other"]
      
      TaxpayerType_en_pit_summary_te<-TaxpayerType_en_pit_summary_te%>%
        dplyr::filter(year==SimulationYear)
      
      

      
