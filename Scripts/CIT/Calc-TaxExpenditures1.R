base_year <- unique(cit_raw$Year)[1]
end_year <- base_year + 4
SimulationYear <- SimulationYear  # Year from slider
forecast_horizon <- seq(base_year, end_year)

# Define the scenarios
scenarios <- c("t0", "t1", "t2", "t3", "t4")

# Simulation parameters must be in data.table
cit_simulation_parameters_raw <- cit_simulation_parameters_raw %>% data.table()
#cit_simulation_parameters_updated <- cit_simulation_parameters_updated %>% data.table()

cit_simulation_parameters_updated_te<-cit_simulation_parameters_raw

cit_simulation_parameters_updated_te[3,7]<-0
cit_simulation_parameters_updated_te[8,7]<-0
cit_simulation_parameters_updated_te[10,7]<-0
cit_simulation_parameters_updated_te[11,7]<-0



# 1. Tax Calculation Function -------------------------------------------------------

start.time <- proc.time()

tax_calc_fun <- function(dt_scn, params_dt) {
  cit_rate <- get_param_fun(params_dt, "cit_rate")
  cit_rate_ins <- get_param_fun(params_dt, "cit_rate_ins")
  rate_spl_all <- get_param_fun(params_dt, "rate_spl_all")
  rate_tax_rent <- get_param_fun(params_dt, "rate_tax_rent")
  rate_small_trans_ag <- get_param_fun(params_dt, "rate_small_trans_ag")
  min_payment_small <- get_param_fun(params_dt, "min_payment_small")
  rate_small_service_prof <- get_param_fun(params_dt, "rate_small_service_prof")
  rate_max_charitable_ded <- get_param_fun(params_dt, "rate_max_charitable_ded")
  Loss_CFLimit <- get_param_fun(params_dt, "Loss_CFLimit")
  rate_ded_culture_youth <- get_param_fun(params_dt, "rate_ded_culture_youth")
  rate_ded_sponsorship<- get_param_fun(params_dt, "rate_ded_sponsorship")
  
  # I. Corporations ----------------------------------------------
  # 1.[16] Total adjustment to income (add lines [11] to [15]) ----------------------------------------------------
  dt_scn[, calc_totadjinc_l11_l15 :=  forsourceincsch_a + recbaddebts_sch_b + capgain_sch_c + div_sch_d+otherincgain_sch_e]
  
  # 2.[17] Profit (loss) after adjustment to income (line [10] + line [16]) ------------------------------------------------------------------------
  dt_scn[, calc_profitloss_afteradjinc := profitloss_afteradjinc]
  ' 2. Adjustments to Expenses (mostly negative numbers shown in brackets - except line 24)'
  # 3.[27] Total adjustment to expenses (add lines [18] to [26]).------------------------------------------
  
  dt_scn[, calc_totadjexp_l18_l26 := disallowedexp_sch_f + repcosts_sch_g + reservefunds_sch_h + paymrelper_sch_i + amort_sch_j + dep_sch_k + (specallownewassets_sch_l * rate_spl_all) + caploss_sch_c + otherexp_sch_m]
  
  
  # 4.[28] Profit (loss) of business after adjustment to expenses (line [17]- line [27]), (When line 27 is in brackets add the whole numbers)---------------
  
  dt_scn[, calc_grossinc := calc_profitloss_afteradjinc - calc_totadjexp_l18_l26]
  
  
  # 5.[29] Charitable contributions (attach receipts), (limit of 5% of line 28) -----------------------------------
  
  dt_scn[, calc_charitycontrib_box28 := pmin(charitycontrib_box28, pmax((rate_max_charitable_ded*calc_grossinc), 0))]
  
  
  dt_scn[, calc_gti := calc_grossinc - calc_charitycontrib_box28]
  
  
  # 6.[31] Add lines [29] and [30]-------------------------------------
  
  dt_scn[, `:=`(
    newloss1 = fifelse(calc_gti < 0, Loss_lag1 + calc_gti, Loss_lag1 - pmin(abs(Loss_lag1), calc_gti)),
    calc_nti = fifelse(calc_gti < 0, 0, calc_gti - pmin(abs(Loss_lag1), calc_gti))
  )]
  
  # 7.[33].Corporate income tax (If line 32 is a profit, multiply by 10%. If line [32] is a loss, enter 0) -------------------------------
  
  dt_scn[, calc_cit_non_insurance :=  pmax(calc_nti * cit_rate, 0)]
  
  # For Insurance Companies only
  
  # 8.[35] Corporate Income Tax for Insurance Companies ( [34] * 5%) ------------------------------
  
  dt_scn[, calc_cit_insurance :=   pmax(gross_prem * cit_rate_ins, 0)]
  
  dt_scn[, calc_cit_large :=   calc_cit_non_insurance + calc_cit_insurance]
  
  # 9.[39] Total Credits for the period [37] +[38]------------------------
  
  dt_scn[, calc_totcredits_l37_l38 :=  forstattaxcredit_sch_o + taxwithheld_sch_p]
  
  # 10.[40] Line [36] less [39] ------------------------
  
  dt_scn[, calc_l36_minus_l39 :=  pmax(0, calc_cit_large - calc_totcredits_l37_l38)]
  
  
  # [43] Discounts for sponsorship in the field of sports (max.30% of the box [42]
  
  
  dt_scn[, calc_sportspodisc_max30pct_box42 :=  pmin(sportspodisc_max30pct_box42, (rate_ded_sponsorship * calc_cit_large))]
  
  # 44 Discounts for sponsorship in the field of Culture and Youth (Max. 20% of Box [42]
  dt_scn[, calc_culyouthspodisc_max20pct_box42 :=  pmin(culyouthspodisc_max20pct_box42, (rate_ded_culture_youth * calc_cit_large))]
  
  # 11.[45]	Total discounts for sponsorship ([43]+[44], max.30% of box [42]-------------------
  
  dt_scn[, calc_totalspodisc_max30pct_box42 := calc_sportspodisc_max30pct_box42 + calc_culyouthspodisc_max20pct_box42]
  
  # 12.Tax for payment with this form ([36]-[39]-[41]-[45]) ----------------
  
  dt_scn[, calc_cit := pmax((calc_cit_large - calc_totalspodisc_max30pct_box42), 0)  ]
 
}

# 2. Helper to Retrieve Growth Factors for Each Variable-------------------------------

vars_to_grow <- c(
                    "netprofit_fd",
                    "forsourceincsch_a",
                    "recbaddebts_sch_b",
                    "capgain_sch_c",
                    "div_sch_d",
                    "otherincgain_sch_e",
                    "totadjinc_l11_l15",
                    "profitloss_afteradjinc",
                    "disallowedexp_sch_f",
                    "repcosts_sch_g",
                    "reservefunds_sch_h",
                    "paymrelper_sch_i",
                    "amort_sch_j",
                    "dep_sch_k",
                    "specallownewassets_sch_l",
                    "caploss_sch_c",
                    "otherexp_sch_m",
                    "totadjexp_l18_l26",
                    "proflossaftadjexp_l17_l27",
                    "charitycontrib_box28",
                    "losscarryforward",
                    "add_l29_l30",
                    "adjprofit_l28_l31",
                    "corpinctax_l32",
                    "gross_prem",
                    "corpinctax_insurcomp",
                    "corpinctax_total",
                    "forstattaxcredit_sch_o",
                    "taxwithheld_sch_p",
                    "totcredits_l37_l38",
                    "l36_minus_l39",
                    "installmentspaid_sch_q",
                    "taxobl_befdisc_box36",
                    "sportspodisc_max30pct_box42",
                    "culyouthspodisc_max20pct_box42",
                    "totalspodisc_max30pct_box42",
                    "taxpaywithform_l36_l39_l41_l45",
                    "tax_obligation",
                    "tax_refund",
                    "pun_installment",
                    "base_installment",
                    "grossinc",
                    "openinginv",
                    "costofprod",
                    "total_l61_l62",
                    "endinginventory",
                    "costofgoodssold_l63_l64",
                    "grossprofit_l60_l65",
                    "grosswages",
                    "depamortexp_notincost",
                    "sellingexp",
                    "genadminexp_notincost",
                    "rnd_costs",
                    "otheropexp",
                    "totalopexp_l67_l72",
                    "profitloss_oper_l66_l73",
                    "otherrevgains",
                    "otherexploss",
                    "profitloss_nonopact_l75_l76",
                    "netprofitloss_l74_l77"
                  )

get_growth_factor_row <- function(scenario) {
  gf_row <- growth_factors_cit[scenarios == scenario]
  out <- numeric(length(vars_to_grow))
  names(out) <- vars_to_grow
  
  for (v in vars_to_grow) {
    gf_col <- sub("_adjusted", "", v)  
    out[v] <- gf_row[[gf_col]]
  }
  return(out)
}

# 3. Business as usual  ------------------------------------------------------

CIT_BU_list_TE <- list()

# Start from baseline
dt_cit_BU <- copy(cit_raw)

for (s in scenarios) {
  
  # 1) Retrieve scenario growth factors
  gf_values <- get_growth_factor_row(s)
  
  # 2) Multiply each variable by gf_values[v] * weights_cit[[s]]
  for (v in vars_to_grow) {
    dt_cit_BU[, (v) := get(v) * gf_values[v] * weights_cit[[s]]]
  }
  
  # 3) Row-wise tax logic
  tax_calc_fun(dt_cit_BU, cit_simulation_parameters_raw)
  
  # 4) ADD a 'weight' column that references weights_cit[[s]]
  dt_cit_BU[, weight := weights_cit[[s]]]
  
  # 5) Store in CIT_BU_list_TE
  CIT_BU_list_TE[[s]] <- copy(dt_cit_BU)
}

# 4. Simulation --------------------------------------------------------------

start_index <- match(SimulationYear, scenario_years) # 1..5

CIT_SIM_list_TE <- list()

# 1) Copy "early" scenarios (before SimulationYear) directly from CIT_BU_list_TE.
#    e.g. if start_index=4 => we copy t0, t1, t2 (indexes 1..3).
if (start_index > 1) {
  for (i in seq_len(start_index - 1)) {
    s_early <- scenarios[i]
    CIT_SIM_list_TE[[s_early]] <- copy(CIT_BU_list_TE[[s_early]])
  }
}

# 2) Determine the starting data for re-simulation
if (start_index == 1) {
  # SimulationYear=2021 => start from original cit_raw
  dt_cit_SIM <- copy(cit_raw)
} else {
  # e.g. if start_index=4 => scenario t3 => the previous scenario is t2
  prev_scenario <- scenarios[start_index - 1]
  dt_cit_SIM <- copy(CIT_BU_list_TE[[prev_scenario]])
}

# 3) Chain from scenario index = start_index .. 5
for (i in seq(from = start_index, to = length(scenarios))) {
  s <- scenarios[i]
  
  gf_values <- get_growth_factor_row(s)
  
  # Multiply each variable by growth factor * row-weight for scenario s
  for (v in vars_to_grow) {
    dt_cit_SIM[, (v) := get(v) * gf_values[v] * weights_cit[[s]]]
  }
  
  # Run row-wise calculations with updated parameters
  tax_calc_fun(dt_cit_SIM, cit_simulation_parameters_updated_te)
  
  # **Add a 'weight' column** with the row-specific weights_cit for scenario s
  dt_cit_SIM[, weight := weights_cit[[s]]]
  
  # Store final data in CIT_SIM_list_TE
  CIT_SIM_list_TE[[s]] <- copy(dt_cit_SIM)
}

message("Block 2 (CIT_SIM_list_TE) done, including early years from CIT_BU_list_TE, plus 'weight' column.\n")
message("All done!\n")

rm(dt_cit_BU, dt_cit_SIM)

# 5. Aggregation of simulated data -----------------------------------------------------

      # Function to sum the specified columns in the list and store the results in a data frame
      summary_SIM <- summarize_CIT_fun(CIT_SIM_list_TE, "_sim")
      summary_BU <- summarize_CIT_fun(CIT_BU_list_TE, "_bu")
      
      merged_CIT_BU_SIM_TE <- merge(summary_BU, summary_SIM, by = "scenarios", all = TRUE)
      merged_CIT_BU_SIM_TE$year <- as.character(forecast_horizon)
      merged_CIT_BU_SIM_TE <- merged_CIT_BU_SIM_TE[, c("year", names(merged_CIT_BU_SIM_TE)[-length(merged_CIT_BU_SIM_TE)])]
      
      numeric_columns <- sapply(merged_CIT_BU_SIM_TE, is.numeric)
      merged_CIT_BU_SIM_TE[, numeric_columns] <- merged_CIT_BU_SIM_TE[, numeric_columns] / LCU_unit

          # Convert data for presentation in GUI
          cit_summary_df_TE <- merged_CIT_BU_SIM_TE %>%
            pivot_longer(cols = -year, 
                         names_to = c("variable", ".value"), 
                         names_pattern = "(.*)_(bu|sim)")
          
          # Calculate the difference between _sim and _bu columns
          cit_summary_df_TE <- cit_summary_df_TE %>%
            mutate(difference = sim - bu)
          
          
          cit_summary_df_TE <- cit_summary_df_TE %>%
            mutate(across(c(bu, sim, difference), ~ round(., 1)))%>%
            filter(variable=='calc_cit')
          
          # Arrange the columns
          cit_summary_df_TE <- cit_summary_df_TE %>%
            select(year, bu, sim, difference)%>%
            dplyr::rename(
              "Current law (LCU Mil)"="bu",
              "Simulation (LCU Mil)"="sim",
              "Fiscal impact (LCU Mil)"="difference",
            )
          
          
          MACRO_FISCAL_INDICATORS$Year<-as.character(MACRO_FISCAL_INDICATORS$Year)
          
          cit_summary_df_TE<-left_join(cit_summary_df_TE,MACRO_FISCAL_INDICATORS,by=c("year"="Year"))%>%
            select(year,"Current law (LCU Mil)","Simulation (LCU Mil)","Fiscal impact (LCU Mil)",Nominal_GDP)%>%
            dplyr::mutate( `Current law (Pct of GDP)`= round(`Current law (LCU Mil)`/Nominal_GDP*100,2),
                           `Simulation (Pct of GDP)`=round(`Simulation (LCU Mil)`/ Nominal_GDP*100,2),
                           `Fiscal impact (Pct of GDP)`=round(`Fiscal impact (LCU Mil)`/ Nominal_GDP*100,2))%>%
            dplyr::select(-c(Nominal_GDP))
          
          
          cit_summary_df_TE <- as.data.table(cit_summary_df_TE)
          
          
          
          #print(merged_CIT_BU_SIM_TE)
          
          end.time <- proc.time()
          save.time <- end.time - start.time
          cat("\n Number of minutes running:", save.time[3] / 60, "\n \n")

          
          
          
          
          
          # # I. Data preparation --------------------------------------------------------
          # 1.Summarize data and prepare table for GUI with TE -------------------------------------------------------
          #       # Extract by type of TE
          # patterns <- c('year',
          #               "pitax_bu",
          #               "pitax_sim"
          # )
          # 
          # 
          # # Create a select statement with all the patterns
          # selected_columns <- select(merged_CIT_BU_SIM_TE, starts_with(patterns))
          # 
          # 
          # # Reshape the data to long format
          # te_summary_df <- selected_columns %>%
          #   pivot_longer(cols = -year,
          #                names_to = c("variable", ".value"),
          #                names_pattern = "(.*)_(bu|sim)")
          # 
          # # Calculate the TE as difference between _sim and _bu columns
          # te_summary_df <- te_summary_df %>%
          #   mutate(difference = sim - bu)
          # 
          # te_summary_df <- te_summary_df %>%
          #   mutate(across(c(bu, sim, difference), ~ round(., 3)))
          # 
          # # Arrange the columns
          # te_summary_df <- te_summary_df %>%
          #   select(year, variable, bu, sim, difference)%>%
          #   dplyr::rename("tax incentive"="variable",
          #                 #"tax_incentive"="variable",
          #                 "current law"="bu",
          #                 "benchmark"="sim",
          #                 "tax expenditure"="difference",
          #   )
          # 
          # 
          
          
          te_summary_df <-cit_summary_df_TE%>%
            dplyr::select(year,`Current law (LCU Mil)`,`Simulation (LCU Mil)`, `Fiscal impact (LCU Mil)`)%>%
            dplyr::rename(#"tax incentive"="variable",
                          "current law"=`Current law (LCU Mil)`,
                          "benchmark"= `Simulation (LCU Mil)`,
                          "tax expenditure"=`Fiscal impact (LCU Mil)`,
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
          
          
          # Identify numerical columns and round them to 0 decimals
          te_summary_df <- te_summary_df %>%
            dplyr::mutate(across(where(is.numeric), round, 1))
          
          # 2.Tax expenditures by years ------------------------------------------------------------------------
          # #       
          # te_agg<-te_summary_df%>%
          #   dplyr::select(year, `tax incentive`, `tax expenditure`)%>%
          #   dplyr:: filter(`tax incentive` %in% c("pitax"))
          # #dplyr:: filter(`tax incentive` %in% c("calc_pitax",'pit_diplomatic_consular'))
          # 
          # 
          # 
          # te_agg <- te_agg %>%
          #   mutate(`tax incentive` = recode(`tax incentive`,
          #                                   calc_pitax = "Tax_Expenditures"
          #   ))
          # 
          # 
          # 
          
          
          
# 6. Calculation of Deciles and Percentiles (ONLY FOR Big corporations )----------------------------------
      # 1.Preparation of subsets for estimation of decile and percentiles -------
      # 1.1 Big corporations ----------------------------------------------------
      # 1.1.1 Business as usual----------------------------------------------------
      
      # Preparation of subsets fo estimation of decile and percentiles
      big_corporations_CIT_BU_list_TE <- lapply(CIT_BU_list_TE, function(table) {
        table %>%
          filter(TypeOfTax == 0) #%>%
          #select(-calc_tqi_3pct, -calc_tqi_9pct, -calc_tot_amount_3pct_9pct, -calc_rent_tax, -totax, -calc_turnover_small)
      })
      
      # Estimation of decile and percentiles
      
      for (name in names(big_corporations_CIT_BU_list_TE)) {
        df <- big_corporations_CIT_BU_list_TE[[name]]
        df$decile_group <- cal_weighted_deciles_fun(df$calc_grossinc, df$weight)
        df$centile_group <- cal_weighted_centiles_fun(df$calc_grossinc, df$weight)
        big_corporations_CIT_BU_list_TE[[name]] <- df
      }
      
      # 1.1.2 Simulation ------------------------------------------------------
      
      # Preparation of subsets fo estimation of decile and percentiles
      big_corporations_CIT_SIM_list_TE <- lapply(CIT_SIM_list_TE, function(table) {
        table %>%
          filter(TypeOfTax == 0) #%>%
         # select(-calc_tqi_3pct, -calc_tqi_9pct, -calc_tot_amount_3pct_9pct, -calc_rent_tax, -totax, -calc_turnover_small)
      })
      
      # Estimation of decile and percentiles
      for (name in names(big_corporations_CIT_SIM_list_TE)) {
        df <- big_corporations_CIT_SIM_list_TE[[name]]
        df$decile_group <- cal_weighted_deciles_fun(df$calc_grossinc, df$weight)
        df$centile_group <- cal_weighted_centiles_fun(df$calc_grossinc, df$weight)
        big_corporations_CIT_SIM_list_TE[[name]] <- df
      }
      
      
      # te_decile_agg_sim<-big_corporations_CIT_SIM_list_TE%>%
      #   dplyr::select(decile,calc_cit)%>%
      #   dplyr::group_by(decile)%>%
      #   dplyr::summarise(calc_cit=sum(calc_cit,na.rm = TRUE))
      # 
    

# Extraction of deciles ---------------------------------------------------

          # testing decile
          
          
          extract_te_nace_fun <- function(cit_raw, scenario) {
            cit_raw[, .(calc_cit, decile_group,scenario = scenario)]
          }
          
          # Extract and process cit_BU_list_TE
          extracted_tables_bu <- mapply(extract_te_nace_fun, big_corporations_CIT_BU_list_TE, scenarios, SIMPLIFY = FALSE)
          combined_data_bu <- rbindlist(extracted_tables_bu)
          result_bu <- combined_data_bu[, .(total_calc_citax_bu = sum(calc_cit,na.rm = TRUE)), by = .(scenario, decile_group)]
          
          # Extract and process cit_SIM_list_TE
          
          extracted_tables_sim <- mapply(extract_te_nace_fun, big_corporations_CIT_SIM_list_TE, scenarios, SIMPLIFY = FALSE)
          combined_data_sim <- rbindlist(extracted_tables_sim)
          result_sim <- combined_data_sim[, .(total_calc_citax_sim = sum(calc_cit,na.rm = TRUE)), by = .(scenario, decile_group)]
          
          # Add year column to both results
          result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
          result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
          
          
         
          
          
          # Combine both results into one data frame
          nace_cit_summary_te_deciles <- merge(result_bu, result_sim, by = c("scenario", "year","decile_group"), all = TRUE)
          
          # Remove rows with NA in decile_group for the plot
          nace_cit_summary_te_deciles <- nace_cit_summary_te_deciles[!is.na(decile_group)]
          
          nace_cit_summary_te_deciles<-nace_cit_summary_te_deciles%>%
            dplyr::mutate(tax_expenditures=total_calc_citax_sim-total_calc_citax_bu)
          
          
          nace_cit_summary_te_deciles<-nace_cit_summary_te_deciles%>%
            select(year,decile_group,tax_expenditures)
          
          
          
          #nace_cit_summary_te[is.na(decile), decile := "Other"]
          
          # Left join
          
          # Convert NACE in data.table format
          nace_cit_summary_te_deciles<-nace_cit_summary_te_deciles%>%
            data.table()
          
          # # Perform the left join
          # nace_cit_summary_te <- left_join(nace_cit_summary_te, df_nace_names, by = c("section" = "section"))
          
          # Preparation for Chart 
          
          nace_cit_summary_te_deciles<-nace_cit_summary_te_deciles%>%
            dplyr::filter(year==SimulationYear)
          
          
          
          
      




# 7. Revenues by NACE sections (Big corporations) -----------------------------------------------------------------------
          # Function to extract columns and add scenario identifier
          extract_te_nace_fun <- function(cit_raw, scenario) {
            cit_raw[, .(calc_cit, section,description, scenario = scenario)]
          }
          
          # Extract and process cit_BU_list_TE
          extracted_tables_bu <- mapply(extract_te_nace_fun, CIT_BU_list_TE, scenarios, SIMPLIFY = FALSE)
          combined_data_bu <- rbindlist(extracted_tables_bu)
          result_bu <- combined_data_bu[, .(total_calc_citax_bu = sum(calc_cit)), by = .(scenario, section,description)]
          
          # Extract and process cit_SIM_list_TE
          
          extracted_tables_sim <- mapply(extract_te_nace_fun, CIT_SIM_list_TE, scenarios, SIMPLIFY = FALSE)
          combined_data_sim <- rbindlist(extracted_tables_sim)
          result_sim <- combined_data_sim[, .(total_calc_citax_sim = sum(calc_cit)), by = .(scenario, section,description)]
          
          # Add year column to both results
          result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
          result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
          
          # Combine both results into one data frame
          nace_cit_summary <- merge(result_bu, result_sim, by = c("scenario", "year","section"), all = TRUE)
          
          
          nace_cit_summary<-nace_cit_summary%>%
            dplyr::mutate(tax_expenditures=total_calc_citax_sim-total_calc_citax_bu)
          
          
          nace_cit_summary_te<-nace_cit_summary%>%
            select(year,section,tax_expenditures)
          
          
          # Remove rows with NA in section for the plot
          nace_cit_summary_te <- nace_cit_summary_te[!is.na(section)]
          #nace_cit_summary_te[is.na(section), section := "Other"]
          
          # Left join
          
          # Convert NACE in data.table format
          df_nace_names<-df_nace_names%>%
            data.table()
          
          # Perform the left join
          nace_cit_summary_te <- left_join(nace_cit_summary_te, df_nace_names, by = c("section" = "section"))
          
          # Preparation for Chart 
          
          nace_cit_summary_tbl<-nace_cit_summary_te%>%
            dplyr::filter(year==SimulationYear)
          
          
          nace_cit_summary_tbl$value<-"Value"

          

# 8. Distribution of CIT revenues by type of companies---------------------------
          
          
          # Function to extract columns and add scenario identifier
          extract_te_company_type_fun <- function(cit_raw, scenario) {
            cit_raw[, .(calc_cit, name_en, scenario = scenario)]
          }
          
          # Extract and process cit_BU_list_TE
          extracted_tables_bu <- mapply(extract_te_company_type_fun, CIT_BU_list_TE, scenarios, SIMPLIFY = FALSE)
          combined_data_bu <- rbindlist(extracted_tables_bu)
          result_bu <- combined_data_bu[, .(total_calc_citax_bu = sum(calc_cit)), by = .(scenario, name_en)]
          
          # Extract and process cit_SIM_list_TE
          
          extracted_tables_sim <- mapply(extract_te_company_type_fun, CIT_SIM_list_TE, scenarios, SIMPLIFY = FALSE)
          combined_data_sim <- rbindlist(extracted_tables_sim)
          result_sim <- combined_data_sim[, .(total_calc_citax_sim = sum(calc_cit)), by = .(scenario, name_en)]
          
          # Add year column to both results
          result_bu[, year := forecast_horizon[match(scenario, scenarios)]]
          result_sim[, year := forecast_horizon[match(scenario, scenarios)]]
          
          # Combine both results into one data frame
          company_type_cit_summary_te <- merge(result_bu, result_sim, by = c("scenario", "year","name_en"), all = TRUE)
          
          
          # do ovde moze da se koristi samo za prihodi a od ovde moze da se koristi za presmetka na TE
          
          company_type_cit_summary_te<-company_type_cit_summary_te%>%
            dplyr::mutate(tax_expenditures=total_calc_citax_sim-total_calc_citax_bu)
          
          
          company_type_cit_summary_te<-company_type_cit_summary_te%>%
            select(year,name_en,tax_expenditures)
          
          
          # Remove rows with NA in section for the plot
          company_type_cit_summary_te <- company_type_cit_summary_te[!is.na(name_en)]
          #nace_cit_summary_te[is.na(section), section := "Other"]
          
          # Left join
          
          # Convert NACE in data.table format
          company_type_cit_summary_te<-company_type_cit_summary_te%>%
            data.table()
          
          # # Perform the left join
          # nace_cit_summary_te <- left_join(nace_cit_summary_te, df_company_type_names, by = c("section" = "section"))
          # 
          # Preparation for Chart 
          
          company_type_cit_summary_te<-company_type_cit_summary_te%>%
            dplyr::filter(year==SimulationYear)%>%
            dplyr::filter(tax_expenditures>0)
          
          
         
                            
          

          
          
    


          