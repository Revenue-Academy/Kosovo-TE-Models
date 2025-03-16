'Distribution tables'


# I.FUNCTIONS ---------------------------------------------------------------

        # Function to extract columns and add scenario
        extract_centile_rev_fun <- function(cit_raw, scenario) {
          cit_raw[, .(centile_group,calc_cit,calc_grossinc, scenario = scenario)]
        }

        # Function to extract columns and add scenario
        extract_dec_rev_fun <- function(cit_raw, scenario) {
          cit_raw[, .(decile_group,calc_cit,calc_grossinc, scenario = scenario)]
        }

        # Function to extract columns and add scenario
        extract_bins_rev_fun <- function(cit_raw, scenario) {
          cit_raw[, .(id_n,weight,calc_grossinc,calc_cit, scenario = scenario)]
        }
        
# II.ESTIMATIONS OF PERCENTILE AND DECILE ---------------------------------
      # 1.Centile Groups ---------------------------------------------------------
          # 1.1 BU --------------------------------------------------------------------
                       extracted_dist_tables_bu <- mapply(extract_centile_rev_fun, big_corporations_CIT_BU_list, scenarios, SIMPLIFY = FALSE)
                        combined_dt <- rbindlist(extracted_dist_tables_bu)
                        
                        
                        combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
                        
                        combined_dt<-combined_dt%>%
                          filter(year==SimulationYear)
                        
                        
                        # # # Convert your dataset to a data.table
                         combined_dt <- as.data.table(combined_dt)
      
                        # Perform the required operations
                        cit_centile_distribution_bu <- combined_dt[, .(
                                                                            # sum_calc_citax = sum(calc_cit, na.rm = TRUE),
                                                                            # sum_total_gross_income = sum(calc_grossinc, na.rm = TRUE)
                                                                            
                                                                            sum_calc_citax = sum(calc_cit, na.rm = TRUE),
                                                                            sum_total_gross_income = sum(calc_grossinc, na.rm = TRUE)
                                                                          ), by = .(scenario, centile_group)]
      
                        # Calculate ETR
                        cit_centile_distribution_bu[, etr := sum_calc_citax / sum_total_gross_income]
                        cit_centile_distribution_bu[, year := forecast_horizon[match(scenario, scenarios)]]
                        setorder(cit_centile_distribution_bu, centile_group)
                  
                        
          # 1.2 SIM -------------------------------------------------------------------
                        extracted_dist_tables_sim <- mapply(extract_centile_rev_fun, big_corporations_CIT_SIM_list, scenarios, SIMPLIFY = FALSE)
                        combined_dt <- rbindlist(extracted_dist_tables_sim)
                       
                        combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
                        combined_dt<-combined_dt%>%
                          filter(year==SimulationYear)
                        
                        
                        
                        combined_dt <- as.data.table(combined_dt)
      
                        cit_centile_distribution_sim <- combined_dt[, .(
                          sum_calc_citax = sum(calc_cit, na.rm = TRUE),
                          sum_total_gross_income = sum(calc_grossinc, na.rm = TRUE)
                        ), by = .(scenario, centile_group)]
      
                        # Calculate ETR
                        cit_centile_distribution_sim[, etr := sum_calc_citax / sum_total_gross_income]
                        cit_centile_distribution_sim[, year := forecast_horizon[match(scenario, scenarios)]]
                        setorder(cit_centile_distribution_sim, centile_group)
                        
                     
      
          # 1.3 MERGE BU AND SIM -----------------------------------------------------------------
                        setkey(cit_centile_distribution_bu, centile_group, scenario, year)
                        setkey(cit_centile_distribution_sim, centile_group, scenario, year)
                        
                        cit_centile_distribution_bu_sim <- merge(cit_centile_distribution_bu, cit_centile_distribution_sim, by = c("centile_group", "scenario", "year"), suffixes = c("_bu", "_sim"))
                        setorder(cit_centile_distribution_bu_sim, centile_group)
      
          # 1.3 Chart -------------------------------------------------------------------
                      # cit_centile_distribution_bu_sub<-cit_centile_distribution_bu_sim%>%
                      #                               filter(year==SimulationYear)
                      # 
            
      # 2.Decile Groups ---------------------------------------------------------
          # 1.BU --------------------------------------------------------------------
                       extracted_dist_tables_bu <- mapply(extract_dec_rev_fun, big_corporations_CIT_BU_list, scenarios, SIMPLIFY = FALSE)
                        combined_dt <- rbindlist(extracted_dist_tables_bu)
                       
                        combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
                        
                        combined_dt<-combined_dt%>%
                          filter(year==SimulationYear)
                   
                        
                        cit_decile_distribution_bu <- combined_dt[, .(
                                        sum_calc_citax = sum(calc_cit, na.rm = TRUE),
                                        mean_calc_citax = mean(calc_cit, na.rm = TRUE),
                                        sum_total_gross_income = sum(calc_grossinc, na.rm = TRUE)
                        ), by = .(scenario, decile_group)]
                        
                     
                        cit_decile_distribution_bu[, year := forecast_horizon[match(scenario, scenarios)]]
                        
                        
                        setorder(cit_decile_distribution_bu, decile_group )
                        
          # 2.SIM -------------------------------------------------------------------
                        extracted_dist_tables_sim <- mapply(extract_dec_rev_fun, big_corporations_CIT_SIM_list, scenarios, SIMPLIFY = FALSE)
                        combined_dt <- rbindlist(extracted_dist_tables_sim)
                      
                        combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]
                        
                        combined_dt<-combined_dt%>%
                          filter(year==SimulationYear)
                        
                        
                        
                        
                        #setorder(combined_dt, decile_group)
                        
                        cit_decile_distribution_sim <- combined_dt[, .(
                                                      sum_calc_citax = sum(calc_cit, na.rm = TRUE),
                                                      mean_calc_citax = mean(calc_cit, na.rm = TRUE),
                                                      sum_total_gross_income = sum(calc_grossinc, na.rm = TRUE)
                                                    ), by = .(scenario, decile_group)]
                        
                        # Calculate ETR
                        #cit_decile_distribution_sim[, etr := sum_calc_citax / sum_total_gross_income]
                        
                        cit_decile_distribution_sim[, year := forecast_horizon[match(scenario, scenarios)]]
                        
                        
                        setorder(cit_decile_distribution_sim, decile_group )
                        
                        
          # 3.MERGE BU AND SIM -----------------------------------------------------------------
                        setkey(cit_decile_distribution_bu, decile_group, scenario, year)
                        setkey(cit_decile_distribution_sim, decile_group, scenario, year)
                        cit_decile_distribution_bu_sim_raw <- merge(cit_decile_distribution_bu, cit_decile_distribution_sim, by = c("decile_group", "scenario", "year"), suffixes = c("_bu", "_sim"))
                        
                        cit_decile_distribution_bu_sim<-cit_decile_distribution_bu_sim_raw
      
                        cit_decile_distribution_bu_sim$year<-as.character(cit_decile_distribution_bu_sim$year)
                        cit_decile_distribution_bu_sim$decile_group<-as.character(cit_decile_distribution_bu_sim$decile_group)
                        
                        cit_decile_distribution_bu_sim<-setnames(cit_decile_distribution_bu_sim,
                                 old = c('decile_group','sum_calc_citax_bu', 'mean_calc_citax_bu', 'sum_total_gross_income_bu','sum_calc_citax_sim', 'mean_calc_citax_sim', 'sum_total_gross_income_sim'),
                                 new = c( 'Decile groups', 'Total CIT liability (business as usual)', 'Average CIT liability (business as usual)', 'Total gross income (business as usual)',
                                         'Total CIT liability (simulation)', 'Average CIT liability (simulation)', 'Total gross income (simulation)'
                                 ))
      
      
                        
      
                       
                        # cit_decile_distribution_bu_sim <- cit_decile_distribution_bu_sim %>%
                        #   mutate_if(is.numeric, ~ round(. / 1e06, 1))
      
                        
                        # cit_decile_distribution_bu_sim <- cit_decile_distribution_bu_sim %>%
                        #   mutate(across(
                        #     .cols = where(is.numeric) & !starts_with("Average CIT liability (business as usual)") & 
                        #       !starts_with("Average CIT liability (simulation)"),
                        #     .fns = ~ round(. / 1e06, 1)
                        #   ))
                          # mutate(across(
                          #   .cols = where(is.numeric) & (starts_with("Average CIT liability (business as usual)") | 
                          #                                  starts_with("Average CIT liability (simulation)")),
                          #   .fns = ~ round(. / 1000, 1)
                          # ))
                          # mutate(across(
                          #   .cols = (starts_with("Total")),
                          #                                  
                          #   .fns = ~ round(. /  1e06,, 1)
                          # ))
                          
                        # 
                        # cols_to_divide <- c("Total CIT liability (business as usual)", 
                        #                     "Average CIT liability (business as usual)", 
                        #                     "Total gross income (business as usual)", 
                        #                     "Total CIT liability (simulation)", 
                        #                     "Average CIT liability (simulation)", 
                        #                     "Total gross income (simulation)")
                        # 
                        
                        cols_to_divide <- c("Total CIT liability (business as usual)", 
                                            #"Average CIT liability (business as usual)", 
                                            "Total gross income (business as usual)", 
                                            "Total CIT liability (simulation)", 
                                            #"Average CIT liability (simulation)", 
                                            "Total gross income (simulation)")

                        cit_decile_distribution_bu_sim[, (cols_to_divide) := lapply(.SD, function(x) round(x / 1e6, 1)), .SDcols = cols_to_divide]
                        
                        
                        cols_to_divide <- c(#"Total CIT liability (business as usual)", 
                                            "Average CIT liability (business as usual)", 
                                            #"Total gross income (business as usual)", 
                                            #"Total CIT liability (simulation)", 
                                            "Average CIT liability (simulation)"
                                            #"Total gross income (simulation)"
                                            )
                        
                        cit_decile_distribution_bu_sim[, (cols_to_divide) := lapply(.SD, function(x) round(x / 1e3, 1)), .SDcols = cols_to_divide]
                        
                          
                        
                        
                        cit_decile_distribution_bu_sim<-setnames(cit_decile_distribution_bu_sim,
                                                                 old = c('Decile groups', 'Total CIT liability (business as usual)', 'Average CIT liability (business as usual)', 'Total gross income (business as usual)',
                                                                         'Total CIT liability (simulation)', 'Average CIT liability (simulation)', 'Total gross income (simulation)'),
                                                                 new = c( 'Decile groups', 'Total CIT liability (business as usual) in MIL', 'Average CIT liability (business as usual) in THOUSAND', 'Total gross income (business as usual) in MIL',
                                                                          'Total CIT liability (simulation) in MIL', 'Average CIT liability (simulation) in THOUSAND', 'Total gross income (simulation) in MIL'
                                                                 ))
                        
                        
                        
                        setorder(cit_decile_distribution_bu_sim, year)
                        
                        cit_decile_distribution_bu_sim<-cit_decile_distribution_bu_sim%>%
                          filter(year==SimulationYear)
      
                        cit_decile_distribution_bu_sim$scenario<-NULL
                        cit_decile_distribution_bu_sim$year<-NULL
                        
      # 3. Chart -------------------------------------------------------------------
                      # cit_decile_distribution_bu_sub<-cit_decile_distribution_bu_sim%>%
                      #                               filter(year==SimulationYear)
                    
                        
# II.CIT Distribution Table Income Breaks -----------------------------------------------------------
    # 1.BU ----------------------------------------------------------------------
            
                      # Define the breakpoints and labels
                      breaks <- c(-Inf, 0, 1, 0.5e6, 1e6, 1.5e6, 2e6, 3e6, 4e6, 5e6, 10e6, Inf)
                      labels <- c("<0", "0", "0-0.5m", "0.5-1m", "1-1.5m", "1.5-2m", "2-3m", "3-4m", "4-5m", "5-10m", ">10m")
                      
        # Apply the transformations across all scenarios in CIT_BU_list
                      combined_dt_bins_fun <- rbindlist(lapply(names(big_corporations_CIT_BU_list), function(scenario) {
                        # Extract the data frame for each scenario
                        data <- big_corporations_CIT_BU_list[[scenario]] %>%
                          select(id_n, weight, calc_grossinc, calc_cit) %>%
                          mutate(
                            weight_g = weight * calc_grossinc,
                            bin_group = cut(calc_grossinc, breaks = breaks, labels = labels, right = FALSE)
                          ) %>%
                          # Add the scenario identifier to the data frame
                          mutate(scenario = scenario)
                        
                        # Convert to data.table for efficient operations
                        as.data.table(data)
                      }))
                      
                      # Calculate the sum of calc_cit for each bin_group and scenario
                      cit_result_bins <- combined_dt_bins_fun[, .(sum_calc_citax = sum(calc_cit)), by = .(bin_group, scenario)]
                      
                      # Calculate the sum for the "ALL" category for each scenario
                      all_scenarios <- combined_dt_bins_fun[, .(bin_group = "ALL", sum_calc_citax = sum(calc_cit)), by = scenario]
                      
                      # Combine the results with the "ALL" category
                      cit_result_bins_bu <- rbind(cit_result_bins, all_scenarios, fill = TRUE)
                      
                      # Add the year column using the forecast_horizon vector
                      cit_result_bins_bu[, year := forecast_horizon[match(scenario, scenarios)]]
                      
          
    # Chart -------------------------------------------------------------------
    
            cit_result_bins_bu_sub <- cit_result_bins_bu %>%
                      filter(year == SimulationYear) %>%
                      filter(bin_group != "ALL" & bin_group != "0")
        
    # 2.SIM -------------------------------------------------------------------
            
                      combined_dt_bins_fun <- rbindlist(lapply(names(big_corporations_CIT_SIM_list), function(scenario) {
                        # Extract the data frame for each scenario
                        data <- big_corporations_CIT_SIM_list[[scenario]] %>%
                          select(id_n, weight, calc_grossinc, calc_cit) %>%
                          mutate(
                            weight_g = weight * calc_grossinc,
                            bin_group = cut(calc_grossinc, breaks = breaks, labels = labels, right = FALSE)
                          ) %>%
                          # Add the scenario identifier to the data frame
                          mutate(scenario = scenario)
                        
                        # Convert to data.table for efficient operations
                        as.data.table(data)
                      }))
                      
                      # Calculate the sum of calc_cit for each bin_group and scenario
                      cit_result_bins <- combined_dt_bins_fun[, .(sum_calc_citax = sum(calc_cit)), by = .(bin_group, scenario)]
                      
                      # Calculate the sum for the "ALL" category for each scenario
                      all_scenarios <- combined_dt_bins_fun[, .(bin_group = "ALL", sum_calc_citax = sum(calc_cit)), by = scenario]
                      
                      # Combine the results with the "ALL" category
                      cit_result_bins_sim <- rbind(cit_result_bins, all_scenarios, fill = TRUE)
                      
                      # Add the year column using the forecast_horizon vector
                      cit_result_bins_sim[, year := forecast_horizon[match(scenario, scenarios)]]
                      
        
    
    # Chart -------------------------------------------------------------------
    
            cit_result_bins_sim_sub <- cit_result_bins_sim %>%
              filter(year == SimulationYear) %>%
              filter(bin_group != "ALL" & bin_group != "0")%>%
              select(-c(scenario,year))
            
