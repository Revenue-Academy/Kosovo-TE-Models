library(plotly)
library(stringr)
library(reshape2)
library(base64enc)
library(plyr)


# I.  Define function for calculation of customs duties-------------------------------------------------------
        calc_customs_duties_fun <- function(customs_subset, simulationSlider_Customs_1_t, simulationSlider_Customs_2_t, BenchmarkCustomsRate) {
  customs_subset_simulation <- customs_subset %>%
                    dplyr::mutate(
                      calc_customs_value = customsValue * simulationSlider_Customs_1_t,
                      calc_customs_duties = calc_customs_value * (CustomsRate / 100),
                      calc_quantity = quantity * simulationSlider_Customs_2_t,
                      calc_specific_duties = calc_quantity * SpecificRate * 61.5,
                      calc_customs_and_specific = calc_customs_duties + calc_specific_duties,
                      calc_max_duties = customsValue * (MaximumRate / 100),
                      calc_total_customs_duties = ifelse(calc_max_duties > calc_customs_and_specific, calc_max_duties, calc_customs_and_specific),
                      calc_total_customs_duties_benchmark = calc_customs_value*(BenchmarkCustomsRate / 100) , # Estimation of TE
                      cacl_tax_expenditures=calc_total_customs_duties_benchmark-calc_total_customs_duties,
                      calc_effective_vat_rate=vat_import/(customsValue+customs_and_specific+excise_import+fees+vat_import),
                      calc_total_vat=(calc_customs_value+calc_customs_and_specific+fees+excise_import)*calc_effective_vat_rate,
                      
    )
  return(customs_subset_simulation)
}

# II. Simulation with iteration  -----------------------------------------
        base_year <- unique(customs_subset_raw$year)[1]  # Assuming there is only one unique base year
        end_year <- base_year+4
        simulation_year <- SimulationYear # Year from slider 
        forecast_horizon <- seq(base_year, end_year)
        # Define the base year and the end year
      
        CUSTOMS_DUTIES_BU_list <- list()
        CUSTOMS_DUTIES_SIM_list <- list()

# Default scenario
          # t0
          simulationSlider_Customs_1_t0 <- 1
          simulationSlider_Customs_2_t0 <- 1

            # Define the scenarios
            scenarios <- c("t0", "t1", "t2", "t3", "t4")
            
            # Function to determine which dataset to use based on simulation_year
            get_dataset <- function(scenario_index, simulation_year, base_year) {
              year_diff <- simulation_year - base_year
              if (year_diff <= 0) {
                return(customs_subset_raw)
              } else if (scenario_index < year_diff) {
                return(customs_subset_raw)
              } else {
                return(customs_subset_updated)
              }
            }
            
            # Loop through each scenario
            for (scenario_index in seq_along(scenarios)) {
              scenario <- scenarios[scenario_index]
              
              # Check if the variables exist before assigning
              for (i in 1:2) {  # Only loop through the defined indices
                var_name <- paste0("simulationSlider_Customs_", i, "_", scenario)
                if (exists(var_name)) {
                  assign(paste0("simulationSlider_Customs_", i, "_t"), get(var_name))
                } else {
                  assign(paste0("simulationSlider_Customs_", i, "_t"), 1)  # Default value
                  warning(paste("Variable", var_name, "does not exist. Using default value of 1"))
                }
              }
              
              # Call the function for CUSTOMS_DUTIES_BU using customs_subset_raw
              CUSTOMS_DUTIES_BU <- calc_customs_duties_fun(
                customs_subset_raw,
                get(paste0("simulationSlider_Customs_", 1, "_t")), # <--Simulacijata se pravi so ovie parametri koi se menuvaat so sekoja iteracija i gi povikuvaat vrednostite od sliderot !!!
                get(paste0("simulationSlider_Customs_", 2, "_t")),
                BenchmarkCustomsRate
              )
              
              # Determine which dataset to use for CUSTOMS_DUTIES_SIM
              dataset <- get_dataset(scenario_index - 1, simulation_year, base_year)  # Adjust index for 0-based
              
              # Call the function for CUSTOMS_DUTIES_SIM
              CUSTOMS_DUTIES_SIM <- calc_customs_duties_fun(
                dataset,
                get(paste0("simulationSlider_Customs_", 1, "_t")),
                get(paste0("simulationSlider_Customs_", 2, "_t")),
                BenchmarkCustomsRate
              )
              
              # Store the results in the lists
              CUSTOMS_DUTIES_BU_list[[scenario]] <- CUSTOMS_DUTIES_BU
              CUSTOMS_DUTIES_SIM_list[[scenario]] <- CUSTOMS_DUTIES_SIM
            }
            
            print("Simulation is Done!")


# III. Preparing tables  --------------------------------------------
            # Function to sum the specified columns in the list and store the results in a data frame
            summarize_customs_duties <- function(customs_duties_list, suffix) {
              result <- data.frame(
                scenarios = character(),
                customs_value = numeric(),
                total_customs_duties = numeric(),
                total_customs_duties_benchmark = numeric(),
                tax_expenditures=numeric(),
                total_vat = numeric(),
                specific_duties = numeric(),
                stringsAsFactors = FALSE
              )
              
              for (scenario in names(customs_duties_list)) {
                df <- customs_duties_list[[scenario]]
                customs_value_sum <- sum(df$calc_customs_value, na.rm = TRUE)
                total_customs_duties_sum <- sum(df$calc_total_customs_duties, na.rm = TRUE)
                total_customs_duties_benchmark_sum <- sum(df$calc_total_customs_duties_benchmark, na.rm = TRUE)
                total_vat_sum <- sum(df$calc_total_vat, na.rm = TRUE)
                total_tax_expenditures_sum <- sum(df$cacl_tax_expenditures, na.rm = TRUE)
                specific_duties_sum <- sum(df$calc_specific_duties, na.rm = TRUE)
                
                result <- rbind(result, data.frame(
                  scenarios = scenario,
                  customs_value = customs_value_sum,
                  total_customs_duties = total_customs_duties_sum,
                  total_customs_duties_benchmark = total_customs_duties_benchmark_sum,
                  total_vat = total_vat_sum,
                  specific_duties = specific_duties_sum,
                  tax_expenditures=total_tax_expenditures_sum,
                  stringsAsFactors = FALSE
                ))
              }
              
              # Add the suffix to the column names
              names(result)[-1] <- paste0(names(result)[-1], suffix)
              
              return(result)
            }
            
            # Summarize the CUSTOMS_DUTIES_SIM_list and CUSTOMS_DUTIES_BU_list
            summary_SIM <- summarize_customs_duties(CUSTOMS_DUTIES_SIM_list, "_sim")
            summary_BU <- summarize_customs_duties(CUSTOMS_DUTIES_BU_list, "_bu")
            
            # Merge the two summary tables into one
            merged_customs_BU_SIM <- merge(summary_BU, summary_SIM, by = "scenarios")
            
            # Add the year column
            merged_customs_BU_SIM$year <- as.character (forecast_horizon)
            
            # Reorder columns to have year as the first column
            merged_customs_BU_SIM <- merged_customs_BU_SIM[, c("year", names(merged_customs_BU_SIM)[-length(merged_customs_BU_SIM)])]
            
            # Divide only numeric columns by 1e06
            # numeric_columns <- sapply(merged_customs_BU_SIM, is.numeric)
            # merged_customs_BU_SIM[numeric_columns] <- merged_customs_BU_SIM[numeric_columns] / 1e06
            
            # Print the merged summary with the year column
            print(merged_customs_BU_SIM)
            
   

        # Chart 1. Comparison of Customs Revenues -----------------------------------------------------------------
        
                    customsRevenuesTotal_plt<- plot_ly(
                                                    merged_customs_BU_SIM, 
                                                    x = ~year, 
                                                    y = ~total_customs_duties_bu, 
                                                    name = "Baseline", 
                                                    type = 'scatter', 
                                                    mode = 'lines',
                                                    line = list(width = 4, dash = "solid") 
                                                  ) %>%
                                                    add_trace(
                                                      x = ~year, 
                                                      y = ~total_customs_duties_sim, 
                                                      name = 'Simulation ',
                                                      line = list(width = 4, dash = "dot")  
                                                    ) %>%
                                                    layout(
                                                      title = paste("Total Customs Revenues,", min(forecast_horizon),"-",max(forecast_horizon)),
                                                      xaxis = list(title = '', tickformat = 'd'),
                                                      yaxis = list(title = ' ', rangemode = 'tozero'), 
                                                      annotations = list(
                                                        x = 0,
                                                        y = -0.07,
                                                        text = "Source: WB staff estimation",
                                                        showarrow = F,
                                                        xref = 'paper',
                                                        yref = 'paper',
                                                        align = 'left'
                                                      )
                                                    )
                                     # customsRevenuesTotal_plt
        
        
        # Chart 2. Comparison of Specific  Customs Duties ----------------------------------------------------------------------
        
                    customsRevenuesSpecific_plt<- plot_ly(
                                    merged_customs_BU_SIM, 
                                    x = ~year, 
                                    y = ~specific_duties_bu, 
                                    name = "Baseline", 
                                    type = 'scatter', 
                                    mode = 'lines',
                                    line = list(width = 4, dash = "solid") 
                                  ) %>%
                                    add_trace(
                                      x = ~year, 
                                      y = ~specific_duties_sim, 
                                      name = 'Simulation ',
                                      line = list(width = 4, dash = "dot")  
                                    ) %>%
                                    layout(
                                      title = paste("Total Specific Customs Revenues,", min(forecast_horizon),"-",max(forecast_horizon)),
                                      xaxis = list(title = '', tickformat = 'd'),
                                      yaxis = list(title = ' ', rangemode = 'tozero'), 
                                      annotations = list(
                                        x = 0,
                                        #y = -0.05,
                                        y = -0.07,
                                        text = "Source: WB staff estimation",
                                        showarrow = F,
                                        xref = 'paper',
                                        yref = 'paper',
                                        align = 'left'
                                      )
                                    )
                    #customsRevenuesSpecific_plt
          
        # Chart 3. Comparison of VAT revenues ----------------------------
        
                    vatRevenues_plt<- plot_ly(
                                  merged_customs_BU_SIM, 
                                  x = ~year, 
                                  y = ~total_vat_bu, 
                                  name = "Baseline", 
                                  type = 'scatter', 
                                  mode = 'lines',
                                  line = list(width = 4, dash = "solid") 
                                ) %>%
                                  add_trace(
                                    x = ~year, 
                                    y = ~total_vat_sim, 
                                    name = 'Simulation ',
                                    line = list(width = 4, dash = "dot")  
                                  ) %>%
                                  layout(
                                    title = paste("Total VAT Revenues from Import,", min(forecast_horizon),"-",max(forecast_horizon)),
                                    xaxis = list(title = '', tickformat = 'd'),
                                    yaxis = list(title = ' ', rangemode = 'tozero'), 
                                    annotations = list(
                                      x = 0,
                                      y = -0.07,
                                      #y = -0.05,
                                      text = "Source: WB staff estimation",
                                      showarrow = F,
                                      xref = 'paper',
                                      yref = 'paper',
                                      align = 'left'
                                    )
                                  )
                    vatRevenues_plt
                    
        
                    
        # Chart 4. Tax Expenditures --------------------------------------------------------
        
                    # taxExpenditures_plt<- plot_ly(
                    #                     merged_customs_BU_SIM, 
                    #                     x = ~year, 
                    #                     y = ~tax_expenditures_bu, 
                    #                     name = "Baseline", 
                    #                     type = 'scatter', 
                    #                     mode = 'lines',
                    #                     line = list(width = 4, dash = "solid") 
                    #                   ) %>%
                    #                     add_trace(
                    #                       x = ~year, 
                    #                       y = ~tax_expenditures_sim, 
                    #                       name = 'Simulation ',
                    #                       line = list(width = 4, dash = "dot")  
                    #                     ) %>%
                    #                     layout(
                    #                       title = paste("Tax Expenditures,", min(forecast_horizon),"-",max(forecast_horizon)),
                    #                       xaxis = list(title = '', tickformat = 'd'),
                    #                       yaxis = list(title = ' ', rangemode = 'tozero'), 
                    #                       annotations = list(
                    #                         x = 0,
                    #                         #y = -0.05,
                    #                         y = -0.08,
                    #                         text = "Source: WB staff estimation",
                    #                         showarrow = F,
                    #                         xref = 'paper',
                    #                         yref = 'paper',
                    #                         align = 'left'
                    #                       )
                    #                     )
                    # taxExpenditures_plt
                    
                    #
                    
                    taxExpenditures_plt <- plot_ly(
                      merged_customs_BU_SIM, 
                      x = ~year, 
                      y = ~tax_expenditures_bu, 
                      name = "Baseline", 
                      type = 'bar'
                    ) %>%
                      add_trace(
                        x = ~year, 
                        y = ~tax_expenditures_sim, 
                        name = 'Simulation',
                        type = 'bar'
                      ) %>%
                      layout(
                        title = paste("Tax Expenditures,", min(forecast_horizon), "-", max(forecast_horizon)),
                        xaxis = list(title = '', tickformat = 'd'),
                        yaxis = list(title = ' ', rangemode = 'tozero'), 
                        annotations = list(
                          x = 0,
                          y = -0.07,
                          text = "Source: WB staff estimation",
                          showarrow = F,
                          xref = 'paper',
                          yref = 'paper',
                          align = 'left'
                        ),
                        barmode = 'group'  # This will display bars side by side
                      )
                    taxExpenditures_plt
                    #
        
                    

# IV. Customs grouped -----------------------------------------------------

            # Function to sum the specified columns in the list and store the results in a data frame, grouped by Product_group
            summarize_customs_duties_grouped <- function(customs_duties_list, suffix) {
              result <- data.frame(
                scenarios = character(),
                customs_value = numeric(),
                Product_group = character(),
                total_customs_duties = numeric(),
                total_customs_duties_benchmark = numeric(),
                tax_expenditures = numeric(),
                total_vat = numeric(),
                specific_duties = numeric(),
                stringsAsFactors = FALSE
              )
              
              for (scenario in names(customs_duties_list)) {
                df <- customs_duties_list[[scenario]]
                
                # Group by Product_group and summarize the required columns
                grouped_df <- df %>%
                  group_by(Product_group) %>%
                  summarise(
                    customs_value = sum(calc_customs_value, na.rm = TRUE),
                    total_customs_duties = sum(calc_total_customs_duties, na.rm = TRUE),
                    total_customs_duties_benchmark = sum(calc_total_customs_duties_benchmark, na.rm = TRUE),
                    tax_expenditures = sum(cacl_tax_expenditures, na.rm = TRUE),
                    total_vat = sum(calc_total_vat, na.rm = TRUE),
                    specific_duties = sum(calc_specific_duties, na.rm = TRUE)
                  ) %>%
                  mutate(scenarios = scenario)
                
                # Bind the grouped data frame to the result
                result <- rbind(result, grouped_df)
              }
              
              # Add the suffix to the column names except scenarios and Product_group
              names(result)[-c(1, 1)] <- paste0(names(result)[-c(1, 1)], suffix)
              
              return(result)
            }
            
            # Summarize the CUSTOMS_DUTIES_SIM_list and CUSTOMS_DUTIES_BU_list with grouping
            summary_SIM_grouped <- summarize_customs_duties_grouped(CUSTOMS_DUTIES_SIM_list, "_sim")
            summary_BU_grouped <- summarize_customs_duties_grouped(CUSTOMS_DUTIES_BU_list, "_bu")
            
            # Rename the scenarios column to match for merging
            names(summary_SIM_grouped)[names(summary_SIM_grouped) == "scenarios_sim"] <- "scenarios"
            names(summary_BU_grouped)[names(summary_BU_grouped) == "scenarios_bu"] <- "scenarios"
            
            # Merge the two summary tables into one
            merged_customs_BU_SIM_grouped <- merge(summary_BU_grouped, summary_SIM_grouped, by = c("scenarios", "Product_group"))
            
            # Add the year column
            merged_customs_BU_SIM_grouped$year <- rep(forecast_horizon, each = nrow(merged_customs_BU_SIM_grouped) / length(forecast_horizon))
            
            # Reorder columns to have year as the first column
            merged_customs_BU_SIM_grouped <- merged_customs_BU_SIM_grouped %>% 
              relocate(year, .before = scenarios)
            
            # Print the merged summary with the year column
            print(merged_customs_BU_SIM_grouped)
            
            

# Chart -------------------------------------------------------------------

            
            CustomsDuties_TE_MTN<-merged_customs_BU_SIM_grouped%>%
              dplyr::group_by(year,Product_group,tax_expenditures_bu,tax_expenditures_sim)%>%
              dplyr::summarise(tax_expenditures_bu=sum(tax_expenditures_bu,na.rm = TRUE),
                               tax_expenditures_sim=sum(tax_expenditures_sim,na.rm = TRUE),
                               )
            
            CustomsDuties_TE_MTN$Product_group <- ifelse(is.na( CustomsDuties_TE_MTN$Product_group), "Other",  CustomsDuties_TE_MTN$Product_group)
            
            
            
            # Factor the "Sections" column without ordering other strings
            CustomsDuties_TE_MTN$Product_group <- factor( CustomsDuties_TE_MTN$Product_group)
            
            
            CustomsDuties_TE_MTN<-CustomsDuties_TE_MTN%>%
              filter(year==2019)%>%
              select(-c(year))
            
            ProductGroups_MTN <- plot_ly(
                                          CustomsDuties_TE_MTN, 
                                          y = ~Product_group,#reorder(Product_group), 
                                          x = ~tax_expenditures_sim, 
                                          type = 'bar', 
                                          text = ' ', 
                                          hoverinfo = 'x+text',
                                          hovertext = ~Product_group,
                                          marker = list(color = '#d62728')
                                        ) %>%
              layout(
                title = paste("Tax expenditures by Multilateral Trade Negotiations Categories (in LCU),", base_year),
                xaxis = list(title = ""),
                yaxis = list(title = "")
              )
            
            ProductGroups_MTN
            