'Preparation of Aggregated Revenue Tables
'
start.time <- proc.time()
# 
# # I. Customs Revenues  --------------------------------------------


              # Function to extract columns and add scenario
              extract_TypeOfProducts_fun <- function(dt, scenario) {
                                        dt[, .(TypeOfProducts,calc_vat, scenario = scenario)]
                                      }
                                      
              

# 1. Type of Products ---------------------------------------------------

      # 1.1 BU ------------------------------------------------------------------
      
                    extracted_tables_bu <- mapply(extract_TypeOfProducts_fun, Customs_BU_list, scenarios, SIMPLIFY = FALSE)
                    combined_dt <- rbindlist(extracted_tables_bu)
                    
                    combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]%>%as.data.table()
      
                    customs_d_typeRev_bu <- combined_dt[, .(
                                 calc_vat_duties = sum(calc_vat, na.rm = TRUE)
                                  ), by = .(year, TypeOfProducts)]
                    
                    setnames(customs_d_typeRev_bu, "calc_vat_duties", "calc_vat_bu") 
      
      # 1.2 SIM -----------------------------------------------------------------
      
                    extracted_tables_sim <- mapply(extract_TypeOfProducts_fun, Customs_SIM_list, scenarios, SIMPLIFY = FALSE)
                    combined_dt <- rbindlist(extracted_tables_sim)
                    
                    combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]%>%as.data.table()
                    
                    customs_d_typeRev_sim <- combined_dt[, .(
                      calc_vat_duties = sum(calc_vat, na.rm = TRUE)
                    ), by = .(year, TypeOfProducts)]
                               
                    
                    setnames(customs_d_typeRev_sim, "calc_vat_duties", "calc_vat_sim") 
                    
              # 1.3  MERGED BU AND SIM -----------------------------------------------
              TypeRev_VAT_data <- merge(customs_d_typeRev_bu,customs_d_typeRev_sim,by = c("year", "TypeOfProducts"), all = TRUE)              
              TypeRev_VAT_data<-TypeRev_VAT_data%>%
                dplyr::rename("Baseline"="calc_vat_bu",
                              "Simulation"="calc_vat_sim"
                              )

              
              # da se dodade TypeRev_VAT_data
              
# 2. Chapter ------------------------------------------------------
       # 2.1 BU ------------------------------------------------------------------
              extract_customs_rev_chapter_fun <- function(dt, scenario) {
                                              dt[, .(Chapter,calc_vat, scenario = scenario)]
                                               }
                                            
              extracted_tables_bu <- mapply(extract_customs_rev_chapter_fun, Customs_BU_list, scenarios, SIMPLIFY = FALSE)
              combined_dt <- rbindlist(extracted_tables_bu)
              combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]%>%as.data.table()
              
              customs_d_Chapter_bu <- combined_dt[, .(
                                                    calc_vat_duties = sum(calc_vat, na.rm = TRUE)
                                                  ), by = .(year, Chapter)]
              
              setnames(customs_d_Chapter_bu, "calc_vat_duties", "calc_vat_bu")
              

       # 2.1 SIM -----------------------------------------------------------------

              extract_customs_rev_chapter_fun <- function(dt, scenario) {
                dt[, .(Chapter,calc_vat, scenario = scenario)]
              }
              
              extracted_tables_sim <- mapply(extract_customs_rev_chapter_fun, Customs_SIM_list, scenarios, SIMPLIFY = FALSE)
              combined_dt <- rbindlist(extracted_tables_sim)
              combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]%>%as.data.table( )
              
              customs_d_Chapter_sim <- combined_dt[, .(
                calc_vat_duties = sum(calc_vat, na.rm = TRUE)
              ), by = .(year, Chapter)]
              
              setnames(customs_d_Chapter_sim, "calc_vat_duties", "calc_vat_sim")            



       # 2.3  MERGED BU AND SIM -----------------------------------------------
              Chapter_VAT_data <- merge(customs_d_Chapter_bu,customs_d_Chapter_sim,by = c("year", "Chapter"), all = TRUE)
              
              Chapter_VAT_data<-left_join(Chapter_VAT_data,HS_Sections,by =c("Chapter"))
              
              
              Sections_customs_data<-Chapter_VAT_data%>%
                select(year,Sections,Section_description,calc_vat_bu,calc_vat_sim)%>%
                dplyr::group_by(year,Sections,Section_description)%>%
                dplyr::summarise(
                              Baseline =sum(calc_vat_bu ,na.rm = TRUE),
                              Simulation =sum(calc_vat_sim ,na.rm = TRUE)
                )
              
              
              
              Chapter_VAT_data_agg<-Chapter_VAT_data%>%
                select(year,Chapter,Chapters_description,calc_vat_bu,calc_vat_sim)%>%
                dplyr::group_by(year,Chapter,Chapters_description)%>%
                dplyr::summarise(
                  Baseline =sum(calc_vat_bu ,na.rm = TRUE),
                  Simulation =sum(calc_vat_sim ,na.rm = TRUE)
                )
              
              
              # ova Chapter_VAT_data_agg
              
# 3. MTN ------------------------------------------------------------------
      # 3.1 BU --------------------------------------------------------
              extract_customs_rev_MTN_fun <- function(dt, scenario) {
                                                                      dt[, .(Six_digit,calc_vat, scenario = scenario)]
                                                                    }
              extracted_tables_bu <- mapply(extract_customs_rev_MTN_fun, Customs_BU_list, scenarios, SIMPLIFY = FALSE)
              combined_dt <- rbindlist(extracted_tables_bu)
              combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]%>%as.data.table( )
              
              customs_d_MTN_bu <- combined_dt[, .(
                calc_vat_duties = sum(calc_vat, na.rm = TRUE)
              ), by = .(year, Six_digit)]
              
              setnames(customs_d_MTN_bu, "calc_vat_duties", "calc_vat_bu")
              

      # 3.2 SIM -----------------------------------------------------------------

              
              extracted_tables_sim <- mapply(extract_customs_rev_MTN_fun, Customs_SIM_list, scenarios, SIMPLIFY = FALSE)
              combined_dt <- rbindlist(extracted_tables_sim)
              combined_dt[, year := forecast_horizon[match(scenario, scenarios)]]%>%as.data.table( )
              
              customs_d_MTN_sim <- combined_dt[, .(
                calc_vat_duties = sum(calc_vat, na.rm = TRUE)
              ), by = .(year, Six_digit)]
              
              setnames(customs_d_MTN_sim, "calc_vat_duties", "calc_vat_sim")         
              

      # 3.3 MERGED BU AND SIM ----------------------------------------------------

              MTN_VAT_data <- merge(customs_d_MTN_bu,customs_d_MTN_sim,by = c("year", "Six_digit"), all = TRUE)

             
              MTN_VAT_data_agg<-MTN_VAT_data%>%
                dplyr::group_by(year ,Six_digit)%>%
                dplyr::summarise(calc_vat_bu =sum(calc_vat_bu ,na.rm = TRUE),
                                 calc_vat_sim =sum(calc_vat_sim ,na.rm = TRUE)
                                 )
              
              
              
              MTN_VAT_data_agg$Six_digit <- substr(MTN_VAT_data_agg$Six_digit, 1, 4) %>% 
                                                          paste(substr(MTN_VAT_data_agg$Six_digit, 5, 6))
              
              
              
              # New
              WTO_MTN_subset <- WTO_MTN %>%
                              filter(
                                (SimulationYear >= 2022 & str_detect(HS_year, ">2022")) |
                                  (SimulationYear < 2022 & str_detect(HS_year, "<2022"))
                              )
              

              MTN_VAT_data_agg<-left_join(MTN_VAT_data_agg,WTO_MTN_subset,by =c("Six_digit"))

              MTN_VAT_data_agg <- distinct(MTN_VAT_data_agg)
              
              MTN_VAT_data_agg<-MTN_VAT_data_agg%>%
                dplyr::group_by(year,Product_group)%>%
                dplyr::summarise(
                  Baseline =sum(calc_vat_bu ,na.rm = TRUE),
                  Simulation =sum(calc_vat_sim ,na.rm = TRUE)
                )
              
              MTN_VAT_data_agg$Product_group <- ifelse(is.na( MTN_VAT_data_agg$Product_group), "Other",  MTN_VAT_data_agg$Product_group)

              MTN_VAT_data_agg$Product_group <- factor( MTN_VAT_data_agg$Product_group)
              
# -------------------------------------------------------------------------

              end.time <- proc.time()
              save.time <- end.time - start.time
              cat("\n Number of minutes running-Customs Revenues:", save.time[3] / 60, "\n \n")