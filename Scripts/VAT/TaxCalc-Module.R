'
TAX CALCULATOR-MODULE
'
options(scipen=999)

#conversion_factor=1e06
conversion_factor=1000

              # 1. SIMULATION ----
                        # 1.1 Pre-processing COICOP and setting parameters for simulation ------------------------------------------
                              # 1.1.0 Estimation of Calibration factor -----------------------------------------------------
                            '
                                  Warning
                                  This part begins with copy-paste of the parameters entered previously and from this point begins with their new calculation with the aim of obtaining TE
                                  '
    
                            SIMULATION_0 = copy(SIMULATION_CALIBRATION_FACTOR)
    
                            SIMULATION_0 <- SIMULATION_0 %>%
                              dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                                            Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                                            Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
                            
suppressMessages({                         
                              # 1.1.1 Aggregate table ----------------------------------------
                              
                              SUPPLY_DOM_AGGREGATE_1<-SUPPLY_DOM%>%
                                dplyr::select(PRODUCT_INDUSTRY_CODE,value)%>%
                                dplyr::group_by(PRODUCT_INDUSTRY_CODE)%>%
                                dplyr::summarize(value=sum(value,na.rm = TRUE))%>%
                                dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                              
                              SUPPLY_DOM_AGGREGATE_1$value[is.na(SUPPLY_DOM_AGGREGATE_1$value)] <- 0
                              
                              CPA_PRODUCTS_1<-CPA_PRODUCTS$Supply%>%
                                dplyr::select(PRODUCT_INDUSTRY_CODE,Imports_CIF)%>%
                                dplyr::group_by(PRODUCT_INDUSTRY_CODE )%>%
                                dplyr::summarize(value=sum(Imports_CIF,na.rm = TRUE))
                              
                              CPA_PRODUCTS_1$value[is.na(CPA_PRODUCTS_1$value)] <- 0
                              
                              # 1.1.2 Estimation of TE part -----------------------------------------------------
                              '
                                  Warning
                                  This part begins with copy-paste of the parameters entered previously and from this point begins with their new calculation with the aim of obtaining TE
                                  '
                              SIMULATION_1 = copy(SIMULATION)
                              
                              # Adding zeros for calculation of Tax expenditures
                              SIMULATION_1$Simulation_Toggles_Reduced_Rate<-TE_EXEMPT
                              SIMULATION_1$Simulation_Toggles_Exempt<-TE_REDUCED_RATE
                              
                              # Simulation parameters
                              SIMULATION_1 <- SIMULATION_1 %>%
                                dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                                              Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                                              Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
                              
                              
                              # 1.1.3 Estimation of effective VAT RATE ----------------------------------------------------------
                              # 
                              SIMULATION_2 = copy(SIMULATION)
                              SIMULATION_2$Simulation_Toggles_Reduced_Rate<-TE_EXEMPT

                              SIMULATION_2 <- SIMULATION_2 %>%
                                 dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                                               Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                                               Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)

                        # 1.2 Benchmark revenues -----
                            
                            
                           # Benchmark_tax_rate <- Benchmark_tax_rate
              
                              # Zabeleska Use_K_NetPurch da se promeni so   USE_K_DOM_NETPURCH. Podatocite se isti no zaradi usoglasenost so Excelot
                            
                            CPA_PRODUCTS$BM_Rev <- CPA_PRODUCTS$Use_K_NetPurch %>%
                                      dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Final_consumption_expenditure_by_households) %>%
                                      dplyr::mutate(Final_Demand_HH = Final_consumption_expenditure_by_households*Benchmark_tax_rate) %>%
                                      dplyr::select(-Final_consumption_expenditure_by_households)
                                    
    
                            CPA_PRODUCTS$BM_Rev$Final_Demand_HH[is.na(CPA_PRODUCTS$BM_Rev$Final_Demand_HH)] <- 0
                            CPA_PRODUCTS$BM_Rev$Final_Demand_HH[grepl("Imputed rent", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0
                            
                            
                            CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH <- CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_NPISH*Benchmark_tax_rate
                            CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH[is.na(CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH)] <- 0
                            CPA_PRODUCTS$BM_Rev$Final_Demand_NPISH[grepl("Imputed rent", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0
                            
                            CPA_PRODUCTS$BM_Rev$Final_Demand_Government <- CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_government*Benchmark_tax_rate
                            
                            CPA_PRODUCTS$BM_Rev$Final_Demand_Government[is.na(CPA_PRODUCTS$BM_Rev$Final_Demand_Government)] <- 0
                            CPA_PRODUCTS$BM_Rev$Final_Demand_Government[grepl("Imputed rent", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] <- 0
                            #OLD
                            #CPA_PRODUCTS$BM_Rev$Final_Demand_HH[grepl("Construction services", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] = 0
                            CPA_PRODUCTS$BM_Rev$Final_Demand_HH[grepl("Constructions and construction works", CPA_PRODUCTS$BM_Rev$PRODUCT_INDUSTRY_NAME)] = 0
                            
                            
                            
                            CPA_PRODUCTS$BM_Rev <- CPA_PRODUCTS$BM_Rev %>%
                              dplyr::mutate(Final_Demand_Total = psum(Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government))
                            
                            Sum_of_Final_Demand_Total = sum(CPA_PRODUCTS$BM_Rev$Final_Demand_Total)
                            
                        # 1.3 Est.IS ----
                              # 1.3.1 Estimation of Calibration factor ---------------------------------------------------
                            
                            # Copy paste table
                            SUPPLY_DOM_0 = copy(SUPPLY_DOM)
                            
                            NACE_INDUSTRIES_0 = copy(NACE_INDUSTRIES)
                            
    
                            EST.IS_0 <- merge.data.frame(SUPPLY_DOM_0, SIMULATION_0, key = "PRODUCT_INDUSTRY_CODE")
                            EST.IS_0$value = EST.IS_0$value*EST.IS_0$Simulated_Policy_Exempt
                            
    
                            
                            EST.IS_0 <- EST.IS_0 %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
                            EST.IS_0$value[is.na(EST.IS_0$value)] <- 0
                            
                            
                          
                            NACE_INDUSTRIES_0$Est.IS <- EST.IS_0 %>% 
                              dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
                              dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
                              dplyr::summarise(Industry_Share = sum(value, na.rm = T))
                            
                            NACE_INDUSTRIES_0$Est.IS <- NACE_INDUSTRIES_0$Est.IS %>%
                              dplyr::arrange(INDUSTRY_CODE)
                            
                           
                            NACE_INDUSTRIES_0$Supply_Dom <- NACE_INDUSTRIES_0$Supply_Dom %>%
                              dplyr::arrange(INDUSTRY_CODE)
                            
                            NACE_INDUSTRIES_0$Est.IS$Industry_Share <- NACE_INDUSTRIES_0$Est.IS$Industry_Share/NACE_INDUSTRIES_0$Supply_Dom$Total_output_by_industries_at_basic_prices
                            
                            NACE_INDUSTRIES_0$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES_0$Est.IS$Industry_Share)] <- 0
                            
    
                              # 1.3.2 Main estimation  --------------------------------------------------
                              
                              EST.IS <- merge.data.frame(SUPPLY_DOM, SIMULATION, key = "PRODUCT_INDUSTRY_CODE")
                              EST.IS$value = EST.IS$value*EST.IS$Simulated_Policy_Exempt
                              
                              EST.IS <- EST.IS %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
                              EST.IS$value[is.na(EST.IS$value)] <- 0
                              
                              NACE_INDUSTRIES$Est.IS <- EST.IS %>% 
                                dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
                                dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
                                dplyr::summarise(Industry_Share = sum(value, na.rm = T))
                              
                              NACE_INDUSTRIES$Est.IS <- NACE_INDUSTRIES$Est.IS %>%
                                dplyr::arrange(INDUSTRY_CODE)
                              
                              NACE_INDUSTRIES$Supply_Dom <- NACE_INDUSTRIES$Supply_Dom %>%
                                dplyr::arrange(INDUSTRY_CODE)
                              
                              NACE_INDUSTRIES$Est.IS$Industry_Share <- NACE_INDUSTRIES$Est.IS$Industry_Share/NACE_INDUSTRIES$Supply_Dom$Total_output_by_industries_at_basic_prices
                              
                              NACE_INDUSTRIES$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES$Est.IS$Industry_Share)] <- 0
                              
    
                              # 1.3.3 Estimation of TE part ---------------------------------------------------
                              
                              # Copy paste table
                              SUPPLY_DOM_1 = copy(SUPPLY_DOM)
                              NACE_INDUSTRIES_1 = copy(NACE_INDUSTRIES)
                              
                              EST.IS_1 <- merge.data.frame(SUPPLY_DOM_1, SIMULATION_1, key = "PRODUCT_INDUSTRY_CODE")
                              EST.IS_1$value = EST.IS_1$value*EST.IS_1$Simulated_Policy_Exempt
                              
                              EST.IS_1 <- EST.IS_1 %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
                              EST.IS_1$value[is.na(EST.IS_1$value)] <- 0
                              
                              NACE_INDUSTRIES_1$Est.IS <- EST.IS_1 %>% 
                                dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
                                dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
                                dplyr::summarise(Industry_Share = sum(value, na.rm = T))
                              
                              NACE_INDUSTRIES_1$Est.IS <- NACE_INDUSTRIES_1$Est.IS %>%
                                dplyr::arrange(INDUSTRY_CODE)
                              
                              NACE_INDUSTRIES_1$Supply_Dom <- NACE_INDUSTRIES_1$Supply_Dom %>%
                                dplyr::arrange(INDUSTRY_CODE)
                              
                              NACE_INDUSTRIES_1$Est.IS$Industry_Share <- NACE_INDUSTRIES_1$Est.IS$Industry_Share/NACE_INDUSTRIES_1$Supply_Dom$Total_output_by_industries_at_basic_prices
                              
                              NACE_INDUSTRIES_1$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES_1$Est.IS$Industry_Share)] <- 0
                              
                              # 1.3.4 Estimation of effective VAT RATE ----------------------------------------
                              # 
                              # # Copy paste table
                              # SUPPLY_DOM_2 = copy(SUPPLY_DOM)
                              # NACE_INDUSTRIES_2 = copy(NACE_INDUSTRIES)
                              # 
                              # EST.IS_2 <- merge.data.frame(SUPPLY_DOM_2, SIMULATION_2, key = "PRODUCT_INDUSTRY_CODE")
                              # EST.IS_2$value = EST.IS_2$value*EST.IS_2$Simulated_Policy_Exempt
                              # 
                              # EST.IS_2 <- EST.IS_2 %>% select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, INDUSTRY_CODE, INDUSTRY_NAME, value)
                              # EST.IS_2$value[is.na(EST.IS_2$value)] <- 0
                              # 
                              # NACE_INDUSTRIES_2$Est.IS <- EST.IS_2 %>% 
                              #   dplyr::filter(PRODUCT_INDUSTRY_CODE != "NA" & INDUSTRY_CODE != "NA") %>%
                              #   dplyr::group_by(INDUSTRY_CODE, INDUSTRY_NAME) %>%
                              #   dplyr::summarise(Industry_Share = sum(value, na.rm = T))
                              # 
                              # NACE_INDUSTRIES_2$Est.IS <- NACE_INDUSTRIES_2$Est.IS %>%
                              #   dplyr::arrange(INDUSTRY_CODE)
                              # 
                              # NACE_INDUSTRIES_2$Supply_Dom <- NACE_INDUSTRIES_2$Supply_Dom %>%
                              #   dplyr::arrange(INDUSTRY_CODE)
                              # 
                              # NACE_INDUSTRIES_2$Est.IS$Industry_Share <- NACE_INDUSTRIES_2$Est.IS$Industry_Share/NACE_INDUSTRIES_2$Supply_Dom$Total_output_by_industries_at_basic_prices
                              # 
                              # NACE_INDUSTRIES_2$Est.IS$Industry_Share[is.na(NACE_INDUSTRIES_2$Est.IS$Industry_Share)] <- 0
                               
                        # 1.4 Est Rev ----
                              # 1.4.1 Estimation of Calibration factor ---------------------------------------------------
                              USE_K_DOM_NETPURCH_0 = copy(USE_K_DOM_NETPURCH)
                              CPA_PRODUCTS_0 = copy(CPA_PRODUCTS)
                              
                              # OLD
                              # EST_REV_0 <- USE_K_DOM_NETPURCH_0 %>% 
                              #   merge.data.frame(NACE_INDUSTRIES_0$Est.IS, key = "INDUSTRY_NAME") %>%
                              #   merge.data.frame(SIMULATION_0, key = "PRODUCT_INDUSTRY_NAME") %>%
                              #   dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*Industry_Share*value) %>%
                              #   dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
                              
                              # Change of INDUSTRY_NAME  with INDUSTRY_cODE
                              EST_REV_CALC_1 <- inner_join(USE_K_DOM_NETPURCH_0,NACE_INDUSTRIES_0$Est.IS,by = c("INDUSTRY_CODE"="INDUSTRY_CODE"))
                              EST_REV_CALC_2 <- inner_join(EST_REV_CALC_1,SIMULATION_0,by = c("PRODUCT_INDUSTRY_CODE"="PRODUCT_INDUSTRY_CODE"))
                              
                              EST_REV_0 <- EST_REV_CALC_2%>%
                                dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*Industry_Share*value) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
                              
                              
                              
                              CPA_PRODUCTS_0$Est_Rev <- EST_REV_0 %>% 
                                dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
                                dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                              
                              CPA_PRODUCTS_0$Est_Rev <- CPA_PRODUCTS_0$Use_K_NetPurch %>% 
                                merge.data.frame(SIMULATION_0, key = "PRODUCT_INDUSTRY_NAME") %>%
                                dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_by_households,
                                              Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_NPISH,
                                              Final_Demand_Government = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_by_government) %>%
                                merge.data.frame(CPA_PRODUCTS_0$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
                                dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_NAME)
                              
                              # OLD
                              # CPA_PRODUCTS_0$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_0$Est_Rev$PRODUCT_INDUSTRY_NAME == "Construction services"] <- 
                              #   CPA_PRODUCTS_0$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_0$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Construction services"]*
                              #  (1-SIMULATION$Simulated_Policy_Exempt[72])*vat_rate_on_residential_construction
                              
                              # NEW 17.03.2024 new simulation 1
                              CPA_PRODUCTS_0$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_0$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
                                CPA_PRODUCTS_0$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_0$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
                                (1-SIMULATION$Simulated_Policy_Exempt[27])*vat_rate_on_residential_construction
                                
                              
                            
                              
                              CPA_PRODUCTS_0$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_0$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                                               CPA_PRODUCTS_0$Est_Rev$Final_Demand_HH, 
                                                                               CPA_PRODUCTS_0$Est_Rev$Final_Demand_NPISH, 
                                                                               CPA_PRODUCTS_0$Est_Rev$Final_Demand_Government, na.rm = T) 
                              
                            
    
                              # 1.4.2 Estimation of revenues business as usual ----------------
    
                              USE_K_DOM_NETPURCH_BU = copy(USE_K_DOM_NETPURCH)
                              CPA_PRODUCTS_BU = copy(CPA_PRODUCTS)
                              
                              # EST_REV_BU <- USE_K_DOM_NETPURCH_BU %>% 
                              #   merge.data.frame(NACE_INDUSTRIES$Est.IS, key = "INDUSTRY_NAME") %>%
                              #   merge.data.frame(SIMULATION, key = "PRODUCT_INDUSTRY_NAME") %>%
                              #   dplyr::mutate(value = (Current_Policy_Reduced_Rate*Preferential_VAT_rate_bu +Current_Policy_Fully_Taxable*Standard_VAT_rate_bu )*Industry_Share*value) %>%
                              #   dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
                    
  # New 22/09/2023      
                              TAXABLE_PROPORTION_BU_1 <- TAXABLE_PROPORTION_BU
                              colnames(TAXABLE_PROPORTION_BU_1) <- column_names
                              TAXABLE_PROPORTION_BU_1 <- TAXABLE_PROPORTION_BU %>%
                                dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(Simulation_Toggles_Exempt), Current_Policy_Exempt, Simulation_Toggles_Exempt),
                                              Simulated_Policy_Reduced_Rate = ifelse(is.na(Simulation_Toggles_Reduced_Rate), Current_Policy_Reduced_Rate, Simulation_Toggles_Reduced_Rate),
                                              Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)

                              
                              # NEW
                              EST_REV_CALC_1 <- inner_join(USE_K_DOM_NETPURCH_BU,NACE_INDUSTRIES$Est.IS,by = c("INDUSTRY_CODE"="INDUSTRY_CODE"))
                              EST_REV_CALC_2 <- inner_join(EST_REV_CALC_1,TAXABLE_PROPORTION_BU_1,by = c("PRODUCT_INDUSTRY_CODE"="PRODUCT_INDUSTRY_CODE"))
                              
                              EST_REV_BU <- EST_REV_CALC_2%>%
                                        dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate +Simulated_Policy_Fully_Taxable*Standard_VAT_Rate )*Industry_Share*value) %>%
                                        dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
                              
                              CPA_PRODUCTS_BU$Est_Rev <- EST_REV_BU %>% 
                                        dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
                                        dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
                                        dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                              
                              CPA_PRODUCTS_BU$Est_Rev <- CPA_PRODUCTS_BU$Use_K_NetPurch %>% 
                                        merge.data.frame(TAXABLE_PROPORTION_BU_1, key = "PRODUCT_INDUSTRY_NAME") %>%
                                        dplyr::mutate(Final_Demand_HH = (Current_Policy_Reduced_Rate*Preferential_VAT_Rate + Current_Policy_Fully_Taxable*Standard_VAT_Rate )*
                                                        Final_consumption_expenditure_by_households,
                                                        Final_Demand_NPISH = (Current_Policy_Reduced_Rate*Preferential_VAT_Rate + Current_Policy_Fully_Taxable*Standard_VAT_Rate )*
                                                        Final_consumption_expenditure_NPISH,
                                                        Final_Demand_Government = (Current_Policy_Reduced_Rate*Preferential_VAT_Rate + Current_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                        Final_consumption_expenditure_by_government) %>%
                                        merge.data.frame(CPA_PRODUCTS_BU$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
                                        dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
                                        dplyr::arrange(PRODUCT_INDUSTRY_NAME)
                              
                              # OLD
                              # CPA_PRODUCTS_BU$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_BU$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
                              #   CPA_PRODUCTS_BU$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_BU$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
                              #   (1-TAXABLE_PROPORTION_BU_1$Current_Policy_Exempt[72])*vat_rate_on_residential_construction
                              
                              CPA_PRODUCTS_BU$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_BU$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
                                CPA_PRODUCTS_BU$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_BU$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
                                (1-TAXABLE_PROPORTION_BU_1$Current_Policy_Exempt[27])*vat_rate_on_residential_construction
                              
                              
                              CPA_PRODUCTS_BU$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_BU$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                                                CPA_PRODUCTS_BU$Est_Rev$Final_Demand_HH, 
                                                                                CPA_PRODUCTS_BU$Est_Rev$Final_Demand_NPISH, 
                                                                                CPA_PRODUCTS_BU$Est_Rev$Final_Demand_Government, na.rm = T)  
                              
                              # Extracting NACE names
                              NACE_NAMES<-CPA_PRODUCTS_BU$Est_Rev%>%
                                select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME)
                              
                              # 1.4.3 Main estimation ----------------------------------------------------
                      
                              # NEW
                              EST_REV_CALC_1 <- inner_join(USE_K_DOM_NETPURCH,NACE_INDUSTRIES$Est.IS,by = c("INDUSTRY_CODE"="INDUSTRY_CODE"))
                              EST_REV_CALC_2 <- inner_join(EST_REV_CALC_1,SIMULATION,by = c("PRODUCT_INDUSTRY_CODE"="PRODUCT_INDUSTRY_CODE"))
                              
                              EST_REV <- EST_REV_CALC_2%>%
                                dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*Industry_Share*value) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
                              
                              
                              CPA_PRODUCTS$Est_Rev <- EST_REV %>% 
                                dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
                                dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                              
                              CPA_PRODUCTS$Est_Rev <- CPA_PRODUCTS$Use_K_NetPurch %>% 
                                merge.data.frame(SIMULATION, key = "PRODUCT_INDUSTRY_NAME") %>%
                                dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_by_households,
                                              Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_NPISH,
                                              Final_Demand_Government = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_by_government) %>%
                                merge.data.frame(CPA_PRODUCTS$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
                                dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_NAME)
                              
                              # OLD
                              # CPA_PRODUCTS$Est_Rev$Final_Demand_HH[CPA_PRODUCTS$Est_Rev$PRODUCT_INDUSTRY_NAME == "Construction services"] <- 
                              #   CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Construction services"]*
                              #   (1-SIMULATION$Simulated_Policy_Exempt[72])*vat_rate_on_residential_construction
                              
                              # NEW 17.03.2024				   
                              CPA_PRODUCTS$Est_Rev$Final_Demand_HH[CPA_PRODUCTS$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
                                CPA_PRODUCTS$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
                                (1-SIMULATION$Simulated_Policy_Exempt[27])*vat_rate_on_residential_construction
                              
                              
                              CPA_PRODUCTS$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                                             CPA_PRODUCTS$Est_Rev$Final_Demand_HH, 
                                                                             CPA_PRODUCTS$Est_Rev$Final_Demand_NPISH, 
                                                                             CPA_PRODUCTS$Est_Rev$Final_Demand_Government, na.rm = T)  
                              
                              # Extracting NACE names
                              NACE_NAMES<-CPA_PRODUCTS$Est_Rev%>%
                                select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME)
            
                              # 1.4.4 Estimation of TE part ---------------------------------------------------
                              USE_K_DOM_NETPURCH_1 = copy(USE_K_DOM_NETPURCH)
                              CPA_PRODUCTS_1 = copy(CPA_PRODUCTS)
    
                              # NEW
                              EST_REV_CALC_1 <- inner_join(USE_K_DOM_NETPURCH_1,NACE_INDUSTRIES_1$Est.IS,by = c("INDUSTRY_CODE"="INDUSTRY_CODE"))
                              EST_REV_CALC_2 <- inner_join(EST_REV_CALC_1,SIMULATION_1,by = c("PRODUCT_INDUSTRY_CODE"="PRODUCT_INDUSTRY_CODE"))
                              
                              EST_REV_1 <- EST_REV_CALC_2%>%
                                dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*Industry_Share*value) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
                              
                              CPA_PRODUCTS_1$Est_Rev <- EST_REV_1 %>% 
                                dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
                                dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                              
                              CPA_PRODUCTS_1$Est_Rev <- CPA_PRODUCTS_1$Use_K_NetPurch %>% 
                                merge.data.frame(SIMULATION_1, key = "PRODUCT_INDUSTRY_NAME") %>%
                                dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_by_households,
                                              Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_NPISH,
                                              Final_Demand_Government = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_by_government) %>%
                                merge.data.frame(CPA_PRODUCTS_1$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
                                dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_NAME)
                              
                                CPA_PRODUCTS_1$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_1$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                                               CPA_PRODUCTS_1$Est_Rev$Final_Demand_HH, 
                                                                               CPA_PRODUCTS_1$Est_Rev$Final_Demand_NPISH, 
                                                                               CPA_PRODUCTS_1$Est_Rev$Final_Demand_Government, na.rm = T) 
                              
    
                              # 1.4.5 Estimation of effective VAT RATE ----------------------------------------
                              
                              USE_K_DOM_NETPURCH_2 = copy(USE_K_DOM_NETPURCH)
                              CPA_PRODUCTS_2 = copy(CPA_PRODUCTS) ### <------Copy of CPA_PRODUCTS
                              
                              # NEW
                              EST_REV_CALC_1 <- inner_join(USE_K_DOM_NETPURCH_2,NACE_INDUSTRIES$Est.IS,by = c("INDUSTRY_CODE"="INDUSTRY_CODE"))
                              EST_REV_CALC_2 <- inner_join(EST_REV_CALC_1,SIMULATION_2,by = c("PRODUCT_INDUSTRY_CODE"="PRODUCT_INDUSTRY_CODE"))
                              
                              EST_REV_2 <- EST_REV_CALC_2%>%
                                dplyr::mutate(value = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*Industry_Share*value) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_CODE, INDUSTRY_CODE)
                              
                              
                              
                              CPA_PRODUCTS_2$Est_Rev <- EST_REV_2 %>%  #### <- oVA DA SE VIDI DALI TREBA DA SE EKPORTIRA ?
                                dplyr::group_by(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME) %>%
                                dplyr::summarise(Total_Revenues_from_Intermediate_Inputs = sum(value, na.rm = T)) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                              
                              CPA_PRODUCTS_2$Est_Rev <- CPA_PRODUCTS_2$Use_K_NetPurch %>% 
                                merge.data.frame(SIMULATION_2, key = "PRODUCT_INDUSTRY_NAME") %>%
                                dplyr::mutate(Final_Demand_HH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_by_households,
                                              Final_Demand_NPISH = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_NPISH,
                                              Final_Demand_Government = (Simulated_Policy_Reduced_Rate*Preferential_VAT_Rate+Simulated_Policy_Fully_Taxable*Standard_VAT_Rate)*
                                                Final_consumption_expenditure_by_government) %>%
                                merge.data.frame(CPA_PRODUCTS_2$Est_Rev, key = "PRODUCT_INDUSTRY_NAME") %>%
                                dplyr::select(PRODUCT_INDUSTRY_CODE, PRODUCT_INDUSTRY_NAME, Total_Revenues_from_Intermediate_Inputs, Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government) %>%
                                dplyr::arrange(PRODUCT_INDUSTRY_NAME)
                              
                              # OLD
                              # CPA_PRODUCTS_2$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_2$Est_Rev$PRODUCT_INDUSTRY_NAME == "Construction services"] <- 
                              #   CPA_PRODUCTS_2$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_2$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Construction services"]*
                              #   (1-SIMULATION$Simulated_Policy_Exempt[72])*vat_rate_on_residential_construction
                              
                              # NEW 17.03.2024									
                              CPA_PRODUCTS_2$Est_Rev$Final_Demand_HH[CPA_PRODUCTS_2$Est_Rev$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"] <- 
                                CPA_PRODUCTS_2$Use_K_NetPurch$Final_consumption_expenditure_by_households[CPA_PRODUCTS_2$Use_K_NetPurch$PRODUCT_INDUSTRY_NAME == "Constructions and construction works"]*
                                (1-SIMULATION$Simulated_Policy_Exempt[27])*vat_rate_on_residential_construction
                              
                              CPA_PRODUCTS_2$Est_Rev$Final_Demand_Total = psum(CPA_PRODUCTS_2$Est_Rev$Total_Revenues_from_Intermediate_Inputs, 
                                                                               CPA_PRODUCTS_2$Est_Rev$Final_Demand_HH, 
                                                                               CPA_PRODUCTS_2$Est_Rev$Final_Demand_NPISH, 
                                                                               CPA_PRODUCTS_2$Est_Rev$Final_Demand_Government, na.rm = T) 
                              
            
              # 2. SIMULATION RESULTS ----  
                              # 2.1.0 Estimation of Calibration factor ---------------------------------------------------
                              # Old functional
                              # Results_0 <- as.list(c("VAT_Gap", "Simulation"))
                              # names(Results_0) <- c("VAT_Gap", "Simulation")
                              # 
                              # Results_0$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_0$BM_Rev$Final_Demand_Total))
                              # colnames(Results_0$VAT_Gap) <- "Benchmark_VAT_M_of_LCU"
                              # 
                              # 
                              # Uncalibrated_VAT<- sum(CPA_PRODUCTS_0$Est_Rev$Final_Demand_Total, na.rm = T)
                              # VAT_control<-sum(CPA_PRODUCTS_0$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_0$Use_VAT$Exports_FOB, na.rm = T)
                              # 
                              # Locked_Calibration_Factor<-VAT_control/Uncalibrated_VAT
                              # 
                              # 
                              # Results_0$VAT_Gap$Calibrated_VAT_Est.M_of_LCU <- sum(CPA_PRODUCTS_0$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
                              # Results_0$VAT_Gap$VAT_Control_Total.M_of_LCU <- sum(CPA_PRODUCTS_0$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_0$Use_VAT$Exports_FOB, na.rm = T)
                              # Results_0$VAT_Gap$Total_VAT_Gap.M_of_LCU <- Results_0$VAT_Gap$Benchmark_VAT_M_of_LCU - Results_0$VAT_Gap$VAT_Control_Total.M_of_LCU
                              # Results_0$VAT_Gap$Total_VAT_Gap.Prc <- Results_0$VAT_Gap$Total_VAT_Gap.M_of_LCU/Results_0$VAT_Gap$VAT_Control_Total.M_of_LCU
                              #View(Results_0)
                              
                              Results_0 <- as.list(c("VAT_Gap", "Simulation"))
                              names(Results_0) <- c("VAT_Gap", "Simulation")
                              
                              Results_0$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_BU$BM_Rev$Final_Demand_Total))
                              colnames(Results_0$VAT_Gap) <- "Benchmark_VAT_M_of_LCU"
                              
                              
                              Uncalibrated_VAT<- sum(CPA_PRODUCTS_BU$Est_Rev$Final_Demand_Total, na.rm = T)
                              VAT_control<-sum(CPA_PRODUCTS_BU$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_BU$Use_VAT$Exports_FOB, na.rm = T)
                              
                              Locked_Calibration_Factor<-VAT_control/Uncalibrated_VAT
                              
                              
                              Results_0$VAT_Gap$Calibrated_VAT_Est.M_of_LCU <- sum(CPA_PRODUCTS_BU$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
                              Results_0$VAT_Gap$VAT_Control_Total.M_of_LCU <- sum(CPA_PRODUCTS_BU$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_BU$Use_VAT$Exports_FOB, na.rm = T)
                              Results_0$VAT_Gap$Total_VAT_Gap.M_of_LCU <- Results_0$VAT_Gap$Benchmark_VAT_M_of_LCU - Results_0$VAT_Gap$VAT_Control_Total.M_of_LCU
                              Results_0$VAT_Gap$Total_VAT_Gap.Prc <- Results_0$VAT_Gap$Total_VAT_Gap.M_of_LCU/Results_0$VAT_Gap$VAT_Control_Total.M_of_LCU
                              
                              
                              
                              # 2.1.1 Main estimation --------------------------------------------------
                              
                              Results <- as.list(c("VAT_Gap", "Simulation"))
                              names(Results) <- c("VAT_Gap", "Simulation")
                              
                              
                              Results$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS$BM_Rev$Final_Demand_Total))
                              colnames(Results$VAT_Gap) <- "Benchmark_VAT_M_of_LCU"
                              
                              Results$VAT_Gap$Uncalibrated_VAT_Est.M_of_LCU <- sum(CPA_PRODUCTS$Est_Rev$Final_Demand_Total, na.rm = T)
                              
                              
                              
                              Results$VAT_Gap$Calibrated_VAT_Est.M_of_LCU <- sum(CPA_PRODUCTS$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
                              Results$VAT_Gap$VAT_Control_Total.M_of_LCU <- sum(CPA_PRODUCTS$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS$Use_VAT$Exports_FOB, na.rm = T)
                              Results$VAT_Gap$Total_VAT_Gap.M_of_LCU <- Results$VAT_Gap$Benchmark_VAT_M_of_LCU - Results$VAT_Gap$VAT_Control_Total.M_of_LCU
                              Results$VAT_Gap$Total_VAT_Gap.Prc <- Results$VAT_Gap$Total_VAT_Gap.M_of_LCU/Results$VAT_Gap$VAT_Control_Total.M_of_LCU
                              
                        
                              # Final output - Change in Revenues
                              
                              Results$Simulation <- as.data.frame(as.integer(Results$VAT_Gap$Calibrated_VAT_Est.M_of_LCU-Results$VAT_Gap$VAT_Control_Total.M_of_LCU))
                              colnames(Results$Simulation) <- "Simulated_Change_in_Revenues.M_of_LCU"
                              
                              Results$Simulation$Simulated_Change_in_Revenues.Prc <- (Results$Simulation$Simulated_Change_in_Revenues.M_of_LCU/Results$VAT_Gap$VAT_Control_Total.M_of_LCU)*100
                              
                              
                              # Estimation of VAT-C-efficiency
                              
                             
                              FinalConsumption=(sum(CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government,na.rm=TRUE)+
                                                  sum(CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH,na.rm=TRUE)+
                                                  sum(CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households,na.rm = TRUE))
                              
                              VAT_C_efficiency_SIM=  Results$VAT_Gap$Calibrated_VAT_Est.M_of_LCU/(standard_VAT_rate*FinalConsumption)
                                
                                
                              
                              # 2.1.2 Estimation of TE part ---------------------------------------------------
                              
                              Results_1 <- as.list(c("VAT_Gap", "Simulation"))
                              names(Results_1) <- c("VAT_Gap", "Simulation")
                              
                              # NEW
                              Results_1$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_1$BM_Rev$Final_Demand_Total))
                              colnames(Results_1$VAT_Gap) <- "Benchmark_VAT_M_of_LCU"
    
                              
                    # NOVA GRESKA !!! OVDE NAMESTO ESTIMACIJA OD MODELOT KORISTEN E E BENCHMARK Est_Rev ??
                              ###
                              
                              # Check if PRODUCT_INDUSTRY_NAME is equal to "Imputed rent"
                              imputed_rent_rows <- CPA_PRODUCTS_1$Est_Rev$PRODUCT_INDUSTRY_NAME == "Imputed rent"
                              
                              # Replace Final_Demand_HH, Final_Demand_NPISH, Final_Demand_Government, and Final_Demand_Total with 0 where PRODUCT_INDUSTRY_NAME is "Imputed rent"
                              CPA_PRODUCTS_1$Est_Rev$Final_Demand_HH[imputed_rent_rows] <- 0
                              CPA_PRODUCTS_1$Est_Rev$Final_Demand_NPISH[imputed_rent_rows] <- 0
                              CPA_PRODUCTS_1$Est_Rev$Final_Demand_Government[imputed_rent_rows] <- 0
                              CPA_PRODUCTS_1$Est_Rev$Final_Demand_Total[imputed_rent_rows] <- 0
                              
                              
                              
                              ####
                              
                              Results_1$VAT_Gap$Uncalibrated_VAT_Est.M_of_LCU <- sum(CPA_PRODUCTS_1$Est_Rev$Final_Demand_Total, na.rm = T)
                              Results_1$VAT_Gap$Calibrated_VAT_Est.M_of_LCU <- sum(CPA_PRODUCTS_1$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
                              Results_1$VAT_Gap$VAT_Control_Total.M_of_LCU <- sum(CPA_PRODUCTS_1$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_1$Use_VAT$Exports_FOB, na.rm = T)
                              Results_1$VAT_Gap$Total_VAT_Gap.M_of_LCU <- Results_1$VAT_Gap$Benchmark_VAT_M_of_LCU - Results_1$VAT_Gap$VAT_Control_Total.M_of_LCU
                              Results_1$VAT_Gap$Total_VAT_Gap.Prc <- Results_1$VAT_Gap$Total_VAT_Gap.M_of_LCU/Results_1$VAT_Gap$VAT_Control_Total.M_of_LCU
    
                              # Final output - Change in Revenues
                              
                              
                              Results_1$Simulation <- as.data.frame(as.integer(Results_1$VAT_Gap$Calibrated_VAT_Est.M_of_LCU-Results_1$VAT_Gap$VAT_Control_Total.M_of_LCU))
                              colnames(Results_1$Simulation) <- "Simulated_Change_in_Revenues.M_of_LCU"
                              
                              Results_1$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_1$Simulation$Simulated_Change_in_Revenues.M_of_LCU/Results_1$VAT_Gap$VAT_Control_Total.M_of_LCU)*100
                              
                              
                              # Extracting part foR TE
                              # Results$VAT_Gap$Policy_Gap.M_of_LCU<-Results_1$VAT_Gap$Policy_Gap.M_of_LCU
                              
                              #View(Results_1)
                              
                              
                              # 2.1.1 Adding TE'S ----------------------------------------------------------------
                                 # This part is from 1.1.1 main estimation
                              Results$VAT_Gap$Policy_Gap.M_of_LCU <-Results_1$Simulation$Simulated_Change_in_Revenues.M_of_LCU
                              Results$VAT_Gap$Policy_Gap.Prc <- Results$VAT_Gap$Policy_Gap.M_of_LCU/Results$VAT_Gap$VAT_Control_Total.M_of_LCU
                              Results$VAT_Gap$Compliance_Gap.M_of_LCU <- Results$VAT_Gap$Total_VAT_Gap.M_of_LCU-Results$VAT_Gap$Policy_Gap.M_of_LCU
                              Results$VAT_Gap$Compliance_Gap.Prc <- Results$VAT_Gap$Compliance_Gap.M_of_LCU/Results$VAT_Gap$VAT_Control_Total.M_of_LCU
                              Results$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor
                              
                             # This part is for calibration factor
                              Results_0$VAT_Gap$Policy_Gap.M_of_LCU <- Results_1$Simulation$Simulated_Change_in_Revenues.M_of_LCU
                              
                              #New
                              Results_0$VAT_Gap$Policy_Gap.Prc <- Results_0$VAT_Gap$Policy_Gap.M_of_LCU/Results_0$VAT_Gap$VAT_Control_Total.M_of_LCU
                              Results_0$VAT_Gap$Compliance_Gap.M_of_LCU <- Results_0$VAT_Gap$Total_VAT_Gap.M_of_LCU-Results_0$VAT_Gap$Policy_Gap.M_of_LCU
                              Results_0$VAT_Gap$Compliance_Gap.Prc <- Results_0$VAT_Gap$Compliance_Gap.M_of_LCU/Results_0$VAT_Gap$VAT_Control_Total.M_of_LCU
                              Results_0$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor
                              
                              
                              # Final output - Change in Revenues
                              
                              
                              Results_0$Simulation <- as.data.frame(as.integer(Results_0$VAT_Gap$Calibrated_VAT_Est.M_of_LCU-Results_0$VAT_Gap$VAT_Control_Total.M_of_LCU))
                              colnames(Results_0$Simulation) <- "Simulated_Change_in_Revenues.M_of_LCU"
                              
                              Results_0$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_0$Simulation$Simulated_Change_in_Revenues.M_of_LCU/Results_0$VAT_Gap$VAT_Control_Total.M_of_LCU)*100
                              
                              
                              
                              
                              
                              # 2.1.3 Estimation of effective VAT RATE ------------------------------
                              
                              Results_2 <- as.list(c("VAT_Gap", "Simulation"))
                              names(Results_2) <- c("VAT_Gap", "Simulation")
                              
                              
                              Results_2$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_2$BM_Rev$Final_Demand_Total))
                              colnames(Results_2$VAT_Gap) <- "Benchmark_VAT_M_of_LCU"
                              
                              Results_2$VAT_Gap$Uncalibrated_VAT_Est.M_of_LCU <- sum(CPA_PRODUCTS_2$Est_Rev$Final_Demand_Total, na.rm = T)
                              
                              
                              
                              Results_2$VAT_Gap$Calibrated_VAT_Est.M_of_LCU <- sum(CPA_PRODUCTS_2$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
                              Results_2$VAT_Gap$VAT_Control_Total.M_of_LCU <- sum(CPA_PRODUCTS_2$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_2$Use_VAT$Exports_FOB, na.rm = T)
                              
                              Results_2$VAT_Gap$Total_VAT_Gap.M_of_LCU <- Results_2$VAT_Gap$Benchmark_VAT_M_of_LCU - Results_2$VAT_Gap$VAT_Control_Total.M_of_LCU
                              Results_2$VAT_Gap$Total_VAT_Gap.Prc <- Results_2$VAT_Gap$Total_VAT_Gap.M_of_LCU/Results_2$VAT_Gap$VAT_Control_Total.M_of_LCU
                              
                              # Manual input 
                              
                             # Results_2$VAT_Gap$Policy_Gap.M_of_LCU <- 24407  # <------- Manual input in simulation
                              
                              Results_2$VAT_Gap$Policy_Gap.M_of_LCU <-Results_1$Simulation$Simulated_Change_in_Revenues.M_of_LCU  
                              Results_2$VAT_Gap$Policy_Gap.Prc <- Results_2$VAT_Gap$Policy_Gap.M_of_LCU/Results_2$VAT_Gap$VAT_Control_Total.M_of_LCU
                              Results_2$VAT_Gap$Compliance_Gap.M_of_LCU <- Results_2$VAT_Gap$Total_VAT_Gap.M_of_LCU-Results_2$VAT_Gap$Policy_Gap.M_of_LCU
                              Results_2$VAT_Gap$Compliance_Gap.Prc <- Results_2$VAT_Gap$Compliance_Gap.M_of_LCU/Results_2$VAT_Gap$VAT_Control_Total.M_of_LCU
                              Results_2$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor 
                              
                              
                              
                              # Final output - Change in Revenues
                              
                            
                              Results_2$Simulation <- as.data.frame(as.integer(Results_2$VAT_Gap$Calibrated_VAT_Est.M_of_LCU-Results_2$VAT_Gap$VAT_Control_Total.M_of_LCU))
                              colnames(Results_2$Simulation) <- "Simulated_Change_in_Revenues.M_of_LCU"
                              Results_2$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_2$Simulation$Simulated_Change_in_Revenues.M_of_LCU/Results_2$VAT_Gap$VAT_Control_Total.M_of_LCU)*100
                              
    
                      # 2.1.4 NEW TEST FOR BU REVENUES-----------------------------------------------------------

                          Results_3 <- as.list(c("VAT_Gap", "Simulation"))
                          names(Results_3) <- c("VAT_Gap", "Simulation")
                          # 
                          Results_3$VAT_Gap <- as.data.frame(sum(CPA_PRODUCTS_BU$BM_Rev$Final_Demand_Total))
                          colnames(Results_3$VAT_Gap) <- "Benchmark_VAT_M_of_LCU"
                          # 
                          Results_3$VAT_Gap$Uncalibrated_VAT_Est.M_of_LCU <- sum(CPA_PRODUCTS_BU$Est_Rev$Final_Demand_Total, na.rm = T)
                          # 
                          Results_3$VAT_Gap$Calibrated_VAT_Est.M_of_LCU <- sum(CPA_PRODUCTS_BU$Est_Rev$Final_Demand_Total, na.rm = T)*Locked_Calibration_Factor
                          Results_3$VAT_Gap$VAT_Control_Total.M_of_LCU <- sum(CPA_PRODUCTS_BU$Use_VAT$Total_use_at_basic_prices, na.rm = T) - sum(CPA_PRODUCTS_BU$Use_VAT$Exports_FOB, na.rm = T)

                          
                          Results_3$VAT_Gap$Total_VAT_Gap.M_of_LCU <- Results_3$VAT_Gap$Benchmark_VAT_M_of_LCU - Results_3$VAT_Gap$VAT_Control_Total.M_of_LCU
                          Results_3$VAT_Gap$Total_VAT_Gap.Prc <- Results_3$VAT_Gap$Total_VAT_Gap.M_of_LCU/Results_3$VAT_Gap$VAT_Control_Total.M_of_LCU

                          Results_3$VAT_Gap$Policy_Gap.M_of_LCU <-Results_1$Simulation$Simulated_Change_in_Revenues.M_of_LCU # <-- Warning. This come from TE'S
                           Results_3$VAT_Gap$Policy_Gap.Prc <- Results_3$VAT_Gap$Policy_Gap.M_of_LCU/Results_3$VAT_Gap$VAT_Control_Total.M_of_LCU
                           Results_3$VAT_Gap$Compliance_Gap.M_of_LCU <- Results_3$VAT_Gap$Total_VAT_Gap.M_of_LCU-Results_3$VAT_Gap$Policy_Gap.M_of_LCU
                           Results_3$VAT_Gap$Compliance_Gap.Prc <- Results_3$VAT_Gap$Compliance_Gap.M_of_LCU/Results_3$VAT_Gap$VAT_Control_Total.M_of_LCU
                           Results_3$VAT_Gap$Calibration_Factor <- Locked_Calibration_Factor 
                           
                          
                          # # Final output - Change in Revenues
                          # 
                          Results_3$Simulation <- as.data.frame(as.integer(Results_3$VAT_Gap$Calibrated_VAT_Est.M_of_LCU-Results_3$VAT_Gap$VAT_Control_Total.M_of_LCU))
                          colnames(Results_3$Simulation) <- "Simulated_Change_in_Revenues.M_of_LCU"
                          Results_3$Simulation$Simulated_Change_in_Revenues.Prc <- (Results_3$Simulation$Simulated_Change_in_Revenues.M_of_LCU/Results_3$VAT_Gap$VAT_Control_Total.M_of_LCU)*100
                          
                           
                          # Estimation of VAT-C-efficiency
                          FinalConsumption=(sum(CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_government,na.rm=TRUE)+
                                              sum(CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_NPISH,na.rm=TRUE)+
                                              sum(CPA_PRODUCTS$Use_Purchaser$Final_consumption_expenditure_by_households,na.rm = TRUE))
                          
                          VAT_C_efficiency_BU= Results_3$VAT_Gap$Calibrated_VAT_Est.M_of_LCU/(Benchmark_tax_rate_bu*FinalConsumption)
                          
                          
                          # Estimation of VAT 
                          
                          
                # 3.PREPARATION TABLE FOR EXPORT AND PLOTTING ------------------------------------------

                          # 3.1 Result (Business as usual) without intervention --------------------------------------------------------------------
                          
                          Export_Main_Results_BU<- Results_3$VAT_Gap 
                          Export_Main_Results_BU$VAT_benchmark_rate<-Benchmark_tax_rate
                          Export_Main_Results_BU$C_Efficiency<-VAT_C_efficiency_BU
                          
                          
                          Export_Main_Results_BU<- Export_Main_Results_BU%>%
                            dplyr:: rename(
                                          "Benchmark_VAT"="Benchmark_VAT_M_of_LCU",
                                          "Uncalibrated_VAT" ="Uncalibrated_VAT_Est.M_of_LCU",
                                          "Calibration_Factor"= "Calibration_Factor",
                                          "Calibrated_VAT"= "Calibrated_VAT_Est.M_of_LCU",
                                          "Actual_VAT"= "VAT_Control_Total.M_of_LCU",
                                          "Total_VAT_Gap"= "Total_VAT_Gap.M_of_LCU",
                                          "Total_VAT_Gap_Pct"= "Total_VAT_Gap.Prc",
                                          "Policy_Gap"= "Policy_Gap.M_of_LCU",
                                          "Policy_Gap_Pct"= "Policy_Gap.Prc",
                                          "Compliance_Gap"= "Compliance_Gap.M_of_LCU",
                                          "Compliance_Gap_Pct"= "Compliance_Gap.Prc",
                                          "VAT_Benchmark_Rate"= "VAT_benchmark_rate")
                          # Melt table  
                          Export_Main_Results_BU<- melt(Export_Main_Results_BU)
                          
                          # Convert into billions
                          Export_Main_Results_BU <- Export_Main_Results_BU %>%
                                                  dplyr::mutate(value = ifelse(
                                                    !str_ends(variable, "Pct|Rate|Factor|Efficiency"),
                                                    value / conversion_factor,
                                                    value
                                                  ))
                          # Convert into percent
                          Export_Main_Results_BU <- Export_Main_Results_BU %>%
                            dplyr::mutate(value = ifelse(
                              str_ends(variable, "Pct|Rate|Factor"),
                              value * 100,
                              value
                            ))
                          
                    
                          
                          # Rounding 
                          Export_Main_Results_BU$value<-round(Export_Main_Results_BU$value,2)
                          
                          
                          
                          # 3.2 Main results --------------------------------------------------------

                              Export_Main_Results_SIM<- Results$VAT_Gap 
                              Export_Main_Results_SIM$VAT_benchmark_rate<-Benchmark_tax_rate
                              Export_Main_Results_SIM$C_Efficiency<-VAT_C_efficiency_SIM
                              
                              
                              Export_Main_Results_SIM<- Export_Main_Results_SIM%>%
                                          dplyr:: rename(
                                                        "Benchmark_VAT"="Benchmark_VAT_M_of_LCU",
                                                        "Uncalibrated_VAT" ="Uncalibrated_VAT_Est.M_of_LCU",
                                                        "Calibration_Factor"= "Calibration_Factor",
                                                        "Calibrated_VAT"= "Calibrated_VAT_Est.M_of_LCU",
                                                        "Actual_VAT"= "VAT_Control_Total.M_of_LCU",
                                                        "Total_VAT_Gap"= "Total_VAT_Gap.M_of_LCU",
                                                        "Total_VAT_Gap_Pct"= "Total_VAT_Gap.Prc",
                                                        "Policy_Gap"= "Policy_Gap.M_of_LCU",
                                                        "Policy_Gap_Pct"= "Policy_Gap.Prc",
                                                        "Compliance_Gap"= "Compliance_Gap.M_of_LCU",
                                                        "Compliance_Gap_Pct"= "Compliance_Gap.Prc",
                                                        "VAT_Benchmark_Rate"= "VAT_benchmark_rate")
                              # Melt table  
                              Export_Main_Results_SIM<- melt(Export_Main_Results_SIM)
                              
                              # Convert into billions
                              Export_Main_Results_SIM <- Export_Main_Results_SIM %>%
                                              dplyr::mutate(value = ifelse(
                                                        !str_ends(variable, "Pct|Rate|Factor|Efficiency"),
                                                        value / conversion_factor,
                                                        value
                                                      ))
                              # Convert into percent
                              Export_Main_Results_SIM <- Export_Main_Results_SIM %>%
                                                dplyr::mutate(value = ifelse(
                                                        str_ends(variable, "Pct|Rate|Factor"),
                                                        value * 100,
                                                        value
                                                      ))
      
                              
                              
                              # Rounding 
                              Export_Main_Results_SIM$value<-round(Export_Main_Results_SIM$value,1)
                          

                          # 3.3 Merging tables ------------------------------------------------------

                              Export_Main_Results_BU$value<-round(Export_Main_Results_BU$value,1)
                              Export_Main_Results_SIM$value<-round(Export_Main_Results_SIM$value,1)
                              
                              Export_Main_Results<-inner_join(Export_Main_Results_BU,Export_Main_Results_SIM,by=c('variable'='variable'))

                   
                              Export_Main_Results<- Export_Main_Results%>%
                                dplyr:: rename(
                                              "Actual"="value.x",
                                              "Simulation"="value.y")%>%
                                dplyr::mutate(Difference= Simulation-Actual,
                                              Pct_Change=((Simulation/Actual)*100-100)
                                              )
                              
                              
                              Uncalibrated_Reform<-Export_Main_Results%>%
                                          dplyr::filter(variable==("Uncalibrated_VAT"))%>%
                                          dplyr::select(Difference)
                              
                              Calibrated_Reform<-Export_Main_Results%>%
                                        dplyr::filter(variable==("Calibrated_VAT"))%>%
                                        dplyr::select(Difference)
                              
                              # Export_Main_Results<-Export_Main_Results%>%
                              #           dplyr::filter(variable==c("Benchmark_VAT"))%>%
                              #           dplyr::select(variable,Simulation)
                              #           dplyr::mutate(Benchmark_VAT  )
                              
                              
                            
                            # Assuming your data frame is named Export_Main_Results
                              
                              # Total_VAT_Gap
                                        Export_Main_Results$Simulation[Export_Main_Results$variable == "Total_VAT_Gap"] <- 
                                                Export_Main_Results$Actual[Export_Main_Results$variable == "Total_VAT_Gap"] - 
                                                Export_Main_Results$Difference[Export_Main_Results$variable == "Calibrated_VAT"]
                                      
                                # Policy_Gap        
                                        Export_Main_Results$Simulation[Export_Main_Results$variable == "Policy_Gap"] <-
                                          Export_Main_Results$Actual[Export_Main_Results$variable == "Policy_Gap"] -
                                          Export_Main_Results$Difference[Export_Main_Results$variable == "Calibrated_VAT"]
                                        
                                        
                                # Total_VAT_Gap_Pct
                                        Export_Main_Results$Simulation[Export_Main_Results$variable == "Total_VAT_Gap_Pct"] <-
                                          ((Export_Main_Results$Simulation[Export_Main_Results$variable == "Total_VAT_Gap"] /
                                          Export_Main_Results$Simulation[Export_Main_Results$variable == "Calibrated_VAT"])*100)
                                        
                                    
                                # Policy_Gap_Pct
                                        Export_Main_Results$Simulation[Export_Main_Results$variable == "Policy_Gap_Pct"] <-
                                          ((Export_Main_Results$Simulation[Export_Main_Results$variable == "Policy_Gap"] /
                                              Export_Main_Results$Simulation[Export_Main_Results$variable == "Calibrated_VAT"])*100)   
                                        
                                # Rounding
                                        
                                        Export_Main_Results$Actual<-round(Export_Main_Results$Actual ,1)
                                        Export_Main_Results$Simulation<-round(Export_Main_Results$Simulation ,1)
                                
                                        
                                        
                                        Export_Main_Results<-Export_Main_Results%>%
                                        dplyr::mutate(Difference= Simulation-Actual,
                                                      "Simulation as % of GDP"=((abs(Difference)/GDP_2022*100)),
                                                      Pct_Change=((Simulation/Actual)*100-100)
                                        )
                                        
                                        
                                        # Filter table
                                       # Export_Main_Results <- subset(Export_Main_Results, !grepl("Pct$|Factor$|Actual_VAT$", variable))
                                        
                                        # new
                                        Export_Main_Results <- subset(Export_Main_Results, !grepl("Factor$|Actual_VAT$", variable))
                                        
                                      
                                        # Assuming your data frame is called filtered_df
                                        # Export_Main_Results <- Export_Main_Results %>%
                                        #   mutate(variable = ifelse(!grepl("Rate$|Efficiency$", variable) | variable == "",
                                        #                            paste0(variable, " (in LCU Billions)"),
                                        #                            variable))
                                        
                                        
                                        Export_Main_Results <- Export_Main_Results %>%
                                          mutate(variable = ifelse(variable %in% c("VAT_Benchmark_Rate", "C_Efficiency"),
                                                                   variable,
                                                                   paste0(variable, " (in LCU Millions)")))

                                        
                                        # Export_Main_Results[7,1]<-c("VAT_Benchmark_Rate (in %)")
                                        # Export_Main_Results[8,1]<-c("C_Efficiency")
                                        
                                        # new
                                        Export_Main_Results[5,1]<-c("Total_VAT_Gap_Pct  (in %)")
                                        Export_Main_Results[7,1]<-c("Policy_Gap_Pct (in %)")
                                        Export_Main_Results[9,1]<-c("Compliance_Gap_Pct (in %)")
                                       
                                        Export_Main_Results[10,1]<-c("VAT_Benchmark_Rate (in %)")
                                        Export_Main_Results[11,1]<-c("C_Efficiency")
                                        
                                        
                                       
                                        
                                        # Rename columns
                                        
                                        Export_Main_Results<- Export_Main_Results%>%
                                          dplyr:: rename(
                                            "Difference(Simulation-Actual)"="Difference",
                                            "Difference  as % of GDP"="Simulation as % of GDP",
                                            "Pct_Change(Simulation/Actual)"="Pct_Change"
                                          )
                                        
                                        

                              
                              # Rounding 
                              
                              Export_Main_Results$Simulation<-round(Export_Main_Results$Simulation,1)
                              Export_Main_Results$`Difference(Simulation-Actual)`<-round(Export_Main_Results$`Difference(Simulation-Actual)`,1)
                              Export_Main_Results$`Difference  as % of GDP`<-round(Export_Main_Results$`Difference  as % of GDP`,1)
                              Export_Main_Results$`Pct_Change(Simulation/Actual)`<-round(Export_Main_Results$`Pct_Change(Simulation/Actual)`,1)
                              
                              
                              #Export_Main_Results[8,6]<-c("NA")
                              
                          # 3.2 Simulation results --------------------------------------------------

                          
                          Simulation_Results<-Results$Simulation
                          
                          
                          Revenue_VAT_TOTAL<-CPA_PRODUCTS$Est_Rev%>%
                            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_Revenues_from_Intermediate_Inputs,Final_Demand_HH,Final_Demand_NPISH,Final_Demand_Government,Final_Demand_Total)
                          
                          # Without text column
                          Revenue_VAT_TOTAL[3:7]<-Revenue_VAT_TOTAL[3:7]*Locked_Calibration_Factor
                          
                          #  Result from tax expenditures estimation
                          # Result 1
                          # NEW
                          Simulation_Results_1<-Results_1$Simulation
                          #Simulation_Results_1_te<-Results_1$VAT_Gap$Policy_Gap.M_of_LCU
                          Simulation_Results_1_te<-Results_1$Simulation$Simulated_Change_in_Revenues.M_of_LCU 
                          
                          
                          # Est_Rev1<-CPA_PRODUCTS_1$Est_Rev%>%
                          #   dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                          
                          Est_Rev1<-CPA_PRODUCTS$Est_Rev%>%
                            dplyr::arrange(PRODUCT_INDUSTRY_CODE)

                          
                          # Export BU
                          
                          Est_Rev_BU<-CPA_PRODUCTS_BU$Est_Rev%>%
                            dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                          
                          revenue_vat_total_bu<-CPA_PRODUCTS_BU$Est_Rev%>%
                            dplyr::select(PRODUCT_INDUSTRY_CODE,PRODUCT_INDUSTRY_NAME,Total_Revenues_from_Intermediate_Inputs,Final_Demand_HH,Final_Demand_NPISH,Final_Demand_Government,Final_Demand_Total)%>%
                            dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                          
                          # Without text column
                          revenue_vat_total_bu[3:7]<-revenue_vat_total_bu[3:7]*Locked_Calibration_Factor
                          
                          
                          # TE PART
                            Est_Rev_BU_TE<-CPA_PRODUCTS_BU$BM_Rev%>%
                            dplyr::arrange(PRODUCT_INDUSTRY_CODE)
                            
                            
                        # Preparation other tables for export
                            
                            Supply_export<-CPA_PRODUCTS$Supply
                            Use_Purchaser_export<-CPA_PRODUCTS$Use_Purchaser
                          
                          
                          
                            
})    
