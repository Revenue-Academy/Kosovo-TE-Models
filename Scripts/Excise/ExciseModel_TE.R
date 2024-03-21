' Data preparation,estimation of tax expenditures and preparation of data for charts
'
# options(warn = -1)

suppressMessages({

library(tidyverse)
options(scipen=999)
  
# I. Estimation of tax expenditures for excise duties ----------------------------------------------------------
        # 1.Import and Processing of data ------------------------------------------------------
                    # Change the column names EN
                    colnames(Import_Excise_Data)[1:11] <- c("Year","Month","TradePolicy","Countries","Code_Description","Quantity","Value","Netweight","CustomsRevenue","ExciseRevenue","VAT_Revenue")
                    
                    # Split the Code_Description column into two parts using "-"
                    split_columns_hs <- strsplit(Import_Excise_Data$Code_Description, "-")
                    
                    # Split the Code_Description column into two parts using "-"
                    # Create new columns with the split parts
                    Import_Excise_Data$HS_code <- sapply(split_columns_hs, `[`, 1)
                    
                    
                    # Extract the second part of the split using "[, 2]"
                    Import_Excise_Data$Description <- sapply(split_columns_hs, `[`, 2)
                    
                    # Split the Code for countries column into two parts using "-"
                    split_columns_countries <- strsplit(Import_Excise_Data$Countries, "-")
                    Import_Excise_Data$iso2c <- sapply(split_columns_countries, `[`, 1)
                    
                    Import_Excise_Data<-Import_Excise_Data%>%
                      dplyr::mutate(HS_code = trimws(HS_code, which = "both"))
                    
                   # Import_Excise_Data$Code_Description<-NULL
                    Import_Excise_Data$TradePolicy<-NULL
                    Import_Excise_Data$Countries<-NULL
            
                    
                    Import_Excise_Data<-Import_Excise_Data%>%
                      dplyr::select("HS_code","Description",
                                    "iso2c","Month","Year","Quantity","Value","Netweight","CustomsRevenue","ExciseRevenue","VAT_Revenue")
                   
                    
                    actual_year_simulation <- unique(Import_Excise_Data$Year)
                    
                    # Trim data
                    Import_Excise_Data<-Import_Excise_Data%>%
                      dplyr::mutate(iso2c = trimws(iso2c, which = "both"))
            
                    
                   
                    
                  # Estimation of tax expenditures 
                   CustomsDuties_base<-Import_Excise_Data%>%
                                                  dplyr::select(HS_code,Description,iso2c,Month,Year,Value,Quantity,Netweight,CustomsRevenue,ExciseRevenue,VAT_Revenue)%>%
                                                  dplyr::mutate(Effective_VAT_rate=round(VAT_Revenue/(Value+ExciseRevenue+CustomsRevenue),2),
                                                                Effective_Customs_rate=round(CustomsRevenue/(Value),2),
                                                                Effective_Excise_rate=round((ExciseRevenue)/Quantity,3)
                                                                )
                  
                   # Merging with GeoDimension
                    CustomsDuties_base<-left_join(CustomsDuties_base,GeoDimension,by =c("iso2c"))
        
                    # Replace NA values in CustomsDuties_TE with "FreeTradeAgreements" where NoFreeTradeAgreement is NA
                    CustomsDuties_base$FreeTradeAgreements[is.na(CustomsDuties_base$FreeTradeAgreements)] <- "NoFreeTradeAgreement"
                    CustomsDuties_base$HS_code_s <-gsub('(.{4})', '\\1 ', CustomsDuties_base$HS_code)
                   
                    rm(split_columns_hs,split_columns_countries)
            
        # 2. Adding FreeTrade agreements --------------------------------------------

            TreatmentOfGoods<-data.frame(FreeTradeAgreements=c("EU27","CEFTA","NoFreeTradeAgreement","TR","GBR"),
                                                 Treatment=c("Preferential","Preferential","NonPreferential",
                                                             "Preferential","Preferential"))
                    
            CustomsDuties_base<-left_join(CustomsDuties_base,TreatmentOfGoods,by =c("FreeTradeAgreements"))
            
            
            CustomsDuties_base <- CustomsDuties_base %>%
              mutate(
                FreeTradeAgreements = ifelse(iso3c == 'MNE', 'CEFTA', FreeTradeAgreements),
                Treatment = ifelse(iso3c == 'MNE', 'Preferential', Treatment)
              )
            
            # Assuming your data frame is named CustomsDuties_base
            CustomsDuties_base$FreeTradeAgreements <- ifelse(is.na(CustomsDuties_base$FreeTradeAgreements), 'NoFreeTradeAgreement', CustomsDuties_base$FreeTradeAgreements)
            CustomsDuties_base$Treatment <- ifelse(is.na(CustomsDuties_base$Treatment), 'NonPreferential', CustomsDuties_base$Treatment)
            

        # 3.Estimation of Tax Expenditures for Excise duties -----------------------
                # 3.1 Countries -----------------------------------------------------------
    
                        CustomsDuties_TE_agg_countries<-CustomsDuties_base%>%
                          dplyr::group_by(HS_code,HS_code_s,iso2c,iso3c,countries)%>% #Treatment
                         # dplyr::filter(Treatment=="NonPreferential")%>%
                          dplyr::summarise(Value=sum(Value,na.rm = TRUE),
                                           Quantity=sum(Quantity,na.rm = TRUE),
                                           Netweight=sum(Netweight,na.rm = TRUE),
                                           CustomsRevenue=sum(CustomsRevenue,na.rm = TRUE),
                                           ExciseRevenue=sum(ExciseRevenue,na.rm = TRUE),
                                           VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE))

                        
                        CustomsDuties_TE_agg_countries$HS_code<-NULL
                        CustomsDuties_TE_agg_countries$HS_code_s<-NULL

                # 3.2 Harmonized System-HS  --------------------------------------------------------------------
    
                      CustomsDuties_TE_agg_HS<-CustomsDuties_base%>%
                        dplyr::group_by(HS_code,HS_code_s)%>% #Treatment
                        #dplyr::filter(Treatment=="NonPreferential")%>%
                        dplyr::summarise(Value=sum(Value,na.rm = TRUE),
                                         Quantity=sum(Quantity,na.rm = TRUE),
                                         Netweight=sum(Netweight,na.rm = TRUE),
                                         CustomsRevenue=sum(CustomsRevenue,na.rm = TRUE),
                                         ExciseRevenue=sum(ExciseRevenue,na.rm = TRUE),
                                         VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE),
                                         Effective_Excise_rate=round((ExciseRevenue)/Quantity,3)
                                         ) 

                      
    
                        # Adding desegregation by HS codes
                        CustomsDuties_TE_agg_HS <- mutate(CustomsDuties_TE_agg_HS,
                                                       Chapter = substr(HS_code_s, 1, 2),
                                                       Four_digit = substr(HS_code_s, 1, 4),
                                                       Six_digit = substr(HS_code_s, 1, 7),
                                                       Eight_digit = paste0(substr(HS_code_s, 1, 4),
                                                                            "",
                                                                            substr(HS_code_s, 5, 7),
                                                                            " ",
                                                                            substr(HS_code_s, 8, 9)))
    
                      
                        
    
        # 4. Data Sub-setting-------------------------------------------------------
            # 4.1 Fuels ------------------------------------------------------------------------

                              # Subset Chapter 27
                              HS_Chapter_27<-CustomsDuties_TE_agg_HS %>%
                                filter(Chapter == '27')
                        
                        
                              HS_Chapter_27<-HS_Chapter_27%>%
                                dplyr::filter(ExciseRevenue>0)
                              
                              HS_Chapter_27$Subdataset<-c(" ") 
                        
                        
                              # Subset 1
                              HS_Chapter_27 <- HS_Chapter_27 %>%
                                dplyr::mutate(Subdataset = case_when(
                                  Eight_digit == "2710 19 43" ~ 'EURO DIESEL',
                                  Eight_digit == "2710 12 45" ~ 'EUROSUPER BC 95',
                                  Eight_digit == "2710 12 49" ~ 'EUROSUPER BS 100',
                                  Eight_digit == "2711 12 94" ~ 'LPG PROPANE',
                                  Eight_digit == "2710 19 81" ~ 'LUBRICATING OILS', # MOTOR OIL
                                  Eight_digit == "2711 13 97" ~ 'LPG BUTANE',
                                  Eight_digit == "2710 19 99" ~ 'LUBRICATING OILS', # Other lubricating oils
                                  Eight_digit == "2710 19 67" ~ 'HEAVY OILS', # MAZUT M-1
                                  Eight_digit == "2710 19 83" ~ 'LUBRICATING OILS', #Hydraulic oils
                                  Eight_digit == "2710 19 87" ~ 'LUBRICATING OILS', #MOTOR GEAR OIL
                                  Eight_digit == "2711 12 97" ~ 'LPG PROPANE',
                                  Eight_digit == "2711 12 11" ~ 'LPG PROPANE',
                                  Eight_digit == "2711 13 91" ~ 'LPG BUTANE',
                                  Eight_digit == "2710 19 93" ~ 'LUBRICATING OILS',#Electrical insulating oils
                                  Eight_digit == "2711 12 19" ~ 'LPG PROPANE',
                                  Eight_digit == "2710 19 47" ~ 'OTHER',
                                  Eight_digit == "2707 30 00" ~ 'LUBRICATING OILS',#Oils and other products of the distillation
                                  Eight_digit == "2710 12 31" ~ 'AVIATION GASOLINE',
                                  Eight_digit == "2710 12 90" ~ 'AVIATION GASOLINE',
                                  TRUE ~ 'OTHER'
                                ))
                              
                              # Subset 2 - Cyclic and Acyclic hydrocarbons- ORGANIC CHEMICALS
                               CyclicAcyclicHydrocarbons <- CustomsDuties_TE_agg_HS %>%
                                dplyr::filter(Four_digit %in% c('2901','2902'))
                               
                               CyclicAcyclicHydrocarbons$Subdataset<-c("CHEMICAL PRODUCTS")
                               
                               # Subset 3  -Other which are used for the same purposes as mineral oils-MISCELLANEOUS CHEMICAL PRODUCTS
                                   # Organic composite solvents and solvents
                                   # Alkylbenzenes and mixed Alkylnaphthalenes
                               
                                Other_Ch_38 <- CustomsDuties_TE_agg_HS %>%
                                 dplyr::filter(Four_digit %in% c('3811','3814','3817'))
                                
                                Other_Ch_38$Subdataset<-c("CHEMICAL PRODUCTS")
                                
                               #  Merging subsets
                                Fuel_tbl <- bind_rows(HS_Chapter_27,CyclicAcyclicHydrocarbons,Other_Ch_38)
                              
                              #  Removing Nan from effective tax rates
                                Fuel_tbl$Effective_Excise_rate<-ifelse(Fuel_tbl$Quantity==0,0,Fuel_tbl$Effective_Excise_rate)
                              
                              # Filter dataset with excise
                                Fuel_tbl<-Fuel_tbl%>%
                                dplyr::filter(ExciseRevenue>0)
                              
                                Fuel_tbl$DataSet<-c("Fuels")
                              
                                
                                Fuel_tbl$Quantity_HL<-as.numeric(0)
                                Fuel_tbl$PotentialExcise<-as.numeric(0)
                                
                                # Fuel_tbl_subset<-Fuel_tbl%>%
                                #   select(Eight_digit,Subdataset,Effective_Excise_rate)
                                #  View(Fuel_tbl_subset)
                                
                                # view(Fuel_tbl)
                             
                                
                                # 
                                # FuelSubset<-Fuel_tbl%>%
                                #   ungroup()%>%
                                #   dplyr::select(Subdataset,Value,Quantity,Netweight,CustomsRevenue,ExciseRevenue)%>%
                                #   dplyr::group_by(Subdataset)%>%
                                #   dplyr::summarise(
                                #     Value=sum(Value,na.rm = TRUE),
                                #     Quantity=sum(Quantity,na.rm = TRUE),
                                #     Netweight=sum(Netweight,na.rm = TRUE),
                                #     CustomsRevenue=sum(CustomsRevenue,na.rm = TRUE),
                                #     ExciseRevenue=sum(ExciseRevenue,na.rm = TRUE),
                                #     
                                #     
                                #   )


            # 4.1a Adding emission factors -------------------------------------------------

                                # Create a data frame
                                EmmisionFactors <- data.frame(Subdataset =  c("AVIATION GASOLINE", "EURO DIESEL", "EUROSUPER BC 95", "EUROSUPER BS 100", "HEAVY OILS", "LPG BUTANE", "LPG PROPANE"),
                                                   CO2_per_1000_units = c(2.54, 2.68, 2.29, 2.29, 2.83, 2.98, 2.98),
                                                   StatutoryExcisePerLiter = c(0.385, 0.360, 0.385, 0.385, 0.250, 0.100, 0.100)
                                                   
                                                   )
                                
                                
                                
                                Fuel_tbl<-left_join(Fuel_tbl,EmmisionFactors, by=c("Subdataset"="Subdataset"))
                                         
                                
                                # Filter rows where CO2_per_1000_units is not NaN
                                Fuel_tbl <- Fuel_tbl[complete.cases(Fuel_tbl$CO2_per_1000_units), ]
                                
                                
                               # Adding calculation of TE with alternative approach
                                Fuel_tbl<-Fuel_tbl%>%
                                  dplyr::mutate(PotentialExcise=StatutoryExcisePerLiter*Quantity)
                                  
                                
                                # View(Fuel_tbl)
                                
                                
                                # sum(Fuel_tbl$PotentialExcise)
                                # 
                                # sum(Fuel_tbl$ExciseRevenue)
                                
                                
                                
                                # write.csv(Fuel_tbl,"Fuel_tbl2.csv")
                                
            # 4.2 Tobacco ---------------------------------------------------------------

                              ## 2401-Unmanufactured tobacco; tobacco refuse:
                              ## 2402-Cigars, cheroots, cigarillos and cigarettes, of tobacco or of tobacco substitutes
                              ## 2403-Other manufactured tobacco and manufactured tobacco substitutes; "homogenized" or "reconstituted" tobacco; tobacco extracts and essences:
                              ## 2404-Products containing tobacco, reconstituted tobacco, nicotine, or tobacco or nicotine substitutes, intended for inhalation without combustion; other nicotine containing products intended for the intake of nicotine into the human body
                              
                              
                              # Subset Chapter 24
                              HS_Chapter_24<-CustomsDuties_TE_agg_HS %>%
                                filter(Chapter == '24')
                              
                              HS_Chapter_24$Subdataset<-c("Tobacco") #<---- Ova ke treba detealno da se definira po tip na proizvodi ili po tarifen broj 
                              
                              # Removing Nan from effective tax rates
                              HS_Chapter_24$Effective_Excise_rate<-ifelse(HS_Chapter_24$Quantity==0,0,HS_Chapter_24$Effective_Excise_rate)
                              
                              Tobacco_tbl<-HS_Chapter_24%>%
                                dplyr::filter(ExciseRevenue>0)
                              
                              Tobacco_tbl$DataSet<-c("Tobacco")
                              
                              Tobacco_tbl$Quantity_HL<-as.numeric(0)
                              Tobacco_tbl$PotentialExcise<-as.numeric(0)
                              
                              
                              Tobacco_tbl <- Tobacco_tbl %>%
                                dplyr::mutate(Subdataset = case_when(
                                  Eight_digit == "2402 10 00" ~ 'CIGARS AND CIGARILLOS',
                                  Eight_digit == "2402 20 10" ~ 'CIGARETTES',
                                  Eight_digit == "2402 20 90" ~ 'CIGARETTES',
                                  TRUE ~ 'TOBACCO'
                                ))
                              
                             # View(Tobacco_tbl)
                              
                              # Calculation of base for calculation
                              # Conversion factors, (equivalent of grams tobacco):
                              
                                      # . 1 stick cigarette = 0.7g
                                      # . 1 cigarillo = 3 g
                                      # . 1 cigar = 20 g
                                      # . 1 ml (E-cig) = 1 g # Electronic cigarettes and similar personal electric vaporizing devices-Contains sets together with cartridges for smoking
                                      # . Fine cut and HNB are with the same weight as is declared converted in grams
                              
                              
                              Tobacco_tbl$ExciseRate<-as.numeric(35)

                              Tobacco_tbl <- Tobacco_tbl %>%
                                mutate(ExciseRate = ifelse(Eight_digit == "2402 10 00", 1.1, ExciseRate))
                              
                              
                              Tobacco_tbl <- Tobacco_tbl %>%
                                mutate(ExciseRate = ifelse(Eight_digit == "2402 20 90", 51, ExciseRate))
                              

                              Tobacco_tbl <- Tobacco_tbl %>%
                                dplyr::mutate(
                                  ConventionalUnit = ExciseRevenue / ExciseRate, # For Cigars, cigarillos and cigarettes is 1000 sticks, for fine-cut is 1 KG
                                  ExciseEstimation = ConventionalUnit * ExciseRate,
                                  VolumeBySticks = case_when(
                                                  Eight_digit == "2402 10 00" ~ Quantity * 1000,
                                                  Eight_digit == "2402 20 90" ~ ConventionalUnit * 1000,
                                                  TRUE ~ Quantity * 1000
                                                ),
                                  # Base for calculation of TE's
                                  Base_EquivalentOfGramsTobacco = case_when(
                                                  Eight_digit == "2402 10 00" ~ VolumeBySticks * 11.5, 
                                                  Eight_digit == "2402 20 90" ~ VolumeBySticks * 0.7,
                                                  TRUE ~ VolumeBySticks
                                                ),
                                  # Equivalent of tobacco in grams
                                  ExciseByEquivalentGramsTobacco=ExciseEstimation/Base_EquivalentOfGramsTobacco,
                                  ExciseByEquivalentGramsTobacco_1000=ExciseByEquivalentGramsTobacco*1000,
                                  CrossCheckExise=Base_EquivalentOfGramsTobacco*ExciseByEquivalentGramsTobacco
                                  # TE_TEST=Base_EquivalentOfGramsTobacco/1000*72.857143,
                                  # TE=TE_TEST-CrossCheckExise
                                )
                              
                              
                              Tobacco_tbl <- Tobacco_tbl %>%
                                dplyr::filter(Subdataset %in% c("CIGARETTES", "TOBACCO"))
                              
                             
                              
                                # View(Tobacco_tbl)
                              
                              # Tobacco_tbl_subset<-Tobacco_tbl%>%
                              #   select(Subdataset,Quantity,ExciseRevenue,ExciseRate,ConventionalUnit,VolumeBySticks,Base_EquivalentOfGramsTobacco,
                              #          ExciseByEquivalentGramsTobacco,ExciseByEquivalentGramsTobacco_1000,CrossCheckExise
                              #          )
                              # 
                              # 
                              # write.csv(Tobacco_tbl_subset,"Tobacco_tbl_subset.csv")
                              # 
            # 4.3 Alcohol -----------------------------------------------------------          
                  # 4.3.1 Beer --------------------------------------------------------------------
                         
                                              BeerQuantity <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2203)
                                              
                              
                                              # Adding columns
                                              BeerQuantity$Subdataset<-c("BEER")
                                              # Adding excis rate
                                              BeerQuantity$ExciseRate<-as.numeric(800)
                                           

                  # 4.3.2 Sugar-sweetened beverages(SSB) ------------------------------------------------------------------
# Do tuka !!! Ova da se vnese 
                              # # # Non Alcoholic Beer is included here
                              SSB_Quantity <- CustomsDuties_TE_agg_HS %>%
                                filter(Four_digit == 2202)

                                SSB_Quantity$Subdataset<-c("SSB")

                                              
                  # 4.3.3 Wine of fresh grapes ----------------------------------------------------
                          
                           # Wine of fresh grapes, including fortified wines; grape must other than that of heading 2009)                    
                                              WineQuantity <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2204)
                                              
                                              WineQuantity$Subdataset<-c("WINE")
                                              # Adding excis rate
                                              WineQuantity$ExciseRate<-as.numeric(500)
                                           
                                              # View(WineQuantity)
                                              # sum(WineQuantity$Quantity)/1e06
                                             
                          
                          
                  # 4.3.4 Vermouth ---------------------------------------------------
                          
                                             # Vermouth and other wine of fresh grapes flavoured with plants or aromatic substances
                                              VermouthQuantity <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2205)

                                              
                                              # View(VermouthQuantity)
                                              # sum(VermouthQuantity$Quantity)/1e06
                                              VermouthQuantity$Subdataset<-c("VERMOUTH")
                                              
                                              # Adding excis rate
                                              VermouthQuantity$ExciseRate<-as.numeric(500)
                          
                          
                  # 4.3.5 Other fermented beverages --------------------------------------------------
                          
                                              # Other fermented beverages (for example, cider, perry, mead); mixtures of fermented beverages and mixtures of fermented beverages and non-alcoholic beverages, not elsewhere specified or included
                                              
                                              OtherFermentedBeveragesQuantity  <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2206)
                                              
                                              OtherFermentedBeveragesQuantity$Subdataset<-c("OTHER FERMENTED BEVERAGES")
                                              
                                              # Adding excis rate
                                              OtherFermentedBeveragesQuantity$ExciseRate<-as.numeric(500)


                  # 4.3.5a Undenatured ethyl alcohol of an alcoholic strength by volume of 80 % vol or higher; ethyl alcohol and other spirits, denatured, of any strength:-----------------------------------------

                                              # Not included ! With 80 strength by volume of 80 % vol or higher is not for driniking
                                              # 
                                              # UndenaturedethylAlcoholQuantity  <- CustomsDuties_TE_agg_HS %>%
                                              #   filter(Four_digit == 2207)
                                              # 
                                              # UndenaturedethylAlcoholQuantity$Subdataset<-c("UndenaturedethylAlcoholQuantity")
                                              # 
                                              
                                              
                  # 4.3.6 Undenatured ethyl alcohol ---------------------------------------------
                          
                                  # Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80 % vol; spirits, liqueurs and other spirituous beverages
                                  UndenaturedEthylAlcoholQuantity  <- CustomsDuties_TE_agg_HS %>%
                                  dplyr::filter(Four_digit == 2208)
                                                        
                                                        
                                   # View(UndenaturedEthylAlcoholQuantity)
                                   #  sum(UndenaturedEthylAlcoholQuantity$Quantity)/1e06
                                    UndenaturedEthylAlcoholQuantity$Subdataset<-c("ALCHOLIC BEVERAGE")
                                    
                                    # Adding excis rate
                                    UndenaturedEthylAlcoholQuantity$ExciseRate<-as.numeric(800)

                  # 4.3.7 Merging data --------------------------------------------------

                    # SSB is not included in the dataset !!!
                          
                   # AlcoholQuantity <- bind_rows(BeerQuantity,WineQuantity,VermouthQuantity,OtherFermentedBeveragesQuantity,UndenaturedEthylAlcoholQuantity)
                      
                    AlcoholQuantity <- bind_rows(BeerQuantity,WineQuantity,VermouthQuantity,OtherFermentedBeveragesQuantity,UndenaturedEthylAlcoholQuantity,SSB_Quantity)
                                              
                                                                  
                    # Removing Nan from effective tax rates
                    AlcoholQuantity$Effective_Excise_rate<-ifelse(AlcoholQuantity$Quantity==0,0,AlcoholQuantity$Effective_Excise_rate)
                                     
                    
                             
                    Alcohol_tbl<-AlcoholQuantity%>%
                                                dplyr::filter(ExciseRevenue>0)                    
                              
                    Alcohol_tbl$DataSet<-c("Alcohol")
                    
                    # Calculation of abs. alc 100%
                    
                    Alcohol_tbl<-Alcohol_tbl%>%
                      dplyr::mutate(Quantity_HL=Quantity/100, # Conversion in HL
                                   PotentialExcise=Quantity_HL*ExciseRate,
                                   Alc_Content=ExciseRevenue/PotentialExcise,
                                   Pure_Alc=Quantity_HL*Alc_Content
                                  # test=ExciseRate*Pure_Alc
                                   )
                      
                    Alcohol_tbl$Quantity_HL<-NULL
                    Alcohol_tbl$PotentialExcise<-NULL
                    
                    # Cross check
                    # sum(Alcohol_tbl$ExciseRevenue)
                    # sum(Alcohol_tbl$test)
                    
                    Alcohol_tbl$Alc_Content<-round(Alcohol_tbl$Alc_Content,2)
                              
                    
                    # Assuming your data frame is named Alcohol_tbl
                    Alcohol_tbl <- Alcohol_tbl %>%
                      mutate(DataSet  = case_when(
                        grepl("SSB", Subdataset, ignore.case = TRUE) ~ "SSB",
                        TRUE ~ DataSet  # If not containing 'SSB', keep the original value
                      ))
                    
                    
                    #write.csv(Alcohol_tbl,"Alcohol_tbl.csv")
                    
            #View(Alcohol_tbl)
                    
                    Alcohol_tbl_subset_export<-Alcohol_tbl%>%
                      select(Subdataset,ExciseRate,Alc_Content,Pure_Alc,Quantity,ExciseRevenue)%>%
                      group_by(Subdataset,ExciseRate,Alc_Content)%>%
                      summarise(Pure_Alc=sum(Pure_Alc),
                                Quantity=sum(Quantity),
                                ExciseRevenue=sum(ExciseRevenue)

                                )

                   # View(Alcohol_tbl_subset_export)
                    # 
                    # write.csv(Alcohol_tbl_subset_export,"Alcohol_tbl_subset_export.csv")
                    
                   
           # 5.Cars ------------------------------------------------------------------

                    CarsSet <- CustomsDuties_TE_agg_HS %>%
                             dplyr::filter(Four_digit %in% c('8703'))

                    
                    Cars_tbl<-left_join(CarsSet,Cars_ExciseRates, by=c("HS_code_s"="HS"))
                    
                    
                    
                    
                    Cars_tbl$Subdataset<-c("Cars")
                    Cars_tbl$DataSet<-c("Cars")


                    Cars_tbl$Quantity_HL<-as.numeric(0)
                    Cars_tbl$PotentialExcise<-as.numeric(0)

                    
                   
                    
                    
                    
                    
                    # Cars_tbl<-CarsSet%>%
                    #   dplyr::filter(ExciseRevenue>0)
                    #
           

                   # View(Cars_tbl)
                                    
           # 6.Merging Data Sets and estimation of TE'S -------------------------------------------------------
                  
                    #ExciseFinal_tbl <- bind_rows(Fuel_tbl, Tobacco_tbl,Alcohol_tbl,Cars_tbl)
                    ExciseFinal_tbl <- bind_rows(Fuel_tbl, Tobacco_tbl,Alcohol_tbl,Cars_tbl)
                    ExciseFinal_tbl$ExciseRate[is.nan(ExciseFinal_tbl$ExciseRate)]<-0
                    ExciseFinal_tbl$Alc_Content[is.nan(ExciseFinal_tbl$Alc_Content)]<-0
                    ExciseFinal_tbl$Pure_Alc[is.nan( ExciseFinal_tbl$Pure_Alc)]<-0
                   # Benchmark_ExciseFuels<-1
                    
                    calculate_benchmark_fun <- function(data) {
                      data %>%
                        mutate(
                          result = case_when(
                            #DataSet == "Fuels" ~ Quantity * Benchmark_ExciseFuels,
                            Subdataset == "AVIATION GASOLINE" ~ Quantity * Benchmark_AVIATION_GASOLINE,
                            Subdataset == "EURO DIESEL" ~ Quantity * Benchmark_EURO_DIESEL,
                            Subdataset == "EUROSUPER BC 95" ~ Quantity * Benchmark_EUROSUPER_BC_95,
                            Subdataset == "EUROSUPER BS 100" ~ Quantity * Benchmark_EUROSUPER_BS_100,
                            Subdataset == "HEAVY OILS" ~ Quantity * Benchmark_HEAVY_OILS,
                            Subdataset == "LPG BUTANE" ~ Quantity * Benchmark_LPG_BUTANE,
                            Subdataset == "LPG PROPANE" ~ Quantity * Benchmark_LPG_PROPANE,
                            DataSet == "Tobacco" ~ (Base_EquivalentOfGramsTobacco / 1000) * Benchmark_ExciseTobacco,
                            DataSet == "Alcohol" ~ Pure_Alc * Benchmark_ExciseAlcohol,
                            DataSet == "Cars" ~ Quantity * ExciseRate
                          )
                        )
                    }
                    
                  
                    # Apply the function 
                    Estimation_TE <- calculate_benchmark_fun(ExciseFinal_tbl)%>%
                      dplyr::rename("Excise_BenchmarkRevenue"="result")%>%
                      dplyr::mutate(Excise_TE=Excise_BenchmarkRevenue-ExciseRevenue)
                    
                    # Remove NaN
                    Estimation_TE[is.na(Estimation_TE)] <- 0
                    
                    # Define a function to round up two columns
                    round_up_columns <- function(data, columns) {
                      data %>%
                        mutate(across(all_of(columns), ~ceiling(.)))
                    }
                    
                    # Apply the function to round up 
                    Estimation_TE <- round_up_columns(Estimation_TE, c("Excise_BenchmarkRevenue", "Excise_TE"))
                    
                   # View(Estimation_TE)
                    
                    #write.csv(Estimation_TE,"Estimation_TE.csv")

# II.Preparing data and charts --------------------------------------------
        # 1.1 Historic Data Tab -----------------------------------------------------------
            #  1.1.1 Excise_Pct Of GDP ---------------------------------------
        
      MacroFiscalData_plt <- MacroFiscalData[!is.na(MacroFiscalData$`Taxes on imports excluding VAT and duties`), ]
                    
                    MacroFiscalData_plt$Year<-as.integer(MacroFiscalData_plt$Year)
                    
                    MacroFiscalData_plt<-MacroFiscalData_plt%>%
                     # filter(Year <=  actual_year_simulation)
                    filter(Year <  actual_year_simulation+1)
                   
                    df_plt <- MacroFiscalData_plt %>%
                      dplyr::select(Year, GDP, Excise_PctOfGDP) %>%
                      mutate(Excise_PctOfGDP = round(Excise_PctOfGDP, 1))
                            
                            
                            Excise_PctOfGDP <- plot_ly(df_plt)
                            Excise_PctOfGDP <- Excise_PctOfGDP %>% add_trace(x = ~Year, y = ~GDP, type = 'bar', name = 'GDP')
                            Excise_PctOfGDP <- Excise_PctOfGDP %>% add_trace(x = ~Year, y = ~Excise_PctOfGDP, type = 'scatter', mode = 'lines+markers', name = 'Share of excise revenue in GDP ',
                                                                             yaxis = 'y2', line = list(dash = 'dot', color = "#FFA500", width = 6))
                            
                            Excise_PctOfGDP <- Excise_PctOfGDP %>% layout(title = paste("Nominal GDP and share of excise revenues in GDP,2015-", actual_year_simulation),
                                                                           xaxis = list(title = ""),
                                                                           yaxis = list(side = 'left', title = 'In million LCU', showgrid = FALSE, zeroline = FALSE),
                                                                           yaxis2 = list(side = 'right', overlaying = "y", title = 'Percentage', showgrid = FALSE, zeroline = FALSE, font = list(size = 11)), 
                                                                           annotations = list(
                                                                             #x = -0.1, y = -0.056,
                                                                             x = 0.01, y = -0.056,
                                                                             text = "Source: National authorities",
                                                                             showarrow = FALSE,
                                                                             xref = 'paper',
                                                                             yref = 'paper',
                                                                             align = 'left'))
        
                            
        
            #  1.1.2 Structure Of Tax Revenues Nominal --------------------------------------------------------------
 
                            year_df <- MacroFiscalData_plt%>%
                              dplyr::select(Year, "VAT", "ImportDuties", "Taxes on imports excluding VAT and duties",
                                            "Taxes on products, except VAT and import taxes",
                                            "Other taxes on production", "Property income",
                                            "Current taxes on income, wealth, etc.")
                            
                            year_df$Year<-as.factor(year_df$Year)
                            year_df<-melt(year_df)
                            year_df$color <- factor(year_df$variable, labels =c( "orange","brown","forestgreen","red", "cyan","royalblue","blue"))
                            
                            Excise_StructureOfTaxRevenues_Nominal <- plot_ly(year_df, x = ~Year, y = ~value,
                                                                       type = 'bar',
                                                                       marker = list(color = ~color), name = ~variable) %>%
                                                                  layout(title = 'Structure of tax revenues',font = t_11,
                                                                         xaxis = list(title = ''),
                                                                         yaxis = list(title = ' '),
                                                                         annotations =
                                                                           list( x = 0, y = -0.05,
                                                                                 text = "Source:National authorities",#font = t_8,
                                                                                 showarrow = F,
                                                                                 xref='paper',
                                                                                 yref='paper',align='left'),barmode = 'stack')
        
            #  1.1.3 Structure Of Tax Revenues Percentage -------------------------------------------------------------------------
        
                            year_df<-group_by(year_df, Year) %>% mutate(Pct = value/sum(value))
                            
                            
                            Excise_StructureOfTaxRevenuesPct <- plot_ly(year_df, x = ~Year, y = ~Pct*100,
                                                                        type = 'bar',
                                                                        marker = list(color = ~color), name = ~variable) %>%
                                                                  layout(title = 'Structure of revenues in percentage, 2015-2022',font = t_11,
                                                                         xaxis = list(title = ''),
                                                                         yaxis = list(title = 'In percentage '),
                                                                         annotations =
                                                                           list( x = 0, y = -0.05,
                                                                                 text = "Source:National authorities",#font = t_8,
                                                                                 showarrow = F,
                                                                                 xref='paper',
                                                                                 yref='paper',align='left'),barmode = 'stack')

            #  1.1.4 Excise Goods Regular Import -------------------------------------------------------------------------
                        ExciseGoodRegulaImport<-Estimation_TE%>%
                              dplyr::select(DataSet,Value)%>%
                              dplyr::group_by(DataSet)%>%
                              dplyr::summarise(Value=sum(Value,na.rm = TRUE))
                            
                            # Factor the "Sections" column without ordering other strings
                            ExciseGoodRegulaImport$Chapter <- factor(ExciseGoodRegulaImport$DataSet)
                            
                            ExciseGoodRegulaImport$HS<-"Excise by types of goods "
                            
                                                               
                            Excise_RegularImport <- plot_ly(
                              data = ExciseGoodRegulaImport,
                              type = "treemap",
                              values = ~round(Value/1e06, 1),
                              labels = ~DataSet,
                              parents = ~HS,
                              name = " ",
                              text = ~DataSet,
                              textinfo = "label+value+percent parent"
                            ) %>%
                              layout(
                                title = paste("Structure of Regular Import by types of excise goods in LCU (Millions),", actual_year_simulation),
                                font = t_11,
                                annotations = list(
                                  x = 0,
                                  y = -0.05,
                                  xref = 'paper',
                                  yref = 'paper',
                                  text = "Source: National authorities",
                                  showarrow = FALSE,
                                  font = list(size = 11)
                                )
                              )
                            
                            
                            
                            
            #  1.1.5 Import Structure Excise -----------------------------------------------------------------
        
                           
                            ExciseRevenueStr<-Estimation_TE%>%
                                                      dplyr::select(DataSet,ExciseRevenue)%>%
                                                      dplyr::group_by(DataSet)%>%
                                                      dplyr::summarise(Value=sum(ExciseRevenue,na.rm = TRUE))
                            
                            
                            ExciseRevenueStr$DataSet <- factor(ExciseRevenueStr$DataSet)
                            ExciseRevenueStr$HS<-"Excise by types of goods "
                            Excise_RevenueStructure<-plot_ly(data =ExciseRevenueStr, type = "treemap", values = ~round(Value/1e06,1), labels = ~DataSet,
                                                         parents = ~HS,
                                                         name = " ",
                                                         text = ~DataSet   ,
                                                         textinfo="label+value+percent parent")%>%
                                                         #layout(title=paste("Structure of excise revenues by type of excise goods in LCU (Millions),", actual_year_simulation),font =t_11)
                                                            layout(
                                                              title = paste("Structure of Regular Imports by Types of Excise Goods in LCU (Millions),", actual_year_simulation),
                                                              font = t_11,
                                                              annotations = list(
                                                                x = 0,
                                                                y = -0.05,
                                                                xref = 'paper',
                                                                yref = 'paper',
                                                                text = "Source: National authorities",
                                                                showarrow = FALSE,
                                                                font = list(size = 11)
                                                              ))
                                                        
                            
        
            #  1.1.6 Structure of excise for mineral oils  ----------------------------------------
                    
                            MineralFuels_Structure<-Fuel_tbl%>%
                                    dplyr::select(Subdataset,ExciseRevenue)%>%
                                    dplyr::group_by(Subdataset)%>%
                                    dplyr::summarise(Value=sum(ExciseRevenue,na.rm = TRUE))
                                  
                            ExciseRevenueBasePie<-melt(MineralFuels_Structure)

                            Structure_Excise_MineralOils <- ExciseRevenueBasePie %>%
                                   plot_ly(labels = ~Subdataset, values = ~value)

                            Structure_Excise_MineralOils <- Structure_Excise_MineralOils %>% add_pie(hole = 0.6)
                            Structure_Excise_MineralOils <- Structure_Excise_MineralOils %>% layout(
                                  title = paste("Structure of Excise Revenues by Type of Mineral Oils,", actual_year_simulation),
                                  font = t_11,
                                  showlegend = TRUE,
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  annotations = list(
                                    x = 0, y = -0.05,
                                    #x = 0.2, y = -0.05,
                                    text = "Source: National authorities",
                                    showarrow = FALSE,
                                    xref = 'paper',
                                    yref = 'paper',
                                    align = 'left'
                                  ),
                                  font = t_8)
                            

            #  1.1.7 Structure of excise for tobacco products  -------------------------------

                            TobaccoProducts_Structure<-Tobacco_tbl%>%
                              dplyr::select(Subdataset,ExciseRevenue)%>%
                              dplyr::group_by(Subdataset)%>%
                              dplyr::summarise(Value=sum(ExciseRevenue,na.rm = TRUE))
                            
                            ExciseRevenueBasePie<-melt(TobaccoProducts_Structure)
                            
                            Structure_Excise_TobaccoProducts <- ExciseRevenueBasePie %>%
                              plot_ly(labels = ~Subdataset, values = ~value)
                            
                            Structure_Excise_TobaccoProducts <- Structure_Excise_TobaccoProducts %>% add_pie(hole = 0.6,rotation = 60)
                             Structure_Excise_TobaccoProducts <- Structure_Excise_TobaccoProducts %>% layout(
                              title = paste("Structure of Excise Revenues by Type of Tobacco Products,", actual_year_simulation),
                              font = t_11,
                              showlegend = TRUE,
                              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                              annotations = list(
                                x = 0, y = -0.05,
                                #x = 0.2, y = -0.05,
                                text = "Source: National authorities",
                                showarrow = FALSE,
                                xref = 'paper',
                                yref = 'paper',
                                align = 'left'
                              ),
                              font = t_8)
                            
                            
            #  1.1.8 Structure of excise for alcohol ----------------------------------

                            AlcoholProducts_Structure<-Alcohol_tbl%>%
                                    dplyr::filter(DataSet=='Alcohol')%>%
                                    dplyr::select(Subdataset,ExciseRevenue)%>%
                                    dplyr::group_by(Subdataset)%>%
                                    dplyr::summarise(Value=sum(ExciseRevenue,na.rm = TRUE))
                            
                            ExciseRevenueAlcoholProducts<-melt(AlcoholProducts_Structure)
                            
                            Structure_Excise_AlcoholProducts <- ExciseRevenueAlcoholProducts %>%
                              plot_ly(labels = ~Subdataset, values = ~value)
                            
                            Structure_Excise_AlcoholProducts <- Structure_Excise_AlcoholProducts %>% add_pie(hole = 0.6)
                            Structure_Excise_AlcoholProducts <- Structure_Excise_AlcoholProducts %>% layout(
                                                                title = paste("Structure of Excise Revenues by Type of Alcohol Products,", actual_year_simulation),
                                                                font = t_11,
                                                                showlegend = TRUE,
                                                                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                                annotations = list(
                                                                  x = 0, y = -0.05,
                                                                  #x = 0.2, y = -0.05,
                                                                  text = "Source: National authorities",
                                                                  showarrow = FALSE,
                                                                  xref = 'paper',
                                                                  yref = 'paper',
                                                                  align = 'left'
                                                                ),
                                                                font = t_8)
                            
                            
                            # 
                            # # Arrange in a facet format
                            # facet_plot <- subplot(
                            #   Structure_Excise_MineralOils, 
                            #   Structure_Excise_TobaccoProducts,
                            #   nrows = 1
                            # )
                            # 
                            # #facet_plot
                            
                            
            #  1.1.9 Structure of excise pure alcohol-----------------------------------------------

                            Alcohol_tbl_subset<-Alcohol_tbl%>%
                              dplyr::select(Subdataset,DataSet,Pure_Alc)

                            Alcohol_tbl_subset$Subdataset[Alcohol_tbl_subset$Subdataset == "OTHER FERMENTED BEVERAGES" | Alcohol_tbl_subset$Subdataset == "VERMOUTH"] <- "OTHER"

                            Alcohol_tbl_subset<-Alcohol_tbl_subset%>%
                              dplyr::group_by(Subdataset,DataSet)%>%
                              dplyr::summarise(Pure_Alc=sum(Pure_Alc,na.rm = TRUE))

                           # Alcohol_tbl_subset <- Alcohol_tbl_subset[order(Alcohol_tbl_subset$Category), ]

                            #old version
                            col.pal = c("ALCHOLIC BEVERAGE" = "red",
                                        "WINE" = "maroon",
                                        BEER  = "blue"
                                        )



                            # Alcohol_ChordPlot<-chordDiagram(Alcohol_tbl_subset,
                            #              grid.col = col.pal,
                            #              title(main = "Distribution of Pure Alcohol,by Excise products"))

                            # library(cowplot)
                            # #plot(runif(10))
                            # chordDiagram(Alcohol_tbl_subset,
                            #              grid.col = col.pal,
                            #              title(main = "Distribution of Pure Alcohol,by Excise products"))
                            # 
                            # Alcohol_ChordPlot <- recordPlot()
                            # #ggdraw(Alcohol_ChordPlot)
                            # 
                            # Alcohol_ChordPlot <- ggplotly(Alcohol_ChordPlot)
                            # 
                            # new version
                            # Da se proba ovaa varijanta https://r-graph-gallery.com/chord-diagram-interactive.html
                            
                            # Alcohol_tbl_subset<-as.matrix(Alcohol_tbl_subset)
        # 1.2 Tax Expenditures Tab -----------------------------------------------------
            # 1.2.1 TE's by Chapters ---------------------------------------------------------
                      Excise_TE_Chapters<-Estimation_TE%>%
                              dplyr::select(Chapter,Excise_TE)%>%
                              dplyr::group_by(Chapter)%>%
                              dplyr::summarise(Excise_TE=sum(Excise_TE,na.rm = TRUE))
        
                      Excise_TE_Chapters<-left_join(Excise_TE_Chapters,HS_Sections,by =c("Chapter"))%>%
                              dplyr::select(Chapter,Chapters_description,Excise_TE)%>%
                              dplyr::group_by(Chapter,Chapters_description)%>%
                              dplyr::summarise(Excise_TE=sum(Excise_TE,na.rm = TRUE))
        
                      # Factor the "Sections" column without ordering other strings
                      Excise_TE_Chapters$Chapter <- factor(Excise_TE_Chapters$Chapter)
        
                       
            # 1.2.2 TE's by WTO classification (MTN) Categories -----------------------------------------
                      Excise_TE_MTN<-Estimation_TE%>%
                        dplyr::select(Six_digit,Excise_TE)%>%
                        dplyr::group_by(Six_digit)%>%
                        dplyr::summarise(Excise_TE=sum(Excise_TE,na.rm = TRUE))
                      
                      
                      WTO_MTN_subset <- WTO_MTN %>%
                        filter(
                          (actual_year_simulation >= 2022 & str_detect(HS_year, ">2022")) |
                            (actual_year_simulation < 2022 & str_detect(HS_year, "<2022"))
                        )
                      
                      
                      
                      Excise_TE_MTN<-left_join(Excise_TE_MTN,WTO_MTN_subset,by =c("Six_digit"))%>%
                        dplyr::select(Product_group,Excise_TE)
                      
                      Excise_TE_MTN <- distinct(Excise_TE_MTN)
                      
                      Excise_TE_MTN<-Excise_TE_MTN%>%
                        dplyr::group_by(Product_group)%>%
                        dplyr::summarise(Excise_TE=sum(Excise_TE,na.rm = TRUE))
                      
                      Excise_TE_MTN$Product_group <- ifelse(is.na( Excise_TE_MTN$Product_group), "Other",  Excise_TE_MTN$Product_group)
                      
                      
                      
                      # Factor the "Sections" column without ordering other strings
                      Excise_TE_MTN$Product_group <- factor( Excise_TE_MTN$Product_group)
                      
        
                      
            # 1.2.3 TE's by Excise Product Categories --------------------------------------
                      
                      ExciseProducts_TE<-Estimation_TE%>%
                        dplyr::select(DataSet,ExciseRevenue)%>%
                        dplyr::group_by(DataSet)%>%
                        dplyr::summarise(Value=sum(ExciseRevenue,na.rm = TRUE))
                      
                      
                      ExciseProducts_TE<-melt(ExciseProducts_TE)
                      
                      ExciseProducts_TE$DataSet<- factor(ExciseProducts_TE$DataSet)
                      
                      Excise_ProductCategoriesNominal <- plot_ly(ExciseProducts_TE, x = ~DataSet , y = ~value, type = 'bar', text = ' ', hoverinfo = 'y+text', color = ~DataSet, colors = colors) %>% 
                                                        layout(
                                                          title = paste("Tax expenditures by Product Categories,", actual_year_simulation),
                                                          font = list(size = 11),
                                                          xaxis = list(title = ''),
                                                          yaxis = list(title = 'In LCU'),
                                                          #barmode = 'stack',  # Use 'stack' for multiple colors within a single bar
                                                          annotations = list(
                                                            x = 0, y = -0.056,
                                                            text = "Source: Calculations by WB staff based on data from National authorities",
                                                            showarrow = FALSE,
                                                            xref = 'paper',
                                                            yref = 'paper',
                                                            align = 'left'
                                                          ),
                                                          legend = list(orientation = 'v', x = 1.02, y = 0.5)
                                                        )
                      
            # 1.2.4 TE's by Chapters ---------------------------------------------------------       
                      
                      Chapters_HS1 <- plot_ly(Excise_TE_Chapters, x = ~reorder(Chapter, -Excise_TE), y = ~Excise_TE, type = 'bar', text = ' ', hoverinfo = 'y+text',#color = ~Treatment, colors = colors,
                                              hovertext = ~Chapters_description) %>%
                        layout(
                          title = paste("Distribution of Tax Expenditures Across HS Chapters,", actual_year_simulation),font = t_11,
                          font = list(size = 11),
                          xaxis = list(title = ''),
                          yaxis = list(title = 'In LCU'),
                          # barmode = 'stack',
                          annotations = list(
                            x = 0, y = -0.056,
                            text = "Source: Calculations by WB staff based on data from National authorities",
                            showarrow = FALSE,
                            xref = 'paper',
                            yref = 'paper',
                            align = 'left'
                          ),
                          #legend = list(orientation = 'h')
                          legend = list(orientation = 'v', x = 1.02, y = 0.5)
                        )
        
            # 1.2.5 TE's by WTO classification (MTN) Categories---------------------------------------------
                      ProductGroups_MTN <- plot_ly(
                        Excise_TE_MTN, 
                        y = ~reorder(Product_group, Excise_TE), 
                        x = ~Excise_TE, 
                        type = 'bar', 
                        text = ' ', 
                        hoverinfo = 'x+text',
                        hovertext = ~Product_group,
                        marker = list(color = '#d62728')
                      ) 
                      
                      # Add the layout step separately
                      ProductGroups_MTN1 <- ProductGroups_MTN %>% 
                        layout(
                          title = paste("Tax expenditures by Multilateral Trade Negotiations Categories,", actual_year_simulation),
                          font = t_11,
                          font = list(size = 11),
                          yaxis = list(title = ''),
                          xaxis = list(title = 'In LCU'),
                          annotations = list(
                            x = -0.1, y = -0.056,
                            text = "Source: Calculations by WB staff based on data from National authorities",
                            showarrow = FALSE,
                            xref = 'paper',
                            yref = 'paper',
                            align = 'left'
                          )
                        )
                      
                      
                      

                              

            # 1.2.6 TE'S by type of Mineral oils----------------------------------------------

                      ExciseMineralOils_TE<-Estimation_TE%>%
                        dplyr::filter(DataSet=='Fuels')%>%
                        dplyr::select(Subdataset,Excise_TE)%>%
                        dplyr::group_by(Subdataset)%>%
                        dplyr::summarise(Value=sum(Excise_TE,na.rm = TRUE))
                      
                      
                       ExciseMineralOils_TE<-melt(ExciseMineralOils_TE)
                      
                       #,"purple","green","gold"
                     
                       ExciseMineralOils_TE$color <- factor(ExciseMineralOils_TE$Subdataset, labels =c( "royalblue","orange","forestgreen","brown","red", "cyan","blue"))

                      # Reorder the levels of Subdataset based on value in descending order
                      ExciseMineralOils_TE$Subdataset <- factor(ExciseMineralOils_TE$Subdataset, levels = ExciseMineralOils_TE$Subdataset[order(ExciseMineralOils_TE$value, decreasing = TRUE)])
                      
                      # Create the plot with reordered Subdataset levels
                      ExciseStructure_MineralOils <- plot_ly(ExciseMineralOils_TE, x = ~Subdataset , y = ~value, type = 'bar', text = ' ', hoverinfo = 'y+text', 
                                                             marker = list(color = ~color), name = ~Subdataset
                                                                ) %>% 
                                                                  layout(
                                                                    title = paste("Tax expenditures by Mineral Oils,", actual_year_simulation),
                                                                    font = list(size = 11),
                                                                    xaxis = list(title = ''),
                                                                    yaxis = list(title = 'In LCU'),
                                                                    #barmode = 'stack',  # Use 'stack' for multiple colors within a single bar
                                                                    annotations = list(
                                                                      x = 0, y = -0.056,
                                                                      text = "Source: Calculations by WB staff based on data from National authorities",
                                                                      showarrow = FALSE,
                                                                      xref = 'paper',
                                                                      yref = 'paper',
                                                                      align = 'left'
                                                                    ),
                                                                    legend = list(orientation = 'v', x = 1.02, y = 0.5)
                                                                  )
                      
                      
            # 1.2.7 TE'S by type of Tobacco Products----------------------------------------


                      TobaccoProducts_TE<-Estimation_TE%>%
                        dplyr::filter(DataSet=='Tobacco')%>%
                        dplyr::select(Subdataset,Excise_TE)%>%
                        dplyr::group_by(Subdataset)%>%
                        dplyr::summarise(Value=sum(Excise_TE,na.rm = TRUE))
                      
                      
                      TobaccoProducts_TE<-melt(TobaccoProducts_TE)
                      
                      
                      TobaccoProducts_TE$color <- factor(TobaccoProducts_TE$Subdataset, labels =c( "royalblue","orange")) #,"forestgreen"
                      
                      # Reorder the levels of Subdataset based on value in descending order
                      TobaccoProducts_TE$Subdataset <- factor(TobaccoProducts_TE$Subdataset, levels = TobaccoProducts_TE$Subdataset[order(TobaccoProducts_TE$value, decreasing = TRUE)])
                      
                      # Create the plot with reordered Subdataset levels
                      ExciseStructure_TobaccoProducts <- plot_ly(TobaccoProducts_TE, x = ~Subdataset , y = ~value, type = 'bar', text = ' ', hoverinfo = 'y+text', 
                                                             marker = list(color = ~color), name = ~Subdataset
                                                                ) %>% 
                                                                  layout(
                                                                    title = paste("Tax expenditures by Tobacco Products,", actual_year_simulation),
                                                                    font = list(size = 11),
                                                                    xaxis = list(title = ''),
                                                                    yaxis = list(title = 'In LCU'),
                                                                    annotations = list(
                                                                      x = 0, y = -0.056,
                                                                      text = "Source: Calculations by WB staff based on data from National authorities",
                                                                      showarrow = FALSE,
                                                                      xref = 'paper',
                                                                      yref = 'paper',
                                                                      align = 'left'
                                                                    ),
                                                                    legend = list(orientation = 'v', x = 1.02, y = 0.5)
                                                                  )
                                                                
                     

            # 1.2.8 TE'S by type of Alcohol Products---------------------------------

                      AlcoholProducts_TE<-Estimation_TE%>%
                        dplyr::filter(DataSet=='Alcohol')%>%
                        dplyr::select(Subdataset,Excise_TE)%>%
                        dplyr::group_by(Subdataset)%>%
                        dplyr::summarise(Value=sum(Excise_TE,na.rm = TRUE))
                      
                      
                      AlcoholProducts_TE<-melt(AlcoholProducts_TE)
                      
                      
                      AlcoholProducts_TE$color <- factor(AlcoholProducts_TE$Subdataset, labels =c( "royalblue","orange","forestgreen","brown","cyan"))
                      
                      # Reorder the levels of Subdataset based on value in descending order
                      AlcoholProducts_TE$Subdataset <- factor(AlcoholProducts_TE$Subdataset, levels = AlcoholProducts_TE$Subdataset[order(AlcoholProducts_TE$value, decreasing = TRUE)])
                      
                      # Create the plot with reordered Subdataset levels
                      ExciseStructure_AlcoholProducts <- plot_ly(AlcoholProducts_TE, x = ~Subdataset , y = ~value, type = 'bar', text = ' ', hoverinfo = 'y+text', 
                                                                       marker = list(color = ~color), name = ~Subdataset
                                                                          ) %>% 
                                                                            layout(
                                                                              title = paste("Tax expenditures by Alcohol Products,", actual_year_simulation),
                                                                              font = list(size = 11),
                                                                              xaxis = list(title = ''),
                                                                              yaxis = list(title = 'In LCU'),
                                                                              annotations = list(
                                                                                x = 0, y = -0.056,
                                                                                text = "Source: Calculations by WB staff based on data from National authorities",
                                                                                showarrow = FALSE,
                                                                                xref = 'paper',
                                                                                yref = 'paper',
                                                                                align = 'left'
                                                                              ),
                                                                              legend = list(orientation = 'v', x = 1.02, y = 0.5)
                                                                            )
                                                                          
                      
                      
            # 1.2.9 TE'S Sankey (Normative approach) ------------------------------------------

                      Estimation_TE_subset<-Estimation_TE%>%
                        dplyr::select(Chapter,Four_digit,DataSet,Subdataset,Excise_TE)%>%
                        dplyr::filter(DataSet %in% c("Fuels","Tobacco","Alcohol","SSB"))%>%
                        dplyr::group_by(Chapter,Four_digit,DataSet,Subdataset)%>%
                        dplyr::summarise(total = sum(Excise_TE), .groups = "drop")
                      
                      Estimation_TE_subset$RegularImport<-c('RegularImport')
                      
                     DistributionOfTE<-sankey_ly(Estimation_TE_subset,cat_cols = c('RegularImport',"Chapter","Four_digit",
                                                                  "DataSet","Subdataset"), 
                                                                    num_col = "total",
                                                                    title = "Distribution of Tax Expenditures (Normative approach)")
                      
                  
                     DistributionOfTE<-plotly::add_annotations(DistributionOfTE,
                                                               text= c("HS CHAPTERS", "HS FOUR DIGITS" ), 
                                                               x = c(0.2, 0.5), 
                                                               y = c(1, 1), showarrow = FALSE)
                     

# 1.2.10 TE'S Sankey (Legal approach) ---------------------------------------------------------

                     # Emissions approach
                     TE_MineralOils_total <- Estimation_TE %>% select(Chapter,Four_digit,DataSet,Excise_TE)%>%filter(DataSet == "Fuels")%>%group_by(Chapter,Four_digit,DataSet)%>% summarise(Excise_TE = sum(Excise_TE, na.rm = TRUE) / 1e+06)%>%select(Excise_TE)
                     
                     # Exemption approach
                     TE_MineralOils_total_exemption_sankey <- Estimation_TE %>% 
                       ungroup()%>%
                       select(Chapter,Four_digit,DataSet,ExciseRevenue,PotentialExcise)%>%
                       #filter(DataSet == "Fuels")%>%
                       dplyr::filter(DataSet %in% c("Fuels"))%>%
                       group_by(Chapter,Four_digit,DataSet)%>% 
                       mutate(PotentialExcise = sum(PotentialExcise, na.rm = TRUE),
                              ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE),
                              Excise_TE=(PotentialExcise-ExciseRevenue)/ 1e+06)%>%
                       distinct()%>%
                       select(Excise_TE)
                     
                    # TE_MineralOils_total_exemption_sankey<-TE_MineralOils_total_exemption$Excise_TE
                     TE_MineralOils_cars <- Estimation_TE %>% select(Chapter,Four_digit,Subdataset,DataSet,Excise_TE)%>%filter(DataSet == "Cars")%>%group_by(Chapter,Four_digit,Subdataset,DataSet,)%>% summarise(Excise_TE = sum(Excise_TE, na.rm = TRUE) / 1e+06)%>%select(Excise_TE)
                     
                     
                     # TE_MineralOils_total_exemption_sankey
                     # TE_MineralOils_cars
                     
                     MergedDataSet <- bind_rows(TE_MineralOils_total_exemption_sankey, TE_MineralOils_cars)
                     
                     MergedDataSet1<-MergedDataSet%>%
                       dplyr::select(Chapter,Four_digit,DataSet,Subdataset,Excise_TE)%>%
                       #dplyr::filter(DataSet %in% c("Fuels","Tobacco","Alcohol","SSB"))%>%
                       dplyr::group_by(Chapter,Four_digit,DataSet,Subdataset)%>%
                       dplyr::summarise(total = sum(Excise_TE), .groups = "drop")
                     
                     MergedDataSet1$RegularImport<-c('RegularImport')
                     
                     DistributionOfTE_sankey<-sankey_ly(MergedDataSet1,cat_cols = c('RegularImport',"Chapter","Four_digit",
                                                                                   "DataSet","Subdataset"), 
                                                 num_col = "total",
                                                 title = "Distribution of Tax Expenditures (Legal approach)")
                     
                     
                     DistributionOfTE_sankey<-plotly::add_annotations(DistributionOfTE_sankey,
                                                               text= c("HS CHAPTERS", "HS FOUR DIGITS" ), 
                                                               x = c(0.2, 0.5), 
                                                               y = c(1, 1), showarrow = FALSE)
                     
                     
                     
                      
# III.Tables  ------------------------------------------------------
          
          # 1.Main table ------------------------------------------------------------
          
          
                                # Emissions approach
                                TE_MineralOils_total <- Estimation_TE %>% select(DataSet,Excise_TE)%>%filter(DataSet == "Fuels")%>%group_by(DataSet)%>% summarise(Excise_TE = sum(Excise_TE, na.rm = TRUE) / 1e+06)%>%select(Excise_TE)
                                
                                 # Exemption approach
                                TE_MineralOils_total_exemption <- Estimation_TE %>% 
                                  ungroup()%>%
                                  select(DataSet,ExciseRevenue,PotentialExcise)%>%
                                  filter(DataSet == "Fuels")%>%
                                  group_by(DataSet)%>% 
                                  mutate(PotentialExcise = sum(PotentialExcise, na.rm = TRUE),
                                        ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE),
                                        Excise_TE=(PotentialExcise-ExciseRevenue)/ 1e+06)%>%
                                  distinct()%>%
                                  select(Excise_TE)
                                
                                TE_MineralOils_total_exemption<-TE_MineralOils_total_exemption$Excise_TE
                                         
                                  #summarise(Excise_TE = sum(PotentialExcise, na.rm = TRUE)-summarise(ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE) / 1e+06))%>%select(Excise_TE)
                     
                                TE_TobaccoProducts_total <- Estimation_TE %>% select(DataSet,Excise_TE)%>%filter(DataSet == "Tobacco")%>%group_by(DataSet)%>% summarise(Excise_TE = sum(Excise_TE, na.rm = TRUE) / 1e+06)%>%select(Excise_TE)
                                TE_Alcohol_total <- Estimation_TE %>% select(DataSet,Excise_TE)%>%filter(DataSet == "Alcohol")%>%group_by(DataSet)%>% summarise(Excise_TE = sum(Excise_TE, na.rm = TRUE) / 1e+06)%>%select(Excise_TE)
                                TE_Cars_total <- Estimation_TE %>% select(DataSet,Excise_TE)%>%filter(DataSet == "Cars")%>%group_by(DataSet)%>% summarise(Excise_TE = sum(Excise_TE, na.rm = TRUE) / 1e+06)%>%select(Excise_TE)
                                
                                
          
                                    MainResultsExcise<-MacroFiscalData%>%
                                                                    dplyr::filter(Year==actual_year_simulation)%>%
                                                                    dplyr::mutate(
                                                                      # Total
                                                                      `Actual Total Import`=sum(Estimation_TE$Value,na.rm=TRUE)/1e+06,
                                                                      `Actual Total Excise Revenues`=sum(Estimation_TE$ExciseRevenue,na.rm=TRUE)/1e+06,
                                                                      `Excise Revenue Benchmark`=sum(Estimation_TE$Excise_BenchmarkRevenue,na.rm=TRUE)/1e+06,
            
                                                                      `Tax Expenditures`=sum(Estimation_TE$Excise_TE,na.rm=TRUE)/1e+06,
                                                                      `Tax Expenditures as % of GDP`=(`Tax Expenditures`/GDP)*100,
                                                                      `Tax Expenditures as % of Government Revenue`=(`Tax Expenditures`/GeneralGovernmentRevenue)*100,
                                                                      `Tax Expenditures (as % of Taxes On Products)`=(`Tax Expenditures`/TaxesOnProducts)*100,
                                                                      `Tax Expenditures (as % of ExciseRevenue)`=(`Tax Expenditures`/TaxesOnProducts)*100,
                                                                      # TE's by type of products
                                                                      `Tax Expenditures by Mineral Oils`= TE_MineralOils_total$Excise_TE,
                                                                      `Tax Expenditures by Tobacco Products`= TE_TobaccoProducts_total$Excise_TE,
                                                                      `Tax Expenditures by Alcohol Products`= TE_Alcohol_total$Excise_TE,
                                                                      `Tax Expenditures by Cars`= 0 #TE_Cars_total$Excise_TE
                                                                      )%>%
                                                                    dplyr::select(
                                                                      `Actual Total Import`,
                                                                      `Actual Total Excise Revenues`,
                                                                      `Excise Revenue Benchmark`,
                                                                      `Tax Expenditures`,
                                                                      `Tax Expenditures as % of GDP`,
                                                                      `Tax Expenditures as % of Government Revenue`,
                                                                      `Tax Expenditures (as % of Taxes On Products)`,
                                                                      `Tax Expenditures (as % of ExciseRevenue)`,
                                                                      `Tax Expenditures by Mineral Oils`,
                                                                      `Tax Expenditures by Tobacco Products`,
                                                                      `Tax Expenditures by Alcohol Products`,
                                                                      `Tax Expenditures by Cars`
                                                                      )
            
            
            
                                                                  MainResultsExcise<-melt(MainResultsExcise)
                                                                  MainResultsExcise$value<-round(MainResultsExcise$value,2)
            
                                                                  MainResultsExcise<-MainResultsExcise%>%
                                                                    dplyr::rename("Description"= "variable",
                                                                                  #"Value"="value")
                                                                                  "Normative_Approach"="value")
                                                      
                            
                          ## Second table
                                                                                   
                                                                  
                                                  MainResultsExcise_1<-MacroFiscalData%>%
                                                                    dplyr::filter(Year==actual_year_simulation)%>%
                                                                    dplyr::mutate(
                                                                      # Total
                                                                      `Actual Total Import`=sum(Estimation_TE$Value,na.rm=TRUE)/1e+06,
                                                                      `Actual Total Excise Revenues`=sum(Estimation_TE$ExciseRevenue,na.rm=TRUE)/1e+06,
                                                                      `Excise Revenue Benchmark`=(sum(Estimation_TE$ExciseRevenue)/1e+06) + TE_MineralOils_total_exemption + TE_TobaccoProducts_total$Excise_TE +TE_Alcohol_total$Excise_TE,
                                                                        
                                                                        #sum(Estimation_TE$Excise_BenchmarkRevenue,na.rm=TRUE)/1e+06,
                                                                      
                                                                      #`Tax Expenditures`= (TE_MineralOils_total_exemption + TE_TobaccoProducts_total$Excise_TE +TE_Alcohol_total$Excise_TE),
                                                                      `Tax Expenditures`= (TE_MineralOils_total_exemption + TE_Cars_total$Excise_TE),
                                                                        
                                                                        #sum(Estimation_TE$Excise_TE,na.rm=TRUE)/1e+06,
                                                                      `Tax Expenditures as % of GDP`=(`Tax Expenditures`/GDP)*100,
                                                                      `Tax Expenditures as % of Government Revenue`=(`Tax Expenditures`/GeneralGovernmentRevenue)*100,
                                                                      `Tax Expenditures (as % of Taxes On Products)`=(`Tax Expenditures`/TaxesOnProducts)*100,
                                                                      `Tax Expenditures (as % of ExciseRevenue)`=(`Tax Expenditures`/TaxesOnProducts)*100,
                                                                      # TE's by type of products
                                                                      `Tax Expenditures by Mineral Oils`= TE_MineralOils_total_exemption,
                                                                      `Tax Expenditures by Tobacco Products`= 0, #TE_TobaccoProducts_total$Excise_TE,
                                                                      `Tax Expenditures by Alcohol Products`= 0, #TE_Alcohol_total$Excise_TE,
                                                                      `Tax Expenditures by Cars`= TE_Cars_total$Excise_TE
                                                                    )%>%
                                                                    dplyr::select(
                                                                      `Actual Total Import`,
                                                                      `Actual Total Excise Revenues`,
                                                                      `Excise Revenue Benchmark`,
                                                                      `Tax Expenditures`,
                                                                      `Tax Expenditures as % of GDP`,
                                                                      `Tax Expenditures as % of Government Revenue`,
                                                                      `Tax Expenditures (as % of Taxes On Products)`,
                                                                      `Tax Expenditures (as % of ExciseRevenue)`,
                                                                      `Tax Expenditures by Mineral Oils`,
                                                                      `Tax Expenditures by Tobacco Products`,
                                                                      `Tax Expenditures by Alcohol Products`,
                                                                      `Tax Expenditures by Cars`
                                                                    )
                                                                  
                                                                  
                                                                  
                                                                   MainResultsExcise_1<-melt(MainResultsExcise_1)
                                                                   MainResultsExcise_1$value<-round(MainResultsExcise_1$value,2)
                                                                  
                                                                   MainResultsExcise_1<-MainResultsExcise_1%>%
                                                                    dplyr::rename("Description"= "variable",
                                                                                  #"Value"="value")%>%
                                                                                  "Legal_Approach"="value")%>%
                                                                     select(-c(Description))
                                                                    
                                                                     #MainResultsExciseFinal
                                                                  
                                                                  
                                                                  
                                                                   MainResultsExciseFinal<-cbind(MainResultsExcise,MainResultsExcise_1)
                                                                  
                                                                
          # 2.TE's by HS -------------------------------------------------------------------
          
                                                     # Estimation_TE              
                                                      
                                                      CustomsDuties_base_Description<-CustomsDuties_base%>%
                                                        dplyr::select(HS_code,Description)
                                                      
                                                      Estimation_Excise_TE_HS<-left_join(Estimation_TE,CustomsDuties_base_Description, by=c('HS_code'))%>%
                                                        dplyr::select(HS_code,Description,Excise_TE)%>%
                                                        dplyr::group_by(HS_code,Description)%>%
                                                        dplyr::summarise(Excise_TE=sum(Excise_TE))%>%
                                                        dplyr::arrange(desc(Excise_TE)) %>%
                                                        dplyr::mutate(Excise_TE = round(Excise_TE / 1e06, 2))

                                                      


                                                        
          rm(Import_Excise_Data)        
                                         
          
          
})


print("Simulation is done")