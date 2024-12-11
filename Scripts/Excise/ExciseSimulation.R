

# 4.3 Alcohol and SSB  -----------------------------------------------------------          
          # 4.3.1 Beer --------------------------------------------------------------------
          
                      BeerQuantity <- CustomsDuties_TE_agg_HS %>%
                        filter(Four_digit == 2203)
                      
                      
                      # Adding columns
                      BeerQuantity$Subdataset<-c("BEER")
                      # Adding excis rate
                      BeerQuantity$ExciseRate<-as.numeric(800)
                      
          
          # 4.3.2 Sugar-sweetened beverages(SSB) ------------------------------------------------------------------
          
          # Non Alcoholic Beer is included here
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
          UndenaturedethylAlcoholQuantity_1  <- CustomsDuties_TE_agg_HS %>%
            filter(Four_digit == 2207)

          UndenaturedethylAlcoholQuantity_1$Subdataset<-c("UndenaturedEthylAlcoholVolume_80")
          UndenaturedethylAlcoholQuantity_1$ExciseRate<-as.numeric(500)

          
          # 4.3.6 Undenatured ethyl alcohol ---------------------------------------------
          
          # Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80 % vol; spirits, liqueurs and other spirituous beverages
          UndenaturedEthylAlcoholQuantity_2  <- CustomsDuties_TE_agg_HS %>%
            dplyr::filter(Four_digit == 2208)
          
          
          # View(UndenaturedEthylAlcoholQuantity)
          #  sum(UndenaturedEthylAlcoholQuantity$Quantity)/1e06
          UndenaturedEthylAlcoholQuantity_2$Subdataset<-c("ALCHOLIC BEVERAGE")
          
          # Adding excis rate
          UndenaturedEthylAlcoholQuantity_2$ExciseRate<-as.numeric(800)
          
          # 4.3.7 Merging data --------------------------------------------------
          
          # SSB is not included in the dataset !!!
          
          # AlcoholQuantity <- bind_rows(BeerQuantity,WineQuantity,VermouthQuantity,OtherFermentedBeveragesQuantity,UndenaturedEthylAlcoholQuantity)
          
          AlcoholQuantity <- bind_rows(BeerQuantity,WineQuantity,VermouthQuantity,OtherFermentedBeveragesQuantity,UndenaturedethylAlcoholQuantity_1,UndenaturedEthylAlcoholQuantity_2,
                                       SSB_Quantity)
          
          
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
                          Pure_Alc=Quantity_HL*Alc_Content,
                          test=ExciseRate*Pure_Alc
            )
          
          Alcohol_tbl$Quantity_HL<-NULL
          Alcohol_tbl$PotentialExcise<-NULL
          
          # Cross check
          sum(Alcohol_tbl$ExciseRevenue)
          sum(Alcohol_tbl$test)
          
          Alcohol_tbl$Alc_Content<-round(Alcohol_tbl$Alc_Content,2)
          
          
          Alcohol_tbl <- Alcohol_tbl %>%
            mutate(DataSet  = case_when(
              grepl("SSB", Subdataset, ignore.case = TRUE) ~ "SSB",
              TRUE ~ DataSet  # If not containing 'SSB', keep the original value
            ))
          
          
          Alcohol_tbl_subset_export<-Alcohol_tbl%>%
            select(Subdataset,ExciseRate,Alc_Content,Pure_Alc,Quantity,ExciseRevenue)%>%
            group_by(Subdataset,ExciseRate,Alc_Content)%>%
            summarise(Pure_Alc=sum(Pure_Alc),
                      Quantity=sum(Quantity),
                      ExciseRevenue=sum(ExciseRevenue)
                      
            )
          
          View(Alcohol_tbl_subset_export)
          
          
          

        

          
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
          
          
          
          View(Tobacco_tbl)
          
          # Tobacco_tbl_subset<-Tobacco_tbl%>%
          #   select(Subdataset,Quantity,ExciseRevenue,ExciseRate,ConventionalUnit,VolumeBySticks,Base_EquivalentOfGramsTobacco,
          #          ExciseByEquivalentGramsTobacco,ExciseByEquivalentGramsTobacco_1000,CrossCheckExise
          #          )
          # 
          # 
          # write.csv(Tobacco_tbl_subset,"Tobacco_tbl_subset.csv")
          
          
          
          # STIGNATO DO OVDE !!! DA SE PRODOLZI SO PROVERKA NA LOGIKATA NA KALKULACIJA
          
          
          
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
          
          
          
          sum(Fuel_tbl$PotentialExcise)

          sum(Fuel_tbl$ExciseRevenue)
          
          View(Fuel_tbl)
          
          # write.csv(Fuel_tbl,"Fuel_tbl2.csv")
          
          
# 5.Cars ------------------------------------------------------------------
          
          CarsSet <- CustomsDuties_TE_agg_HS %>%
            dplyr::filter(Four_digit %in% c('8703'))
          
          
          Cars_tbl<-left_join(CarsSet,Cars_ExciseRates, by=c("HS_code_s"="HS"))
          
          
          
          
          Cars_tbl$Subdataset<-c("Cars")
          Cars_tbl$DataSet<-c("Cars")
          
          
          Cars_tbl$Quantity_HL<-as.numeric(0)
          Cars_tbl$PotentialExcise<-as.numeric(0)
          
          
          View(Cars_tbl)
          
          
# 6.Merging Data Sets and estimation of TE'S -------------------------------------------------------
          
          #ExciseFinal_tbl <- bind_rows(Fuel_tbl, Tobacco_tbl,Alcohol_tbl,Cars_tbl)
          ExciseFinal_tbl <- bind_rows(Fuel_tbl, Tobacco_tbl,Alcohol_tbl,Cars_tbl)
          ExciseFinal_tbl$ExciseRate[is.nan(ExciseFinal_tbl$ExciseRate)]<-0
          ExciseFinal_tbl$Alc_Content[is.nan(ExciseFinal_tbl$Alc_Content)]<-0
          ExciseFinal_tbl$Pure_Alc[is.nan( ExciseFinal_tbl$Pure_Alc)]<-0
          # Benchmark_ExciseFuels<-1
          
          View(ExciseFinal_tbl)
          
          # calculate_benchmark_fun <- function(data) {
          #   data %>%
          #     mutate(
          #       result = case_when(
          #         #DataSet == "Fuels" ~ Quantity * Benchmark_ExciseFuels,
          #         #Subdataset == "AVIATION GASOLINE" ~ Quantity * Benchmark_AVIATION_GASOLINE,
          #         Subdataset == "EURO DIESEL" ~ Quantity * Benchmark_EURO_DIESEL,
          #         Subdataset == "EUROSUPER BC 95" ~ Quantity * Benchmark_EUROSUPER_BC_95,
          #         Subdataset == "EUROSUPER BS 100" ~ Quantity * Benchmark_EUROSUPER_BS_100,
          #         Subdataset == "HEAVY OILS" ~ Quantity * Benchmark_HEAVY_OILS,
          #         Subdataset == "LPG BUTANE" ~ Quantity * Benchmark_LPG_BUTANE,
          #         Subdataset == "LPG PROPANE" ~ Quantity * Benchmark_LPG_PROPANE,
          #         DataSet == "Tobacco" ~ (Base_EquivalentOfGramsTobacco / 1000) * Benchmark_ExciseTobacco,
          #         DataSet == "Alcohol" ~ Pure_Alc * Benchmark_ExciseAlcohol,
          #         DataSet == "Cars" ~ Quantity * ExciseRate
          #       )
          #     )
          # }
          # 
          # 
          # # Apply the function 
          # Estimation_TE <- calculate_benchmark_fun(ExciseFinal_tbl)%>%
          #   dplyr::rename("Excise_BenchmarkRevenue"="result")%>%
          #   dplyr::mutate(Excise_TE=Excise_BenchmarkRevenue-ExciseRevenue)
          # 
          # # Remove NaN
          # Estimation_TE[is.na(Estimation_TE)] <- 0
          # 
          # # Define a function to round up two columns
          # round_up_columns <- function(data, columns) {
          #   data %>%
          #     mutate(across(all_of(columns), ~ceiling(.)))
          # }
          # 
          # # Apply the function to round up 
          # Estimation_TE <- round_up_columns(Estimation_TE, c("Excise_BenchmarkRevenue", "Excise_TE"))
          
           View(Estimation_TE)
          
          