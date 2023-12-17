' Data preparation,estimation of tax expenditures and preparation of data for charts
'
# options(warn = -1)
# 
# suppressMessages({

library(tidyverse)
options(scipen=999)
  
# I. Estimation of tax expenditures for excise duties ----------------------------------------------------------
        # 1.Import and Processing of data ------------------------------------------------------
            # Ova ke se vnese direktno preku GUI
                  Import_raw_monthly <- read_excel("~/Models/Kosovo-TE-Models/Data/ImportData/Open_DATA_Import-Janar-Dhjetor-2022.xlsx")      
                            
                
                    # Change the column names EN
                    colnames(Import_raw_monthly)[1:11] <- c("Year","Month","TradePolicy","Countries","Code_Description","Quantity","Value","Netweight","CustomsRevenue","ExciseRevenue","VAT_Revenue")
                    
                    # Split the Code_Description column into two parts using "-"
                    split_columns_hs <- strsplit(Import_raw_monthly$Code_Description, "-")
                    
                    # Split the Code_Description column into two parts using "-"
                    # Create new columns with the split parts
                    Import_raw_monthly$HS_code <- sapply(split_columns_hs, `[`, 1)
                    
                    
                    # Extract the second part of the split using "[, 2]"
                    Import_raw_monthly$Description <- sapply(split_columns_hs, `[`, 2)
                    
                    # Split the Code for countries column into two parts using "-"
                    split_columns_countries <- strsplit(Import_raw_monthly$Countries, "-")
                    Import_raw_monthly$iso2c <- sapply(split_columns_countries, `[`, 1)
                    
                    Import_raw_monthly<-Import_raw_monthly%>%
                      dplyr::mutate(HS_code = trimws(HS_code, which = "both"))
                    
                   # Import_raw_monthly$Code_Description<-NULL
                    Import_raw_monthly$TradePolicy<-NULL
                    Import_raw_monthly$Countries<-NULL
            
                    
                    Import_raw_monthly<-Import_raw_monthly%>%
                      dplyr::select("HS_code","Description",
                                    "iso2c","Month","Year","Quantity","Value","Netweight","CustomsRevenue","ExciseRevenue","VAT_Revenue")
                   
                    # Trim data
                    Import_raw_monthly<-Import_raw_monthly%>%
                      dplyr::mutate(iso2c = trimws(iso2c, which = "both"))
            
                  # Estimation of tax expenditures 
                   CustomsDuties_base<-Import_raw_monthly%>%
                                                  dplyr::select(HS_code,Description,iso2c,Month,Year,Value,Quantity,Netweight,CustomsRevenue,ExciseRevenue,VAT_Revenue)%>%
                                                  dplyr::mutate(Effective_VAT_rate=round(VAT_Revenue/(Value+ExciseRevenue+CustomsRevenue),2),
                                                                Effective_Customs_rate=round(CustomsRevenue/(Value),2),
                                                                Effective_Excise_rate=round((ExciseRevenue)/Quantity,3)
                                                                
                                                                )
                                                                #CustomsDuties_Benchmark=Value*Benchmark_Customs_Rate,
                                                                #CustomsDuties_TE=round(CustomsDuties_Benchmark-CustomsRevenue,1))
                   View(CustomsDuties_base) 
                   
                   
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
                                           VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE))#,
                                          # CustomsDuties_Benchmark=sum(CustomsDuties_Benchmark,na.rm = TRUE),
                                           #CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                        
                        
                        CustomsDuties_TE_agg_countries$HS_code<-NULL
                        CustomsDuties_TE_agg_countries$HS_code_s<-NULL
                        
                        # 
                        # CustomsDuties_TE_agg_countries <- CustomsDuties_TE_agg_countries %>%
                        # #  dplyr::select(iso3c,CustomsDuties_TE)%>%
                        #   dplyr::select(iso3c)%>%
                        #   dplyr::group_by(iso3c) %>%
                        #   dplyr::summarise(across(where(is.numeric), sum))
                          
    
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
                                         #Effective_Excise_rate=ifelse(Quantity=0,0,(round((ExciseRevenue)/Quantity,3)))
                                         ) #,
                                        # CustomsDuties_Benchmark=sum(CustomsDuties_Benchmark,na.rm = TRUE),
                                         #CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                      
    
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
                    
                  

                  # 1 Dataset from Customs Administration -----------------------------------
          

                              # HS_EXCISE <- read_excel("HS-EXCISE.xlsx")
                              # 
                              # HS_Chapter_27<- read_excel("Trade_Chapter_27_2022.xlsx")%>%
                              #   dplyr::rename("HS_code_s"="Kodi tarifor",
                              #                 "Excise"="Taksa_Akcizes",
                              #                 "Description"="Përshkrimi tarifor",
                              #                 "CommercialName"="Përshkrimi",
                              #                 "NetWeight"="Pesha neto",
                              #                 "GrossWeight"="Pesha bruto",
                              #                 "Quantity"="Sasia"
                              #   )%>%
                              #   select(HS_code_s,Description,CommercialName,Excise,NetWeight,GrossWeight,Quantity)%>%
                              #   dplyr::mutate(
                              #     Effective_Excise_rate=round((Excise)/Quantity,3)
                              #   )
                              # 
                              # # Adding desegregation by HS codes
                              # HS_Chapter_27 <- mutate(HS_Chapter_27,
                              #                         Chapter = substr(HS_code_s, 1, 2),
                              #                         Four_digit = substr(HS_code_s, 1, 4),
                              #                         Six_digit = substr(HS_code_s, 1, 6),
                              #                         Eight_digit = paste0(substr(HS_code_s, 1, 4),
                              #                                              "",
                              #                                              substr(HS_code_s, 5, 8)
                              #                                              # " ",
                              #                                              #substr(HS_code_s, 8, 8))
                              #                         ))
                              # 
                              # 
                              # 
                              # view(HS_Chapter_27)
                              # # 
                              # 
                              # 
                              # sum(HS_Chapter_27$Quantity,na.rm=TRUE)/1E06
                              # 
                              # 
                              # str(HS_Chapter_27)
                              # str(HS_EXCISE)
                              # 
                              # # subset<-HS_Chapter_27%>%
                              # #   filter(Four_digit=="2707")
                              # # 
                              # # unique(subset$Eight_digit)
                              # # 
                              # # 
                              # # summary(HS_Chapter_27)
                              # 
                              # 
                              # HS_Chapter_27_Agg<-HS_Chapter_27%>%
                              #   dplyr::select(Eight_digit,Excise,Quantity)%>%
                              #   dplyr::group_by(Eight_digit)%>%
                              #   dplyr::summarise(Excise=sum(Excise),Quantity=sum(Quantity))
                              # 
                              # 
                              # 
                              # 
                              # HS_FUEL<-left_join(HS_Chapter_27_Agg,HS_EXCISE,by=c("Eight_digit"))%>%
                              #   dplyr::select(Group,Excise,Quantity)%>%
                              #   dplyr::group_by(Group)%>%
                              #   dplyr::summarise(Excise=sum(Excise,na.rm = TRUE),Quantity=sum(Quantity,na.rm = TRUE))
                              # 
                              # 
                              # view(HS_FUEL)
                              
          
                  # 2. Simulation Dataset ---------------------------------------------------
          
                              # 1.Subset Chapter 27
                              HS_Chapter_27<-CustomsDuties_TE_agg_HS %>%
                                filter(Chapter == '27')
                              
                              # 2.Subset Alcohol For Engine
                               AlcoholForEngine <- CustomsDuties_TE_agg_HS %>%
                                dplyr::filter(Eight_digit %in% c('2710 12 31','2710 12 90'))
                              
                              
                              # 3.Cyclic and Acyclic hydrocarbons
                               CyclicAcyclicHydrocarbons <- CustomsDuties_TE_agg_HS %>%
                                dplyr::filter(Four_digit %in% c('2901','2902'))
                               
                               
                               # 4.Other which are used for the same purposes as mineral oils
                               # Organic composite solvents and solvents
                               # Alkylbenzenes and mixed Alkylnaphthalenes
                               
                                Other_Ch_38 <- CustomsDuties_TE_agg_HS %>%
                                 dplyr::filter(Four_digit %in% c('3811','3814','3817'))
                              
                               # Merge the 
                               
                              FuelQuantity <- bind_rows(HS_Chapter_27, AlcoholForEngine,CyclicAcyclicHydrocarbons,Other_Ch_38)
                              
                              # Removing Nan from effective tax rates
                              FuelQuantity$Effective_Excise_rate<-ifelse(FuelQuantity$Quantity==0,0,FuelQuantity$Effective_Excise_rate)
                              
                              FuelQuantity<-FuelQuantity%>%
                                dplyr::filter(ExciseRevenue>0)
                              
                              
                              #write.csv(FuelQuantity,"FuelQuantity.csv")
                              
                              sum(FuelQuantity$ExciseRevenue)
                              # 272672809 LCU
                              
                              HS_EXCISE <- read_excel("Data/Excise/HS-EXCISE.xlsx")
                              
                             
                              
                              FuelQuantityFinal<-left_join(FuelQuantity,HS_EXCISE,by=c('Eight_digit'))%>%
                                dplyr::filter(ExciseRevenue>0)
                              
                              
                              sum(FuelQuantityFinal$ExciseRevenue)
                              
                              # Calculation of TE'S for fuels
                              
                              Benchmark_Customs_Rate <- input$simulationSlider
                             
                              
# DO OVDE !!! TREBA DA SE PRODOLZI SO DETALEN PREGLED NA CHAPTER 27 FAILOT OD CARINA ZA DA SE VIDAT TRGOVSKITE IMINJA I TAKA DA SE PRODOLZI
                              # SO SLIDERI I SO GRUPI
                              
                              View(FuelQuantityFinal)
     
                              
                              # Ovde da se stavi subset sto ke gi povrze ovie ovie so benchmark !!!
                    
                    
                    
            # 4.2 Tobacco ---------------------------------------------------------------
                  # 1.Dataset from Customs Administration ----------------------------------
                    # Euromonitor
                    # Euromonitor <- read_excel("Passport_Stats_09-12-2023_0908_GMT.xlsx")
                    # 
                    # # Customs administration
                    # HS_Chapter_24 <- read_excel("Import of tobacco and electronic cigarettes_2022.xlsx")%>%
                    #   dplyr::rename("HS_code_s"="Kodi tarifor",
                    #                 "Excise"="Taksa_Akcizes",
                    #                 "Description"="Përshkrimi tarifor",
                    #                 "CommercialName"="Përshkrimi",
                    #                 "NetWeight"="Pesha neto",	
                    #                 "GrossWeight"="Pesha bruto",
                    #                 "Quantity"="Sasia"
                    #   )%>%
                    #   select(HS_code_s,Description,CommercialName,Excise,NetWeight,GrossWeight,Quantity)%>%
                    #   dplyr::mutate(
                    #     Effective_Excise_rate=round((Excise)/Quantity,3)
                    #   )
                    # 
                    # 
                    # View(HS_Chapter_24)
                    # 
                    # 
                    # sum(HS_Chapter_24$Quantity,na.rm = TRUE)/1e06
                    # 
                    # 
                    # # Adding desegregation by HS codes
                    # HS_Chapter_24 <- mutate(HS_Chapter_24,
                    #                         Chapter = substr(HS_code_s, 1, 2),
                    #                         Four_digit = substr(HS_code_s, 1, 4),
                    #                         Six_digit = substr(HS_code_s, 1, 6),
                    #                         Eight_digit = paste0(substr(HS_code_s, 1, 4),
                    #                                              "",
                    #                                              substr(HS_code_s, 5, 8)
                    #                                              # " ",
                    #                                              #substr(HS_code_s, 8, 8))
                    #                         ))
                    # 
                    # 
                    # 
                    # # 
                    # 
                    # # Words to detect and replace
                    # word_to_detect <- c("Bo x", "Cheste rfield", "C IGARE", "PRESTI GE", "DAVIDO FF", "ROTHMA NS", "Gauloi ses", "Winsto n", "Dunhil l", "Ma rlboro","Marlbo ro","Cigare  qe permbajne duhan:","Cigare qe permbajne duhan:","Ch esterfield")
                    # replace_words <- c("Box", "Chesterfield", "CIGARE", "PRESTIGE", "DAVIDOFF", "ROTHMANS", "Gauloises", "Winston", "Dunhill", "Marlboro","Marlboro","","","Chesterfield")
                    # 
                    # # Function to detect and replace specific words
                    # detect_and_replace <- function(text, words_to_detect, replace_words) {
                    #   # Iterate over each word to detect and replace
                    #   for (i in seq_along(words_to_detect)) {
                    #     word_pattern <- paste0("\\b", words_to_detect[i], "\\b")
                    #     text <- str_replace_all(text, word_pattern, replace_words[i])
                    #   }
                    #   return(text)
                    # }
                    # 
                    # # Apply the function to the CommercialName column
                    # HS_Chapter_24$CommercialName <- sapply(HS_Chapter_24$CommercialName, detect_and_replace, words_to_detect = word_to_detect, replace_words = replace_words)
                    # 
                    # 
                    # 
                    # View(HS_Chapter_24)
                    # # Testing data
                    
                    ## 2401-Unmanufactured tobacco; tobacco refuse:
                    ## 2402-Cigars, cheroots, cigarillos and cigarettes, of tobacco or of tobacco substitutes
                    ## 2403-Other manufactured tobacco and manufactured tobacco substitutes; "homogenized" or "reconstituted" tobacco; tobacco extracts and essences:
                    ## 2404-Products containing tobacco, reconstituted tobacco, nicotine, or tobacco or nicotine substitutes, intended for inhalation without combustion; other nicotine containing products intended for the intake of nicotine into the human body
                    
                    # HS_Chapter_24_agg<-HS_Chapter_24%>%
                    #   dplyr::select(Four_digit,Excise,Quantity)%>%
                    #   dplyr::group_by(Four_digit)%>%
                    #   dplyr::summarise(Excise=sum(Excise,na.rm = TRUE),Quantity=sum(Quantity,na.rm = TRUE))
                    # 
                    # 
                    # View(HS_Chapter_24_agg)
                    
                    # Do tuka !!!
                    # Da se prodolzi so cistenje na bazata za cigari od aspekt na iminja.
                    # Da se proverat stapkite na akcizi i mernata edinica
                    # Da se najde za bencmark i da pocne da se presmetuva 

                  # 2.Simulation Dataset ------------------------------------------------------

                              ## 2401-Unmanufactured tobacco; tobacco refuse:
                              ## 2402-Cigars, cheroots, cigarillos and cigarettes, of tobacco or of tobacco substitutes
                              ## 2403-Other manufactured tobacco and manufactured tobacco substitutes; "homogenized" or "reconstituted" tobacco; tobacco extracts and essences:
                              ## 2404-Products containing tobacco, reconstituted tobacco, nicotine, or tobacco or nicotine substitutes, intended for inhalation without combustion; other nicotine containing products intended for the intake of nicotine into the human body
                              
                              
                              # Subset Chapter 27
                              HS_Chapter_24<-CustomsDuties_TE_agg_HS %>%
                                filter(Chapter == '24')
                              
                              # Ovde da se stavi subset sto ke gi povrze ovie ovie so benchmark !!!
                      
                    
            # 4.3 Alcoholic beverage -----------------------------------------------------------          
                    
                    
                    
                    
                    
                    
                    
                  # 4.3.1 Beer --------------------------------------------------------------------
                          
                                              # First filter
                                              AlcoholicBeer <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2203)
                                              
                                              # Second filter
                                              NonAlcoholicBeer <- CustomsDuties_TE_agg_HS %>%
                                                filter(Eight_digit == '2202 91 00')
                                              
                                              # Merge the results
                                              BeerQuantity <- bind_rows(AlcoholicBeer, NonAlcoholicBeer)
                                              
                          
                  # 4.3.2 Wine of fresh grapes ----------------------------------------------------
                          
                           # Wine of fresh grapes, including fortified wines; grape must other than that of heading 2009)                    
                                              
                                             
                                              WineQuantity <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2204)
                                              
                                           
                                              View(WineQuantity)
                                              sum(WineQuantity$Quantity)/1e06
                                             
                          
                          
                  # 4.3.3 Vermouth ---------------------------------------------------
                          
                                             # Vermouth and other wine of fresh grapes flavoured with plants or aromatic substances
                                              
                                              VermouthQuantity <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2205)
                                              
                                              
                                              View(VermouthQuantity)
                                              sum(VermouthQuantity$Quantity)/1e06
                          
                          
                  # 4.3.4 Other fermented beverages --------------------------------------------------
                          
                                              # Other fermented beverages (for example, cider, perry, mead); mixtures of fermented beverages and mixtures of fermented beverages and non-alcoholic beverages, not elsewhere specified or included
                                              
                                              OtherFermentedBeveragesQuantity  <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2206)
                                              
                                              
                                              View(OtherFermentedBeveragesQuantity)
                                              sum(OtherFermentedBeveragesQuantity$Quantity)/1e06
                                              
                                              
                          
                  # 4.3.5 Undenatured ethyl alcohol ---------------------------------------------
                          
                          
                                              # Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80 % vol; spirits, liqueurs and other spirituous beverages
                                              
                                              
                                              UndenaturedEthylAlcoholQuantity  <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2208)
                                              
                                              
                                              View(UndenaturedEthylAlcoholQuantity)
                                              sum(UndenaturedEthylAlcoholQuantity$Quantity)/1e06
                                              
                                              
                                                                  
                                              
                              
                              
                              
                                    
              # tEST 
                                    Test1  <- CustomsDuties_TE_agg_HS %>%
                                      filter(Chapter == '27')
                                    
                                    
                                    View(Test1)
                                    sum(Test1$Quantity)/1e06
                                    
                                    
                                    Test1  <- CustomsDuties_TE_agg_HS %>%
                                      filter(Chapter == '24')
                                    
                                    
                                    View(Test1)
                                    sum(Test1$Quantity)/1e06
                                    
                
           # 5.Cars ------------------------------------------------------------------

                    
                     CarsSet <- CustomsDuties_TE_agg_HS %>%
                              dplyr::filter(Four_digit %in% c('8703'))
                                    
                                    
                                    
                                    
        # 5.Merging Data Sets -------------------------------------------------------
                  
                                    
                # Do tuka!!!
                # Kaj site prethodni da se stavat iminja za spojuvanje i da se spojat so ovoj dataset !!
                                    
                                    
                                    
                                    
# II.Preparing data for charts and for the main table --------------------------------------------

      # 1.Preparing data for Charts ---------------------------------------
            # 1.1 Regular Import------------------------------------------
                    
                    CustomsValue<-CustomsDuties_TE_agg_HS%>%
                      dplyr::select(Chapter,Value)%>%
                      dplyr::group_by(Chapter)%>%
                      dplyr::summarise(Value=sum(Value,na.rm = TRUE))
                    
                    CustomsValue<-left_join(CustomsValue,HS_Sections,by =c("Chapter"))%>%
                      dplyr::select(Chapter,Chapters_description,Value)%>%
                      dplyr::group_by(Chapter,Chapters_description)%>%
                      dplyr::summarise(Value=sum(Value,na.rm = TRUE))
                    
                    
                    # Factor the "Sections" column without ordering other strings
                    CustomsValue$Chapter <- factor(CustomsValue$Chapter)
                    
                    CustomsValue$HS<-"Chapters by Harmonized System "
                    
                    
            # 1.2 Pie chart  --------------------------------------------------------------
                    
                    CustomsDuties_base_pie<-CustomsDuties_base%>%
                      dplyr::select(Treatment,Value)%>%
                      dplyr::group_by(Treatment)%>%
                      dplyr::summarise(Value=sum(Value,na.rm = TRUE))
                    
                    CustomsDuties_base_pie<-melt(CustomsDuties_base_pie)

            # 1.3 TE's by HS Type of products -----------------------------------------
  
                  CustomsDuties_TE_type_products<-CustomsDuties_TE_agg_HS%>%
                    dplyr::select(Chapter,Treatment,CustomsDuties_TE)%>%
                    dplyr::group_by(Chapter,Treatment)%>%
                    dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
          
                    # Select Agricultural
                    CustomsDuties_TE_Agricultural<-CustomsDuties_TE_type_products%>%
                                dplyr::filter(Chapter %in% c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))
                    
                    # Select Industrial
                    CustomsDuties_TE_Industrial<-CustomsDuties_TE_type_products%>%
                                dplyr::filter(!Chapter %in% c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))
                        
                    CustomsDuties_TE_Agricultural$type_products<-"AgriculturalProducts"
                   
                    CustomsDuties_TE_Agricultural<-CustomsDuties_TE_Agricultural%>%
                      select(Treatment,type_products,CustomsDuties_TE)%>%
                      dplyr::group_by(Treatment,type_products)%>%
                      dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                    
                    
                     CustomsDuties_TE_Industrial$type_products<-"Non_AgriculturalProducts"
                     
                     CustomsDuties_TE_Industrial<-CustomsDuties_TE_Industrial%>%
                       select(Treatment,type_products,CustomsDuties_TE)%>%
                       dplyr::group_by(Treatment,type_products)%>%
                       dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                    
                    
                    
                    CustomsDuties_TE_type_products<-rbind(CustomsDuties_TE_Agricultural,CustomsDuties_TE_Industrial)
                    CustomsDuties_TE_type_products$Chapter<-NULL
                    
                    CustomsDuties_TE_type_products<-melt(CustomsDuties_TE_type_products)
                    
                    CustomsDuties_TE_type_products$Treatment<- factor(CustomsDuties_TE_type_products$Treatment)
                  
          
            # 1.4 TE's by HS Sections --------------------------------------------------------------------
              
              CustomsDuties_TE_Sections<-CustomsDuties_TE_agg_HS%>%
                          dplyr::select(Chapter,Treatment,CustomsDuties_TE)%>%
                          dplyr::group_by(Chapter,Treatment)%>%
                          dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
              
              
              CustomsDuties_TE_Sections<-left_join(CustomsDuties_TE_Sections,HS_Sections,by =c("Chapter"))%>%
                        dplyr::select(Sections,Section_description,Treatment,CustomsDuties_TE)%>%
                        dplyr::group_by(Sections,Section_description,Treatment)%>%
                        dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))%>%
                        data.table()
              
              CustomsDuties_TE_Sections$Treatment<- factor(CustomsDuties_TE_Sections$Treatment)
              
              
            # 1.5 TE's by Chapters ---------------------------------------------------------
              CustomsDuties_TE_Chapters<-CustomsDuties_TE_agg_HS%>%
                      dplyr::select(Chapter,Treatment,CustomsDuties_TE)%>%
                      dplyr::group_by(Chapter,Treatment)%>%
                      dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
            
              CustomsDuties_TE_Chapters<-left_join(CustomsDuties_TE_Chapters,HS_Sections,by =c("Chapter"))%>%
                      dplyr::select(Chapter,Chapters_description,Treatment,CustomsDuties_TE)%>%
                      dplyr::group_by(Chapter,Chapters_description,Treatment)%>%
                      dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
            
              # Factor the "Sections" column without ordering other strings
              CustomsDuties_TE_Chapters$Chapter <- factor(CustomsDuties_TE_Chapters$Chapter)
             
            # 1.6 TE's by WTO classification (MTN) Categories -----------------------------------------
      
              # Multilateral Trade Negotiations (MTN) Categories, HS 2017
              # Multilateral Trade Negotiations (MTN) categories were first defined in the Tokyo Round and adapted for the Harmonized System in the Uruguay Round.
              # The product group breakdown in this publication deviates slightly from the previous definition, which was based on the HS 1992 nomenclature.
              
              CustomsDuties_TE_MTN<-CustomsDuties_TE_agg_HS%>%
                dplyr::select(Six_digit,Treatment,CustomsDuties_TE)%>%
                dplyr::group_by(Six_digit,Treatment)%>%
                dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))


              CustomsDuties_TE_MTN<-left_join(CustomsDuties_TE_MTN,WTO_MTN,by =c("Six_digit"))%>%
                dplyr::select(Product_group,Treatment,CustomsDuties_TE)

              CustomsDuties_TE_MTN <- distinct(CustomsDuties_TE_MTN)

              CustomsDuties_TE_MTN<-CustomsDuties_TE_MTN%>%
              dplyr::group_by(Product_group,Treatment)%>%
                dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))

              CustomsDuties_TE_MTN$Product_group <- ifelse(is.na( CustomsDuties_TE_MTN$Product_group), "Other",  CustomsDuties_TE_MTN$Product_group)

              
              
              # Factor the "Sections" column without ordering other strings
              CustomsDuties_TE_MTN$Product_group <- factor( CustomsDuties_TE_MTN$Product_group)
              
              
            # 1.7 TE's by Countries(Choropleth) -----------------------------------------------
            
              # Adding ISO 3 codes to names of countries from ggplot2
              CustomsDuties_TE_agg_countries<-CustomsDuties_TE_agg_countries%>%
                dplyr::mutate(CustomsDuties_TE=CustomsDuties_TE/1000000)
              
              
              
              # Extracting table for GUI
              CustomsDuties_TE_agg_countries_tbl<-CustomsDuties_TE_agg_countries%>%
              dplyr::arrange(desc(CustomsDuties_TE))
              
              
              CustomsDuties_TE_agg_countries_tbl<-left_join(CustomsDuties_TE_agg_countries_tbl,GeoDimension, by=c("iso3c"))%>%
              select(iso3c,countries,CustomsDuties_TE)%>%
              dplyr::rename("Countries"="countries")
              
              
              CustomsDuties_TE_agg_countries_tbl$CustomsDuties_TE<-round(CustomsDuties_TE_agg_countries_tbl$CustomsDuties_TE,3)
              
              CustomsDuties_TE_agg_countries_tbl<-CustomsDuties_TE_agg_countries_tbl%>%
                dplyr::filter(CustomsDuties_TE>0)%>%
                na.omit()

              mapdata3 <- left_join(mapdata_iso3c,CustomsDuties_TE_agg_countries,by = c("iso3"="iso3c"))

              # Removing Antarctica and Nan values
              mapdata3<-mapdata3[!(mapdata3$region=="Antarctica"),]

               
            # 1.8 TE's by HS codes ---------------------------------------------------------
              CustomsDuties_TE_agg_HS_subset<-CustomsDuties_base%>%
                dplyr::select(HS_code_s,Description,Treatment,CustomsDuties_TE)%>%
                dplyr::filter(Treatment=="NonPreferential")%>%
                dplyr::group_by(HS_code_s,Description,Treatment)%>%
                dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE))
              
              
              CustomsDuties_TE_agg_HS_subset$CustomsDuties_TE<-round(CustomsDuties_TE_agg_HS_subset$CustomsDuties_TE,1)
              
              CustomsDuties_TE_agg_HS_subset <- CustomsDuties_TE_agg_HS_subset %>%
                arrange(desc(CustomsDuties_TE))%>%
                dplyr::rename("HS_code"="HS_code_s")
              
              
              CustomsDuties_TE_agg_HS_subset<-CustomsDuties_TE_agg_HS_subset %>%
                pivot_wider(names_from = Treatment, values_from = CustomsDuties_TE)

              CustomsDuties_TE_agg_HS_subset$NonPreferential <- abs(round(CustomsDuties_TE_agg_HS_subset$NonPreferential, 0))
              
              
              CustomsDuties_TE_agg_HS_subset<-CustomsDuties_TE_agg_HS_subset%>%
                dplyr::rename('TaxExpenditures'='NonPreferential' )
              
              
              # # For treemap
              # CustomsDuties_TE_agg_HS_treemap<-CustomsDuties_TE_agg_HS_subset
              # CustomsDuties_TE_agg_HS_treemap$HS<-"HS codes"
              
              
            # 1.9 TE's by CPA and Combined Nomenclature -------------------------------------------------

        # links https://circabc.europa.eu/ui/group/c1b49c83-24a7-4ff2-951c-621ac0a89fd8/library/d3056f31-a684-430a-a77d-12d1d0fdfffa?p=2&n=10&sort=modified_DESC
              
              actual_year_simulation <- unique(Import_raw_monthly$Year)

               CustomsDuties_TE_CPA<-CustomsDuties_TE_agg_HS%>%
                       dplyr::select(Eight_digit,Treatment,CustomsDuties_TE)%>%
                       dplyr::filter(Treatment=="NonPreferential")%>%
                       dplyr::group_by(Eight_digit,Treatment)%>%
                       dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))


              CPA_CN_subset<-CPA_CN%>%
                       dplyr::filter(year==actual_year_simulation)


              
               CustomsDuties_TE_CPA<-left_join(CustomsDuties_TE_CPA,CPA_CN_subset,by = c("Eight_digit"="CN_CODE"))%>%
                 dplyr::select(CPA_CODE,Treatment,CustomsDuties_TE)%>%
                 dplyr::group_by(CPA_CODE,Treatment)%>%
                 dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))


               CustomsDuties_TE_CPA <- distinct(CustomsDuties_TE_CPA)

               
               # Adding desegregation by CPA codes
               CustomsDuties_TE_CPA <- mutate(CustomsDuties_TE_CPA,
                                              CPA_CODE2 = substr(CPA_CODE, 1, 2))
               
               
               # Preparing subset for table with CPA and NACE section
               CustomsDuties_TE_CPA<-CustomsDuties_TE_CPA%>%
                 dplyr::group_by(CPA_CODE2,Treatment)%>%
                 dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))


               CPA_NACE_subset<-CPA_NACE%>%
                 dplyr::select(CPA_CODE2,CPA21_NAME,Heading_CPA,Sector)%>%
                   na.omit()
               
               CPA_NACE_subset$CPA_CODE2<-as.double(CPA_NACE_subset$CPA_CODE2)
               CustomsDuties_TE_CPA$CPA_CODE2<-as.double(CustomsDuties_TE_CPA$CPA_CODE2)
               
               CustomsDuties_TE_CPA<-left_join(CustomsDuties_TE_CPA,CPA_NACE_subset,by = c("CPA_CODE2"="CPA_CODE2"))
               
              
               CustomsDuties_TE_SECTORS<-CustomsDuties_TE_CPA%>%
                 dplyr::select(Sector,CPA21_NAME,CustomsDuties_TE,Treatment)%>%
                 dplyr::group_by(Sector,CPA21_NAME,Treatment)%>%
                 dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
               
               
                CustomsDuties_TE_SECTORS$CPA21_NAME <- ifelse(is.na( CustomsDuties_TE_SECTORS$CPA21_NAME), "Other",  CustomsDuties_TE_SECTORS$CPA21_NAME)
                CustomsDuties_TE_SECTORS$Sector <- ifelse(is.na( CustomsDuties_TE_SECTORS$Sector), "Other",  CustomsDuties_TE_SECTORS$Sector)


            

      # 2.Main Simulation Results ----------------------------------------------

      
                CustomsDuties_base_non_preferential<-CustomsDuties_base%>%
                  dplyr::filter(Treatment=="NonPreferential")
                
                MainResultsCustoms1<-MacroFiscalData%>%
                  dplyr::filter(Year==actual_year_simulation)%>%
                  dplyr::mutate(
                    # Total
                    `Actual Total Import`=sum(CustomsDuties_base$Value,na.rm=TRUE)/1e+06, 
                    `Actual Total Customs Revenues`=sum(CustomsDuties_base$CustomsRevenue,na.rm=TRUE)/1e+06, 
                    `Actual Total Effective Customs Rate (%)`=sum(CustomsDuties_base$CustomsRevenue,na.rm=TRUE)/sum(CustomsDuties_base$Value,na.rm=TRUE)*100,
                    # Estimated tax expenditures
                    `Import(without FTA)`=sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)/1e+06, 
                    `Actual Customs Revenues(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsRevenue,na.rm=TRUE)/1e+06, 
                    `Effective Customs Rate (%) (without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsRevenue,na.rm=TRUE)/sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)*100,
                    
                    `Customs Revenue Benchmark(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsDuties_Benchmark,na.rm=TRUE)/1e+06,
                    `Benchmark Effective Customs Rate(without FTA)`=`Customs Revenue Benchmark(without FTA)`/(sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)/1e+06)*100,
                    `Tax Expenditures(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsDuties_TE,na.rm=TRUE)/1e+06,
                    `Tax Expenditures(without FTA) as % of GDP`=(`Tax Expenditures(without FTA)`/GDP)*100,
                    `Tax Expenditures(without FTA) as % of import FOB`=(`Tax Expenditures(without FTA)`/ImportsOfGoods_FOB)*100,
                    `Tax Expenditures(without FTA) as % of Government Revenue`=(`Tax Expenditures(without FTA)`/GeneralGovernmentRevenue)*100,
                    `Tax Expenditures (as % of Taxes On Products)`=(`Tax Expenditures(without FTA)`/TaxesOnProducts)*100,
                    `Tax Expenditures (as % of CustomsRevenue)`=(`Tax Expenditures(without FTA)`/ImportDuties)*100)%>%
                  dplyr::select(
                    `Actual Total Import`,
                    `Actual Total Customs Revenues`,
                    `Actual Total Effective Customs Rate (%)`,
                    `Import(without FTA)`,
                    `Actual Customs Revenues(without FTA)`,
                    `Effective Customs Rate (%) (without FTA)`,
                    `Customs Revenue Benchmark(without FTA)`,
                    `Benchmark Effective Customs Rate(without FTA)`,
                    `Tax Expenditures(without FTA)`,
                    `Tax Expenditures(without FTA) as % of GDP`,
                    `Tax Expenditures(without FTA) as % of import FOB`,
                    `Tax Expenditures(without FTA) as % of Government Revenue`,
                    `Tax Expenditures (as % of Taxes On Products)`,
                    `Tax Expenditures (as % of CustomsRevenue)`)
                  
                
                
                MainResultsCustoms1<-melt(MainResultsCustoms1)
                MainResultsCustoms1$value<-round(MainResultsCustoms1$value,2)
                
                MainResultsFinal<-MainResultsCustoms1%>%
                  dplyr::rename("Description"= "variable",
                                "Value"="value")
               
# })
#         
        