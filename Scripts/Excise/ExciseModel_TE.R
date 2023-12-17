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
                    #Import_Excise_Data <- read_excel("~/Models/Kosovo-TE-Models/Data/ImportData/Open_DATA_Import-Janar-Dhjetor-2022.xlsx")      
                   #Import_Excise_Data
                
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
                                                                #CustomsDuties_Benchmark=Value*Benchmark_Customs_Rate,
                                                                #CustomsDuties_TE=round(CustomsDuties_Benchmark-CustomsRevenue,1))
                   #View(CustomsDuties_base) 
                   
                   
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

                              # Subset Chapter 27
                              HS_Chapter_27<-CustomsDuties_TE_agg_HS %>%
                                filter(Chapter == '27')
                              
                             HS_Chapter_27$Subdataset<-c("Fuel") #<---- Ova ke treba detealno da se definira po tip na proizvodi ili po tarifen broj 
                        
                        
                              # Subset Alcohol For Engine
                               AlcoholForEngine <- CustomsDuties_TE_agg_HS %>%
                                dplyr::filter(Eight_digit %in% c('2710 12 31','2710 12 90'))

                               AlcoholForEngine$Subdataset<-c("AlcoholForEngine")
                               
                              # Cyclic and Acyclic hydrocarbons
                               CyclicAcyclicHydrocarbons <- CustomsDuties_TE_agg_HS %>%
                                dplyr::filter(Four_digit %in% c('2901','2902'))
                               
                               CyclicAcyclicHydrocarbons$Subdataset<-c("CyclicAcyclicHydrocarbons")
                               
                               #  Other which are used for the same purposes as mineral oils
                                   # Organic composite solvents and solvents
                                   # Alkylbenzenes and mixed Alkylnaphthalenes
                               
                                Other_Ch_38 <- CustomsDuties_TE_agg_HS %>%
                                 dplyr::filter(Four_digit %in% c('3811','3814','3817'))
                                
                                Other_Ch_38$Subdataset<-c("Other_Ch_38")
                                
                               #  Merging subsets
                                Fuel_tbl <- bind_rows(HS_Chapter_27, AlcoholForEngine,CyclicAcyclicHydrocarbons,Other_Ch_38)
                              
                              #  Removing Nan from effective tax rates
                                Fuel_tbl$Effective_Excise_rate<-ifelse(Fuel_tbl$Quantity==0,0,Fuel_tbl$Effective_Excise_rate)
                              
                              # Filter dataset with excise
                                Fuel_tbl<-Fuel_tbl%>%
                                dplyr::filter(ExciseRevenue>0)
                              
                                Fuel_tbl$DataSet<-c("Fuels")
                              
                              # Estimation of TE's
                              
                              #Benchmark_ExciseFuels <- input$simulationSlider
                              #Benchmark_ExciseFuels <-  0.385 #<---This neeed to be set into GUI
                              
                              # 
                              # Fuel_tbl<-FuelQuantity%>%
                              #   dplyr::mutate(Excise_BenchmarkRevenue=Quantity*Benchmark_ExciseFuels,
                              #                 Excise_TE=Excise_BenchmarkRevenue-ExciseRevenue)
                              # 
                              # 
                              # View(Fuel_tbl)
                              # 
                              # 
                              # #write.csv(FuelQuantity,"FuelQuantity.csv")
                              # 
                              # sum(Fuel_tbl$ExciseRevenue)/1000000
                              # sum(Fuel_tbl$Excise_TE)/1000000
                           
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
                              
                              #sum(Tobacco_tbl$ExciseRevenue)/1000000
                             # 213.8331
                    
            # 4.3 Alcohol -----------------------------------------------------------          
                  # 4.3.1 Beer --------------------------------------------------------------------
                         
                                              # First filter
                                              BeerQuantity <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2203)
                                              
                                              BeerQuantity$Subdataset<-c("Beer")
                              
                                              # # Second filter
                                              # NonAlcoholicBeer <- CustomsDuties_TE_agg_HS %>%
                                              #   filter(Eight_digit == '2202 91 00')
                                              
                                              # Merge the results
                                              #BeerQuantity <- bind_rows(AlcoholicBeer, NonAlcoholicBeer)
                                              

                  # 4.3.2 Sugar-sweetened beverages(SSB) ------------------------------------------------------------------
                              
                              # Non Alcoholic Beer is included here
                              SSB_Quantity <- CustomsDuties_TE_agg_HS %>%
                                filter(Four_digit == 2202)
                              
                                SSB_Quantity$Subdataset<-c("SSB")
                                              
                                              
                  # 4.3.3 Wine of fresh grapes ----------------------------------------------------
                          
                           # Wine of fresh grapes, including fortified wines; grape must other than that of heading 2009)                    
                                              
                                             
                                              WineQuantity <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2204)
                                              
                                              WineQuantity$Subdataset<-c("Wine")
                                           
                                              # View(WineQuantity)
                                              # sum(WineQuantity$Quantity)/1e06
                                             
                          
                          
                  # 4.3.4 Vermouth ---------------------------------------------------
                          
                                             # Vermouth and other wine of fresh grapes flavoured with plants or aromatic substances
                                              
                                              VermouthQuantity <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2205)
                                              
                                              
                                              # View(VermouthQuantity)
                                              # sum(VermouthQuantity$Quantity)/1e06
                                              VermouthQuantity$Subdataset<-c("VermouthAndOtherWines")
                          
                          
                  # 4.3.5 Other fermented beverages --------------------------------------------------
                          
                                              # Other fermented beverages (for example, cider, perry, mead); mixtures of fermented beverages and mixtures of fermented beverages and non-alcoholic beverages, not elsewhere specified or included
                                              
                                              OtherFermentedBeveragesQuantity  <- CustomsDuties_TE_agg_HS %>%
                                                filter(Four_digit == 2206)
                                              
                                              OtherFermentedBeveragesQuantity$Subdataset<-c("OtherFermentedBeverages")

                  # 4.3.6 Undenatured ethyl alcohol ---------------------------------------------
                          
                        # Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80 % vol; spirits, liqueurs and other spirituous beverages
                        UndenaturedEthylAlcoholQuantity  <- CustomsDuties_TE_agg_HS %>%
                        dplyr::filter(Four_digit == 2208)
                                              
                                              
                         # View(UndenaturedEthylAlcoholQuantity)
                         #  sum(UndenaturedEthylAlcoholQuantity$Quantity)/1e06
                          UndenaturedEthylAlcoholQuantity$Subdataset<-c("UndenaturedEthylAlcohol")
                                              

                  # 4.3.7 Merge data --------------------------------------------------

                    # SSB is not included in the dataset      
                          
                    AlcoholQuantity <- bind_rows(BeerQuantity, WineQuantity,VermouthQuantity,OtherFermentedBeveragesQuantity,UndenaturedEthylAlcoholQuantity)
                                              
                                                                  
                    # Removing Nan from effective tax rates
                    AlcoholQuantity$Effective_Excise_rate<-ifelse(AlcoholQuantity$Quantity==0,0,AlcoholQuantity$Effective_Excise_rate)
                                              
                    Alcohol_tbl<-AlcoholQuantity%>%
                                                dplyr::filter(ExciseRevenue>0)                    
                              
                    Alcohol_tbl$DataSet<-c("Alcohol")
                              
            #View(Alcohol_tbl)
                   
           # 5.Cars ------------------------------------------------------------------

                     CarsSet <- CustomsDuties_TE_agg_HS %>%
                              dplyr::filter(Four_digit %in% c('8703'))
                                    
            
                    Cars_tbl<-CarsSet%>%
                      dplyr::filter(ExciseRevenue>0)
                    
                    Cars_tbl$Subdataset<-c("Cars")
                    Cars_tbl$DataSet<-c("Cars")            
                                
                   # View(Cars_tbl)
                                    
        # 5.Merging Data Sets -------------------------------------------------------
                  
                    ExciseFinal_tbl <- bind_rows(Fuel_tbl, Tobacco_tbl,Alcohol_tbl,Cars_tbl)
                    
# 
#                     unique(ExciseFinal_tbl$Subdataset)
#                     unique(ExciseFinal_tbl$DataSet)
                    
                    # Benchmark_ExciseFuels <-  0.385
                    # #Benchmark_ExciseFuels <- input$simulationSlider
                    # #Benchmark_ExciseFuels <-  0.36 #<---This neeed to be set into GUI
                    # Benchmark_ExciseTobacco <-  53 #<---This neeed to be set into GUI
                    # Benchmark_ExciseAlcohol <-  0.385 #<---This neeed to be set into GUI
                    # Benchmark_ExciseCars <-  1800 #<---This neeed to be set into GUI
                    
                    
                    apply_math <- function(data) {
                      data %>%
                        mutate(
                          result = case_when(
                            DataSet == "Fuels" ~ Quantity * Benchmark_ExciseFuels,
                            DataSet == "Tobacco" ~ Quantity * Benchmark_ExciseTobacco,
                            DataSet == "Alcohol" ~ Quantity * Benchmark_ExciseAlcohol,
                            DataSet == "Cars" ~ Quantity * Benchmark_ExciseCars,
                          )
                        )
                    }
                    
                    # Apply the function to your data
                    Estimation_TE <- apply_math(ExciseFinal_tbl)%>%
                      dplyr::rename("Excise_BenchmarkRevenue"="result")%>%
                      dplyr::mutate(Excise_TE=Excise_BenchmarkRevenue-ExciseRevenue)
                    
                    
                    # Define a function to round up two columns
                    round_up_columns <- function(data, columns) {
                      data %>%
                        mutate(across(all_of(columns), ~ceiling(.)))
                    }
                    
                    # Apply the function to round up "value1" and "value2"
                    Estimation_TE <- round_up_columns(Estimation_TE, c("Excise_BenchmarkRevenue", "Excise_TE"))
                    
                    
                    
                   # View(Estimation_TE)
                    
                    
                    
                    
                    print("Simulation is done")
                    # sum(Estimation_TE$ExciseRevenue)/1e06
                    # sum(Estimation_TE$Excise_TE)/1e06
                    
                    #View(ExciseFinal_tbl)     
                                    
# II.Preparing data for charts and for the main table --------------------------------------------

# 1.Preparing data for Charts ---------------------------------------

# Chart 1
                    
                    MacroFiscalData$ImportDuties_PctOfGDP<-round(MacroFiscalData$ImportDuties_PctOfGDP,2)
                    
                    df_plt <- MacroFiscalData %>%
                      dplyr::select(Year, GDP, `Taxes on imports excluding VAT and duties`)
                    
                    
                    ImportDuties_PctOfGDP <- plot_ly(df_plt)
                    ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% add_trace(x = ~Year, y = ~GDP, type = 'bar', name = 'GDP')
                    ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% add_trace(x = ~Year, y = ~`Taxes on imports excluding VAT and duties`, type = 'scatter', mode = 'lines+markers', name = 'Share of excise revenue in GDP ',
                                                                                 yaxis = 'y2', line = list(dash = 'dot', color = "#FFA500", width = 6))
                    
                    ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% layout(title = 'Nominal GDP and share of customs revenues in GDP,2015-2022',
                                                                              xaxis = list(title = ""),
                                                                              yaxis = list(side = 'left', title = 'In million LCU', showgrid = FALSE, zeroline = FALSE),
                                                                              yaxis2 = list(side = 'right', overlaying = "y", title = 'Percentage', showgrid = FALSE, zeroline = FALSE, font = list(size = 11)), 
                                                                              annotations = list(
                                                                                x = -0.1, y = -0.056,
                                                                                text = "Source: National authorities",
                                                                                showarrow = FALSE,
                                                                                xref = 'paper',
                                                                                yref = 'paper',
                                                                                align = 'left'))
                    
                    # ImportDuties_PctOfGDP
# Chart 2
                    
                  
                    year_df <- MacroFiscalData%>% 
                      dplyr::select(Year, "VAT", "ImportDuties", "Taxes on imports excluding VAT and duties",
                                    "Taxes on products, except VAT and import taxes",
                                    "Other taxes on production", "Property income",
                                    "Current taxes on income, wealth, etc.")
                    
                    year_df$Year<-as.factor(year_df$Year)
                    year_df<-melt(year_df)
                    year_df$color <- factor(year_df$variable, labels =c( "orange","brown","forestgreen","red", "cyan","royalblue","blue")) 
                    
                    StructureOfTaxRevenues_Nominal <- plot_ly(year_df, x = ~Year, y = ~value,
                                                              type = 'bar',
                                                              marker = list(color = ~color), name = ~variable) %>%
                      layout(title = 'Structure of tax revenues',font = t_11,
                             xaxis = list(title = ''), 
                             yaxis = list(title = ' '),
                             annotations =
                               list( x = 0, y = -0.05, 
                                     text = "Source:National authorities",font = t_8,
                                     showarrow = F,
                                     xref='paper',
                                     yref='paper',align='left'),barmode = 'stack')
                    
                    # StructureOfTaxRevenues_Nominal
                    
# Chart 3
                    year_df<-group_by(year_df, Year) %>% mutate(Pct = value/sum(value))
                    
                    
                    StructureOfTaxRevenues_Percentage <- plot_ly(year_df, x = ~Year, y = ~Pct*100,
                                                                 type = 'bar',
                                                                 marker = list(color = ~color), name = ~variable) %>%
                      layout(title = 'Structure of revenues in percentage, 2015-2022',font = t_11,
                             xaxis = list(title = ''), 
                             yaxis = list(title = 'In percentage '),
                             annotations =
                               list( x = 0, y = -0.05, 
                                     text = "Source:National authorities",font = t_8,
                                     showarrow = F,
                                     xref='paper',
                                     yref='paper',align='left'),barmode = 'stack')
                    
                    StructureOfTaxRevenues_Percentage
                    
# Chart 4
  
            # 1.1 Regular Import------------------------------------------

                    ExciseGoodRegulaImport<-Estimation_TE%>%
                      dplyr::select(DataSet,Value)%>%
                      dplyr::group_by(DataSet)%>%
                      dplyr::summarise(Value=sum(Value,na.rm = TRUE))

                    # Factor the "Sections" column without ordering other strings
                    ExciseGoodRegulaImport$Chapter <- factor(ExciseGoodRegulaImport$DataSet)

                    ExciseGoodRegulaImport$HS<-"Excise by types of goods "
                    
                    ExciseGoodRegulaImport_plt<-plot_ly(data =ExciseGoodRegulaImport, type = "treemap", values = ~round(Value/1e06,1), labels = ~DataSet,
                                           parents = ~HS,
                                           name = " ",
                                           text = ~DataSet   ,
                                           textinfo="label+value+percent parent")%>%
                      layout(title=paste("Structure of Regular Import by types of excise goods in LCU (Millions),", actual_year_simulation),font =t_11)
                    
                 
#              1.2 Pie chart  --------------------------------------------------------------

                    
                    ExciseRevenuePie<-Estimation_TE%>%
                      dplyr::select(DataSet,ExciseRevenue)%>%
                      dplyr::group_by(DataSet)%>%
                      dplyr::summarise(Value=sum(ExciseRevenue,na.rm = TRUE))
                    
                                         
                    ExciseRevenueBasePie<-melt(ExciseRevenuePie)
                    
                    ImportStructureExcise <- ExciseRevenueBasePie %>% 
                      plot_ly(labels = ~DataSet, values = ~value)
                    
                    ImportStructureExcise <- ImportStructureExcise %>% add_pie(hole = 0.6)
                    ImportStructureExcise <- ImportStructureExcise %>% layout(
                      title = paste("Structure of excise revenues by type of excise goods", actual_year_simulation),
                      font = t_11, 
                      showlegend = TRUE,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      annotations = list(
                        x = 0, y = -0.1,
                        #title = "Additional Information:",  # New subtitle
                        #text = "Preferential treatment includes goods covered by Free Trade Agreements, while non-preferential treatment includes only imports subject to customs tariffs.",
                        showarrow = FALSE,
                        xref = 'paper',
                        yref = 'paper',
                        align = 'left'
                      ),
                      font = t_8
                    )
                    
                    # ImportStructureExcise

# TaxExpenditures Tab 
#             # 1.3 TE's by HS Type of products -----------------------------------------
   
#                   CustomsDuties_TE_type_products<-CustomsDuties_TE_agg_HS%>%
#                     dplyr::select(Chapter,Treatment,CustomsDuties_TE)%>%
#                     dplyr::group_by(Chapter,Treatment)%>%
#                     dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
#           
#                     # Select Agricultural
#                     CustomsDuties_TE_Agricultural<-CustomsDuties_TE_type_products%>%
#                                 dplyr::filter(Chapter %in% c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))
#                     
#                     # Select Industrial
#                     CustomsDuties_TE_Industrial<-CustomsDuties_TE_type_products%>%
#                                 dplyr::filter(!Chapter %in% c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"))
#                         
#                     CustomsDuties_TE_Agricultural$type_products<-"AgriculturalProducts"
#                    
#                     CustomsDuties_TE_Agricultural<-CustomsDuties_TE_Agricultural%>%
#                       select(Treatment,type_products,CustomsDuties_TE)%>%
#                       dplyr::group_by(Treatment,type_products)%>%
#                       dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
#                     
#                     
#                      CustomsDuties_TE_Industrial$type_products<-"Non_AgriculturalProducts"
#                      
#                      CustomsDuties_TE_Industrial<-CustomsDuties_TE_Industrial%>%
#                        select(Treatment,type_products,CustomsDuties_TE)%>%
#                        dplyr::group_by(Treatment,type_products)%>%
#                        dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
#                     
#                     
#                     
#                     CustomsDuties_TE_type_products<-rbind(CustomsDuties_TE_Agricultural,CustomsDuties_TE_Industrial)
#                     CustomsDuties_TE_type_products$Chapter<-NULL
#                     
#                     CustomsDuties_TE_type_products<-melt(CustomsDuties_TE_type_products)
#                     
#                     CustomsDuties_TE_type_products$Treatment<- factor(CustomsDuties_TE_type_products$Treatment)


                    # Chart 1
                    
                    ExciseProducts_TE<-Estimation_TE%>%
                      dplyr::select(DataSet,ExciseRevenue)%>%
                      dplyr::group_by(DataSet)%>%
                      dplyr::summarise(Value=sum(ExciseRevenue,na.rm = TRUE))
                    
                    
                    ExciseProducts_TE<-melt(ExciseProducts_TE)
                                         
                    ExciseProducts_TE$DataSet<- factor(ExciseProducts_TE$DataSet)
                    
                    ExciseProductCategories <- plot_ly(ExciseProducts_TE, x = ~DataSet , y = ~value, type = 'bar', text = ' ', hoverinfo = 'y+text', color = ~DataSet, colors = colors) %>% 
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
                    
                    ExciseProductCategories        
                    
                             
         
#             # 1.4 TE's by HS Sections --------------------------------------------------------------------

              # CustomsDuties_TE_Sections<-CustomsDuties_TE_agg_HS%>%
              #             dplyr::select(Chapter,Treatment,CustomsDuties_TE)%>%
              #             dplyr::group_by(Chapter,Treatment)%>%
              #             dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
              # 
              # 
              # CustomsDuties_TE_Sections<-left_join(CustomsDuties_TE_Sections,HS_Sections,by =c("Chapter"))%>%
              #           dplyr::select(Sections,Section_description,Treatment,CustomsDuties_TE)%>%
              #           dplyr::group_by(Sections,Section_description,Treatment)%>%
              #           dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))%>%
              #           data.table()
              # 
              # CustomsDuties_TE_Sections$Treatment<- factor(CustomsDuties_TE_Sections$Treatment)
              # 
              
  
              
               
#             # 1.5 TE's by Chapters ---------------------------------------------------------
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

               
              Chapters_HS <- plot_ly(Excise_TE_Chapters, x = ~reorder(Chapter, -Excise_TE), y = ~Excise_TE, type = 'bar', text = ' ', hoverinfo = 'y+text',#color = ~Treatment, colors = colors,
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
              
              Chapters_HS
              
#             # 1.6 TE's by WTO classification (MTN) Categories -----------------------------------------
#       
#               # Multilateral Trade Negotiations (MTN) Categories, HS 2017
#               # Multilateral Trade Negotiations (MTN) categories were first defined in the Tokyo Round and adapted for the Harmonized System in the Uruguay Round.
#               # The product group breakdown in this publication deviates slightly from the previous definition, which was based on the HS 1992 nomenclature.
#               
              # CustomsDuties_TE_MTN<-CustomsDuties_TE_agg_HS%>%
              #   dplyr::select(Six_digit,Treatment,CustomsDuties_TE)%>%
              #   dplyr::group_by(Six_digit,Treatment)%>%
              #   dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
              # 
              # 
              # CustomsDuties_TE_MTN<-left_join(CustomsDuties_TE_MTN,WTO_MTN,by =c("Six_digit"))%>%
              #   dplyr::select(Product_group,Treatment,CustomsDuties_TE)
              # 
              # CustomsDuties_TE_MTN <- distinct(CustomsDuties_TE_MTN)
              # 
              # CustomsDuties_TE_MTN<-CustomsDuties_TE_MTN%>%
              # dplyr::group_by(Product_group,Treatment)%>%
              #   dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
              # 
              # CustomsDuties_TE_MTN$Product_group <- ifelse(is.na( CustomsDuties_TE_MTN$Product_group), "Other",  CustomsDuties_TE_MTN$Product_group)
              # 
              # 
              # 
              # # Factor the "Sections" column without ordering other strings
              # CustomsDuties_TE_MTN$Product_group <- factor( CustomsDuties_TE_MTN$Product_group)

              Excise_TE_MTN<-Estimation_TE%>%
                dplyr::select(Six_digit,Excise_TE)%>%
                dplyr::group_by(Six_digit)%>%
                dplyr::summarise(Excise_TE=sum(Excise_TE,na.rm = TRUE))
              
              
              Excise_TE_MTN<-left_join(Excise_TE_MTN,WTO_MTN,by =c("Six_digit"))%>%
                dplyr::select(Product_group,Excise_TE)
              
              Excise_TE_MTN <- distinct(Excise_TE_MTN)
              
              Excise_TE_MTN<-Excise_TE_MTN%>%
                dplyr::group_by(Product_group)%>%
                dplyr::summarise(Excise_TE=sum(Excise_TE,na.rm = TRUE))
              
              Excise_TE_MTN$Product_group <- ifelse(is.na( Excise_TE_MTN$Product_group), "Other",  Excise_TE_MTN$Product_group)
              
              
              
              # Factor the "Sections" column without ordering other strings
              Excise_TE_MTN$Product_group <- factor( Excise_TE_MTN$Product_group)
              
              
              
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
              ProductGroups_MTN <- ProductGroups_MTN %>% 
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

#               
#             # 1.7 TE's by Countries(Choropleth) -----------------------------------------------
#             
#               # Adding ISO 3 codes to names of countries from ggplot2
#               CustomsDuties_TE_agg_countries<-CustomsDuties_TE_agg_countries%>%
#                 dplyr::mutate(CustomsDuties_TE=CustomsDuties_TE/1000000)
   
#               # Extracting table for GUI
#               CustomsDuties_TE_agg_countries_tbl<-CustomsDuties_TE_agg_countries%>%
#               dplyr::arrange(desc(CustomsDuties_TE))
#               
#               
#               CustomsDuties_TE_agg_countries_tbl<-left_join(CustomsDuties_TE_agg_countries_tbl,GeoDimension, by=c("iso3c"))%>%
#               select(iso3c,countries,CustomsDuties_TE)%>%
#               dplyr::rename("Countries"="countries")
#               
#               
#               CustomsDuties_TE_agg_countries_tbl$CustomsDuties_TE<-round(CustomsDuties_TE_agg_countries_tbl$CustomsDuties_TE,3)
#               
#               CustomsDuties_TE_agg_countries_tbl<-CustomsDuties_TE_agg_countries_tbl%>%
#                 dplyr::filter(CustomsDuties_TE>0)%>%
#                 na.omit()
# 
#               mapdata3 <- left_join(mapdata_iso3c,CustomsDuties_TE_agg_countries,by = c("iso3"="iso3c"))
# 
#               # Removing Antarctica and Nan values
#               mapdata3<-mapdata3[!(mapdata3$region=="Antarctica"),]

              
        # ZA OVA DA RABOTI TREBA DELOT GORE DA SE KORGIRA KADE SE SE ZEMJITE INACE NEMA DA RABOTI !!!
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
           
                            
                            
 
                            
                                       
#             # 1.8 TE's by HS codes ---------------------------------------------------------
#               CustomsDuties_TE_agg_HS_subset<-CustomsDuties_base%>%
#                 dplyr::select(HS_code_s,Description,Treatment,CustomsDuties_TE)%>%
#                 dplyr::filter(Treatment=="NonPreferential")%>%
#                 dplyr::group_by(HS_code_s,Description,Treatment)%>%
#                 dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE))
#               
#               
#               CustomsDuties_TE_agg_HS_subset$CustomsDuties_TE<-round(CustomsDuties_TE_agg_HS_subset$CustomsDuties_TE,1)
#               
#               CustomsDuties_TE_agg_HS_subset <- CustomsDuties_TE_agg_HS_subset %>%
#                 arrange(desc(CustomsDuties_TE))%>%
#                 dplyr::rename("HS_code"="HS_code_s")
#               
#               
#               CustomsDuties_TE_agg_HS_subset<-CustomsDuties_TE_agg_HS_subset %>%
#                 pivot_wider(names_from = Treatment, values_from = CustomsDuties_TE)
# 
#               CustomsDuties_TE_agg_HS_subset$NonPreferential <- abs(round(CustomsDuties_TE_agg_HS_subset$NonPreferential, 0))
#               
#               
#               CustomsDuties_TE_agg_HS_subset<-CustomsDuties_TE_agg_HS_subset%>%
#                 dplyr::rename('TaxExpenditures'='NonPreferential' )
#               
#               
#               # # For treemap
#               # CustomsDuties_TE_agg_HS_treemap<-CustomsDuties_TE_agg_HS_subset
#               # CustomsDuties_TE_agg_HS_treemap$HS<-"HS codes"
#               
#               
#             # 1.9 TE's by CPA and Combined Nomenclature -------------------------------------------------
# 
#         # links https://circabc.europa.eu/ui/group/c1b49c83-24a7-4ff2-951c-621ac0a89fd8/library/d3056f31-a684-430a-a77d-12d1d0fdfffa?p=2&n=10&sort=modified_DESC
#               
#               actual_year_simulation <- unique(Import_raw_monthly$Year)
# 
#                CustomsDuties_TE_CPA<-CustomsDuties_TE_agg_HS%>%
#                        dplyr::select(Eight_digit,Treatment,CustomsDuties_TE)%>%
#                        dplyr::filter(Treatment=="NonPreferential")%>%
#                        dplyr::group_by(Eight_digit,Treatment)%>%
#                        dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
# 
# 
#               CPA_CN_subset<-CPA_CN%>%
#                        dplyr::filter(year==actual_year_simulation)
# 
# 
#               
#                CustomsDuties_TE_CPA<-left_join(CustomsDuties_TE_CPA,CPA_CN_subset,by = c("Eight_digit"="CN_CODE"))%>%
#                  dplyr::select(CPA_CODE,Treatment,CustomsDuties_TE)%>%
#                  dplyr::group_by(CPA_CODE,Treatment)%>%
#                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
# 
# 
#                CustomsDuties_TE_CPA <- distinct(CustomsDuties_TE_CPA)
# 
#                
#                # Adding desegregation by CPA codes
#                CustomsDuties_TE_CPA <- mutate(CustomsDuties_TE_CPA,
#                                               CPA_CODE2 = substr(CPA_CODE, 1, 2))
#                
#                
#                # Preparing subset for table with CPA and NACE section
#                CustomsDuties_TE_CPA<-CustomsDuties_TE_CPA%>%
#                  dplyr::group_by(CPA_CODE2,Treatment)%>%
#                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
# 
# 
#                CPA_NACE_subset<-CPA_NACE%>%
#                  dplyr::select(CPA_CODE2,CPA21_NAME,Heading_CPA,Sector)%>%
#                    na.omit()
#                
#                CPA_NACE_subset$CPA_CODE2<-as.double(CPA_NACE_subset$CPA_CODE2)
#                CustomsDuties_TE_CPA$CPA_CODE2<-as.double(CustomsDuties_TE_CPA$CPA_CODE2)
#                
#                CustomsDuties_TE_CPA<-left_join(CustomsDuties_TE_CPA,CPA_NACE_subset,by = c("CPA_CODE2"="CPA_CODE2"))
#                
#               
#                CustomsDuties_TE_SECTORS<-CustomsDuties_TE_CPA%>%
#                  dplyr::select(Sector,CPA21_NAME,CustomsDuties_TE,Treatment)%>%
#                  dplyr::group_by(Sector,CPA21_NAME,Treatment)%>%
#                  dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
#                
#                
#                 CustomsDuties_TE_SECTORS$CPA21_NAME <- ifelse(is.na( CustomsDuties_TE_SECTORS$CPA21_NAME), "Other",  CustomsDuties_TE_SECTORS$CPA21_NAME)
#                 CustomsDuties_TE_SECTORS$Sector <- ifelse(is.na( CustomsDuties_TE_SECTORS$Sector), "Other",  CustomsDuties_TE_SECTORS$Sector)
# 
# 
#             
# 
#       # 2.Main Simulation Results ----------------------------------------------
# 
#       
#                 CustomsDuties_base_non_preferential<-CustomsDuties_base%>%
#                   dplyr::filter(Treatment=="NonPreferential")
#                 
#                 MainResultsCustoms1<-MacroFiscalData%>%
#                   dplyr::filter(Year==actual_year_simulation)%>%
#                   dplyr::mutate(
#                     # Total
#                     `Actual Total Import`=sum(CustomsDuties_base$Value,na.rm=TRUE)/1e+06, 
#                     `Actual Total Customs Revenues`=sum(CustomsDuties_base$CustomsRevenue,na.rm=TRUE)/1e+06, 
#                     `Actual Total Effective Customs Rate (%)`=sum(CustomsDuties_base$CustomsRevenue,na.rm=TRUE)/sum(CustomsDuties_base$Value,na.rm=TRUE)*100,
#                     # Estimated tax expenditures
#                     `Import(without FTA)`=sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)/1e+06, 
#                     `Actual Customs Revenues(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsRevenue,na.rm=TRUE)/1e+06, 
#                     `Effective Customs Rate (%) (without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsRevenue,na.rm=TRUE)/sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)*100,
#                     
#                     `Customs Revenue Benchmark(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsDuties_Benchmark,na.rm=TRUE)/1e+06,
#                     `Benchmark Effective Customs Rate(without FTA)`=`Customs Revenue Benchmark(without FTA)`/(sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)/1e+06)*100,
#                     `Tax Expenditures(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsDuties_TE,na.rm=TRUE)/1e+06,
#                     `Tax Expenditures(without FTA) as % of GDP`=(`Tax Expenditures(without FTA)`/GDP)*100,
#                     `Tax Expenditures(without FTA) as % of import FOB`=(`Tax Expenditures(without FTA)`/ImportsOfGoods_FOB)*100,
#                     `Tax Expenditures(without FTA) as % of Government Revenue`=(`Tax Expenditures(without FTA)`/GeneralGovernmentRevenue)*100,
#                     `Tax Expenditures (as % of Taxes On Products)`=(`Tax Expenditures(without FTA)`/TaxesOnProducts)*100,
#                     `Tax Expenditures (as % of CustomsRevenue)`=(`Tax Expenditures(without FTA)`/ImportDuties)*100)%>%
#                   dplyr::select(
#                     `Actual Total Import`,
#                     `Actual Total Customs Revenues`,
#                     `Actual Total Effective Customs Rate (%)`,
#                     `Import(without FTA)`,
#                     `Actual Customs Revenues(without FTA)`,
#                     `Effective Customs Rate (%) (without FTA)`,
#                     `Customs Revenue Benchmark(without FTA)`,
#                     `Benchmark Effective Customs Rate(without FTA)`,
#                     `Tax Expenditures(without FTA)`,
#                     `Tax Expenditures(without FTA) as % of GDP`,
#                     `Tax Expenditures(without FTA) as % of import FOB`,
#                     `Tax Expenditures(without FTA) as % of Government Revenue`,
#                     `Tax Expenditures (as % of Taxes On Products)`,
#                     `Tax Expenditures (as % of CustomsRevenue)`)
#                   
#                 
#                 
#                 MainResultsCustoms1<-melt(MainResultsCustoms1)
#                 MainResultsCustoms1$value<-round(MainResultsCustoms1$value,2)
#                 
#                 MainResultsFinal<-MainResultsCustoms1%>%
#                   dplyr::rename("Description"= "variable",
#                                 "Value"="value")
#                
# # })

                                            # CustomsDuties_base_non_preferential<-CustomsDuties_base%>%
                                            #   dplyr::filter(Treatment=="NonPreferential")

                                            MainResultsExcise<-MacroFiscalData%>%
                                              dplyr::filter(Year==actual_year_simulation)%>%
                                              dplyr::mutate(
                                                # Total
                                                `Actual Total Import`=sum(Estimation_TE$Value,na.rm=TRUE)/1e+06,
                                                `Actual Total Excise Revenues`=sum(Estimation_TE$ExciseRevenue,na.rm=TRUE)/1e+06,
                                                #`Actual Total Effective Customs Rate (%)`=sum(Estimation_TE$CustomsRevenue,na.rm=TRUE)/sum(Estimation_TE$Value,na.rm=TRUE)*100,
                                                # Estimated tax expenditures
                                                #`Import(without FTA)`=sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)/1e+06,
                                                #`Actual Customs Revenues(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsRevenue,na.rm=TRUE)/1e+06,
                                                #`Effective Customs Rate (%) (without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsRevenue,na.rm=TRUE)/sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)*100,

                                                # `Customs Revenue Benchmark(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsDuties_Benchmark,na.rm=TRUE)/1e+06,
                                                # `Benchmark Effective Customs Rate(without FTA)`=`Customs Revenue Benchmark(without FTA)`/(sum(CustomsDuties_base_non_preferential$Value,na.rm=TRUE)/1e+06)*100,
                                                # `Tax Expenditures(without FTA)`=sum(CustomsDuties_base_non_preferential$CustomsDuties_TE,na.rm=TRUE)/1e+06,
                                                # `Tax Expenditures(without FTA) as % of GDP`=(`Tax Expenditures(without FTA)`/GDP)*100,
                                                # `Tax Expenditures(without FTA) as % of import FOB`=(`Tax Expenditures(without FTA)`/ImportsOfGoods_FOB)*100,
                                                # `Tax Expenditures(without FTA) as % of Government Revenue`=(`Tax Expenditures(without FTA)`/GeneralGovernmentRevenue)*100,
                                                # `Tax Expenditures (as % of Taxes On Products)`=(`Tax Expenditures(without FTA)`/TaxesOnProducts)*100,
                                                # `Tax Expenditures (as % of CustomsRevenue)`=(`Tax Expenditures(without FTA)`/ImportDuties)*100
                                                
                                                )%>%
                                              dplyr::select(
                                                `Actual Total Import`,
                                                `Actual Total Excise Revenues`
                                                # `Actual Total Effective Customs Rate (%)`,
                                                # `Import(without FTA)`,
                                                # `Actual Customs Revenues(without FTA)`,
                                                # `Effective Customs Rate (%) (without FTA)`,
                                                # `Customs Revenue Benchmark(without FTA)`,
                                                # `Benchmark Effective Customs Rate(without FTA)`,
                                                # `Tax Expenditures(without FTA)`,
                                                # `Tax Expenditures(without FTA) as % of GDP`,
                                                # `Tax Expenditures(without FTA) as % of import FOB`,
                                                # `Tax Expenditures(without FTA) as % of Government Revenue`,
                                                # `Tax Expenditures (as % of Taxes On Products)`,
                                                # `Tax Expenditures (as % of CustomsRevenue)`
                                                )



                                            MainResultsExcise<-melt(MainResultsExcise)
                                            MainResultsExcise$value<-round(MainResultsExcise$value,2)

                                            MainResultsExciseFinal<-MainResultsExcise%>%
                                              dplyr::rename("Description"= "variable",
                                                            "Value"="value")

                            
                               