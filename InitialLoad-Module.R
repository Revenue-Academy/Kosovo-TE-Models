
'Install packages and importing of data
                                          '
'Step 1. Set your local path to the model directory'

rm(list = ls())
path1<-" "# <--------Set your path here

'Step 2. Press CTRL+A to select all lines in this script and after that press CTRL+Enter to execute selected lines'

# I. INSTALLING LIBRARIES  -------------------------------------------------


# # Define the list of required packages, removing duplicates
# list.of.packages <- unique(c("shinydashboard",
#                              "DT",
#                              "readxl",
#                              "openxlsx",
#                              "shinyjs",
#                              "plotly",
#                              "ggplot2",
#                              "data.table",
#                              "fontawesome",
#                              "tidyverse",
#                              "countrycode",
#                              "shiny",
#                              "kableExtra",
#                              "stringr",
#                              "reshape2",
#                              "base64enc",
#                              "maps",
#                              "sfo",
#                              "circlize",
#                              "flexdashboard",
#                              "rpivotTable",
#                              "sm",
#                              "ks",
#                              "shinyWidgets",
#                              "plyr",
#                              "shinycssloaders",
#                              "future",
#                              "promises",
#                              "parallel",
#                              "purrr",
#                              "tidyr",
#                              "RColorBrewer",
#                              "Hmisc",
#                              "stringr"))
# 
# # Check for missing packages and install them
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
# if (length(new.packages)) install.packages(new.packages)

# # Load all packages
# lapply(list.of.packages, library, character.only = TRUE)



# II. IMPORTING RAW DATA  -------------------------



           library(tidyverse)
           library(readxl)
           library(countrycode)
           library(maps)
           library(ggplot2)
           library(reshape2)
           #library(rccmisc) 
           library(openxlsx)
           library(readxl)
          library(stringr)
                  # 1.Customs Duties --------------------------------------------------------
              
                      
                        path <- paste0(path1, "/Data/ImportData")
                        setwd(path)
                        getwd()
                      
                       
              
                                # https://dogana.rks-gov.net/tarik/TARIK_VERSION01012023_PDF%20ENG/INTRODUCTION.pdf
                                # https://dogana.rks-gov.net/en/per-doganen/statistikat-dhe-arritjet/trading-balance-based-on-tariffs/
                                
                                # WTO classification
                                WTO_MTN <- read_excel("WTO-CORRELATION/WTO_HS.xlsx", 
                                                      sheet = "WTO_HS")
                                # Classifications on economic statistics-UN
                                # https://unstats.un.org/unsd/classifications/Econ#corresp-hs
              
                                
                                # BEC<-read_excel("BEC/HS-BEC.xlsx",sheet = "HS SITC BEC")%>%
                                #   dplyr::select(HS17,HS22,BEC5)
                                
                                # HS-Sections
                                HS_Sections <- read_excel("WTO-CORRELATION/WTO_HS.xlsx", 
                                                          sheet = "HS_SECTIONS")
                                
                                HS_Sections <- HS_Sections %>%
                                  mutate(Section_description = str_to_title(Section_description))
                                
                                # # Countries
                                GeoDimension <- read_excel("GEO-DIMENSION/GeoDimension.xlsx")%>%
                                  dplyr::select(iso2c,iso3c,countries,FreeTradeAgreements)
                                # 
                                # 
                                # Maps
                                mapdata <- map_data("world")
                                iso3c <- data.frame(iso3=countrycode(mapdata$region, "country.name", "iso3c"))
                                mapdata_iso3c<-cbind(mapdata,iso3c)

                                # Removing Antarctica and Nan values
                                mapdata_iso3c<-mapdata_iso3c[!(mapdata_iso3c$region=="Antarctica"),]

                      
                                # Import Macro Fiscal Data
                                
                                MacroFiscalData <- read_excel("MacroFiscalData/MacroFiscalData.xlsx", 
                                                      sheet = "MacroFiscalData")
                      
                                
                                # CPA LINKS FOR DATA 
                                # https://op.europa.eu/en/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/cpa21
              
                                # CPA_CN <- read_excel("CPA-CORRELATION/CPA21_CN2018_2023.xlsx")
                                # CPA_NACE <- read_excel("CPA-CORRELATION/CPA21_NACE2_Table.xlsx")
              
                                # NEW DATA for 2023
                                
                                #taric_data <- read_excel("~/Models/Kosovo_Models/Data/ImportData/Tarifa_Per_WEB-2023 ..xlsx")%>%
                                  taric_data <- read_excel("Tarifa_Per_WEB-2023 ..xlsx")%>%
                                  select(-c("TAR_ALL","TAR_DSC2","TAR_DSC","TAR_ALL2","TAR_ALL3","MPT_IMPORT","MPT_EKSPORT",'VALID_FROM'))%>%
                                  dplyr::rename('HS_code'='TAR_10',
                                                'Description_EN'='TAR_DSC3',
                                                'SupplementaryUnit'='UOM_COD1',
                                                'Excise_Description'='Përshkrimi Akcizës',
                                                'CustomsRate_MFN'='TAR_T01_DOGANA',
                                                'CustomsRate_CEFTA'='TAR_T04_CEFTA',
                                                'CustomsRate_MSA'='TAR_T05_MSA',
                                                'CustomsRate_TR'='TAR_T06_TRMTL',
                                                'ExciseRate'='TAR_T02_AKCIZA',
                                                'VAT_Rate'='TAR_T03_TVSH'
                                  )
                                
                                
                                taric_data$CustomsRate_MFN<-as.double(taric_data$CustomsRate_MFN)
                                taric_data$ExciseRate <-as.double(taric_data$ExciseRate)
                                taric_data$VAT_Rate <-as.double(taric_data$VAT_Rate)
                                taric_data$CustomsRate_CEFTA<-as.double(taric_data$CustomsRate_CEFTA)
                                taric_data$CustomsRate_MSA<-as.double(taric_data$CustomsRate_MSA)
                                taric_data$CustomsRate_TR <-as.double(taric_data$CustomsRate_TR)
                                
                                
                                
                                taric_data <- mutate(taric_data,Chapter = substr(HS_code, 1, 2))
                                
                                
                                # Add Excise description on English
                                

                                
                                customs_simulation_parameters_raw<-left_join(taric_data,HS_Sections,by=c("Chapter"))%>%
                                  select(Chapter, Chapters_description, everything()) %>%
                                  select(-Sections, -Section_description)
              
                                CN2023_Structure <- read_excel("CN/CN2023_Structure.xlsx")%>%
                                  dplyr::select(CN_CODE,NAME_EN)
                                  
                                
                                CN2023_Structure_filtered <- CN2023_Structure %>%
                                  dplyr::filter(nchar(CN_CODE) == 4)
                                
                                # If you want to add a column extracting the first 4 characters (although in this case all would be 4 characters)
                                # Here we will add a new column 'CN_CODE_first4' which contains the first 4 characters of CN_CODE.
                                CN2023_Structure_filtered <- CN2023_Structure_filtered %>%
                                  dplyr::mutate( HS4_COD = substr(CN_CODE, 1, 4))%>%
                                  dplyr::rename("Description_Chapters " = "NAME_EN")
              
                                
                                customs_simulation_parameters<-left_join(customs_simulation_parameters_raw,CN2023_Structure_filtered,by=c("HS4_COD"="HS4_COD"))
                               # View(customs_simulation_parameters)
                                
                                
                                customs_simulation_parameters <- customs_simulation_parameters %>%
                                  dplyr::mutate(
                                    Chapters_description = paste(Chapter, Chapters_description, sep = "-"),
                                    Description_Chapters = paste(HS4_COD, `Description_Chapters `, sep = "-"),
                                    HS_code1 = paste(HS_code, Description_EN , sep = "-")
                                  ) %>%
                                  dplyr::select(-Chapter,-HS6_COD, -HS4_COD, -`Description_Chapters `)
                                
                                
                                # OLD 21-11-2024
                                customs_simulation_parameters <- customs_simulation_parameters %>%
                                  dplyr::select(Chapters_description, Description_Chapters, HS_code, everything())
                                
                                
                                
              
                  # 1.1 Regular Import ----------------------------------------------------------
              
                                #library(readxl)
                                Import_raw_monthly <- read_excel("Open_DATA_Import Janar-Dhjetor 2023.xlsx")                  
                                
                                
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
                                CustomsDuties_base <- Import_raw_monthly %>%
                                  dplyr::select(HS_code, Value, Quantity, Netweight, CustomsRevenue, ExciseRevenue, VAT_Revenue) %>%
                                  dplyr::group_by(HS_code) %>%
                                  dplyr::summarise(
                                    Value = sum(Value, na.rm = TRUE),
                                    Quantity = sum(Quantity, na.rm = TRUE),
                                    Netweight = sum(Netweight, na.rm = TRUE),
                                    CustomsRevenue = sum(CustomsRevenue, na.rm = TRUE),
                                    ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE),
                                    Effective_Customs_rate = round(sum(CustomsRevenue, na.rm = TRUE) / sum(Value, na.rm = TRUE), 4) * 100,
                                    Effective_Excise_rate = round(sum(ExciseRevenue, na.rm = TRUE) / sum(Quantity), 3),#*100,
                                    Effective_VAT_rate = round(
                                      sum(VAT_Revenue, na.rm = TRUE) / 
                                        (sum(Value, na.rm = TRUE) +sum(CustomsRevenue, na.rm = TRUE)+ sum(ExciseRevenue, na.rm = TRUE)), 2
                                    ) * 100
                                    
                                  )
                                
              
                             # Final merging
                                  
                                
                                customs_simulation_parameters <- customs_simulation_parameters %>%
                                  dplyr::mutate(across(where(is.numeric), ~ replace_na(., 0)),
                                                across(where(is.character), ~ replace_na(., "")))
                                  
                                  customs_simulation_parameters_final<-left_join(customs_simulation_parameters,CustomsDuties_base,by=c("HS_code"))
                                  
              
                                  customs_simulation_parameters_final$CN_CODE<-NULL
                                  customs_simulation_parameters_final$Description_EN<-NULL
                                  #customs_simulation_parameters_final$Year=2023
                                  
                                  customs_simulation_parameters_final$Year=as.numeric( unique(Import_raw_monthly$Year))
                                  
                                  
                                  customs_simulation_parameters_final$HS_code<-NULL
                                  

                                  customs_simulation_parameters_final<-customs_simulation_parameters_final%>%
                                     dplyr::rename("HS_code"="HS_code1")

# 1.2 Adding base for calculation of Excise Base ------------------------------
                                  # 1.2.1 Alcohol and SSB  -----------------------------------------------------------          
                                        # 1.2.2 Beer --------------------------------------------------------------------
                                        customs_simulation_parameters_final$Description_Chapters <- ifelse(
                                          customs_simulation_parameters_final$Description_Chapters == "2203-NA","2203-Beer made from malt",
                                          customs_simulation_parameters_final$Description_Chapters
                                        )
                                  
                                        customs_simulation_parameters_final$Description_Chapters <- ifelse(
                                          customs_simulation_parameters_final$Description_Chapters == "2206-NA","2206-Other fermented beverages",
                                          customs_simulation_parameters_final$Description_Chapters
                                        )
                                        
                                  
                                  
                                  
                            # 1.Conversion excise in statutory rates
                                  
                            customs_simulation_parameters_final_excise <- customs_simulation_parameters_final %>%
                                                mutate(
                                                  ExciseRate = case_when(
                                                    Description_Chapters == "2203-Beer made from malt" ~ ExciseRate * 100,
                                                    Description_Chapters == "2204-Wine of fresh grapes, including fortified wines; grape must other than that of heading 2009" ~ ExciseRate * 100,
                                                    Description_Chapters == "2205-Vermouth and other wine of fresh grapes flavoured with plants or aromatic substances" ~ ExciseRate * 100,
                                                    Description_Chapters == "2206-Other fermented beverages" ~ ExciseRate * 100,
                                                    Description_Chapters == "2207-Undenatured ethyl alcohol of an alcoholic strength by volume of 80 % vol or higher; ethyl alcohol and other spirits, denatured, of any strength" ~ ExciseRate * 100,
                                                    Description_Chapters == "2208-Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80 % vol; spirits, liqueurs and other spirituous beverages"~ ExciseRate * 100,
                                                    TRUE ~ ExciseRate # Keeps the original ExciseRate for all other rows
                                                  )
                                                )
                                              
                            # 2. Commercial names
                            
                            customs_simulation_parameters_final_excise_names <- customs_simulation_parameters_final_excise %>%
                              mutate(
                                Category = case_when(
                                # SSB
                                  Description_Chapters == "2202-Waters, including mineral waters and aerated waters, containing added sugar or other sweetening matter or flavoured, and other non-alcoholic beverages, not including fruit, nut or vegetable juices of heading 2009" ~ "SSB",
                                # Alcohol
                                  Description_Chapters == "2203-Beer made from malt" ~ "BEER",
                                  Description_Chapters == "2204-Wine of fresh grapes, including fortified wines; grape must other than that of heading 2009" ~ "WINE",
                                  Description_Chapters == "2205-Vermouth and other wine of fresh grapes flavoured with plants or aromatic substances" ~ "WINE",
                                  Description_Chapters == "2206-Other fermented beverages" ~ "OTHER FERMENTED BEVERAGES",
                                  Description_Chapters == "2207-Undenatured ethyl alcohol of an alcoholic strength by volume of 80 % vol or higher; ethyl alcohol and other spirits, denatured, of any strength" ~ "INDUSTRIAL ALCOHOL",
                                  Description_Chapters == "2208-Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80 % vol; spirits, liqueurs and other spirituous beverages" ~ "ALCHOLIC BEVERAGE",
                                # Tobacco
                                  Description_Chapters == "2401-Unmanufactured tobacco; tobacco refuse" ~ "TOBACCO",
                                  str_detect(HS_code, "^2402100") ~ "CIGARS AND CIGARILLOS",
                                  str_detect(HS_code, "^24022010") ~ "CIGARETTES",
                                  str_detect(HS_code, "^24022090") ~ "CIGARETTES",
                                  Description_Chapters == "2403-Other manufactured tobacco and manufactured tobacco substitutes; 'homogenised' or 'reconstituted' tobacco; tobacco extracts and essences" ~ "TOBACCO",
                                  Description_Chapters == "2404-Products containing tobacco, reconstituted tobacco, nicotine, or tobacco or nicotine substitutes, intended for inhalation without combustion; other nicotine containing products intended for the intake of nicotine into the human body" ~ "TOBACCO",
                                # Fuels
                                  str_detect(HS_code, "^27101943") ~ "EURO DIESEL",
                                  str_detect(HS_code, "^27101249") ~ "EUROSUPER BC 95",
                                  str_detect(HS_code, "^27101249") ~ "EUROSUPER BS 100",
                                  str_detect(HS_code, "^27111294") ~ "LPG PROPANE",
                                  str_detect(HS_code, "^27101981") ~ "LUBRICATING OILS",
                                  str_detect(HS_code, "^27111397") ~ "LPG BUTANE",
                                  str_detect(HS_code, "^27101999") ~ "LUBRICATING OILS",
                                  str_detect(HS_code, "^27101967") ~ "HEAVY OILS",
                                  str_detect(HS_code, "^27111397") ~ "LPG BUTANE",
                                  str_detect(HS_code, "^27101983") ~ "LUBRICATING OILS",
                                  str_detect(HS_code, "^27101987") ~ "LUBRICATING OILS",
                                  str_detect(HS_code, "^27111297") ~ "LPG PROPANE",
                                  str_detect(HS_code, "^27111211") ~ "LPG PROPANE",
                                  str_detect(HS_code, "^27111391") ~ "LPG BUTANE",
                                  str_detect(HS_code, "^27101993") ~ "LUBRICATING OILS",
                                  str_detect(HS_code, "^27111219") ~ "LPG PROPANE",
                                  str_detect(HS_code, "^27101947") ~ "HEAVY OILS",
                                  str_detect(HS_code, "^27101948") ~ "HEAVY OILS",
                                  str_detect(HS_code, "^27073000") ~ "LUBRICATING OILS",
                                  str_detect(HS_code, "^27101231") ~ "AVIATION GASOLINE",
                                  str_detect(HS_code, "^27101290") ~ "AVIATION GASOLINE",
                                # Chemical Products
                                  str_detect(HS_code, "^2901") ~ "CHEMICAL PRODUCTS",
                                  str_detect(HS_code, "^2902") ~ "CHEMICAL PRODUCTS",
                                  str_detect(HS_code, "^3811") ~ "CHEMICAL PRODUCTS",
                                  str_detect(HS_code, "^3814") ~ "CHEMICAL PRODUCTS",
                                  str_detect(HS_code, "^3817") ~ "CHEMICAL PRODUCTS",
                                # CARS
                                  str_detect(HS_code, "^8703") ~ "CARS",
                                  TRUE ~ NA_character_
                                )
                              )
                            
                            
                            # Create a vector with the categories
                            categories <- c(
                              "SSB", 
                              "BEER", "WINE", "OTHER FERMENTED BEVERAGES",
                              "INDUSTRIAL ALCOHOL", "ALCHOLIC BEVERAGE", 
                              "TOBACCO","CIGARS AND CIGARILLOS", "CIGARETTES", 
                              "LUBRICATING OILS","AVIATION GASOLINE", "EUROSUPER BC 95", "EURO DIESEL",
                              "HEAVY OILS", "LPG PROPANE", "LPG BUTANE",
                              "CHEMICAL PRODUCTS", 
                              "CARS"
                            )
                            
                            # Create a corresponding vector for the group
                            groups <- c(
                              "Beverages", 
                              "Alcohol", "Alcohol",  "Alcohol",
                              "Alcohol", "Alcohol", 
                              
                              "Tobacco","Tobacco", "Tobacco",
                              
                              "Fuels",
                              "Fuels", "Fuels", "Fuels",
                              "Fuels", "Fuels", "Fuels",
                              "Chemicals", "Vehicles"
                            )
                            
                            # Combine into a data frame
                            categories_df <- data.frame(Category = categories, Group = groups)
                            
                            customs_simulation_parameters_final_excise_names<-left_join(customs_simulation_parameters_final_excise_names,categories_df,by=c("Category"="Category"))
                            
                            
                          #View(customs_simulation_parameters_final_excise_names)
                                              

#   # 2. Excise Base ------------------------------------------------------

                          customs_simulation_parameters_final_excise_base <- customs_simulation_parameters_final_excise_names %>%
                            mutate(
                              ExciseBase = case_when(
                                Group == "Alcohol" & Quantity > 0 & ExciseRate > 0 ~ ExciseRevenue / (Quantity / 100 * ExciseRate), # kolicinata da se mnozi so standardna stapka i so ovoj izraz
                                Category=="CIGARS AND CIGARILLOS"& Quantity > 0 & ExciseRate > 0 ~ Quantity * 1000, 
                                TRUE ~ NA_real_ # Assign NA_real_ for numeric columns
                              )
                            )

                                        
                                        # # 1.2.3 Sugar-sweetened beverages(SSB) ------------------------------------------------------------------
                                        # 
                                        # # Non Alcoholic Beer is included here
                                        # SSB_Quantity <- CustomsDuties_TE_agg_HS %>%
                                        #   filter(Four_digit == 2202)
                                        # 
                                        # SSB_Quantity$Subdataset<-c("SSB")
                                        # 
                                        # 
                                        # # 1.2.3 Wine of fresh grapes ----------------------------------------------------
                                        # 
                                        # # Wine of fresh grapes, including fortified wines; grape must other than that of heading 2009)                    
                                        # WineQuantity <- CustomsDuties_TE_agg_HS %>%
                                        #   filter(Four_digit == 2204)
                                        # 
                                        # WineQuantity$Subdataset<-c("WINE")
                                        # # Adding excis rate
                                        # WineQuantity$ExciseRate<-as.numeric(500)
                                        # 
                                        # # View(WineQuantity)
                                        # # sum(WineQuantity$Quantity)/1e06
                                        # 
                                        # # 1.2.4 Vermouth ---------------------------------------------------
                                        # 
                                        # # Vermouth and other wine of fresh grapes flavoured with plants or aromatic substances
                                        # VermouthQuantity <- CustomsDuties_TE_agg_HS %>%
                                        #   filter(Four_digit == 2205)
                                        # 
                                        # 
                                        # # View(VermouthQuantity)
                                        # # sum(VermouthQuantity$Quantity)/1e06
                                        # VermouthQuantity$Subdataset<-c("VERMOUTH")
                                        # 
                                        # # Adding excis rate
                                        # VermouthQuantity$ExciseRate<-as.numeric(500)
                                        # 
                                        # 
                                        # # 1.2.5 Other fermented beverages --------------------------------------------------
                                        # 
                                        # # Other fermented beverages (for example, cider, perry, mead); mixtures of fermented beverages and mixtures of fermented beverages and non-alcoholic beverages, not elsewhere specified or included
                                        # 
                                        # OtherFermentedBeveragesQuantity  <- CustomsDuties_TE_agg_HS %>%
                                        #   filter(Four_digit == 2206)
                                        # 
                                        # OtherFermentedBeveragesQuantity$Subdataset<-c("OTHER FERMENTED BEVERAGES")
                                        # 
                                        # # Adding excis rate
                                        # OtherFermentedBeveragesQuantity$ExciseRate<-as.numeric(500)
                                        # 
                                        # 
                                        # # 1.2.6 Undenatured ethyl alcohol of an alcoholic strength by volume of 80 % vol or higher; ethyl alcohol and other spirits, denatured, of any strength:-----------------------------------------
                                        # 
                                        # # Not included ! With 80 strength by volume of 80 % vol or higher is not for driniking
                                        # # 
                                        # UndenaturedethylAlcoholQuantity_1  <- CustomsDuties_TE_agg_HS %>%
                                        #   filter(Four_digit == 2207)
                                        # 
                                        # UndenaturedethylAlcoholQuantity_1$Subdataset<-c("UndenaturedEthylAlcoholVolume_80")
                                        # UndenaturedethylAlcoholQuantity_1$ExciseRate<-as.numeric(500)
                                        # 
                                        # 
                                        # # 1.2.7 Undenatured ethyl alcohol ---------------------------------------------
                                        # 
                                        # # Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80 % vol; spirits, liqueurs and other spirituous beverages
                                        # UndenaturedEthylAlcoholQuantity_2  <- CustomsDuties_TE_agg_HS %>%
                                        #   dplyr::filter(Four_digit == 2208)
                                        # 
                                        # 
                                        # # View(UndenaturedEthylAlcoholQuantity)
                                        # #  sum(UndenaturedEthylAlcoholQuantity$Quantity)/1e06
                                        # UndenaturedEthylAlcoholQuantity_2$Subdataset<-c("ALCHOLIC BEVERAGE")
                                        # 
                                        # # Adding excis rate
                                        # UndenaturedEthylAlcoholQuantity_2$ExciseRate<-as.numeric(800)
                                        # 
                                        # # 1.2.8 Merging data --------------------------------------------------
                                        # 
                                        # AlcoholQuantity <- bind_rows(BeerQuantity,WineQuantity,VermouthQuantity,OtherFermentedBeveragesQuantity,UndenaturedethylAlcoholQuantity_1,UndenaturedEthylAlcoholQuantity_2,
                                        #                              SSB_Quantity)
                                        # 
                                        # 
                                        # # Removing Nan from effective tax rates
                                        # AlcoholQuantity$Effective_Excise_rate<-ifelse(AlcoholQuantity$Quantity==0,0,AlcoholQuantity$Effective_Excise_rate)
                                        # 
                                        # 
                                        # 
                                        # Alcohol_tbl<-AlcoholQuantity%>%
                                        #   dplyr::filter(ExciseRevenue>0)                    
                                        # 
                                        # Alcohol_tbl$DataSet<-c("Alcohol")
                                        # 
                                        # # Calculation of abs. alc 100%
                                        # 
                                        # Alcohol_tbl<-Alcohol_tbl%>%
                                        #   dplyr::mutate(Quantity_HL=Quantity/100, # Conversion in HL
                                        #                 PotentialExcise=Quantity_HL*ExciseRate,
                                        #                 Alc_Content=ExciseRevenue/PotentialExcise,
                                        #                 Pure_Alc=Quantity_HL*Alc_Content,
                                        #                 test=ExciseRate*Pure_Alc
                                        #   )
                                        # 
                                        # Alcohol_tbl$Quantity_HL<-NULL
                                        # Alcohol_tbl$PotentialExcise<-NULL
                                        # 
                                        # # Cross check
                                        # sum(Alcohol_tbl$ExciseRevenue)
                                        # sum(Alcohol_tbl$test)
                                        # 
                                        # Alcohol_tbl$Alc_Content<-round(Alcohol_tbl$Alc_Content,2)
                                        # 
                                        # 
                                        # Alcohol_tbl <- Alcohol_tbl %>%
                                        #   mutate(DataSet  = case_when(
                                        #     grepl("SSB", Subdataset, ignore.case = TRUE) ~ "SSB",
                                        #     TRUE ~ DataSet  # If not containing 'SSB', keep the original value
                                        #   ))
                                        # 
                                        # 
                                        # Alcohol_tbl_subset_export<-Alcohol_tbl%>%
                                        #   select(Subdataset,ExciseRate,Alc_Content,Pure_Alc,Quantity,ExciseRevenue)%>%
                                        #   group_by(Subdataset,ExciseRate,Alc_Content)%>%
                                        #   summarise(Pure_Alc=sum(Pure_Alc),
                                        #             Quantity=sum(Quantity),
                                        #             ExciseRevenue=sum(ExciseRevenue)
                                        #             
                                        #   )
                                        # 
                                        # View(Alcohol_tbl_subset_export)

                                        
                                        
                                        # END ---------------------------------------------------------------------                                     
                                        
                                        Cars_ExciseRates<-read_excel("HS_ExciseCarsClean.xlsx")

                          
                                      customs_simulation_parameters_final1<-customs_simulation_parameters_final_excise_base%>%
                                        select(-c("CustomsRevenue","ExciseRevenue"))

                          
                          
                          
                                      write.xlsx(customs_simulation_parameters_final1, "customs_simulation_parameters_final.xlsx")
      
                                

                                
                                
                  # 1.2 Preparation of data list -------------------------------------------------
              
                                
                                customs_data<-customs_simulation_parameters_final%>%
                                        select("HS_code", "Value","Quantity","Netweight","CustomsRevenue","ExciseRevenue","TypeOfProducts")
                                
                                customs_data <- customs_data %>%
                                  dplyr::mutate(across(where(is.numeric), ~ replace_na(., 0)))
                                
                                customs_data <- customs_data %>%
                                  mutate(HS = substr(HS_code, 1, 10))
                                
                                
                                
                                customs_data <- mutate(customs_data,
                                                                  Chapter = substr(HS, 1, 2),
                                                                  Four_digit = substr(HS, 1, 4),
                                                                  Six_digit = substr(HS, 1, 7),
                                                                  Eight_digit = paste0(substr(HS, 1, 4),
                                                                                       "",
                                                                                       substr(HS, 5, 7),
                                                                                       " ",
                                                                                       substr(HS, 8, 9)))
                                
                                
                                
                                
                                # Weights       
                                customs_data_weights <- read_csv("customs_weights.csv")%>%
                                  as.data.frame()
                                
                                
                                growth_factors_customs_data<-read_csv("growfactors_customs.csv")%>%
                                  as.data.frame()
                                
                                
                                # Create a list with the data frames
                                customs_data <- list(
                                                    sample = customs_data,
                                                    weights = customs_data_weights,
                                                    growth_factors=growth_factors_customs_data
                                                    
                                                  )
                                
              
                  # 1.3 IMPORT DATA ---------------------------------------------------
              
              
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
                                                Effective_Customs_rate=round(CustomsRevenue/(Value),2))
              
                                
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
                  
                  
                  
                  
                  
                  
                  
                  # 3.Estimation of Tax Expenditures for Customs duties -----------------------
                  # 3.1 Countries -----------------------------------------------------------
                  
                  CustomsDuties_TE_agg_countries<-CustomsDuties_base%>%
                    dplyr::group_by(HS_code,HS_code_s,iso2c,iso3c,Treatment,countries)%>%
                    dplyr::filter(Treatment=="NonPreferential")%>%
                    dplyr::summarise(Value=sum(Value,na.rm = TRUE),
                                     Quantity=sum(Quantity,na.rm = TRUE),
                                     Netweight=sum(Netweight,na.rm = TRUE),
                                     CustomsRevenue=sum(CustomsRevenue,na.rm = TRUE),
                                     ExciseRevenue=sum(ExciseRevenue,na.rm = TRUE),
                                     VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE))

                  
                  
                  CustomsDuties_TE_agg_countries$HS_code<-NULL
                  CustomsDuties_TE_agg_countries$HS_code_s<-NULL
                  

                  
                  # 3.2 Harmonized System-HS  --------------------------------------------------------------------
                  
                  
                  # 3.3 Adding TARIC rates --------------------------------------------------
                  CustomsDuties_TE_agg_HS <- CustomsDuties_base %>%
                    dplyr::group_by(HS_code, Treatment, FreeTradeAgreements, HS_code_s) %>%
                    dplyr::summarise(
                      Value = sum(Value, na.rm = TRUE),
                      Quantity = sum(Quantity, na.rm = TRUE),
                      Netweight = sum(Netweight, na.rm = TRUE),
                      CustomsRevenue = sum(CustomsRevenue, na.rm = TRUE),
                      ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE),
                      VAT_Revenue = sum(VAT_Revenue, na.rm = TRUE),
                      Effective_Customs_rate = sum(CustomsRevenue, na.rm = TRUE) / sum(Value, na.rm = TRUE) * 100,
                      Effective_Excise_rate = sum(ExciseRevenue, na.rm = TRUE) / sum(Quantity), #*100,
                      Effective_VAT_rate = 
                            sum(VAT_Revenue, na.rm = TRUE) / 
                               (sum(Value, na.rm = TRUE) +sum(CustomsRevenue, na.rm = TRUE)+ sum(ExciseRevenue, na.rm = TRUE)
                           ) * 100
                    )
                  
                  CustomsDuties_TE_agg_HS<-left_join(CustomsDuties_TE_agg_HS,taric_data,by =c("HS_code"))  
                  
                  
                  
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

             
# III. SAVING DATA IN R ENVIROMENT (RDS FILE)--------------------------------

                setwd(path1)
                getwd()
          
                gc(TRUE)
                
                
                rm(list = ls()[!ls() %in% c("GeoDimension","HS_Sections","path",                    
                                            "path1","WTO_MTN","BEC","taric_data",
                                            "mapdata_iso3c","MacroFiscalData","CPA_CN","CPA_NACE","mapdata_iso3c","CustomsDuties_TE_agg_HS",
                                            "customs_data","Import_raw_monthly","taric_data","Cars_ExciseRates"
                                            )])
                
                save.image(file=".RData") 
        
                
                
                
                