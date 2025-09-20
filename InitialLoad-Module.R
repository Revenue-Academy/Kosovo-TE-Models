'Install packages and importing of data
                                          '
'Step 1. Set your local path to the model directory'

rm(list = ls())
#path1<-" "# <--------Set your path here
path1<-"C:/Users/User/Documents/Models/Kosovo-TE-Models"
#C:/Users/wb591157/OneDrive - WBG/Documents/Models/Kosovo-TE-Models"# <--------Set your path here



'Step 2. Press CTRL+A to select all lines in this script and after that press CTRL+Enter to execute selected lines'

# I. INSTALLING LIBRARIES  -------------------------------------------------


# # Define the list of required packages
list.of.packages <- unique(c("shinydashboard",
                             "DT",
                             "readxl",
                             "openxlsx",
                             "shinyjs",
                             "plotly",
                             "ggplot2",
                             "data.table",
                             "fontawesome",
                             "tidyverse",
                             "countrycode",
                             "shiny",
                             "kableExtra",
                             "stringr",
                             "reshape2",
                             "base64enc",
                             "maps",
                             "sfo",
                             "sf",
                             "circlize",
                             "flexdashboard",
                             "rpivotTable",
                             "sm",
                             "ks",
                             "shinyWidgets",
                             "plyr",
                             "shinycssloaders",
                             "future",
                             "promises",
                             "parallel",
                             "purrr",
                             "tidyr",
                             "RColorBrewer",
                             "Hmisc",
                             "ineq"
                             ))

# Check for missing packages and install them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

# Load all packages
lapply(list.of.packages, library, character.only = TRUE)



# install.packages("https://cran.r-project.org/src/contrib/Archive/IC2/IC2_1.0-1.tar.gz",
#                  repos = NULL, type = "source", method = "wininet")
# 


# Warning manual installation of rccmisc library





# II. MODULES  -------------------------

  library(tidyverse)
  library(readxl)
  library(countrycode)
  library(maps)
  library(ggplot2)
  library(reshape2)
  library(rccmisc) 
  library(openxlsx)
  library(readxl)
  library(stringr)
  library(data.table)
  library(sfo)
  library(sf)

    # 1.IMPORT TAXES Module --------------------------------------------------------
    
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
    
     CPA_CN <- read_excel("CPA-CORRELATION/CPA21_CN2018_2023.xlsx")
    CPA_NACE <- read_excel("CPA-CORRELATION/CPA21_NACE2_Table.xlsx")
    
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
    
    
    
    customs_simulation_parameters <- customs_simulation_parameters %>%
      dplyr::mutate(
        Chapters_description = paste(Chapter, Chapters_description, sep = "-"),
        Description_Chapters = paste(HS4_COD, `Description_Chapters `, sep = "-"),
        HS_code1 = paste(HS_code, Description_EN , sep = "-")
      ) %>%
      dplyr::select(-Chapter,-HS6_COD, -HS4_COD, -`Description_Chapters `)
    
    
    
    customs_simulation_parameters <- customs_simulation_parameters %>%
      dplyr::select(Chapters_description, Description_Chapters, HS_code, everything())
    
    
    
    
          # 1.1 Regular Import ----------------------------------------------------------
          
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
          
          
          # 2. Excise Base ------------------------------------------------------
    
    customs_simulation_parameters_final_excise_base <- customs_simulation_parameters_final_excise_names %>%
      mutate(
        ExciseBase = case_when(
          Group == "Alcohol" & Quantity > 0 & ExciseRate > 0 ~ ExciseRevenue / (Quantity / 100 * ExciseRate), 
          Category=="CIGARS AND CIGARILLOS"& Quantity > 0 & ExciseRate > 0 ~ Quantity * 1000, 
          TRUE ~ NA_real_ # Assign NA_real_ for numeric columns
        )
      )
    
    
          # 2.1 Cars -------------------------------------------------------------------
          
          
          Cars_ExciseRates<-read_excel("HS_ExciseCarsClean.xlsx")
          
          
          customs_simulation_parameters_final1<-customs_simulation_parameters_final_excise_base%>%
            select(-c("CustomsRevenue","ExciseRevenue"))
          
          
          
          # 2.2 Extract Customs Policy Parameters ---------------------------------------
          
          
          
          write.xlsx(customs_simulation_parameters_final1, "customs_simulation_parameters_final.xlsx")
          
          
          
          
          
          # 2.2 Preparation of data list -------------------------------------------------
          
          
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
          
          
          # 2.3 IMPORT DATA ---------------------------------------------------
          
          
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
          
          
          
          # 3. Adding FreeTrade agreements --------------------------------------------
          
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
          
          
          # 4.Estimation of Tax Expenditures for Customs duties -----------------------
          # 4.1 Countries -----------------------------------------------------------
          
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
          
          
          
          # 4.2 Harmonized System-HS  --------------------------------------------------------------------
          
          
          # 4.3 Adding TARIC rates --------------------------------------------------
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
          
          
          

    # 3.VAT Module ------------------------------------------------------------

'DATA PREPROCESSING MODULE'
path3 <- paste0(path1, "/Data/VAT")
setwd(path3)
getwd()

        # 1. DEFINE FUNCTIONS ----
        
        #  The function creates an ntile group vector:
        qgroup = function(numvec, n, na.rm=TRUE){
          qtile = quantile(numvec, probs = seq(0, 1, 1/n), na.rm)  
          out = sapply(numvec, function(x) sum(x >= qtile[-(n+1)]))
          return(out)
        }
        
        #  to extract only English names from SUTs
        trim <- function (x) gsub("^\\s+|\\s+$", "", x) 
        input_output_matrix_to_long_data <- function(matrix){
          
          matrix <- matrix %>%
            dplyr::filter(...2 != "NA")
          
          
          colnames(matrix) <- matrix[1,]
          
          #data <- matrix[c(-1,-2),c(-1,-2)] %>% as.matrix() %>% melt()
          data <- matrix[c(-1,-2),c(-1,-2)] %>% as.matrix() %>% reshape2::melt()
          
          product_industry_name <- matrix[[2]][c(-1,-2)]
          product_industry_code <- matrix[[1]][c(-1,-2)]
          industry_code <-  matrix[2,c(-1,-2)] %>% as.character()
          
          data$Var1 <- rep(product_industry_name, time = length(industry_code))
          
          data <- data %>% 
            dplyr::rename(PRODUCT_INDUSTRY_NAME = Var1,
                          INDUSTRY_NAME = Var2)
          
          
          data$PRODUCT_INDUSTRY_CODE <- rep(product_industry_code, time = length(industry_code))
          data$INDUSTRY_CODE <- rep(industry_code, each = length(product_industry_code))
          
          data <- data %>% 
            dplyr::select(PRODUCT_INDUSTRY_NAME, PRODUCT_INDUSTRY_CODE, INDUSTRY_NAME, INDUSTRY_CODE, value)
          
          
          # Leave the names only in English
          data$PRODUCT_INDUSTRY_NAME<-gsub("^.*\\/", "",  data$PRODUCT_INDUSTRY_NAME) %>% trim()
          data$INDUSTRY_NAME<-gsub("^.*\\/", "",  data$INDUSTRY_NAME) %>% trim()
          
          data$value <- as.numeric(as.character(data$value))
          
          return(data)
          
        }
        
        
        # 2. RAW DATA IMPORT AND PREPROCESS  ----- 
        
                # Initialize empty lists to store the tables
                CPA_TAXABLE_PROPORTIONS_BU_list <- list()
                CPA_TAXABLE_PROPORTIONS_SIM_list <- list()
                
                ' 
                                    In this section data are imported from five files:
                                    
                                    VAT_Model_v9.16a2.xlsx
                                    TaxableProportions-4a.xlsx
                                    MACRO_FISCAL_INDICATORS.xlsx
                                    Data4_hbs2020.xlsx  <---HBS DATA
                                    NACE_SUT_table.xlsx
                                    '
                
                # Name of the version of model
                version_vat_model<-c("Data_SUT_XK_v1.6a.xlsx")
                #version_vat_model<-c("Data_SUT_XK_v1.6.xlsx")
                
                # Taxable proportions
                taxable_proportions_raw <- read_excel("VAT_TaxableProportions.xlsx")
        
        # 2.1 SUTs ------------------------------------
        
                    SUPPLY_raw <- read_excel(version_vat_model, sheet = "Supply_XK", col_names = F)[c(-1,-2,-3,-4),] %>%
                      input_output_matrix_to_long_data()
                    
                    "Each value from Use_Purchaser are imported here"
                    USE_PURCHASER_raw <- read_excel(version_vat_model, sheet = "Use_Purchaser_XK", col_names = F)[c(-1,-2,-3,-4),] %>%
                      input_output_matrix_to_long_data()
                    
                    USE_VAT_raw <- read_excel(version_vat_model, sheet = "VAT_XK_NEW", col_names = F)[c(-1,-2,-3,-4),] %>%
                      input_output_matrix_to_long_data()
                    
                    USE_BASIC_raw <- read_excel(version_vat_model, sheet = "Use_Purchaser_Basic_XK", col_names = F)[c(-1,-2,-3,-4),] %>%
                      input_output_matrix_to_long_data()
        
        
        # 2.2 COICOP table ------------------------------------------------------------
        
                
                base_year_VAT<-2022 # <-This is the same year as the year from which the data originates.
                
                max_time_horizon<-base_year_VAT+5
                
                time_horizon<-seq(base_year_VAT,max_time_horizon)
        
        
        # 2.3 MACRO-FISCAL INDICATORS ---------------------------------------------
        
        MACRO_FISCAL_INDICATORS <-  MacroFiscalData%>%
          dplyr::select(Year,GDP)%>%
          dplyr::rename("Nominal_GDP"="GDP")
        
        
        MACRO_FISCAL_INDICATORS$Year<-as.numeric(MACRO_FISCAL_INDICATORS$Year)
        
        
        FinalConsumption <- MacroFiscalData%>%
          dplyr::select(Year)
        
        
        # 2.4 INSERT TAXABLE PROPORTIONS SIMULATION PARAMETERS ---------------------------------------------

            taxable_proportion_bu <- taxable_proportions_raw %>%
              dplyr::mutate(Simulated_Policy_Exempt = ifelse(is.na(ProportionExempted), Current_Policy_Exempt, ProportionExempted),
                            Simulated_Policy_Reduced_Rate = ifelse(is.na(PreferentialVATRate_1), Current_Policy_Reduced_Rate, PreferentialVATRate_1),
                            Simulated_Policy_Fully_Taxable = 1-Simulated_Policy_Exempt-Simulated_Policy_Reduced_Rate)
            
            
            CPA_TAXABLE_PROPORTIONS_BU<-taxable_proportions_raw
            growfactors_vat <-read_csv("growfactors_vat.csv")
            
            
            
            # Create an empty data.table with the specified structure
            forecast_combined_agg_tbl_wide <- data.table(
              year = numeric(),
              `Current Law (In EUR thousand)` = numeric(),
              `Simulation (In EUR thousand)` = numeric(),
              `Fiscal Impact (In EUR thousand)` = numeric(),
              `Current Law (Pct of GDP)` = numeric(),
              `Simulation (Pct of GDP)` = numeric(),
              `Fiscal Impact (Pct of GDP)` = numeric()
            )




            
    # 4.PIT Module ---------------------------------------------------------------------
      path3 <- paste0(path1, "/Data/PIT")
      setwd(path3)
      getwd()

        # 1.Import data -------------------------------------------------------------
      
          region_data <- read_excel("CorrelationNames.xlsx")
          
          kosovo_regions <- st_read("polbnda_rks.shp")
          
          
          NACE_SUT_table <- read_excel("NACE_SUT_table.xlsx")%>%
            dplyr::select(nace_num,section,description)%>%
            distinct(nace_num, .keep_all = TRUE)
          
          
          
          NACE_SUT_table$nace_num<-as.double(NACE_SUT_table$nace_num)
          
          
          ComapaniesTypes <- read_excel("ComapaniesTypes.xlsx")          
    
        # Define Unit of measurement e.g 1e06,1e03,1e09
        
        
        LCU_unit<-1e03
        
        
       
        
        dt<-read_csv("dataset_pit_xk_2023.csv")%>%data.table()
          
         
        
        
        dt$tax_payer_group<-"0"
        
        
        dt<-dt%>%
          dplyr::mutate(TaxpayerType_en=ifelse(TaxpayerType=="PERSON FIZIK","Others","Individual_businesses"))
        
        dt$City <- iconv(dt$City, from = "latin1", to = "UTF-8")
        
        
        # 2.Growth Factors & Scenario Mapping -------------------------------------
      
        growth_factors_pit <- read_csv("growth_factors_pit.csv")%>%data.table()
          
         
        
        # The 5 scenario labels
        scenarios_5 <- c("t0","t1","t2","t3","t4")
        scenario_years <- c(2023, 2024, 2025, 2026, 2027)
        
        
        # 3.Weights ----------------------------------------------------------------------
        
        # NACE NAMES
        df_nace_names<-structure(list(section = c("A", "B", "C", "D", "E", "F", "G", 
                                                  "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", 
                                                  "U", "Other"), description = c("Agriculture, forestry and fishing", 
                                                                                 "Mining and quarrying", "Manufacturing", "Electricity, gas, steam and air conditioning supply", 
                                                                                 "Water supply; sewerage; waste managment and remediation activities", 
                                                                                 "Construction", "Wholesale and retail trade; repair of motor vehicles and motorcycles", 
                                                                                 "Transporting and storage", "Accommodation and food service activities", 
                                                                                 "Information and communication", "Financial and insurance activities", 
                                                                                 "Real estate activities", "Professional, scientific and technical activities", 
                                                                                 "Administrative and support service activities", "Public administration and defence; compulsory social security", 
                                                                                 "Education", "Human health and social work activities", "Arts, entertainment and recreation", 
                                                                                 "Other services activities", "Activities of households as employers; undifferentiated goods - and services - producing activities of households for own use", 
                                                                                 "Activities of extraterritorial organisations and bodies", "Other"
                                                  )), row.names = c(NA, -22L), class = c("tbl_df", "tbl", "data.frame"
                                                  ))
        
    
                dt<-left_join(dt,ComapaniesTypes,by=c("companytype"="Name_AL"))%>%
                  dplyr::rename("companytype_en"="Name_EN")
                
                
                
                dt$sector<-as.double(dt$sector)
                
                dt<-left_join(dt,NACE_SUT_table,by=c('sector'='nace_num'))
                
                dt$Name_Own<-NULL
                
                
                # Import withholding data
                
                
                clean_witheld_df <-  read_csv("clean_witheld_df1.csv")%>%data.table()

                clean_witheld_df$id_n<-as.character(clean_witheld_df$id_n)
                
                clean_witheld_df$tax_payer_group<-"1"
                
                
                
                
                # Ensure both are data.tables
                dt <- as.data.table(dt)
                clean_witheld_df <- as.data.table(clean_witheld_df)
                
                # Get the union of all column names
                all_cols <- union(names(dt), names(clean_witheld_df))
                
                # Add missing columns in each table with NA
                for (col in setdiff(all_cols, names(dt))) dt[[col]] <- NA
                for (col in setdiff(all_cols, names(clean_witheld_df))) clean_witheld_df[[col]] <- NA
                
                # Set the same column order
                setcolorder(dt, all_cols)
                setcolorder(clean_witheld_df, all_cols)
                
                # Now stack (merge) the datasets row-wise
                combined_dt <- rbindlist(list(dt, clean_witheld_df), use.names = TRUE, fill = TRUE)
                
                
                dt<-combined_dt
                rm(combined_dt)
                
                
                
                n <- NROW(dt)
                
                weights_pit <- data.table(
                  t0 = rep(1, n),
                  t1 = rep(1, n),
                  t2 = rep(1, n),
                  t3 = rep(1, n),
                  t4 = rep(1, n)
                )
                
                
                dt[, description := fifelse(description == "ctivities of extraterritorial organisations and bodies",
                                            "Activities of extraterritorial organisations and bodies", description)]
                
                dt[, description := fifelse(description == "Other service activities",
                                            "Other services activities", description)]
                
                dt[, description := fifelse(description == "Administrative and support service activities",
                                            "Administrative and support services", description)]
                
                
                
                dt[is.na(dt)] <- 0
    
    
    
                
    # 5.CIT Module ---------------------------------------------------------------------

        # Import data
        
        path4 <- paste0(path1, "/Data/CIT")
        setwd(path4)
        getwd()
        

        # 1.Import data -----------------------------------------------------------
        
                
                cit_raw<- read_csv("dataset_cit_xk.csv")%>%data.table()
          
               
                NACE_SUT_table_cit <- read_excel("NACE_SUT_table.xlsx")%>%
                  dplyr::select(nace,section,description)%>%
                  distinct(nace, .keep_all = TRUE)
                
                
                cit_raw<-left_join(cit_raw,NACE_SUT_table_cit,by=c("sector"="nace"))
                
                TypeOfCompanies_CIT <- read_excel("TypeOfCompanies.xlsx")
                
                
                cit_raw<-left_join(cit_raw,TypeOfCompanies_CIT,by=c("companytype_al"="name_al"))
                
        
        # 2.Growth Factors --------------------------------------------------------
        
                
                
                growth_factors_cit<-  read_csv("growfactors_cit_kosovo.csv")%>%data.table()
                  
                
                
                
                scenarios_5 <- c("t0","t1","t2","t3","t4")
                
                scenario_years <- c(2023, 2024, 2025, 2026, 2027)
        
        
        # Weights
        n <- NROW(cit_raw)
        
        weights_cit <- data.table(
          t0 = rep(1, n),
          t1 = rep(1, n),
          t2 = rep(1, n),
          t3 = rep(1, n),
          t4 = rep(1, n)
        )
        
 
        # 3.Weights ---------------------------------------------------------------
        
        
        weights_cit[, (names(weights_cit)) := lapply(.SD, function(x) 1)]
        
        
      
          
# III. SAVING DATA IN R ENVIROMENT (RDS FILE)--------------------------------

setwd(path1)
getwd()

gc(TRUE)


save.image(file=".RData") 


