
'Install packages and importing of data
                                          '


'Step 1. Set your local path to the  model'

path1<-" "# <--------Set your path here

'Step 2. Press CTRL+A to select all lines in this script and after that press CTRL+Enter to execute selected lines'

# I.  Installing libraries -------------------------------------------------


        list.of.packages <- c("shinydashboard",
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
                              "ggplot2",
                              "tidyverse",
                              "kableExtra",
                              "stringr",
                              "reshape2",
                              "base64enc",
                              "maps")

           new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
           if(length(new.packages)) install.packages(new.packages)




# II. Importing raw data from Excel into R environment -------------------------
          library(tidyverse)
          library(readxl)
          library(countrycode)
          library(maps)
          library(ggplot2)
          
          path <- paste0(path1, "/Data/ImportData")
          setwd(path)
          getwd()
        

                  # https://dogana.rks-gov.net/tarik/TARIK_VERSION01012023_PDF%20ENG/INTRODUCTION.pdf
                  # https://dogana.rks-gov.net/en/per-doganen/statistikat-dhe-arritjet/trading-balance-based-on-tariffs/
                  
                  # WTO classification
                  WTO_MTN <- read_excel("WTO-CORRELATION/WTO_HS.xlsx", 
                                        sheet = "WTO_HS")
                  
                  # HS-Sections
                  HS_Sections <- read_excel("WTO-CORRELATION/WTO_HS.xlsx", 
                                            sheet = "HS_SECTIONS")
                  
                  HS_Sections <- HS_Sections %>%
                    mutate(Section_description = str_to_title(Section_description))
                  
                  # Countries
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

# III.Saving data in R environment (RDS file) --------------------------------

                setwd(path1)
                getwd()
          
                gc(TRUE)
                
                
                rm(list = ls()[!ls() %in% c("GeoDimension","HS_Sections","path",                    
                                               "path1","WTO_MTN","mapdata_iso3c","MacroFiscalData","CPA_CN","CPA_NACE","mapdata_iso3c"
                                            )])
                
                save.image(file=".RData") 
        
                
                
                
                