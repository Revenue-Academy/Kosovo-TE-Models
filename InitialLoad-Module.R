'Install packages and importing of data
                                          '
'Step 1. Set your local path to the model directory'

rm(list = ls())


if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable() &&
    nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  
  path1 <- dirname(normalizePath(rstudioapi::getActiveDocumentContext()$path, winslash = "/"))
  
} else {
  
  path1 <- normalizePath(getwd(), winslash = "/")
}


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
                             #"sfo",
                             "sf",
                             "circlize",
                             "flexdashboard",
                             #"rpivotTable",
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
                             "Hmisc"))

# Check for missing packages and install them
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)

# Load all packages
lapply(list.of.packages, library, character.only = TRUE)

# 
# 
# install.packages("https://cran.r-project.org/src/contrib/Archive/IC2/IC2_1.0-1.tar.gz",
#                  repos = NULL, type = "source", method = "wininet")



# Warning manual installation of rccmisc library




library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
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
library(data.table)
#library(sfo)
library(sf)
library(readr)
library(dplyr)
library(data.table)


path <- paste0(path1, "/Data/ImportData")
setwd(path)
getwd()


# I.IMPORT TAXES Module --------------------------------------------------------


# https://dogana.rks-gov.net/tarik/TARIK_VERSION01012023_PDF%20ENG/INTRODUCTION.pdf
# https://dogana.rks-gov.net/en/per-doganen/statistikat-dhe-arritjet/trading-balance-based-on-tariffs/

# WTO classification
# Note: For 2024 and 2025  HS 2022 is in force

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

# CPA_CN <- read_excel("CPA-CORRELATION/CPA21_CN2018_2023.xlsx")
# CPA_NACE <- read_excel("CPA-CORRELATION/CPA21_NACE2_Table.xlsx")

# 1. TARIK DATA --------------------------------------------------------------
          # 1.TARIK-2023 --------------------------------------------------------------------
          
          
          taric_data2023 <- read_excel("Tarik_Data/Tarifa_Per_WEB-2023 ..xlsx")%>%
            select(-c("TAR_ALL","TAR_DSC2","TAR_DSC","TAR_ALL2","TAR_ALL3","MPT_IMPORT","MPT_EKSPORT",'VALID_FROM'))%>%
            dplyr::rename('HS_code'='TAR_10',
                          'Description_EN'='TAR_DSC3',
                          'SupplementaryUnit'='UOM_COD1',
                          #'Excise_Description'='PÃ«rshkrimi AkcizÃ«s',
                          'CustomsRate_MFN'='TAR_T01_DOGANA',
                          'CustomsRate_CEFTA'='TAR_T04_CEFTA',
                          'CustomsRate_MSA'='TAR_T05_MSA',
                          'CustomsRate_TR'='TAR_T06_TRMTL',
                          'ExciseRate'='TAR_T02_AKCIZA',
                          'VAT_Rate'='TAR_T03_TVSH'
            )
          
          colnames(taric_data2023)[7] <- "Excise_Description"
          
          taric_data2023$CustomsRate_MFN<-as.double(taric_data2023$CustomsRate_MFN)
          taric_data2023$ExciseRate <-as.double(taric_data2023$ExciseRate)
          taric_data2023$VAT_Rate <-as.double(taric_data2023$VAT_Rate)
          taric_data2023$CustomsRate_CEFTA<-as.double(taric_data2023$CustomsRate_CEFTA)
          taric_data2023$CustomsRate_MSA<-as.double(taric_data2023$CustomsRate_MSA)
          taric_data2023$CustomsRate_TR <-as.double(taric_data2023$CustomsRate_TR)
          
          
          
          taric_data2023 <- mutate(taric_data2023,Chapter = substr(HS_code, 1, 2))
          
          taric_data2023$Year<-"2023"
          
          
          
          
          taric_data2023 <- taric_data2023 %>%
            dplyr::mutate(
              CustomsRate_MFN   = as.numeric(CustomsRate_MFN),
              ExciseRate        = as.numeric(ExciseRate),
              VAT_Rate          = as.numeric(VAT_Rate),
              CustomsRate_CEFTA = as.numeric(CustomsRate_CEFTA),
              CustomsRate_MSA   = as.numeric(CustomsRate_MSA),
              CustomsRate_TR    = as.numeric(CustomsRate_TR),
              Year              = as.numeric(Year)
            )
          
          
          
          
          # 2.TARIK-2024 --------------------------------------------------------------------
          
          taric_data2024 <- read_excel("Tarik_Data/Kosovo_TARIK_2024_extracted_pages_34_923_1.xlsx", 
                                       sheet = "Tariff_lines")
          
          taric_data2024 <- taric_data2024 %>%
            dplyr::mutate(
              preferential_rate = stringr::str_squish(as.character(preferential_rate)),
              CustomsRate_CEFTA = as.numeric(stringr::str_match(preferential_rate, "CEFTA\\s*:\\s*([0-9.,]+)")[, 2]),
              CustomsRate_MSA   = as.numeric(stringr::str_match(preferential_rate, "MSA/GB\\s*:\\s*([0-9.,]+)")[, 2]),
              CustomsRate_TR    = as.numeric(stringr::str_match(preferential_rate, "TR\\s*:\\s*([0-9.,]+)")[, 2])
            )
          
          taric_data2024 <- taric_data2024 %>%
            dplyr::mutate(
              preferential_rate = stringr::str_squish(as.character(preferential_rate)),
              duty_rate_pct     = as.character(duty_rate_pct),
              vat_pct           = as.character(vat_pct),
              excise_rate       = as.character(excise_rate)
            ) %>%
            dplyr::mutate(
              duty_rate_pct = dplyr::if_else(
                !stringr::str_detect(preferential_rate, "^CEFTA\\s*:"),
                preferential_rate,
                duty_rate_pct
              ),
              vat_pct = dplyr::if_else(
                !stringr::str_detect(preferential_rate, "^CEFTA\\s*:"),
                excise_rate,
                vat_pct
              ),
              preferential_rate = dplyr::if_else(
                !stringr::str_detect(preferential_rate, "^CEFTA\\s*:"),
                NA_character_,
                preferential_rate
              ),
              excise_rate = dplyr::if_else(
                !stringr::str_detect(preferential_rate, "^CEFTA\\s*:"),
                NA_character_,
                excise_rate
              )
            )
          
          
          # Penicillins rate 16% ?
          
          taric_data2024 <- taric_data2024 %>%
            dplyr::mutate(
              duty_rate_pct = stringr::str_trim(stringr::str_remove(as.character(duty_rate_pct), "\\s*\\([0-9]+\\)")),
              vat_pct       = stringr::str_trim(stringr::str_remove(as.character(vat_pct), "\\s*\\([0-9]+\\)"))
            )
          
          
          
          taric_data2024 <- taric_data2024 %>%
            dplyr::mutate(
              vat_pct = as.character(vat_pct),
              excise_rate = as.character(excise_rate),
              excise_rate = dplyr::if_else(
                stringr::str_detect(vat_pct, "â¬"),
                vat_pct,
                excise_rate
              ),
              vat_pct = dplyr::if_else(
                stringr::str_detect(vat_pct, "â¬"),
                NA_character_,
                vat_pct
              )
            )
          
          
          taric_data2024 <- taric_data2024 %>%
            dplyr::mutate(
              excise_rate = stringr::str_squish(as.character(excise_rate)),
              excise_rate = dplyr::na_if(excise_rate, "-"),
              excise_rate = dplyr::na_if(excise_rate, "â"),
              excise_rate = stringr::str_replace_all(excise_rate, ",", "."),
              excise_rate = stringr::str_extract(excise_rate, "[0-9]+(?:\\.[0-9]+)?"),
              excise_rate = as.numeric(excise_rate)
            )
          
          
          
          
          taric_data2024 <- taric_data2024 %>%
            dplyr::mutate(
              duty_rate_pct = as.character(duty_rate_pct),
              vat_pct       = as.character(vat_pct),
              duty_rate_pct = dplyr::if_else(vat_pct == "16", vat_pct, duty_rate_pct),
              vat_pct       = dplyr::if_else(vat_pct == "16", NA_character_, vat_pct)
            )
          
          #View(taric_data2024)
          
          
          
          taric_data2024_na_rows <- taric_data2024 %>%
            dplyr::filter(
              is.na(duty_rate_pct) |
                is.na(vat_pct) |
                #is.na(excise_rate) |
                is.na(CustomsRate_CEFTA) |
                is.na(CustomsRate_MSA) |
                is.na(CustomsRate_TR)
            ) %>%
            dplyr::select(
              goods_code,
              duty_rate_pct,
              vat_pct,
              #excise_rate,
              CustomsRate_CEFTA,
              CustomsRate_MSA,
              CustomsRate_TR
            )
          
          
          "Export of missing data. Data are filled out manually"
          write.csv(taric_data2024_na_rows,"taric_data2024_na_rows.csv")
          
          
          
          taric_data2024_na_rows_update <- read_csv("taric_data2024_na_rows_update.csv")
          
          taric_data2024 <- taric_data2024 %>%
            dplyr::mutate(
              duty_rate_pct = stringr::str_squish(as.character(duty_rate_pct)),
              vat_pct       = stringr::str_squish(as.character(vat_pct)),
              CustomsRate_CEFTA = as.character(CustomsRate_CEFTA),
              CustomsRate_MSA   = as.character(CustomsRate_MSA),
              CustomsRate_TR    = as.character(CustomsRate_TR),
              
              duty_rate_pct = dplyr::na_if(duty_rate_pct, "-"),
              vat_pct       = dplyr::na_if(vat_pct, "-"),
              
              duty_rate_pct = stringr::str_extract(duty_rate_pct, "[0-9]+(?:\\.[0-9]+)?"),
              vat_pct       = stringr::str_extract(vat_pct, "[0-9]+(?:\\.[0-9]+)?"),
              
              duty_rate_pct     = as.numeric(duty_rate_pct),
              vat_pct           = as.numeric(vat_pct),
              CustomsRate_CEFTA = as.numeric(CustomsRate_CEFTA),
              CustomsRate_MSA   = as.numeric(CustomsRate_MSA),
              CustomsRate_TR    = as.numeric(CustomsRate_TR)
            ) %>%
            dplyr::left_join(
              taric_data2024_na_rows_update %>%
                dplyr::rename(
                  duty_rate_pct_new     = duty_rate_pct,
                  vat_pct_new           = vat_pct,
                  CustomsRate_CEFTA_new = CustomsRate_CEFTA,
                  CustomsRate_MSA_new   = CustomsRate_MSA,
                  CustomsRate_TR_new    = CustomsRate_TR
                ),
              by = "goods_code"
            ) %>%
            dplyr::mutate(
              duty_rate_pct     = dplyr::coalesce(duty_rate_pct_new, duty_rate_pct),
              vat_pct           = dplyr::coalesce(vat_pct_new, vat_pct),
              CustomsRate_CEFTA = dplyr::coalesce(CustomsRate_CEFTA_new, CustomsRate_CEFTA),
              CustomsRate_MSA   = dplyr::coalesce(CustomsRate_MSA_new, CustomsRate_MSA),
              CustomsRate_TR    = dplyr::coalesce(CustomsRate_TR_new, CustomsRate_TR)
            ) %>%
            dplyr::select(
              -duty_rate_pct_new,
              -vat_pct_new,
              -CustomsRate_CEFTA_new,
              -CustomsRate_MSA_new,
              -CustomsRate_TR_new
            )
          
          
          
          
          taric_data2024<-taric_data2024%>%
            dplyr::rename("HS_code"="goods_code",
                          "HS6_COD"="subheading_6",
                          "HS4_COD"="heading_4",
                          "SupplementaryUnit"="supplementary_units",
                          "Description_EN"="description",
                          "Chapter"="chapter",
                          "CustomsRate_MFN"="duty_rate_pct",
                          "ExciseRate"="excise_rate",
                          "VAT_Rate"="vat_pct"
                          
            )%>%
            select("HS_code","HS6_COD","HS4_COD","SupplementaryUnit","CustomsRate_MFN","ExciseRate","VAT_Rate",         
                   "CustomsRate_CEFTA","CustomsRate_MSA","CustomsRate_TR","Description_EN","Chapter")
          
          
          taric_data2024$Year<-"2024"
          
          
          taric_data2024 <- taric_data2024 %>%
            dplyr::mutate(HS_code = gsub("\\s+", "", HS_code))
          
          
          
          #  Impurting missing data 
          
          taric_data2024 <- taric_data2024 %>%
            mutate(
              ExciseRate = ifelse(
                HS_code %in% c("2207100000", "2207200090"),
                500,
                ExciseRate
              )
            )
          
          
          
          taric_data2024 <- taric_data2024 %>%
            mutate(
              ExciseRate = ifelse(
                HS_code %in% c("2401300000"),
                35,
                ExciseRate
              )
            )
          
          
          
          taric_data2024 <- taric_data2024 %>%
            mutate(
              ExciseRate = case_when(
                HS_code == "2402100000" ~ 65,
                # HS_code == "2402900000" ~ 55,
                TRUE ~ ExciseRate
              )
            )
          
          
          
          taric_data2024 <- taric_data2024 %>%
            mutate(
              ExciseRate = case_when(
                HS_code == "2404120010" ~ 0.035,
                HS_code == "2404120020" ~ 0.035,
                HS_code == "2404120090" ~ 35,
                HS_code == "2404199000" ~ 0.035,
                TRUE ~ ExciseRate
              )
            )
          
          
          taric_data2024 <- taric_data2024 %>%
            mutate(
              ExciseRate = case_when(
                HS_code == "2710201100" ~ 0.36,
                HS_code == "2710201900" ~ 0.36,
                TRUE ~ ExciseRate
              )
            )
          
          
          
          
          
          taric_data2024 <- taric_data2024 %>%
            mutate(
              ExciseRate = case_when(
                HS_code == "8703239041" ~ 2200,
                HS_code == "8703249051" ~ 3900,
                HS_code == "8703329024" ~ 1500,
                HS_code == "8703329041" ~ 2200,
                
                HS_code == "8703339020" ~ 2200,
                HS_code == "8703339051" ~ 1500,
                HS_code == "8703401003" ~ 10,
                HS_code == "8703409040" ~ 10,
                
                HS_code == "8703409041" ~ 10,
                HS_code == "8703409051" ~ 10,
                HS_code == "8703500003" ~ 10,
                HS_code == "8703500004" ~ 10,
                
                
                HS_code == "8703500040" ~ 10,
                HS_code == "8703601003" ~ 10,
                HS_code == "8703601004" ~ 10,
                HS_code == "8703609000" ~ 10,
                HS_code == "8703700003" ~ 10,
                
                TRUE ~ ExciseRate
              )
            )
          
          
          
          
          
          # View(taric_data2024)
          
          
          tbl_df<- taric_data2024 %>%
            dplyr::group_by(Year) %>%
            dplyr::summarise(across(everything(), ~ sum(is.na(.))))
          
          
          #  View(tbl_df)
          
          
          
          taric_data2024 <- taric_data2024 %>%
            dplyr::mutate(
              Excise_Description = NA_character_,
              CustomsRate_MFN   = as.numeric(CustomsRate_MFN),
              ExciseRate        = as.numeric(ExciseRate),
              VAT_Rate          = as.numeric(VAT_Rate),
              CustomsRate_CEFTA = as.numeric(CustomsRate_CEFTA),
              CustomsRate_MSA   = as.numeric(CustomsRate_MSA),
              CustomsRate_TR    = as.numeric(CustomsRate_TR),
              Year              = as.numeric(Year)
            )
          
          

          # 3.TARIK-2025 ---------------------------------------------------------------------
          
          taric_data2025 <- read_excel("Tarik_Data/Kosovo_TARIK_2025_extracted_pages_34_923_4.xlsx", 
                                       sheet = "Tariff_lines")
          
          taric_data2025 <- taric_data2025 %>%
            dplyr::mutate(
              preferential_rate = stringr::str_squish(as.character(preferential_rate)),
              CustomsRate_CEFTA = as.numeric(stringr::str_match(preferential_rate, "CEFTA\\s*:\\s*([0-9.,]+)")[, 2]),
              CustomsRate_MSA   = as.numeric(stringr::str_match(preferential_rate, "MSA/GB\\s*:\\s*([0-9.,]+)")[, 2]),
              CustomsRate_TR    = as.numeric(stringr::str_match(preferential_rate, "TR\\s*:\\s*([0-9.,]+)")[, 2])
            )
          
          taric_data2025 <- taric_data2025 %>%
            dplyr::mutate(
              preferential_rate = stringr::str_squish(as.character(preferential_rate)),
              duty_rate_pct     = as.character(duty_rate_pct),
              vat_pct           = as.character(vat_pct),
              excise_rate       = as.character(excise_rate)
            ) %>%
            dplyr::mutate(
              duty_rate_pct = dplyr::if_else(
                !stringr::str_detect(preferential_rate, "^CEFTA\\s*:"),
                preferential_rate,
                duty_rate_pct
              ),
              vat_pct = dplyr::if_else(
                !stringr::str_detect(preferential_rate, "^CEFTA\\s*:"),
                excise_rate,
                vat_pct
              ),
              preferential_rate = dplyr::if_else(
                !stringr::str_detect(preferential_rate, "^CEFTA\\s*:"),
                NA_character_,
                preferential_rate
              ),
              excise_rate = dplyr::if_else(
                !stringr::str_detect(preferential_rate, "^CEFTA\\s*:"),
                NA_character_,
                excise_rate
              )
            )
          
          
          
          taric_data2025 <- taric_data2025 %>%
            dplyr::mutate(
              duty_rate_pct = stringr::str_trim(stringr::str_remove(as.character(duty_rate_pct), "\\s*\\([0-9]+\\)")),
              vat_pct       = stringr::str_trim(stringr::str_remove(as.character(vat_pct), "\\s*\\([0-9]+\\)"))
            )
          
          
          
          
          
          taric_data2025 <- taric_data2025 %>%
            dplyr::mutate(
              excise_rate = stringr::str_squish(as.character(excise_rate)),
              excise_rate = dplyr::na_if(excise_rate, "-"),
              excise_rate = dplyr::na_if(excise_rate, "â"),
              excise_rate = dplyr::na_if(excise_rate, "netto"),
              excise_rate = stringr::str_replace_all(excise_rate, ",", "."),
              excise_rate = stringr::str_extract(excise_rate, "[0-9]+(?:\\.[0-9]+)?"),
              excise_rate = as.numeric(excise_rate)
            )
          
          
          
          
          taric_data2025 <- taric_data2025 %>%
            dplyr::mutate(
              vat_pct = stringr::str_squish(as.character(vat_pct)),
              excise_rate = as.character(excise_rate),
              excise_rate = dplyr::if_else(
                stringr::str_detect(vat_pct, "^[0-9]+\\s+[0-9]+(?:[\\.,][0-9]+)?$"),
                stringr::word(vat_pct, 2),
                excise_rate
              ),
              vat_pct = dplyr::if_else(
                stringr::str_detect(vat_pct, "^[0-9]+\\s+[0-9]+(?:[\\.,][0-9]+)?$"),
                stringr::word(vat_pct, 1),
                vat_pct
              ),
              excise_rate = as.numeric(stringr::str_replace_all(excise_rate, ",", ".")),
              vat_pct = as.numeric(vat_pct)
            )
          
          #View(taric_data2025)
          
          
          
          
          
          taric_data2025 <- taric_data2025 %>%
            dplyr::mutate(HS_code = gsub("\\s+", "", HS_code))
          
          
          
          taric_data2025 <- taric_data2025 %>%
            dplyr::mutate(
              HS_code_clean = gsub("\\s+", "", HS_code),
              Chapter = substr(HS_code_clean, 1, 2),
              HS4_COD = substr(HS_code_clean, 1, 4),
              HS6_COD = substr(HS_code_clean, 1, 6)
            ) %>%
            dplyr::select(-HS_code_clean)
          
          
          
          taric_data2025 <- taric_data2025 %>%
            dplyr::rename(
              SupplementaryUnit = Supplementary_units,
              Description_EN    = Description,
              CustomsRate_MFN   = duty_rate_pct,
              ExciseRate        = excise_rate,
              VAT_Rate          = vat_pct
            ) %>%
            dplyr::select(
              HS_code, HS6_COD, HS4_COD, SupplementaryUnit,
              CustomsRate_MFN, ExciseRate, VAT_Rate,
              CustomsRate_CEFTA, CustomsRate_MSA, CustomsRate_TR,
              Description_EN, Chapter
            )
          
          
          taric_data2025$Year<-"2025"
          
          
          
          # Imputing missing data 
          taric_data2025 <- taric_data2025 %>%
            mutate(
              ExciseRate = ifelse(
                HS_code %in% c("2207100000", "2207200090"),
                500,
                ExciseRate
              )
            )
          
          taric_data2025 <- taric_data2025 %>%
            mutate(
              ExciseRate = ifelse(
                HS_code %in% c("2401300000"),
                35,
                ExciseRate
              )
            )
          
          
          taric_data2025 <- taric_data2025 %>%
            mutate(
              ExciseRate = case_when(
                HS_code == "2402100000" ~ 65,
                # HS_code == "2402900000" ~ 55,
                TRUE ~ ExciseRate
              )
            )
          
          
          taric_data2025 <- taric_data2025 %>%
            mutate(
              ExciseRate = case_when(
                HS_code == "2404120010" ~ 0.035,
                HS_code == "2404120020" ~ 0.035,
                HS_code == "2404120090" ~ 35,
                HS_code == "2404199000" ~ 0.035,
                TRUE ~ ExciseRate
              )
            )
          
          
          
          taric_data2025 <- taric_data2025 %>%
            mutate(
              ExciseRate = case_when(
                HS_code == "2710201100" ~ 0.36,
                HS_code == "2710201900" ~ 0.36,
                TRUE ~ ExciseRate
              )
            )
          
          
          
          
          taric_data2025 <- taric_data2025 %>%
            mutate(
              ExciseRate = case_when(
                HS_code == "8703239041" ~ 2200,
                HS_code == "8703249051" ~ 3900,
                HS_code == "8703329024" ~ 1500,
                HS_code == "8703329041" ~ 2200,
                
                HS_code == "8703339020" ~ 2200,
                HS_code == "8703339051" ~ 1500,
                HS_code == "8703401003" ~ 10,
                HS_code == "8703409040" ~ 10,
                
                HS_code == "8703409041" ~ 10,
                HS_code == "8703409051" ~ 10,
                HS_code == "8703500003" ~ 10,
                HS_code == "8703500004" ~ 10,
                
                
                HS_code == "8703500040" ~ 10,
                HS_code == "8703601003" ~ 10,
                HS_code == "8703601004" ~ 10,
                HS_code == "8703609000" ~ 10,
                HS_code == "8703700003" ~ 10,
                
                TRUE ~ ExciseRate
              )
            )
          
          
          
          # taric_data2025 <- taric_data2025 %>%
          #   dplyr::mutate(HS_code = gsub("\\s+", "", HS_code))
          
          #View(taric_data2025)
          
          
          
          colnames(taric_data2025)
          colnames(taric_data2024)
          
          
          
          unique(taric_data2025$CustomsRate_MFN)
          unique(taric_data2025$VAT_Rate)
          unique(taric_data2025$ExciseRate)
          unique(taric_data2025$CustomsRate_CEFTA)
          unique(taric_data2025$CustomsRate_TR)
          
          
          
          
          
          taric_data2025 <- taric_data2025 %>%
            dplyr::mutate(
              Excise_Description = NA_character_,
              Year              = as.numeric(Year),
              CustomsRate_MFN   = as.numeric(CustomsRate_MFN),
              ExciseRate        = as.numeric(ExciseRate),
              VAT_Rate          = as.numeric(VAT_Rate),
              CustomsRate_CEFTA = as.numeric(CustomsRate_CEFTA),
              CustomsRate_MSA   = as.numeric(CustomsRate_MSA),
              CustomsRate_TR    = as.numeric(CustomsRate_TR)
            )
          
          taric_data <- dplyr::bind_rows(
            taric_data2023,
            taric_data2024,
            taric_data2025
          )
          
          
          
          
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
          
          
          
          
          
# II.Regular Import ----------------------------------------------------------

        # 1. Import and combine raw monthly import files --------------------------------
        
        file_paths <- c(
          "Raw-ImportData/Open_DATA_Import Janar-Dhjetor 2023.xlsx",
          "Raw-ImportData/Open_DATA_Import Janar-Dhjetor-2024.xlsx",
          "Raw-ImportData/Open_DATA_Import Janar-Dhjetor 2025.xlsx"
        )
        
        Import_raw_monthly <- purrr::map_dfr(file_paths, read_excel)
        
        # 2. Standardize column names ---------------------------------------------------
        
        colnames(Import_raw_monthly)[1:11] <- c(
          "Year", "Month", "TradePolicy", "Countries", "Code_Description",
          "Quantity", "Value", "Netweight", "CustomsRevenue", "ExciseRevenue", "VAT_Revenue"
        )
        
        # 3. Split HS code / description and country code ------------------------------
        
        split_columns_hs <- strsplit(Import_raw_monthly$Code_Description, "-")
        Import_raw_monthly$HS_code <- sapply(split_columns_hs, `[`, 1)
        Import_raw_monthly$Description <- sapply(split_columns_hs, `[`, 2)
        
        split_columns_countries <- strsplit(Import_raw_monthly$Countries, "-")
        Import_raw_monthly$iso2c <- sapply(split_columns_countries, `[`, 1)
        
        Import_raw_monthly <- Import_raw_monthly %>%
          dplyr::mutate(
            HS_code = trimws(HS_code, which = "both"),
            iso2c   = trimws(iso2c, which = "both")
          )
        
        # 4. Keep relevant columns ------------------------------------------------------
        
        Import_raw_monthly$TradePolicy <- NULL
        Import_raw_monthly$Countries   <- NULL
        
        Import_raw_monthly <- Import_raw_monthly %>%
          dplyr::select(
            HS_code, Description, iso2c, Month, Year,
            Quantity, Value, Netweight, CustomsRevenue, ExciseRevenue, VAT_Revenue
          )
        
        # 5. Estimate tax expenditure base by Year and HS code ---------------------------
        
        CustomsDuties_base <- Import_raw_monthly %>%
          dplyr::select(
            Year, HS_code, Value, Quantity, Netweight,
            CustomsRevenue, ExciseRevenue, VAT_Revenue
          ) %>%
          dplyr::group_by(Year, HS_code) %>%
          dplyr::summarise(
            Value = sum(Value, na.rm = TRUE),
            Quantity = sum(Quantity, na.rm = TRUE),
            Netweight = sum(Netweight, na.rm = TRUE),
            CustomsRevenue = sum(CustomsRevenue, na.rm = TRUE),
            ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE),
            VAT_Revenue = sum(VAT_Revenue, na.rm = TRUE),
            Effective_Customs_rate = round(
              sum(CustomsRevenue, na.rm = TRUE) / sum(Value, na.rm = TRUE), 4
            ) * 100,
            Effective_Excise_rate = round(
              sum(ExciseRevenue, na.rm = TRUE) / sum(Quantity, na.rm = TRUE), 3
            ),
            Effective_VAT_rate = round(
              sum(VAT_Revenue, na.rm = TRUE) /
                (sum(Value, na.rm = TRUE) +
                   sum(CustomsRevenue, na.rm = TRUE) +
                   sum(ExciseRevenue, na.rm = TRUE)),
              2
            ) * 100,
            .groups = "drop"
          )
        

        
        # 6. Prepare simulation parameters ----------------------------------------------
        
        customs_simulation_parameters <- customs_simulation_parameters %>%
          dplyr::mutate(
            across(where(is.numeric), ~ replace_na(., 0)),
            across(where(is.character), ~ replace_na(., ""))
          )
        
        # IMPORTANT:
        # If customs_simulation_parameters does not already contain Year,
        # you need to create one before merging.
        # Example below assumes you want to replicate parameters for each year found in raw data.
        # 
        # years_tbl <- tibble(Year = sort(unique(Import_raw_monthly$Year)))
        # 
        # customs_simulation_parameters <- customs_simulation_parameters %>%
        #   tidyr::crossing(years_tbl)
        
        # 7. Final merge by Year and HS code -------------------------------------------
        
        # old            
        # customs_simulation_parameters_final <- customs_simulation_parameters %>%
        #                 dplyr::left_join(CustomsDuties_base, by = c("Year", "HS_code"))
        
        customs_simulation_parameters_final <- customs_simulation_parameters %>%
          dplyr::mutate(
            HS_code = gsub("\\s+", "", HS_code),
            Year = as.integer(Year)
          ) %>%
          dplyr::full_join(
            CustomsDuties_base %>%
              dplyr::mutate(
                HS_code = gsub("\\s+", "", HS_code),
                Year = as.integer(Year)
              ),
            by = c("Year", "HS_code")
          )
        
        # Sanity check
        # sum(customs_simulation_parameters_final$Value,na.rm = TRUE)
        # sum(customs_simulation_parameters_final$CustomsRevenue,na.rm = TRUE)
        # sum(customs_simulation_parameters_final$ExciseRevenue,na.rm = TRUE)
        # sum(customs_simulation_parameters_final$VAT_Revenue,na.rm = TRUE)
        
        
        # 8. Clean final dataset ------------------------------------------------------
        
        customs_simulation_parameters_final$CN_CODE <- NULL
        customs_simulation_parameters_final$Description_EN <- NULL
        
        customs_simulation_parameters_final <- customs_simulation_parameters_final %>%
          dplyr::mutate(
            across(where(is.numeric), ~ replace_na(., 0)),
            across(where(is.character), ~ replace_na(., ""))
          )
        
        # If your HS code variable in parameters is called HS_code1 instead of HS_code,
        # use this instead before the join:
        # customs_simulation_parameters <- customs_simulation_parameters %>%
        #   dplyr::rename(HS_code = HS_code1)
        
        
        
        # 9. Adding base for calculation of Excise Base ------------------------------
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
              Year == 2023 & Description_Chapters %in% c(
                "2203-Beer made from malt",
                "2204-Wine of fresh grapes, including fortified wines; grape must other than that of heading 2009",
                "2205-Vermouth and other wine of fresh grapes flavoured with plants or aromatic substances",
                "2206-Other fermented beverages",
                "2207-Undenatured ethyl alcohol of an alcoholic strength by volume of 80 % vol or higher; ethyl alcohol and other spirits, denatured, of any strength",
                "2208-Undenatured ethyl alcohol of an alcoholic strength by volume of less than 80 % vol; spirits, liqueurs and other spirituous beverages"
              ) ~ ExciseRate * 100,
              TRUE ~ ExciseRate
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
        
        
        
        
        # 10. Excise Base ------------------------------------------------------
        
        customs_simulation_parameters_final_excise_base <- customs_simulation_parameters_final_excise_names %>%
          mutate(
            ExciseBase = case_when(
              Group == "Alcohol" & Quantity > 0 & ExciseRate > 0 ~ ExciseRevenue / (Quantity / 100 * ExciseRate), 
              Category=="CIGARS AND CIGARILLOS"& Quantity > 0 & ExciseRate > 0 ~ Quantity * 1000, 
              TRUE ~ NA_real_ # Assign NA_real_ for numeric columns
            )
          )
        
        
        # 2.1 Cars -------------------------------------------------------------------
        
        
        Cars_ExciseRates<-read_excel("HS_ExciseCarsClean1.xlsx")
        
        
        Cars_ExciseRates$ExciseRate<-as.integer(Cars_ExciseRates$ExciseRate)
        
        customs_simulation_parameters_final1<-customs_simulation_parameters_final_excise_base%>%
          select(-c("CustomsRevenue","ExciseRevenue"))
        
        
        
        # 2.2 Extract Customs Policy Parameters ---------------------------------------
        
        
        
        write.xlsx(customs_simulation_parameters_final1, "Customs_Parameters.xlsx")
        
        
        
        
        
        # 2.2 Preparation of data list (MAIN SIMULATION FILE IN THE MODEL CUSTOMS_DATA) -------------------------------------------------
        
        customs_data <- customs_simulation_parameters_final %>%
          select(Year, HS_code, Value, Quantity, Netweight, CustomsRevenue, ExciseRevenue, TypeOfProducts)
        
        customs_data <- customs_data %>%
          mutate(across(where(is.numeric), ~ replace_na(., 0)))
        
        customs_data <- customs_data %>%
          mutate(HS = substr(HS_code, 1, 10),
                 Chapter = substr(HS, 1, 2),
                 Four_digit = substr(HS, 1, 4),
                 Six_digit = substr(HS, 1, 7),
                 Eight_digit = paste0(substr(HS, 1, 4),
                                      "",
                                      substr(HS, 5, 7),
                                      " ",
                                      substr(HS, 8, 9)))
        
        
        
        # Weights       
        # customs_data_weights <- read_csv("customs_weights.csv")%>%
        #   as.data.frame()
        
        
        n <- NROW(customs_data)
        
        same_weight<-1
        
        
        customs_data_weights <- data.frame(
          t0 = rep(same_weight, n),
          t1 = rep(1, n),
          t2 = rep(1, n),
          t3 = rep(1, n),
          t4 = rep(1, n),
          t5 = rep(1, n)
        )
        rm(n)
        
        
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
        
        
        CustomsDuties_TE_agg_countries <- CustomsDuties_base %>%
          dplyr::filter(Treatment == "NonPreferential") %>%
          dplyr::group_by(Year) %>%
          dplyr::summarise(
            Value = sum(Value, na.rm = TRUE),
            Quantity = sum(Quantity, na.rm = TRUE),
            Netweight = sum(Netweight, na.rm = TRUE),
            CustomsRevenue = sum(CustomsRevenue, na.rm = TRUE),
            ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE),
            VAT_Revenue = sum(VAT_Revenue, na.rm = TRUE),
            .groups = "drop"
          )
        
        
        # 4.3 Adding TARIC rates --------------------------------------------------
        
        
        CustomsDuties_TE_agg_HS <- CustomsDuties_base %>%
          dplyr::group_by(Year, HS_code, Treatment, FreeTradeAgreements, HS_code_s) %>%
          dplyr::summarise(
            Value = sum(Value, na.rm = TRUE),
            Quantity = sum(Quantity, na.rm = TRUE),
            Netweight = sum(Netweight, na.rm = TRUE),
            CustomsRevenue = sum(CustomsRevenue, na.rm = TRUE),
            ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE),
            VAT_Revenue = sum(VAT_Revenue, na.rm = TRUE),
            Effective_Customs_rate = sum(CustomsRevenue, na.rm = TRUE) / sum(Value, na.rm = TRUE) * 100,
            Effective_Excise_rate = sum(ExciseRevenue, na.rm = TRUE) / sum(Quantity, na.rm = TRUE),
            Effective_VAT_rate = sum(VAT_Revenue, na.rm = TRUE) /
              (sum(Value, na.rm = TRUE) + sum(CustomsRevenue, na.rm = TRUE) + sum(ExciseRevenue, na.rm = TRUE)) * 100,
            .groups = "drop"
          ) %>%
          dplyr::left_join(
            taric_data,
            by = c("Year", "HS_code")
          )
        
        
        #View(CustomsDuties_TE_agg_HS)
        
        
        #View(taric_data)
        
        #  colSums(is.na(CustomsDuties_TE_agg_HS))
        
        
        
        tbl_df<- CustomsDuties_TE_agg_HS %>%
          dplyr::group_by(Year) %>%
          dplyr::summarise(across(everything(), ~ sum(is.na(.))))
        
        
        #View(tbl_df)
        
        
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
        
        
        
       # CustomsDuties_TE_agg_HS
        

# III. PIT --------------------------------------------------------------------

        # PRO link data: https://www.atk-ks.org/en/portfolio/informata-te-pergjithshme-per-tatimet-ne-kosove/
        
        # PRO link calculator : https://crmm.atk-ks.org/en/Public/CalculatorDetail?id=11
        
        
        # Four data sets :
                        # pit_raw_pd,
                        # pit_raw_wm,
                        # pit_raw_wr,
                        # pit_raw_is
        
  
        path3 <- paste0(path1, "/Data/PIT/Data_raw")
        setwd(path3)
        getwd()
        
        # Step1 Find growth factors file automatically ----
        
        growth_files <- list.files(
          path        = path3,
          pattern     = "^growth_factors_.*\\.xlsx$",
          full.names  = TRUE,
          ignore.case = TRUE
        )
        
        # Ignore Excel temporary lock files (e.g. ~$growth_factors_2025.xlsx)
        growth_files <- growth_files[!grepl("^~\\$", basename(growth_files))]
        
        if (length(growth_files) == 0L) {
          stop("No file starting with 'growth_factors_' found in: ", path3)
        }
        
        if (length(growth_files) > 1L) {
          # Pick the file with the highest year in the name; fall back to latest modified
          file_years <- suppressWarnings(
            as.integer(gsub(".*growth_factors_(\\d{4}).*", "\\1", basename(growth_files)))
          )
          
          if (all(!is.na(file_years))) {
            file_path <- growth_files[which.max(file_years)]
          } else {
            file_path <- growth_files[which.max(file.info(growth_files)$mtime)]
          }
          
          message("Multiple growth factor files found. Using: ", basename(file_path))
        } else {
          file_path <- growth_files[1L]
          message("Using growth factor file: ", basename(file_path))
        }
        
        # Step2 Import growth factor sheets ----
        
        pit_growth_factors_pd <- read_excel(file_path, sheet = "PD")
        pit_growth_factors_wm <- read_excel(file_path, sheet = "WM")
        pit_growth_factors_wr <- read_excel(file_path, sheet = "WR")
        pit_growth_factors_is <- read_excel(file_path, sheet = "IS")
        pit_growth_factors_qs <- read_excel(file_path, sheet = "QS")
        
        
        
        
        
        
        file_path <- file.path(path3, "nace_concordance.xlsx")
        
        nace_concordance<- read_excel(file_path, sheet = "NACE_PIT")
        
        
        
        
        dictionary_taxpayers_type <- read_csv("dictionary_taxpayers_type.csv",
          locale = locale(encoding = "ISO-8859-1")
        )
        
        
        
        
       # In the original WM-Wage and payroll data, a column for NACE code is not available, therefore an additional mapping has been made according to which it is 
       # used only for this dataset.
       
        
        file_path <- file.path(path3, "NACE_mapping.xlsx")
        
        nace_mapping<- read_excel(file_path, sheet = "nace_map")
        
           
        
        LCU_unit<-1e03
     
        
        
        
        
        # 1. PD-Annual declaration and payment for personal income tax -------------------------------------------------------------------------
        
        'Annual Declaration Form and payment for Personal Income Tax'
        
        
        "TaxpayerType_en" ###<----- is missing !!!!
        
        csv_folder_pd <- paste0(path1, "/Data/PIT/Data_raw/PD")
        setwd(path3)
        
        csv_files_pd <- list.files(
                                  path = csv_folder_pd,
                                  pattern = "\\.csv$",
                                  full.names = TRUE,
                                  ignore.case = TRUE
                                )
                                
                                if (length(csv_files_pd) == 0) {
                                  stop("No CSV files found in: ", csv_folder_pd)
                                }
                                
                                process_pit_file <- function(file_path) {
                                  
                                  pit_raw_pd <- read_csv(
                                    file_path,
                                    locale = locale(encoding = "ISO-8859-1"),
                                    show_col_types = FALSE
                                  ) |> as.data.table()
          
          # pit_raw_pd <- pit_raw_pd %>%
          #   filter(AKTI_PASIV_MBYLLUR == "AKTIV")
          
          pit_raw_pd <- pit_raw_pd %>%
            select(-c(DATA_FILLIMIT, FISCAL_NO, TPER_DUE_DATE, RECEPTION_DATE,
                      REASS_CREATION_DATE, EMRI, ETL_INSERTED_AT, ETL_UPDATED_AT,
                      ENCRYPTED_AT, SHTETESIA, VERSION_NO, FORM_NO,
                      AKTI_PASIV_MBYLLUR, TPER_MONTH, FORM_DESC))
          
          pit_raw_pd <- pit_raw_pd %>%
                      dplyr::rename(
                        "id_n" = "TAX_PAYER_NO",
                        "Year" = "TPER_YEAR",
                        "city" = "QYTETI",
                        "TaxpayerType" = "LLOJI_KOMPAN",
                        #"companytype" = "LLOJI_KOMPAN",
                        "Activity" = "AKTIVITETI",
                        "SectorDescription" = "PERSHKRIMI_SEKTORIT",
                        "entactivitycode" = "ENT_ACTIVITY_CODE",
                        "size" = "MADHESIA",
                        "gross_wage" = "D8",
                        "net_income_business" = "D9",
                        "net_income_partnership" = "D10",
                        "gross_rents" = "D11",
                        "gross_i_interest_pen_pay" = "D12",
                        "gross_i_interest" = "D13",
                        "gross_i_lottery" = "D14",
                        "gross_i_inta_prop" = "D15",
                        "capital_gain" = "D16",
                        "foreign_s_inc" = "D17",
                        "other_inc_gifts" = "D18",
                        "total_inc" = "D19",
                        "ded_rents_expen_rents10pct" = "D20",
                        "ded_pen_cont" = "D21",
                        "ded_exp_int_prop" = "D22",
                        "other_allowed_ded" = "D23",
                        "total_ded" = "D24",
                        "taxable_amount" = "D25",
                        "loss_carried_for" = "D26",
                        "dis_charity_contribution" = "D27",
                        "tot_additional_ded" = "D28",
                        "taxable_inc_before_tax" = "D29",
                        "tax_on_tax_inc_bracket" = "D30",
                        "tax_withheld_wage_employer" = "D31",
                        "tax_withheld_wage_interest" = "D32",
                        "tax_withheld_rents" = "D33",
                        "tax_witheld_lottery_gains" = "D34",
                        "foreign_tax_credit" = "D35",
                        "total_witheld_credit" = "D36",
                        "total_pay_for" = "D37",
                        "q_advance_business_inc" = "D38",
                        "q_payment_ren_inc_int_prop" = "D39",
                        "total_advance_payment" = "D40",
                        "total_tax_payable" = "D41",
                        "amount_due" = "D42",
                        "sector" = "SEKTORI"
                      ) %>%
              dplyr::select(-c(D43, D44, D45, D46, D47, D48, D49, D50, D51, D52, D53,
                                       D54, D55, D56, D57, D58, D59, D60, D61, D62, D63, D64,
                                       D65, D66, D67, D68, D69, D70, D71, D72, D73, D74, D75,
                                       D76, D77, D78,REGJIONI))
          
          pit_raw_pd <- pit_raw_pd %>%
            dplyr::mutate(
              total_inc_adjusted   = dplyr::if_else(net_income_business > 0, net_income_business, 0)
             # net_income_business  = dplyr::if_else(net_income_business > 0, net_income_business, 0)
            )
          
   
          pit_raw_pd <- pit_raw_pd %>% dplyr::mutate(source_file = basename(file_path))
          return(pit_raw_pd)
        }
        
        dt_list <- lapply(csv_files_pd, process_pit_file)
        
        pit_raw_pd <- dplyr::bind_rows(dt_list)%>%
          data.table()
        
        
        
        pit_raw_pd$nace <- ifelse(
                                            nchar(pit_raw_pd$entactivitycode) == 4,
                                            substr(pit_raw_pd$entactivitycode, 1, 2),
                                            "NotAvailable"
                                          )
        
        
        pit_raw_pd<-left_join(pit_raw_pd,nace_concordance,by=c("nace"="nace"))
        
        
        
        # Impute NA and 0 with the dominant Year value
        dominant_year <- pit_raw_pd[
          !is.na(Year) & Year > 0,
          .N,
          by = Year
        ][
          which.max(N),
          Year
        ]
        
        pit_raw_pd[
          is.na(Year) | Year == 0,
          Year := dominant_year
        ]
        
        
        
            # 2. WM-Wage and payroll data -------------------------------------------------------------------------
        
        'Monthly statement of withholding tax on salaries and pension contributions'
        "Note: Annual frequency"
        
        
        csv_folder_wm <- paste0(path1, "/Data/PIT/Data_raw/WM")
        setwd(csv_folder_wm)
        
        csv_files_wm <- list.files(
          path = csv_folder_wm,
          pattern = "\\.csv$",
          full.names = TRUE,
          ignore.case = TRUE
        )
        
        if (length(csv_files_wm) == 0) {
          stop("No CSV files found in: ", csv_folder_wm)
        }
        
        process_wm_file <- function(file_path) {
          
          pit_raw_wm <- read_csv(
            file_path,
            locale = locale(encoding = "ISO-8859-1"),
            show_col_types = FALSE
          ) |> as.data.table()
          
          pit_raw_wm <- pit_raw_wm %>%
            select(-c("Source_System", "ETL_Created_At", "ATK_WM_YEARLY_ID"))
          
          pit_raw_wm <- pit_raw_wm %>%
            dplyr::rename(
              "id_n"            = "Artificial_ID",
              "entactivitycode" = "ENT_ACTIVITY_CODE",
              "gross_wage_w"    = "Gross_Wage",
              "tax_witheld"     = "Tax_Withheld",
              "employee_cont"   = "Employee_Contribution",
              "employer_cont"   = "Employer_Contribution",
              "section"         = "Sector_Description",
              "description"     = "Sector_Description",
              "size_of_company" = "Size_Of_Company",
              "city"            = "City",
              "gender"          = "Gender",
              #"company_type"   = "Company_Type",
              "TaxpayerType"    = "Company_Type",
              "net_income"      = "Net_Income"
            )
          
          pit_raw_wm <- pit_raw_wm %>%
            dplyr::mutate(source_file = basename(file_path))
          
          return(pit_raw_wm)
        }
        
        pit_raw_wm_list <- lapply(csv_files_wm, process_wm_file)
        
        pit_raw_wm <- dplyr::bind_rows(pit_raw_wm_list) %>%
          data.table()
        
        
        # NACE codes
        
        pit_raw_wm <- left_join(
          pit_raw_wm,
          nace_mapping,
          by = c("description")
        )
        
        

        # Replace ONLY the active WM section in InitialLoadData.R, from
        # '# 2. WM-Wage and payroll data' to immediately before '# 3. WR-Interest'.
        # Leave PD, WR, IS, QS and the commented-out old WM block unchanged.
        # Run initial load once; the WM calculator must use pit_raw_wm_corrected.
        
        # Step1 Import WM payroll records ----
        
        'Monthly statement of withholding tax on salaries and pension contributions'
        "Note: Annual frequency"
        
        csv_folder_wm <- paste0(path1, "/Data/PIT/Data_raw/WM")
        setwd(csv_folder_wm)
        
        csv_files_wm <- list.files(
          path = csv_folder_wm,
          pattern = "\\.csv$",
          full.names = TRUE,
          ignore.case = TRUE
        )
        if (length(csv_files_wm) == 0L) stop("No CSV files found in: ", csv_folder_wm)
        
        process_wm_file <- function(file_path) {
          pit_raw_wm <- read_csv(
            file_path,
            locale = locale(encoding = "ISO-8859-1"),
            show_col_types = FALSE
          ) |> as.data.table()
          
          pit_raw_wm <- pit_raw_wm %>%
            select(-c("Source_System", "ETL_Created_At", "ATK_WM_YEARLY_ID"))
          
          pit_raw_wm <- pit_raw_wm %>%
            dplyr::rename(
              "id_n"            = "Artificial_ID",
              "entactivitycode" = "ENT_ACTIVITY_CODE",
              "gross_wage_w"    = "Gross_Wage",
              "tax_witheld"     = "Tax_Withheld",
              "employee_cont"   = "Employee_Contribution",
              "employer_cont"   = "Employer_Contribution",
              "section"         = "Sector_Description",
              "description"     = "Sector_Description",
              "size_of_company" = "Size_Of_Company",
              "city"            = "City",
              "gender"          = "Gender",
              #"company_type"  = "Company_Type",
              "TaxpayerType"    = "Company_Type",
              "net_income"      = "Net_Income"
            )
          
          pit_raw_wm <- pit_raw_wm %>%
            dplyr::mutate(source_file = basename(file_path))
          return(pit_raw_wm)
        }
        
        pit_raw_wm_list <- lapply(csv_files_wm, process_wm_file)
        pit_raw_wm <- dplyr::bind_rows(pit_raw_wm_list) %>% data.table()
        
        # Step2 Map WM economic sectors ----
        
        pit_raw_wm <- left_join(pit_raw_wm, nace_mapping, by = c("description"))
        pit_raw_wm <- data.table::as.data.table(pit_raw_wm)
        
        required_wm_cols <- c(
          "id_n", "Year", "gross_wage_w", "employee_cont", "employer_cont",
          "tax_witheld", "net_income", "TaxpayerType"
        )
        missing_wm_cols <- setdiff(required_wm_cols, names(pit_raw_wm))
        if (length(missing_wm_cols)) {
          stop("Imported WM data is missing columns: ", paste(missing_wm_cols, collapse = ", "))
        }
        
       
        # Step3 Normalize baseline year before preparing calculator input ----
        
        wm_expected_base_year <- min(as.integer(pit_growth_factors_wm$Year), na.rm = TRUE)
        pit_raw_wm[, Year := suppressWarnings(as.integer(as.character(Year)))]
        
        # Detect the actual year in the WM data
        wm_positive_years <- unique(pit_raw_wm[!is.na(Year) & Year > 0L, Year])
        
        if (length(wm_positive_years) == 0L) {
          stop("WM data contains no valid Year values.")
        }
        
        if (length(wm_positive_years) > 1L) {
          stop("WM data contains multiple years: ", paste(wm_positive_years, collapse = ", "),
               ". Only one baseline year is allowed.")
        }
        
        # If WM data year differs from growth factor start year, overwrite silently.
        # Growth factors always start from 2024 regardless of data year.
        if (wm_positive_years[1L] != wm_expected_base_year) {
          message("WM data year (", wm_positive_years[1L], ") differs from growth factor ",
                  "start year (", wm_expected_base_year, "). ",
                  "Overwriting WM Year with ", wm_expected_base_year, ".")
        }
        
        pit_raw_wm[, Year := wm_expected_base_year]
        
        
        # Step4 Rule 1 - temporarily set aside raw wages above 1 million ----
        # To undo this exclusion later, change wm_exclude_above_1m to FALSE
        # and rerun InitialLoadData.R. Do not delete the saved register.
        
        wm_exclude_above_1m <- TRUE
        wm_high_wage_threshold <- 1000000
        ssc_rate_correction <- 0.05
        wm_discrepancy_tolerance <- 0.10
        
        pit_raw_wm[, wm_rule1_flag := as.integer(
          !is.na(gross_wage_w) & gross_wage_w > wm_high_wage_threshold
        )]
        
        wm_rule1_excluded <- data.table::copy(pit_raw_wm[wm_rule1_flag == 1L])
        
        # All original WM records remain in pit_raw_wm. Retained data are copied
        # before changing any wages. This excludes all >1M wages regardless of SSC.
        wm_work <- data.table::copy(
          if (isTRUE(wm_exclude_above_1m)) pit_raw_wm[wm_rule1_flag == 0L]
          else pit_raw_wm
        )
        wm_work[, `:=`(
          gross_wage_w_original = gross_wage_w,
          net_income_original = net_income
        )]
        
        wm_rule1_summary <- wm_rule1_excluded[, .(
          records_set_aside = .N,
          distinct_ids = data.table::uniqueN(id_n),
          reported_wages_MIL = sum(gross_wage_w, na.rm = TRUE) / 1e6,
          reported_PIT_MIL = sum(tax_witheld, na.rm = TRUE) / 1e6
        )]
        
        # Step5 Rule 2 - correct SSC-inconsistent wages in retained data ----
        # No 50,000 LCU cutoff. Test any POSITIVE reported wage and employee SSC.
        # If relative discrepancy exceeds 10%, replace wage with employee SSC / 5%.
        # Both upward and downward adjustments remain possible, as in prior rule.
        
        wm_work[, ssc_implied_gross := data.table::fifelse(
          !is.na(employee_cont) & employee_cont > 0,
          employee_cont / ssc_rate_correction,
          NA_real_
        )]
        wm_work[, discrepancy_ratio := data.table::fifelse(
          !is.na(gross_wage_w) & gross_wage_w > 0 & !is.na(ssc_implied_gross),
          abs(gross_wage_w - ssc_implied_gross) / gross_wage_w,
          NA_real_
        )]
        wm_work[, correction_flag := as.integer(
          !is.na(gross_wage_w) & gross_wage_w > 0 &
            !is.na(employee_cont) & employee_cont > 0 &
            !is.na(discrepancy_ratio) & discrepancy_ratio > wm_discrepancy_tolerance
        )]
        
        # Preserve prior audit column names for calculator/report compatibility.
        correction_summary_pre <- wm_work[correction_flag == 1L, .(
          records_to_correct = .N,
          recorded_gross_wage_MIL = sum(gross_wage_w, na.rm = TRUE) / 1e6,
          implied_gross_wage_MIL = sum(ssc_implied_gross, na.rm = TRUE) / 1e6,
          overstatement_MIL = sum(gross_wage_w - ssc_implied_gross, na.rm = TRUE) / 1e6,
          records_wage_decreased = sum(ssc_implied_gross < gross_wage_w, na.rm = TRUE),
          records_wage_increased = sum(ssc_implied_gross > gross_wage_w, na.rm = TRUE),
          reported_pit_MIL = sum(tax_witheld, na.rm = TRUE) / 1e6,
          employee_cont_MIL = sum(employee_cont, na.rm = TRUE) / 1e6
        )]
        correction_by_type_pre <- wm_work[correction_flag == 1L, .(
          records = .N,
          recorded_gross_wage_MIL = sum(gross_wage_w, na.rm = TRUE) / 1e6,
          implied_gross_wage_MIL = sum(ssc_implied_gross, na.rm = TRUE) / 1e6,
          overstatement_MIL = sum(gross_wage_w - ssc_implied_gross, na.rm = TRUE) / 1e6
        ), by = TaxpayerType][order(-overstatement_MIL)]
        
        wm_work[correction_flag == 1L, `:=`(
          gross_wage_w = ssc_implied_gross,
          net_income = ssc_implied_gross -
            data.table::fcoalesce(employee_cont, 0) -
            data.table::fcoalesce(tax_witheld, 0)
        )]
        
        # Step6 Save model input and verify rules ----
        
        # This object retains ALL post-correction working records; there is no
        # second, post-correction zero-SSC exclusion. Calculator uses this copy.
        pit_raw_wm_ssc_corrected_all <- data.table::copy(wm_work)
        pit_raw_wm_corrected <- data.table::copy(wm_work)
        
        helper_cols <- c("ssc_implied_gross", "discrepancy_ratio")
        pit_raw_wm_corrected[, (helper_cols) := NULL]
        pit_raw_wm_ssc_corrected_all[, (helper_cols) := NULL]
        
        # Remove stale names from former Condition 4 architecture to prevent
        # a previous run's 81-record register from being mistaken for current rules.
        if (exists("wm_condition4_excluded", envir = .GlobalEnv, inherits = FALSE)) {
          rm(wm_condition4_excluded, envir = .GlobalEnv)
        }
        if (exists("condition4_summary_pre", envir = .GlobalEnv, inherits = FALSE)) {
          rm(condition4_summary_pre, envir = .GlobalEnv)
        }
        
        if (isTRUE(wm_exclude_above_1m) &&
            nrow(pit_raw_wm_corrected) + nrow(wm_rule1_excluded) != nrow(pit_raw_wm)) {
          stop("WM reconciliation failed: retained + set-aside != raw.")
        }
        if (isTRUE(wm_exclude_above_1m) &&
            any(pit_raw_wm_corrected$gross_wage_w_original > wm_high_wage_threshold,
                na.rm = TRUE)) {
          stop("Rule 1 failed: raw wages >1M remain in calculator data.")
        }
        
        # Rule 2 may raise a retained wage above 1M AFTER selection. This does
        # not violate Rule 1, which is explicitly defined on REPORTED RAW wage.
        wm_revised_above_1m <- pit_raw_wm_corrected[
          !is.na(gross_wage_w) & gross_wage_w > wm_high_wage_threshold, .N
        ]
        cat("\n=== WM one-time cleaning audit ===\n")
        cat("Rule 1 active: ", wm_exclude_above_1m, "\n", sep = "")
        cat("Raw WM records: ", nrow(pit_raw_wm), "\n", sep = "")
        cat("Rule 1 records set aside (raw wage >1M): ",
            nrow(wm_rule1_excluded), "\n", sep = "")
        cat("Rule 2 SSC-corrected records: ",
            correction_summary_pre$records_to_correct, "\n", sep = "")
        cat("Records passed to calculator: ", nrow(pit_raw_wm_corrected), "\n", sep = "")
        cat("Revised wages >1M among retained records: ", wm_revised_above_1m,
            " (check separately; Rule 1 uses raw wage)\n", sep = "")
        print(wm_rule1_summary)
        print(correction_summary_pre)
        
        # END WM SECTION. The next unchanged section of InitialLoadData.R is WR.
        
        
        
        
        # Step7 Set the final WM calculator input ----
        # pit_raw_wm_ssc_corrected_all already contains the 498,819 records
        # retained under Rule 1 and corrected under Rule 2.
        pit_raw_wm_corrected <- data.table::copy(
          pit_raw_wm_ssc_corrected_all
        )
        
        stopifnot(
          nrow(pit_raw_wm_corrected) + nrow(wm_rule1_excluded) ==
            nrow(pit_raw_wm)
        )
        
        
        # 3. WR-Interest, property rights, rent, lottery and gambling winnings and non-resident person ----------------------------------------------------------------------

        # 'Monthly statement of withholding tax and payment of tax on interest, property rights, rent, lottery and gambling winnings and non-resident person'
        
        csv_folder_wr <- paste0(path1, "/Data/PIT/Data_raw/WR")
        setwd(csv_folder_wr)
        
        csv_files_wr <- list.files(
                                      path = csv_folder_wr,
                                      pattern = "\\.csv$",
                                      full.names = TRUE,
                                      ignore.case = TRUE
                                    )

                                    if (length(csv_files_wr) == 0) {
                                      stop("No CSV files found in: ", csv_folder_wr)
                                    }

        process_wr_file <- function(file_path) {

          pit_raw_wr <- read_csv(
                                file_path,
                                locale = locale(encoding = "ISO-8859-1"),
                                show_col_types = FALSE
                              ) |> as.data.table()

                              # pit_raw_wr <- pit_raw_wr %>%
                              #   select(-c("Source_System", "ETL_Created_At", "ATK_WM_YEARLY_ID"))


                          pit_raw_wr <- pit_raw_wr %>%
                              dplyr::filter(AKTI_PASIV_MBYLLUR == "AKTIV")


                              pit_raw_wr <- pit_raw_wr %>%
                                dplyr::rename("id_n"="TAX_PAYER_NO",
                                              "city" = "QYTETI",
                                              "size" = "MADHESIA",
                                              "sector" = "SEKTORI",
                                              "entactivitycode" = "ENT_ACTIVITY_CODE",
                                              "Year"="TPER_YEAR",
                                              "month"="TPER_MONTH",
                                              "Activity" = "AKTIVITETI",
                                              "companytype" = "LLOJI_KOMPAN",
                                              "size" = "MADHESIA",
                                              #"TaxpayerType" = "PERSHKRIMI_SEKTORIT",
                                              "TaxpayerType" = "LLOJI_KOMPAN",
                                              #"companytype" = "LLOJI_KOMPAN",
                                              "SectorDescription" = "PERSHKRIMI_SEKTORIT",
                                              "grossInterestPaid"="D8",
                                              "propertyRightsPaid"="D9",
                                              "grossLotteryWinnings"="D10",
                                              "totalMonthlyPayments"="D11",
                                              "standardWithholdingTax"="D12",
                                              "grossRentPaid"="D13",
                                              "rentWithholdingTax"="D14",
                                              "nonResidentGrossPayment"="D15",
                                              "nonResidentServicePayment"="D16",
                                              "nonResidentWithholdingTax"="D17",
                                              "totalTaxesWithheld"="D18",
                                              "nonBusinessIndividualPayments"="D19",
                                              "nonBusinessIndividualTax"="D20",
                                              "totalWithholdingTax"="D21"
                                                                               )%>%
                                dplyr::select(-c("D22", "D23", "D24", "D25", "D26", "D27",
                                                 "D28", "D29", "D30", "D31", "D32", "D33", "D34",
                                                 "D35", "D36", "D37", "D38", "D39", "D40", "D41",
                                                 "D42", "D43", "D44", "D45", "D46", "D47", "D48",
                                                 "D49", "D50", "D51", "D52", "D53", "D54", "D55",
                                                 "D56", "D57", "D58", "D59", "D60", "D61", "D62",
                                                 "D63", "D64", "D65", "D66", "D67", "D68", "D69",
                                                 "D70", "D71", "D72", "D73", "D74", "D75", "D76",
                                                 "D77", "D78", "ETL_INSERTED_AT", "ETL_UPDATED_AT", "ENCRYPTED_AT",
                                                 "DATA_FILLIMIT","FISCAL_NO","EMRI","DATA_FILLIMIT","SHTETESIA","FORM_NO",
                                                 "FORM_DESC","TPER_DUE_DATE","RECEPTION_DATE","REASS_CREATION_DATE","REGJIONI","VERSION_NO","AKTI_PASIV_MBYLLUR","VERSION_NO",
                                                 "REGJIONI"
                                ))

                              pit_raw_wr <- pit_raw_wr %>% dplyr::mutate(source_file = basename(file_path))

                              return(pit_raw_wr)
                            }

        pit_raw_wr_list <- lapply(csv_files_wr, process_wr_file)

        pit_raw_wr <- dplyr::bind_rows(pit_raw_wr_list)%>%
          data.table()


        # NACE code
        pit_raw_wr$nace <- ifelse(
          nchar(pit_raw_wr$entactivitycode) == 4,
          substr(pit_raw_wr$entactivitycode, 1, 2),
          "NotAvailable"
        )



        pit_raw_wr<-left_join(pit_raw_wr,nace_concordance,by=c("nace"="nace"))

       # unique(pit_raw_wr$nace)

        
    
        
        
        # 4. IS-Small individual businesses ----------------------------------------------------------------------

        "Quarterly statement of tax and contributions for small individual businesses"
        
        csv_folder_is <- paste0(path1, "/Data/PIT/Data_raw/IS")
        setwd(csv_folder_is)
        
        csv_files_is <- list.files(
                              path = csv_folder_is,
                              pattern = "\\.csv$",
                              full.names = TRUE,
                              ignore.case = TRUE
                            )
                            
                            if (length(csv_files_is) == 0) {
                              stop("No CSV files found in: ", csv_folder_is)
                            }
                            
                            process_is_file <- function(file_path) {
                              
                              pit_raw_is <- read_csv(
                                file_path,
                                locale = locale(encoding = "ISO-8859-1"),
                                show_col_types = FALSE
                              ) |> as.data.table()
                              
                              # pit_raw_is <- pit_raw_is %>%
                              #   select(-c("Source_System", "ETL_Created_At", "ATK_WM_YEARLY_ID"))
                              
                              pit_raw_is <- pit_raw_is %>%
                                dplyr::rename("id_n"="TAX_PAYER_NO")%>%
                                dplyr::filter( AKTI_PASIV_MBYLLUR=="AKTIV")%>%
                                dplyr::select(-c("D13", "D14", "D15", "D16", "D17", "D18", "D19", "D20",
                                                 "D21", "D22", "D23", "D24", "D25", "D26", "D27",
                                                 "D28", "D29", "D30", "D31", "D32", "D33", "D34",
                                                 "D35", "D36", "D37", "D38", "D39", "D40", "D41",
                                                 "D42", "D43", "D44", "D45", "D46", "D47", "D48",
                                                 "D49", "D50", "D51", "D52", "D53", "D54", "D55",
                                                 "D56", "D57", "D58", "D59", "D60", "D61", "D62",
                                                 "D63", "D64", "D65", "D66", "D67", "D68", "D69",
                                                 "D70", "D71", "D72", "D73", "D74", "D75", "D76",
                                                 "D77", "D78", "ETL_INSERTED_AT", "ETL_UPDATED_AT", "ENCRYPTED_AT",
                                                 "DATA_FILLIMIT","FISCAL_NO","EMRI",
                                                 "TPER_MONTH",,"TPER_DUE_DATE","RECEPTION_DATE","REASS_CREATION_DATE","SHTETESIA",
                                                 "VERSION_NO","FORM_NO","FORM_DESC","AKTI_PASIV_MBYLLUR","REGJIONI"
                                ))%>%
                                dplyr::rename(
                                                "grossIncomeCommercial"="D8",
                                                "grossIncomeServices"="D9",
                                                "taxCommercial"="D10",
                                                "taxServices"="D11",
                                                "taxPaymentTotal"="D12",
                                                "Year"="TPER_YEAR",
                                                "city" = "QYTETI",
                                                #"TaxpayerType" = "PERSHKRIMI_SEKTORIT",
                                                "TaxpayerType" = "LLOJI_KOMPAN",
                                               # "companytype" = "LLOJI_KOMPAN",
                                                "Activity" = "AKTIVITETI",
                                                "SectorDescription" = "PERSHKRIMI_SEKTORIT",
                                                "entactivitycode" = "ENT_ACTIVITY_CODE",
                                                "size" = "MADHESIA",
                                                "sector" = "SEKTORI"
                                )
                              
                              pit_raw_is <- pit_raw_is %>% dplyr::mutate(source_file = basename(file_path))
          
          return(pit_raw_is)
        }
        
        pit_raw_is_list <- lapply(csv_files_is, process_is_file)
        
        
        pit_raw_is <- dplyr::bind_rows(pit_raw_is_list)%>%
          data.table()
   
        
        
        pit_raw_is$TaxpayerType<-"Individual"
        
        
        
        # NACE code
        pit_raw_is$nace <- ifelse(
          nchar(pit_raw_is$entactivitycode) == 4,
          substr(pit_raw_is$entactivitycode, 1, 2),
          "NotAvailable"
        )
        
        
        
        pit_raw_is<-left_join(pit_raw_is,nace_concordance,by=c("nace"="nace"))

        # 5. QS-QUARTERLY STATEMENT OF TAX PAYMENT FOR SMALL CORPORATIONS ----------------------------------------------------------------------

        "I.Taxpayers with gross income up to 30 000€"
        
       
        csv_folder_qs <- paste0(path1, "/Data/PIT/Data_raw/QS")
        setwd(csv_folder_qs)
        
        csv_files_qs <- list.files(
          path = csv_folder_qs,
          pattern = "\\.csv$",
          full.names = TRUE,
          ignore.case = TRUE
        )
        
        if (length(csv_files_qs) == 0) {
          stop("No CSV files found in: ", csv_folder_qs)
        }
        
        process_qs_file <- function(file_path) {
          
          pit_raw_qs <- read_csv(
            file_path,
            locale = locale(encoding = "ISO-8859-1"),
            show_col_types = FALSE
          ) |> as.data.table()
          
          # pit_raw_qs <- pit_raw_qs %>%
          #   select(-c("Source_System", "ETL_Created_At", "ATK_WM_YEARLY_ID"))
          
          pit_raw_qs <- pit_raw_qs %>%
            dplyr::rename("id_n"="TAX_PAYER_NO")%>%
            dplyr::filter( AKTI_PASIV_MBYLLUR=="AKTIV")%>%
            dplyr::select(-c("D18", "D19", "D20",
                             "D21", "D22", "D23", "D24", "D25", "D26", "D27",
                             "D28", "D29", "D30", "D31", "D32", "D33", "D34",
                             "D35", "D36", "D37", "D38", "D39", "D40", "D41",
                             "D42", "D43", "D44", "D45", "D46", "D47", "D48",
                             "D49", "D50", "D51", "D52", "D53", "D54", "D55",
                             "D56", "D57", "D58", "D59", "D60", "D61", "D62",
                             "D63", "D64", "D65", "D66", "D67", "D68", "D69",
                             "D70", "D71", "D72", "D73", "D74", "D75", "D76",
                             "D77", "D78", "ETL_INSERTED_AT", "ETL_UPDATED_AT", "ENCRYPTED_AT",
                             "DATA_FILLIMIT","FISCAL_NO","EMRI",
                             "TPER_MONTH",,"TPER_DUE_DATE","RECEPTION_DATE","REASS_CREATION_DATE","SHTETESIA",
                             "VERSION_NO","FORM_NO","FORM_DESC","AKTI_PASIV_MBYLLUR","REGJIONI"
            ))%>%
            dplyr::rename(
              "grossIncomeCommercial"="D8",
              "grossIncomeServices"="D9",
              "taxCommercial"="D10",
              "taxServices"="D11",
              "taxPaymentTotal"="D12",
              
              "gross_rent_income"="D13",
              "rent_tax_9pct"="D14",
              "rent_tax_withheld_others"="D15",
              "rent_tax_net"="D16",
              "total_amount_due"="D17",
              
              "Year"="TPER_YEAR",
              "city" = "QYTETI",
              "TaxpayerType" = "LLOJI_KOMPAN",
              "SectorDescription" = "PERSHKRIMI_SEKTORIT",
              
              "companytype" = "LLOJI_KOMPAN",
              "Activity" = "AKTIVITETI",
              "SectorDescription" = "PERSHKRIMI_SEKTORIT",
              "entactivitycode" = "ENT_ACTIVITY_CODE",
              "size" = "MADHESIA",
              "sector" = "SEKTORI"
            )
          
          
          pit_raw_qs <- pit_raw_qs %>% dplyr::mutate(source_file = basename(file_path))
          
          return(pit_raw_qs)
        }
        
        pit_raw_qs_list <- lapply(csv_files_qs, process_qs_file)
        
        
        pit_raw_qs <- dplyr::bind_rows(pit_raw_qs_list)%>%
          data.table()
        
        
        
        
        # NACE code
        pit_raw_qs$nace <- ifelse(
          nchar(pit_raw_qs$entactivitycode) == 4,
          substr(pit_raw_qs$entactivitycode, 1, 2),
          "NotAvailable"
        )
        
        
        pit_raw_qs<-left_join(pit_raw_qs,nace_concordance,by=c("nace"="nace"))
        
        # 3.Growth Factors & Scenario Mapping -------------------------------------
        
        # 1. Dynamic horizon (already confirmed working)
        scenario_years_pit <- pit_growth_factors_pd$Year
        scenarios_pit    <- pit_growth_factors_pd$scenarios
        
        
        
        base_year <- unique(pit_growth_factors_pd$Year)[1]
        end_year <- base_year + 4
       # SimulationYear <- SimulationYear  # Year from slider
        forecast_horizon <- seq(base_year, end_year)
     
        
        
# IV.CIT ------------------------------------------------------------------


        # Step1 Locate CIT input files ----
        
        path4 <- file.path(path1, "Data", "CIT", "Data_raw")
        
        find_required_file <- function(filename, folder = path4) {
          file_path <- file.path(folder, filename)
          
          if (!file.exists(file_path)) {
            stop("Required file not found: ", file_path, call. = FALSE)
          }
          
          file_path
        }
        
        nace_file <- find_required_file("NACE_SUT_table.xlsx")
        company_type_file <- find_required_file("TypeOfCompanies.xlsx")
        
        
        # Step2 Find the growth-factor workbook automatically ----
        
        growth_files <- list.files(
          path        = path4,
          pattern     = "^growth_?factors_cit_[0-9]{4}\\.xlsx$",
          full.names  = TRUE,
          ignore.case = TRUE
        )
        
        # Exclude Excel temporary lock files
        growth_files <- growth_files[!grepl("^~\\$", basename(growth_files))]
        
        if (length(growth_files) == 0L) {
          stop(
            "No growth-factor workbook matching ",
            "'growth_factors_cit_YYYY.xlsx' or 'growthfactors_cit_YYYY.xlsx' found in: ",
            path4,
            call. = FALSE
          )
        }
        
        growth_years <- as.integer(
          sub(
            "^growth_?factors_cit_([0-9]{4})\\.xlsx$",
            "\\1",
            basename(growth_files),
            ignore.case = TRUE
          )
        )
        
        # Select the workbook with the highest year in its filename
        growth_file <- growth_files[which.max(growth_years)]
        
        message("Using CIT growth-factor workbook: ", basename(growth_file))
        
        
        # Step3 Import CIT reference tables and growth factors ----
        
        NACE_SUT_table_cit <- readxl::read_excel(nace_file) |>
          dplyr::select(nace, section, description) |>
          dplyr::distinct(nace, .keep_all = TRUE)
        
        TypeOfCompanies_CIT <- readxl::read_excel(company_type_file)
        
        growth_sheet <- "growthfactors_cit"
        available_sheets <- readxl::excel_sheets(growth_file)
        
        if (!growth_sheet %in% available_sheets) {
          stop(
            "Sheet '", growth_sheet, "' not found in ", basename(growth_file),
            ". Available sheets: ", paste(available_sheets, collapse = ", "),
            call. = FALSE
          )
        }
        
        growth_factors_cit <- data.table::as.data.table(
          readxl::read_excel(growth_file, sheet = growth_sheet)
        )
        
        if (!"Year" %in% names(growth_factors_cit)) {
          stop("The growth-factor sheet must contain a 'Year' column.", call. = FALSE)
        }
        
        growth_factors_cit[, Year := as.integer(Year)]
        
        # Add scenario labels only if absent; do not invent or alter growth rates.
        if (!"scenarios" %in% names(growth_factors_cit)) {
          data.table::setorder(growth_factors_cit, Year)
          growth_factors_cit[, scenarios := paste0("t", seq_len(.N) - 1L)]
        }
        
        scenario_years_cit <- growth_factors_cit$Year
        
        cat(
          "Growth-factor years:",
          paste(scenario_years_cit, collapse = ", "),
          "\n"
        )
        cat(
          "Growth-factor columns:",
          paste(names(growth_factors_cit), collapse = ", "),
          "\n"
        )
        
        
        

        'CD-ANNUAL DECLARATION AND PAYMENT FORM FOR CORPORATE INCOME TAX'
       
        # Step1 CD - Annual Declaration and Payment Form for Corporate Income Tax ----
        
        csv_folder_cd <- paste0(path1, "/Data/CIT/Data_raw/")
        setwd(path4)
        
        csv_files_cd <- list.files(
          path        = csv_folder_cd,
          pattern     = "\\.csv$",
          full.names  = TRUE,
          ignore.case = TRUE
        )
        
        if (length(csv_files_cd) == 0) {
          stop("No CSV files found in: ", csv_folder_cd)
        }
        
        process_cit_file <- function(file_path) {
          
          cit_raw_cd <- read_csv(
            file_path,
            locale         = locale(encoding = "ISO-8859-1"),
            show_col_types = FALSE
          ) |> as.data.table()
          
          cit_raw_cd <- cit_raw_cd %>%
            select(-c(
              "ETL_INSERTED_AT", "ETL_UPDATED_AT", "ENCRYPTED_AT",
              "VERSION_NO", "FORM_NO", "FORM_DESC", "TPER_MONTH", "TPER_DUE_DATE",
              "RECEPTION_DATE", "REASS_CREATION_DATE", "FISCAL_NO",
              "EMRI", "REGJIONI",
              "DATA_FILLIMIT",
              "SHTETESIA", "AKTI_PASIV_MBYLLUR"
            ))
          
          # FIX: D8 and D9 are empty/unused 

          
          cit_raw_cd <- cit_raw_cd %>%
            dplyr::rename(
              "id_n"                           = "TAX_PAYER_NO",
              "Year"                           = "TPER_YEAR",
              "city"                           = "QYTETI",
              "TaxpayerType"                   = "LLOJI_KOMPAN",
              "Activity"                       = "AKTIVITETI",
              "SectorDescription"              = "PERSHKRIMI_SEKTORIT",
              "entactivitycode"                = "ENT_ACTIVITY_CODE",
              "size"                           = "MADHESIA",
              "sector"                         = "SEKTORI",
              # D8 and D9 empty - not mapped, dropped below
              "netprofit_fd"                   = "D10",  # form box [10] net profit from FD
              "forsourceincsch_a"              = "D11",  # form box [11] foreign source income
              "recbaddebts_sch_b"              = "D12",  # form box [12] recovery of bad debts
              "capgain_sch_c"                  = "D13",  # form box [13] capital gains
              "div_sch_d"                      = "D14",  # form box [14] dividends
              "otherincgain_sch_e"             = "D15",  # form box [15] other income/gains
              "totadjinc_l11_l15"              = "D16",  # form box [16] total adjustment to income
              "profitloss_afteradjinc"         = "D17",  # form box [17] profit after income adj
              "disallowedexp_sch_f"            = "D18",  # form box [18] disallowed expenses
              "repcosts_sch_g"                 = "D19",  # form box [19] representation costs
              "reservefunds_sch_h"             = "D20",  # form box [20] reserve funds
              "paymrelper_sch_i"               = "D21",  # form box [21] payments to related persons
              "amort_sch_j"                    = "D22",  # form box [22] amortization
              "dep_sch_k"                      = "D23",  # form box [23] depreciation
              "specallownewassets_sch_l"       = "D24",  # form box [24] special allowance new assets
              "caploss_sch_c"                  = "D25",  # form box [25] capital loss
              "otherexp_sch_m"                 = "D26",  # form box [26] other expenses
              "totadjexp_l18_l26"              = "D27",  # form box [27] total adjustment to expenses
              "proflossaftadjexp_l17_l27"      = "D28",  # form box [28] gross income
              "charitycontrib_box28"           = "D29",  # form box [29] charitable contributions
              "losscarryforward"               = "D30",  # form box [30] transferred losses
              "add_l29_l30"                    = "D31",  # form box [31] sum boxes 29+30
              "adjprofit_l28_l31"              = "D32",  # form box [32] adjusted profit (taxable base)
              "corpinctax_l32"                 = "D33",  # form box [33] CIT non-insurance (D32*10%)
              "grossprem"                      = "D34",  # form box [34] gross premiums (insurance)
              "corpinctax_insurcomp"           = "D35",  # form box [35] CIT insurance companies
              "corpinctax_total"               = "D36",  # form box [36] total CIT (D33+D35)
              "forstattaxcredit_sch_o"         = "D37",  # form box [37] foreign tax credit
              "taxwithheld_sch_p"              = "D38",  # form box [38] WHT on interest/royalties/rent
              "totcredits_l37_l38"             = "D39",  # form box [39] total credits
              "l36_minus_l39"                  = "D40",  # form box [40] box 36 minus box 39
              "installmentspaid_sch_q"         = "D41",  # form box [41] paid installments
              "taxobl_befdisc_box36"           = "D42",  # form box [42] tax liability before discounts
              "sportspodisc_max30pct_box42"    = "D43",  # form box [43] sports sponsorship discount
              "culyouthspodisc_max20pct_box42" = "D44",  # form box [44] culture/youth sponsorship
              "totalspodisc_max30pct_box42"    = "D45",  # form box [45] total sponsorship discounts
              "taxpaywithform_l36_l39_l41_l45" = "D46", # form box [46] tax to pay with form
              "amountdue_box43"                = "D47",  # form box [47] obligation
              "taxpaid"                        = "D48"   # form box [48] refund/paid
            ) %>%
            dplyr::select(-c(
              D8, D9,                                    # empty ATK columns - dropped
              D49, D50, D51, D52, D53,
              D54, D55, D56, D57, D58, D59, D60,
              D61, D62, D63, D64, D65, D66, D67,
              D68, D69, D70, D71, D72, D73, D74,
              D75, D76, D77, D78
            ))
          
          cit_raw_cd <- cit_raw_cd %>%
            dplyr::mutate(source_file = basename(file_path))
          
          return(cit_raw_cd)
        }
        
        dt_list <- lapply(csv_files_cd, process_cit_file)
        
        cit_raw_cd <- dplyr::bind_rows(dt_list) %>%
          data.table()
        
        
        # Step2 Derive NACE 2-digit code ----
        
        cit_raw_cd$nace <- ifelse(
          nchar(cit_raw_cd$entactivitycode) == 4,
          substr(cit_raw_cd$entactivitycode, 1, 2),
          "NotAvailable"
        )
        
        cit_raw_cd <- left_join(cit_raw_cd, NACE_SUT_table_cit, by = c("nace" = "nace"))
        cit_raw_cd <- left_join(cit_raw_cd, TypeOfCompanies_CIT, by = c("TaxpayerType" = "name_al"))
        
        
        # Step3 Impute NA and 0 Year with dominant year ----
        
        dominant_year <- cit_raw_cd[
          !is.na(Year) & Year > 0,
          .N,
          by = Year
        ][
          which.max(N),
          Year
        ]
        
        cit_raw_cd[
          is.na(Year) | Year == 0,
          Year := dominant_year
        ]
        
        cit_raw <- cit_raw_cd
        
        
        # Step4 Reconciliation check after corrected mapping ----
        
        cat("\n=== MAPPING RECONCILIATION CHECK ===\n")
        cat("adjprofit_l28_l31 * 10%     :",
            round(sum(cit_raw$adjprofit_l28_l31[cit_raw$adjprofit_l28_l31 > 0],
                      na.rm = TRUE) * 0.10 / 1e6, 2), "EUR mil\n")
        cat("corpinctax_l32              :",
            round(sum(cit_raw$corpinctax_l32,           na.rm = TRUE) / 1e6, 2), "EUR mil\n")
        cat("taxobl_befdisc_box36        :",
            round(sum(cit_raw$taxobl_befdisc_box36,     na.rm = TRUE) / 1e6, 2), "EUR mil\n")
        cat("totalspodisc_max30pct_box42 :",
            round(sum(cit_raw$totalspodisc_max30pct_box42, na.rm = TRUE) / 1e6, 2), "EUR mil\n")
        cat("taxobl - totalspodisc       :",
            round(sum(cit_raw$taxobl_befdisc_box36 -
                        cit_raw$totalspodisc_max30pct_box42, na.rm = TRUE) / 1e6, 2), "EUR mil\n")
        cat("Official cash collected     : 219.48 EUR mil\n")
        
        


# V.VAT -------------------------------------------------------------------

        path5 <- paste0(path1, "/Scripts/VAT/")
        setwd(path5)
        getwd()
        

      # 1. Preparation of SUTs AND VAT matrix -------------------------------------------------------

        
 
        
        source("DataPrep_SUTs.R")
        
        

        

# 2.Preparation of data for VAT model ---------------------------------------

        
        
        
        # Step1 HELPER: FIND YEAR-VERSIONED FILES ----
        
        find_versioned_file <- function(dir, prefix, year, ext = "xlsx") {
          
          # Matches files like PREFIX_2024.xlsx and captures the 4-digit year
          pattern <- paste0("^", prefix, "_([0-9]{4})\\.", ext, "$")
          
          files <- list.files(
            path = dir,
            pattern = pattern,
            full.names = TRUE
          )
          
          if (length(files) == 0) {
            stop(
              paste0("No ", prefix, "_YYYY.", ext, " file was found in ", dir, ".")
            )
          }
          
          years <- suppressWarnings(
            as.integer(sub(pattern, "\\1", basename(files)))
          )
          
          match_idx <- which(years == year)
          
          if (length(match_idx) == 0) {
            stop(
              paste0(
                "No ", prefix, "_", year, ".", ext, " file was found in ", dir,
                ". Available years: ", paste(sort(unique(years)), collapse = ", "), "."
              )
            )
          }
          
          files[[match_idx[1]]]
        }
        
        # Step2 DETECT ACTIVE SUT FILE AND BASE YEAR ----
        
        simulation_data_path <- "Data/VAT/Data_raw"
        
        simulation_files <- list.files(
          path = simulation_data_path,
          pattern = "^SUTs_extrapolated_[0-9]{4}\\.xlsx$",
          full.names = TRUE
        )
        
        if (length(simulation_files) == 0) {
          stop(
            paste0(
              "No SUTs_extrapolated_YYYY.xlsx file was found in ",
              simulation_data_path,
              "."
            )
          )
        }
        
        simulation_years <- suppressWarnings(
          as.integer(
            sub(
              "^SUTs_extrapolated_([0-9]{4})\\.xlsx$",
              "\\1",
              basename(simulation_files)
            )
          )
        )
        
        if (any(is.na(simulation_years))) {
          stop("Could not extract the year from one or more SUTs_extrapolated_YYYY.xlsx files.")
        }
        
        base_year_VAT  <- max(simulation_years)
        SimulationYear <- base_year_VAT
        
        simulation_file <- simulation_files[simulation_years == base_year_VAT][[1]]
        
        cat("\nDetected SUT file:", basename(simulation_file), "\n")
        cat("Detected base year:", base_year_VAT, "\n")
        
        # Step3 DETECT VAT INPUT AND PARAMETER FILES FOR BASE YEAR ----
        
        vat_input_file  <- find_versioned_file(simulation_data_path, "VAT_Simulation_Input", year = base_year_VAT)
        vat_params_file <- find_versioned_file(simulation_data_path, "VAT_Parameters",       year = base_year_VAT)
        
        cat("Detected VAT simulation input file:", basename(vat_input_file), "\n")
        cat("Detected VAT parameters file:", basename(vat_params_file), "\n")
        
        # Step4 IMPORT VAT PARAMETERS AND SIMULATION INPUTS ----
        
        CPA_TAXABLE_PROPORTIONS_BU <- readxl::read_excel(
          vat_params_file
        )
        
        growfactors_vat <- readxl::read_excel(
          vat_input_file,
          sheet = "growth_vat"
        )
        
        nominal_gdp_vat <- readxl::read_excel(
          vat_input_file,
          sheet = "macro_fiscal_indicators"
        )
        
        # Step5 ACTIVE FORECAST YEARS AND GROWTH FACTORS ----
        
        if ("Year" %in% names(growfactors_vat) && !"year" %in% names(growfactors_vat)) {
          names(growfactors_vat)[names(growfactors_vat) == "Year"] <- "year"
        }
        
        if ("grow_factor" %in% names(growfactors_vat) && !"growth_factor" %in% names(growfactors_vat)) {
          names(growfactors_vat)[names(growfactors_vat) == "grow_factor"] <- "growth_factor"
        }
        
        if (!all(c("year", "growth_factor") %in% names(growfactors_vat))) {
          stop(
            paste0(
              "growfactors_vat must contain year/growth_factor. Current columns: ",
              paste(names(growfactors_vat), collapse = ", ")
            )
          )
        }
        
        growfactors_vat$year          <- suppressWarnings(as.integer(growfactors_vat$year))
        growfactors_vat$growth_factor <- suppressWarnings(as.numeric(growfactors_vat$growth_factor))
        
        growfactors_vat <- growfactors_vat |>
          dplyr::filter(!is.na(year), year >= base_year_VAT) |>
          dplyr::arrange(year) |>
          dplyr::mutate(
            growth_factor = dplyr::if_else(year == base_year_VAT, 1, growth_factor)
          )
        
        if (nrow(growfactors_vat) == 0 || !base_year_VAT %in% growfactors_vat$year) {
          stop(
            paste0("No VAT growth factors are available from detected base year ", base_year_VAT, ".")
          )
        }
        
        cat("Active VAT forecast years:", paste(growfactors_vat$year, collapse = ", "), "\n")
        
        # Step6 IMPUTED RENTS FOR BASE YEAR ----
        
        imputed_rents <- readxl::read_excel(
          vat_input_file,
          sheet = "imputed_rents"
        ) |>
          dplyr::filter(year == base_year_VAT) |>
          dplyr::pull(imputed_rents)
        
        if (length(imputed_rents) != 1) {
          stop(
            paste0(
              "Expected exactly one imputed_rents value for base_year_VAT = ",
              base_year_VAT,
              ", but found ",
              length(imputed_rents),
              "."
            )
          )
        }
        
        imputed_rents <- as.numeric(imputed_rents)
        
        
        
        # 4. Remove stale year-specific inputs ----
        
        year_specific_inputs <- grep(
          "^(VAT|sut_supply|sut_use)_[0-9]{4}$",
          ls(envir = .GlobalEnv),
          value = TRUE
        )
        
        if (length(year_specific_inputs) > 0) {
          rm(
            list = year_specific_inputs,
            envir = .GlobalEnv
          )
        }
        
        stale_vat_results <- intersect(
          c(
            "forecast_combined_agg",
            "forecast_combined_agg_tbl_wide",
            "forecast_combined_cpa_selected",
            "forecast_combined_te_selected",
            "forecast_TE_tbl",
            "forecast_TE_tbl_combined",
            "MainResultsVATFinal_BU",
            "MainResultsVATFinal_SIM"
          ),
          ls(envir = .GlobalEnv)
        )
        
        if (length(stale_vat_results) > 0) {
          rm(
            list = stale_vat_results,
            envir = .GlobalEnv
          )
        }
        
        
        # 5. Import active Supply, Use and VAT ----
        
        required_sheets <- c(
          "Supply",
          "Use",
          "VAT"
        )
        
        available_sheets <- readxl::excel_sheets(
          simulation_file
        )
        
        missing_sheets <- setdiff(
          required_sheets,
          available_sheets
        )
        
        if (length(missing_sheets) > 0) {
          stop(
            paste0(
              basename(simulation_file),
              " is missing sheet(s): ",
              paste(missing_sheets, collapse = ", ")
            )
          )
        }
        
        SUPPLY_ACTIVE <- tibble::as_tibble(
          readxl::read_excel(
            simulation_file,
            sheet = "Supply",
            .name_repair = "minimal"
          )
        )
        
        USE_ACTIVE <- tibble::as_tibble(
          readxl::read_excel(
            simulation_file,
            sheet = "Use",
            .name_repair = "minimal"
          )
        )
        
        USE_VAT_raw <- tibble::as_tibble(
          readxl::read_excel(
            simulation_file,
            sheet = "VAT",
            .name_repair = "minimal"
          )
        )
        
        
        # 6. Normalize active industry columns ----
        
        local({
          
          product_codes <- LETTERS[1:21]
          
          if (ncol(SUPPLY_ACTIVE) < 23) {
            stop("SUPPLY_ACTIVE does not contain the expected 21 A-U industry columns.")
          }
          
          if (ncol(USE_ACTIVE) < 23) {
            stop("USE_ACTIVE does not contain the expected 21 A-U industry columns.")
          }
          
          supply_industry_cols <- names(SUPPLY_ACTIVE)[3:23]
          use_industry_cols <- names(USE_ACTIVE)[3:23]
          
          canonical_industry_cols <- stringr::str_replace(
            stringr::str_squish(use_industry_cols),
            "^([A-U])\\s*[-–—]\\s*",
            "\\1-"
          )
          
          supply_industry_codes <- stringr::str_extract(
            stringr::str_squish(supply_industry_cols),
            "^[A-U](?=\\s*[-–—])"
          )
          
          use_industry_codes <- stringr::str_extract(
            stringr::str_squish(use_industry_cols),
            "^[A-U](?=\\s*[-–—])"
          )
          
          if (!identical(as.character(supply_industry_codes), product_codes)) {
            stop(
              paste0(
                "Cannot identify Supply industry columns A-U. Current columns 3:23: ",
                paste(supply_industry_cols, collapse = " | ")
              )
            )
          }
          
          if (!identical(as.character(use_industry_codes), product_codes)) {
            stop(
              paste0(
                "Cannot identify Use industry columns A-U. Current columns 3:23: ",
                paste(use_industry_cols, collapse = " | ")
              )
            )
          }
          
          if (!identical(as.character(supply_industry_codes), as.character(use_industry_codes))) {
            stop("Supply and Use A-U industry columns are not aligned.")
          }
          
          names(SUPPLY_ACTIVE)[3:23] <- canonical_industry_cols
          names(USE_ACTIVE)[3:23] <- canonical_industry_cols
          supply_industry_cols <- canonical_industry_cols
          use_industry_cols <- canonical_industry_cols
          
          vat_names <- names(USE_VAT_raw)
          vat_name_codes <- stringr::str_extract(
            stringr::str_squish(vat_names),
            "^[A-U](?=\\s*[-–—])"
          )
          
          vat_positions <- vapply(
            product_codes,
            function(code) {
              hit <- which(vat_name_codes == code)
              if (length(hit) == 1) hit else NA_integer_
            },
            integer(1)
          )
          
          if (all(!is.na(vat_positions))) {
            names(USE_VAT_raw)[vat_positions] <- use_industry_cols
          } else {
            
            if (ncol(USE_VAT_raw) < 24) {
              stop(
                paste0(
                  "Cannot identify VAT industry columns A-U. VAT sheet has only ",
                  ncol(USE_VAT_raw),
                  " columns."
                )
              )
            }
            
            first_three <- stringr::str_to_lower(
              stringr::str_squish(
                names(USE_VAT_raw)[1:3]
              )
            )
            
            if (!identical(first_three, c("code", "description", "etr"))) {
              stop(
                paste0(
                  "Cannot identify VAT industry columns A-U. Expected the VAT sheet to start with Code, Description, ETR. Current first columns: ",
                  paste(names(USE_VAT_raw)[1:3], collapse = " | ")
                )
              )
            }
            
            # The exported VAT matrix has Code, Description, ETR first, followed by A-U.
            # Align those 21 VAT columns to the active Use-table industry names.
            names(USE_VAT_raw)[4:24] <- use_industry_cols
          }
          
          if (!all(use_industry_cols %in% names(USE_VAT_raw))) {
            stop(
              paste0(
                "VAT industry columns could not be aligned to the active Use table. Missing: ",
                paste(setdiff(use_industry_cols, names(USE_VAT_raw)), collapse = ", ")
              )
            )
          }
          
          if (!all(c("Code", "Description") %in% names(USE_VAT_raw))) {
            stop("USE_VAT_raw must contain Code and Description.")
          }
          
          USE_VAT_raw$Code <- stringr::str_to_upper(
            stringr::str_squish(
              as.character(USE_VAT_raw$Code)
            )
          )
          
          USE_VAT_raw <- USE_VAT_raw[
            match(product_codes, USE_VAT_raw$Code),
            ,
            drop = FALSE
          ]
          
          if (!identical(as.character(USE_VAT_raw$Code), product_codes)) {
            stop("USE_VAT_raw must contain product codes A-U.")
          }
          
          assign("USE_VAT_raw", tibble::as_tibble(USE_VAT_raw), envir = .GlobalEnv)
          
          cat("VAT industry columns aligned to active Use table: A-U\n")
        })
        
        
        # 7. Required inputs ----
        
        local({
          
          required_objects <- c(
            "SUPPLY_ACTIVE",
            "USE_ACTIVE",
            "USE_VAT_raw",
            "CPA_TAXABLE_PROPORTIONS_BU",
            "base_year_VAT"
          )
          
          missing_objects <- setdiff(
            required_objects,
            ls(envir = .GlobalEnv)
          )
          
          if (length(missing_objects) > 0) {
            stop(
              paste0(
                "Missing required object(s): ",
                paste(missing_objects, collapse = ", ")
              )
            )
          }
          
        })
        
        
        # 8. Construction parameters ----
        
        vat_rate_on_residential_construction <- 0.18
        
        RC_prc_of_Constructions_and_construction_works <- 0.30
        
        
        # 9. Validate Supply table ----
        
        if (
          !all(
            c(
              "Code",
              "Description"
            ) %in% names(SUPPLY_ACTIVE)
          )
        ) {
          stop(
            "SUPPLY_ACTIVE must contain Code and Description."
          )
        }
        
        if (nrow(SUPPLY_ACTIVE) != 21) {
          stop(
            "SUPPLY_ACTIVE must contain exactly 21 product rows A-U."
          )
        }
        
        SUPPLY_ACTIVE$Code <- stringr::str_to_upper(
          stringr::str_squish(
            as.character(
              SUPPLY_ACTIVE$Code
            )
          )
        )
        
        SUPPLY_ACTIVE <- SUPPLY_ACTIVE[
          match(
            LETTERS[1:21],
            SUPPLY_ACTIVE$Code
          ),
          ,
          drop = FALSE
        ]
        
        if (
          !identical(
            as.character(
              SUPPLY_ACTIVE$Code
            ),
            LETTERS[1:21]
          )
        ) {
          stop(
            "SUPPLY_ACTIVE must contain product codes A-U."
          )
        }
        
        
        # 10. Validate Use table ----
        
        if (
          !all(
            c(
              "Code",
              "Description"
            ) %in% names(USE_ACTIVE)
          )
        ) {
          stop(
            "USE_ACTIVE must contain Code and Description."
          )
        }
        
        if (nrow(USE_ACTIVE) != 21) {
          stop(
            "USE_ACTIVE must contain exactly 21 product rows A-U."
          )
        }
        
        USE_ACTIVE$Code <- stringr::str_to_upper(
          stringr::str_squish(
            as.character(
              USE_ACTIVE$Code
            )
          )
        )
        
        USE_ACTIVE <- USE_ACTIVE[
          match(
            LETTERS[1:21],
            USE_ACTIVE$Code
          ),
          ,
          drop = FALSE
        ]
        
        if (
          !identical(
            as.character(
              USE_ACTIVE$Code
            ),
            LETTERS[1:21]
          )
        ) {
          stop(
            "USE_ACTIVE must contain product codes A-U."
          )
        }
        
        
        # 11. Validate VAT matrix ----
        
        if (
          is.null(
            USE_VAT_raw
          )
        ) {
          stop(
            "USE_VAT_raw is NULL."
          )
        }
        
        if (
          is.null(
            dim(
              USE_VAT_raw
            )
          )
        ) {
          stop(
            "USE_VAT_raw must be a matrix or data frame."
          )
        }
        
        if (nrow(USE_VAT_raw) != 21) {
          stop(
            paste0(
              "USE_VAT_raw has ",
              nrow(USE_VAT_raw),
              " rows. Expected 21 rows."
            )
          )
        }
        
        
        # 12. Validate policy table ----
        
        local({
          
          required_policy_columns <- c(
            "PRODUCT_INDUSTRY_CODE",
            "PRODUCT_INDUSTRY_NAME",
            "Current_Policy_Exempt",
            "Current_Policy_Reduced_Rate",
            "Current_Policy_Fully_Taxable",
            "PreferentialVATRate_1",
            "StandardVATRate",
            "ProportionExempted",
            "ProportionPreferentialRate1",
            "ProportionStandardRate"
          )
          
          missing_policy_columns <- setdiff(
            required_policy_columns,
            names(CPA_TAXABLE_PROPORTIONS_BU)
          )
          
          if (length(missing_policy_columns) > 0) {
            stop(
              paste0(
                "CPA_TAXABLE_PROPORTIONS_BU is missing column(s): ",
                paste(
                  missing_policy_columns,
                  collapse = ", "
                )
              )
            )
          }
          
        })
        
        
        # 13. Order policy table ----
        
        CPA_TAXABLE_PROPORTIONS_BU <- tibble::as_tibble(
          CPA_TAXABLE_PROPORTIONS_BU
        )
        
        CPA_TAXABLE_PROPORTIONS_BU$PRODUCT_INDUSTRY_CODE <-
          stringr::str_to_upper(
            stringr::str_squish(
              as.character(
                CPA_TAXABLE_PROPORTIONS_BU$PRODUCT_INDUSTRY_CODE
              )
            )
          )
        
        CPA_TAXABLE_PROPORTIONS_BU <-
          CPA_TAXABLE_PROPORTIONS_BU[
            match(
              LETTERS[1:21],
              CPA_TAXABLE_PROPORTIONS_BU$PRODUCT_INDUSTRY_CODE
            ),
            ,
            drop = FALSE
          ]
        
        if (
          !identical(
            as.character(
              CPA_TAXABLE_PROPORTIONS_BU$PRODUCT_INDUSTRY_CODE
            ),
            LETTERS[1:21]
          )
        ) {
          stop(
            "CPA_TAXABLE_PROPORTIONS_BU must contain product codes A-U."
          )
        }
        
        
        # 14. Numeric policy variables ----
        
        CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Exempt <- as.numeric(CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Exempt)
        
        CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Reduced_Rate <- as.numeric(CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Reduced_Rate)
        
        CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Fully_Taxable <- as.numeric(CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Fully_Taxable)
        
        CPA_TAXABLE_PROPORTIONS_BU$PreferentialVATRate_1 <- as.numeric(CPA_TAXABLE_PROPORTIONS_BU$PreferentialVATRate_1)
        
        CPA_TAXABLE_PROPORTIONS_BU$StandardVATRate <- as.numeric(CPA_TAXABLE_PROPORTIONS_BU$StandardVATRate)
        
        CPA_TAXABLE_PROPORTIONS_BU$ProportionExempted <- as.numeric(CPA_TAXABLE_PROPORTIONS_BU$ProportionExempted)
        
        CPA_TAXABLE_PROPORTIONS_BU$ProportionPreferentialRate1 <- as.numeric(CPA_TAXABLE_PROPORTIONS_BU$ProportionPreferentialRate1)
        
        CPA_TAXABLE_PROPORTIONS_BU$ProportionStandardRate <- as.numeric(CPA_TAXABLE_PROPORTIONS_BU$ProportionStandardRate)
        
        
        # 15. Current policy proportions ----
        
        local({
          
          current_policy_sum <-
            CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Exempt +
            CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Reduced_Rate +
            CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Fully_Taxable
          
          if (
            any(
              abs(
                current_policy_sum - 1
              ) > 1e-8
            )
          ) {
            stop(
              "Current policy proportions must sum to 1."
            )
          }
          
        })
        
        
        # 16. Original simulated policy proportions ----
        
        CPA_TAXABLE_PROPORTIONS_BU$Simulated_Policy_Exempt <- ifelse(
          is.na(
            CPA_TAXABLE_PROPORTIONS_BU$ProportionExempted
          ),
          CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Exempt,
          CPA_TAXABLE_PROPORTIONS_BU$ProportionExempted
        )
        
        CPA_TAXABLE_PROPORTIONS_BU$Simulated_Policy_Reduced_Rate <- ifelse(
          is.na(
            CPA_TAXABLE_PROPORTIONS_BU$ProportionPreferentialRate1
          ),
          CPA_TAXABLE_PROPORTIONS_BU$Current_Policy_Reduced_Rate,
          CPA_TAXABLE_PROPORTIONS_BU$ProportionPreferentialRate1
        )
        
        CPA_TAXABLE_PROPORTIONS_BU$Simulated_Policy_Fully_Taxable <-
          1 -
          CPA_TAXABLE_PROPORTIONS_BU$Simulated_Policy_Exempt -
          CPA_TAXABLE_PROPORTIONS_BU$Simulated_Policy_Reduced_Rate
        
        
        # 17. Validate simulated policy ----
        
        local({
          
          simulated_policy_sum <-
            CPA_TAXABLE_PROPORTIONS_BU$Simulated_Policy_Exempt +
            CPA_TAXABLE_PROPORTIONS_BU$Simulated_Policy_Reduced_Rate +
            CPA_TAXABLE_PROPORTIONS_BU$Simulated_Policy_Fully_Taxable
          
          if (
            any(
              abs(
                simulated_policy_sum - 1
              ) > 1e-8
            )
          ) {
            stop(
              "Simulated policy proportions must sum to 1."
            )
          }
          
        })
        
        
        # 18. Validate construction parameters ----
        
        if (
          !is.finite(
            vat_rate_on_residential_construction
          ) ||
          vat_rate_on_residential_construction < 0 ||
          vat_rate_on_residential_construction > 1
        ) {
          stop(
            "vat_rate_on_residential_construction must be between 0 and 1."
          )
        }
        
        if (
          !is.finite(
            RC_prc_of_Constructions_and_construction_works
          ) ||
          RC_prc_of_Constructions_and_construction_works < 0 ||
          RC_prc_of_Constructions_and_construction_works > 1
        ) {
          stop(
            "RC_prc_of_Constructions_and_construction_works must be between 0 and 1."
          )
        }
        
        
        # 19. Completion ----
        
        cat(
          "\nVAT-Init.R completed for base year",
          base_year_VAT,
          "\n"
        )
        
        cat(
          "Simulation file:",
          basename(simulation_file),
          "\n"
        )
        
        cat(
          "SUPPLY_ACTIVE:",
          nrow(SUPPLY_ACTIVE),
          "rows\n"
        )
        
        cat(
          "USE_ACTIVE:",
          nrow(USE_ACTIVE),
          "rows\n"
        )
        
        cat(
          "USE_VAT_raw:",
          nrow(USE_VAT_raw),
          "rows x",
          ncol(USE_VAT_raw),
          "columns\n"
        )
        
        cat(
          "VAT matrix column names:\n"
        )
        
        print(
          names(
            USE_VAT_raw
          )
        )
        
        
        
        
        
        
# Save objects in enviroment----------------------------------------------------------------------
        
        
        rm("pit_raw_wm_list","pit_raw_wr_list","pit_raw_is_list","pit_raw_qs_list","cit_raw_cd")
        
        setwd(path1)
        getwd()
        
        gc(TRUE)
        
        
        save.image(file=".RData")
        # 
        

        