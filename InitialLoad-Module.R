'Install packages and importing of data
                                          '
'Step 1. Set your local path to the model directory'

rm(list = ls())
#path1<-" "# <--------Set your path here
path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/Kosovo-TE-Models"# <--------Set your path here

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


rm(list = ls())
path1<-"C:/Users/wb591157/OneDrive - WBG/Documents/Models/Kosovo-TE-Models"# <--------Set your path here
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
                #'Excise_Description'='Përshkrimi Akcizës',
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
      stringr::str_detect(vat_pct, "€"),
      vat_pct,
      excise_rate
    ),
    vat_pct = dplyr::if_else(
      stringr::str_detect(vat_pct, "€"),
      NA_character_,
      vat_pct
    )
  )


taric_data2024 <- taric_data2024 %>%
  dplyr::mutate(
    excise_rate = stringr::str_squish(as.character(excise_rate)),
    excise_rate = dplyr::na_if(excise_rate, "-"),
    excise_rate = dplyr::na_if(excise_rate, "–"),
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
    excise_rate = dplyr::na_if(excise_rate, "–"),
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


# Testing with original data

# test <- CustomsDuties_base %>%
#   #dplyr::filter(Treatment == "NonPreferential") %>%
#   dplyr::group_by(Year) %>%
#   dplyr::summarise(
#     Value = sum(Value, na.rm = TRUE),
#     Quantity = sum(Quantity, na.rm = TRUE),
#     Netweight = sum(Netweight, na.rm = TRUE),
#     CustomsRevenue = sum(CustomsRevenue, na.rm = TRUE),
#     ExciseRevenue = sum(ExciseRevenue, na.rm = TRUE),
#     VAT_Revenue = sum(VAT_Revenue, na.rm = TRUE),
#     .groups = "drop"
#   )
# 



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



CustomsDuties_TE_agg_HS



setwd(path1)
getwd()

gc(TRUE)


save.image(file=".RData") 



