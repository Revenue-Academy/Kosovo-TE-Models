' Data preparation,estimation of tax expenditures and preparation of data for charts
'
options(scipen = 999)
# Define fonts
t <- list(
  family = "Arial",
  size = 12
)


t_8 <- list(
  family = "Arial",
  size = 8#,
  #face="bold"
)

t_10 <- list(
  family = "Arial",
  size = 10,
  face="bold"
)

# Define fonts
t_11 <- list(
  family = "Arial",
  size = 11,
  face="bold"
)


t_16 <- list(
  family = "Arial",
  size = 16,
  face="bold"
)


# Define custom colors
colors <- c('#1f77b4', '#ff7f0e')

# Original colors
colr <- c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#17becf', '#57a9e2',
          '#ffb574', '#5fd35f', '#7f7f7f', '#e77c7c', '#c6aedc', '#bcbd22',
          '#bc8b81', '#f4cce8', '#b2b2b2', '#9467bd', '#e2e362', '#e377c2',
          '#5fe0ed', '#8c564b', '#103d5d', '#a74e00')

# Generate additional unique colors
additional_colors <- colorRampPalette(c("#00FF00", "#FF0000"))(102 - length(colr))

# Combine the original and additional colors
colr <- c(colr, additional_colors) 



library(data.table)
library(plotly)
library(readxl)
library(dplyr)

Benchmark_Customs_Rate<-0.1

Import_raw_monthly<-read_excel("~/Models/Kosovo_Models/Data/ImportData/Open_DATA_Import Janar-Dhjetor 2023.xlsx")



options(warn = -1)

#suppressMessages({
  
# I. Estimation of tax expenditures for customs duties ----------------------------------------------------------
        # 1.Processing of data ------------------------------------------------------

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
                                                                CustomsDuties_Benchmark=Value*Benchmark_Customs_Rate,
                                                                CustomsDuties_TE=round(CustomsDuties_Benchmark-CustomsRevenue,1))
                    
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
                                       VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE),
                                       CustomsDuties_Benchmark=sum(CustomsDuties_Benchmark,na.rm = TRUE),
                                       CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                    
                    
                    CustomsDuties_TE_agg_countries$HS_code<-NULL
                    CustomsDuties_TE_agg_countries$HS_code_s<-NULL
                    
                    
                    CustomsDuties_TE_agg_countries <- CustomsDuties_TE_agg_countries %>%
                      dplyr::select(iso3c,CustomsDuties_TE)%>%
                      dplyr::group_by(iso3c) %>%
                      dplyr::summarise(across(where(is.numeric), sum))
                      

            # 3.2 Harmonized System-HS  --------------------------------------------------------------------

                    
                    # 2a. Adding TARIC rates --------------------------------------------------
                    
                    
         
                  
                    
                    
                  # OLD
                  # CustomsDuties_TE_agg_HS<-CustomsDuties_base%>%
                  #   dplyr::group_by(HS_code,Treatment,HS_code_s)%>%
                  #   dplyr::filter(Treatment=="NonPreferential")%>%
                  #   dplyr::summarise(Value=sum(Value,na.rm = TRUE),
                  #                    Quantity=sum(Quantity,na.rm = TRUE),
                  #                    Netweight=sum(Netweight,na.rm = TRUE),
                  #                    CustomsRevenue=sum(CustomsRevenue,na.rm = TRUE),
                  #                    ExciseRevenue=sum(ExciseRevenue,na.rm = TRUE),
                  #                    VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE),
                  #                    CustomsDuties_Benchmark=sum(CustomsDuties_Benchmark,na.rm = TRUE),
                  #                    CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))
                  
                  # NEW
                    
                    CustomsDuties_TE_agg_HS<-CustomsDuties_base%>%
                      dplyr::group_by(HS_code,Treatment,FreeTradeAgreements,HS_code_s)%>%
                     #dplyr::filter(Treatment=="NonPreferential")%>%
                      #dplyr::filter(Treatment=="Preferential")%>%
                      dplyr::summarise(Value=sum(Value,na.rm = TRUE),
                                       Quantity=sum(Quantity,na.rm = TRUE),
                                       Netweight=sum(Netweight,na.rm = TRUE),
                                       CustomsRevenue=sum(CustomsRevenue,na.rm = TRUE),
                                       ExciseRevenue=sum(ExciseRevenue,na.rm = TRUE),
                                       VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE),
                                       CustomsDuties_Benchmark=sum(CustomsDuties_Benchmark,na.rm = TRUE),
                                       CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE)
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

                    
                    

# I. PREPARATION OF TAXABLE PROPORTION ------------------------------------

                    
                    CustomsDuties_TE_agg_HS_sub<-CustomsDuties_TE_agg_HS%>%
                                            select(HS_code,FreeTradeAgreements,Value,CustomsRevenue,CustomsRate_MFN,CustomsRate_TR,CustomsRate_CEFTA,CustomsRate_MSA)%>%
                                            dplyr::mutate( CalcCustomsRevenues = case_when(
                                                                                            FreeTradeAgreements == "NoFreeTradeAgreement" ~ Value * (CustomsRate_MFN / 100),
                                                                                            FreeTradeAgreements %in% c("EU27", "GBR") ~ Value * (CustomsRate_MSA / 100),
                                                                                            FreeTradeAgreements == "CEFTA" ~ Value * (CustomsRate_CEFTA / 100),
                                                                                            FreeTradeAgreements == "TR" ~ Value * (CustomsRate_TR / 100),
                                                                                            TRUE ~ 0  # Default case if no condition matches
                                                                                            ))%>% 
                                            dplyr::mutate( CalcCustomsRevenues_MFN=Value * (CustomsRate_MFN / 100),
                                                          Effective_Customs_rate=(round(CustomsRevenue/(Value),2)*100),
                                                          Diff=round(CalcCustomsRevenues-CustomsRevenue,1),
                                                          NonPreferential_share=0,
                                                          Exempted_share=0,
                                                          Preferential_share=0
                                                         
                                                          )

                    View(CustomsDuties_TE_agg_HS_sub)
                    
          # 1. No Free Trade Agreement ------------------------------------------------


                    NoFreeTradeAgreement_sub<-CustomsDuties_TE_agg_HS_sub%>%
                      filter(FreeTradeAgreements=="NoFreeTradeAgreement")
                    
                    
                    
                    # Assuming NoFreeTradeAgreement_sub is your dataset
                    
                    NoFreeTradeAgreement_sub <- NoFreeTradeAgreement_sub %>%
                                dplyr::mutate(
                                                    NonPreferential_share = case_when(
                                                      CustomsRate_MFN == Effective_Customs_rate ~ 100,  # New condition: if CustomsRate_MFN equals Effective_Customs_rate
                                                      Diff > 1 ~ (CustomsRevenue / (CustomsRate_MFN / 100)) / Value * 100,  # Existing condition
                                                      TRUE ~ 0  # Default case
                                                    ),
                                                    Exempted_share = 100 - NonPreferential_share,  # Calculate Exempted_share as the complement of NonPreferential_share
                                                    Preferential_share = case_when(
                                                      Exempted_share == 0 ~ 0,  # Placeholder for Preferential_share (if needed)
                                                      TRUE ~ 0  # Adjust as needed
                                              ))%>%
                                dplyr::mutate(CustomsTest=(Value*(NonPreferential_share/100))*(CustomsRate_MFN/100))%>%
                                dplyr::mutate(diff1=round(CustomsTest-CustomsRevenue,1))

                    
                    sum(NoFreeTradeAgreement_sub$CustomsTest,na.rm = TRUE) # 81297908
                    sum(NoFreeTradeAgreement_sub$CustomsRevenue) # 81122453
                    
                    # Diff in pct : ((81297908/81122453)*100)-100  [1] 0.22 PCT
                    
                    
                    View(NoFreeTradeAgreement_sub)
                    

# 2. CEFTA -------------------------------------------------------------

                    
                    FreeTradeAgreement_CEFTA<-CustomsDuties_TE_agg_HS_sub%>%
                      filter(FreeTradeAgreements=="CEFTA")%>%
                      mutate(Effective_Customs_rate=CustomsRevenue/Value)
                    
                             
                    
                    # Assuming FreeTradeAgreement_CEFTA is your dataset
                  
                    FreeTradeAgreement_CEFTA <- FreeTradeAgreement_CEFTA %>%
                      dplyr::mutate(
                        NonPreferential_share = case_when(
                          Effective_Customs_rate == 0 & CustomsRevenue == 0 ~ 0,  # NonPreferential_share is 0 when both rates and revenue are 0
                          CustomsRate_CEFTA == Effective_Customs_rate ~ 100,  # If the rates are equal, NonPreferential_share is 100
                          Effective_Customs_rate > 0 & CustomsRate_CEFTA > 0 ~ pmin((Effective_Customs_rate / CustomsRate_CEFTA) * 100, 100),  # Proportional based on rate ratio
                          Diff > 1 ~ pmin((Diff / Value) * 100, 100),  # If Diff > 1, calculate proportionally from Diff and Value
                          CustomsRevenue > 0 & Value > 0 ~ (CustomsRevenue / Value) * 100,  # Calculate NonPreferential_share as percentage of CustomsRevenue over Value
                          TRUE ~ 0  # Default case
                        ),
                        
                        Preferential_share = case_when(
                          Effective_Customs_rate == 0 & CustomsRevenue == 0 ~ 100,  # If both are 0, Preferential_share is 100
                          TRUE ~ 100 - NonPreferential_share  # Otherwise, Preferential_share complements NonPreferential_share
                        ),
                        
                        Exempted_share = 0  # Exempted_share should always be 0
                      ) %>%
                      dplyr::mutate(
                        CustomsTest = ((Value * (NonPreferential_share / 100)) * (CustomsRate_MFN / 100))*10  # Calculate CustomsTest based on NonPreferential_share
                      ) %>%
                      dplyr::mutate(
                        diff1 = round(CustomsTest - CustomsRevenue, 1)  # Calculate the difference
                      )
                    
                
                    # Check if the sum of NonPreferential_share, Exempted_share, and Preferential_share is valid
                    check_sums <- FreeTradeAgreement_CEFTA %>%
                      dplyr::mutate(sum_check = NonPreferential_share + Exempted_share + Preferential_share) %>%
                      dplyr::summarise(valid = all(sum_check == 100))
                    
                    print(check_sums)
                    
                    
                    sum(FreeTradeAgreement_CEFTA$CustomsTest,na.rm = TRUE) # 81297908
                    sum(FreeTradeAgreement_CEFTA$CustomsRevenue) # 81122453
                    
                    # Diff in pct : ((81297908/81122453)*100)-100  [1] 0.22 PCT
                    
                    View(FreeTradeAgreement_CEFTA)
                   
                  
                    
                    
                  #
                    
                    #customs_data_raw<-rbind(NoFreeTradeAgreement_sub,FreeTradeAgreement_CEFTA)
                    

                    #write.csv(customs_data_raw,"customs_data_raw.csv")
                    
# 3.TR -------------------------------------------------------------------

                    'do tuka ova ne e zavrseno i dava gresni rezultati !!!!!'
  
                    FreeTradeAgreement_TR<-CustomsDuties_TE_agg_HS_sub%>%
                      filter(FreeTradeAgreements=="TR")%>%
                      mutate(Effective_Customs_rate=(CustomsRevenue/Value)*100)
                    
                    
                    
                    # Assuming FreeTradeAgreement_TR is your dataset
                    
                  FreeTradeAgreement_TR <- FreeTradeAgreement_TR %>%
                      dplyr::mutate(
                        NonPreferential_share = case_when(
                          Effective_Customs_rate == 0 & CustomsRevenue == 0 ~ 0,  # NonPreferential_share is 0 when both rates and revenue are 0
                          CustomsRate_TR == Effective_Customs_rate ~ 100,  # If the rates are equal, NonPreferential_share is 100
                          Effective_Customs_rate > 0 & CustomsRate_TR > 0 ~ pmin((Effective_Customs_rate / CustomsRate_TR) * 100, 100),  # Proportional based on rate ratio
                          Diff > 1 ~ pmin((Diff / Value) * 100, 100),  # If Diff > 1, calculate proportionally from Diff and Value
                          CustomsRevenue > 0 & Value > 0 ~ (CustomsRevenue / Value) * 100,  # Calculate NonPreferential_share as percentage of CustomsRevenue over Value
                          TRUE ~ 0  # Default case
                        ),
                        
                        Preferential_share = case_when(
                          Effective_Customs_rate == 0 & CustomsRevenue == 0 ~ 100,  # If both are 0, Preferential_share is 100
                          CustomsRate_MFN == CustomsRate_TR ~ 100,  # New condition: If CustomsRate_MFN is equal to CustomsRate_TR, Preferential_share is 100
                          TRUE ~ 100 - NonPreferential_share  # Otherwise, Preferential_share complements NonPreferential_share
                        ),
                        
                        Exempted_share = 0  # Exempted_share should always be 0
                      ) %>%
                      dplyr::mutate(
                        CustomsTest1 = ((Value * (Preferential_share / 100)) * (CustomsRate_TR / 100)) * 10,
                        CustomsTest2 = ((Value * (NonPreferential_share / 100)) * (CustomsRate_MFN / 100))*10 ,
                        CustomsTest=CustomsTest1+CustomsTest2
                        # Calculation of CustomsTest based on Preferential_share
                      ) %>%
                      dplyr::mutate(
                        diff1 = round(CustomsTest - CustomsRevenue, 1)  # Calculate the difference
                      )
                    
                    # View the modified dataset
                    print(FreeTradeAgreement_TR)
                    
                    
                    
                    # Check if the sum of NonPreferential_share, Exempted_share, and Preferential_share is valid
                    check_sums <- FreeTradeAgreement_TR %>%
                      dplyr::mutate(sum_check = NonPreferential_share + Exempted_share + Preferential_share) %>%
                      dplyr::summarise(valid = all(sum_check == 100))
                    
                    print(check_sums)
                    
                    
                    sum(FreeTradeAgreement_TR$CustomsTest,na.rm = TRUE) # 81297908
                    sum(FreeTradeAgreement_TR$CustomsRevenue) # 81122453
                    
                    # Diff in pct : ((81297908/81122453)*100)-100  [1] 0.22 PCT
                    
                    View(FreeTradeAgreement_TR)
                    
                    
                    6796452
                    164070500
                    
                    ## NEW TEST
                    
                    View(FreeTradeAgreement_TR)
                    
                    
                    FreeTradeAgreement_TR <- FreeTradeAgreement_TR %>%
                      dplyr::mutate(
                        # calc_customs_revenue_pref= (Value*(CustomsRate_TR/100)),  # All import taxed with preferential rate
                        # diff_pref_non_pref=round(CustomsRevenue-calc_customs_revenue_pref,1), # Difference between calculated preferential tax revenues and CustomsRevenue
                        # calc_base_non_pref=round(diff_pref_non_pref/(CustomsRate_TR/100)), # 
                      # Identifying exempted share
                      Exempted_share = case_when(
                                                      CustomsRevenue == 0 & CustomsRate_MFN > 0 & CustomsRate_TR > 0~ 100,
                                                      TRUE ~ 100 #- Preferential_share
                                                        
                      ),
                      Preferential_share = case_when(
                                                      CustomsRevenue > 0 & CustomsRate_MFN == 0 & CustomsRate_TR == 0 ~ 100,
                        
                        TRUE ~ 100 - NonPreferential_share #- Exempted_share  # Otherwise, Preferential_share complements NonPreferential_share
                      ),
                      
                      )
                    View(FreeTradeAgreement_TR)
                    
                    
                    
                    
                    
                    # FreeTradeAgreement_TR <- FreeTradeAgreement_TR %>%
                    #   dplyr::mutate(
                    #                 Exempted_share = case_when(
                    #                   CustomsRevenue == 0 & CustomsRate_MFN > 0 & CustomsRate_TR > 0 ~ 100,
                    #                   TRUE ~ 0  # If not exempt, set it to 0
                    #                 ),
                    #                 Preferential_share = case_when(
                    #                   CustomsRate_TR == round(Effective_Customs_rate,0) ~ 100,
                    #                   TRUE ~ 0  # If not preferential, set it to 0
                    #                 ),
                    #                 NonPreferential_share = case_when(
                    #                   CustomsRate_MFN == round(Effective_Customs_rate,0) & CustomsRate_TR !=CustomsRate_MFN ~ 100,
                    #                   CustomsRate_MFN > round(Effective_Customs_rate,0) & CustomsRate_TR ==0  ~ 100,
                    #                   TRUE ~ 0  # If not preferential, set it to 0
                    #                 )
                    #               )
                    FreeTradeAgreement_TR <- FreeTradeAgreement_TR %>%
                                        dplyr::mutate(
                                                    Exempted_share = case_when(
                                                      CustomsRevenue == 0 & CustomsRate_MFN > 0 & CustomsRate_TR > 0 ~ 100,
                                                      CustomsRevenue == 0 & CustomsRate_MFN > 0 & CustomsRate_TR == 0 ~ 100,
                                                      TRUE ~ 0  # If not exempt, set it to 0
                                                    ),
                                                    NonPreferential_share = case_when(
                                                                          CustomsRevenue>0 & CustomsRate_MFN == round(Effective_Customs_rate, 0) & CustomsRate_TR != CustomsRate_MFN ~ 100,
                                                                          CustomsRate_MFN >= round(Effective_Customs_rate>0, 0) & CustomsRate_TR == 0 ~round(((Value-(CustomsRevenue / (CustomsRate_MFN / 100)))/Value)*100,1),
                                                                          TRUE ~ 100
                                                      ),
                                                     
                                                      Preferential_share = case_when(
                                                                        CustomsRevenue>0 & CustomsRate_TR == round(Effective_Customs_rate, 0) ~ 100,
                                                                        TRUE ~ 100 - (NonPreferential_share + Exempted_share) # If not preferential, set it to 0
                                                      ),
                                                    )
                                                    
                    
                    # Cross checking !!!
                    FreeTradeAgreement_TR <- FreeTradeAgreement_TR %>%
                      dplyr::mutate(
                        Preferential_share = 100 - NonPreferential_share,
                        cross_check=NonPreferential_share+Exempted_share+Preferential_share,
                        NonPreferential_share=if_else(cross_check == 200, 0, NonPreferential_share),
                        # Preferential_share = if_else(cross_check == 200, 0, Preferential_share),  # Use if_else for vectorized operation
                       # NonPreferential_share = if_else(cross_check == 200, 0, NonPreferential_share),
                        cross_check1=NonPreferential_share+Exempted_share+Preferential_share,
                      )
                    
                                    
                    
                    View(FreeTradeAgreement_TR)
         

                    total_non_zero_counts <- data.frame(
                      Exempted_share_count = sum(FreeTradeAgreement_TR$Exempted_share != 0, na.rm = TRUE),
                      Preferential_share_count = sum(FreeTradeAgreement_TR$Preferential_share != 0, na.rm = TRUE),
                      NonPreferential_share_count = sum(FreeTradeAgreement_TR$NonPreferential_share != 0, na.rm = TRUE)
                    )
                    
                    total_non_zero_counts
                    
                    # Calculate the sum of the counts in the row
                    total_sum <- sum(total_non_zero_counts)
                    
                    total_sum
                    
                    
                    #  2811
                    
                    write.csv(FreeTradeAgreement_TR,"FreeTradeAgreement_TR4.csv")
                    

# TESTING NEW VERSION -----------------------------------------------------

                    
                    # FreeTradeAgreement_TR <- FreeTradeAgreement_TR %>%
                    #   dplyr::mutate(
                    #     Exempted_share = case_when(
                    #       CustomsRevenue == 0 & CustomsRate_MFN > 0 & CustomsRate_TR > 0 ~ 100,
                    #       #CustomsRevenue == 0 & CustomsRate_MFN > 0 & CustomsRate_TR == 0 ~ 100,
                    #       TRUE ~ 0  # If not exempt, set it to 0
                    #         ),
                    #         NonPreferential_share = case_when(
                    #           CustomsRevenue>0 & CustomsRate_MFN == round(Effective_Customs_rate>0, 0) & CustomsRate_TR != CustomsRate_MFN ~ 100,
                    #           #CustomsRate_MFN >= round(Effective_Customs_rate>0, 0) & CustomsRate_TR == 0 ~round(((Value-(CustomsRevenue / (CustomsRate_MFN / 100)))/Value)*100,1),
                    #           TRUE ~ 100
                    #         ),
                    #         
                    #         Preferential_share = case_when(
                    #           CustomsRevenue>0 & CustomsRate_TR == round(Effective_Customs_rate, 0),
                    #          # Effective_Customs_rate=CustomsRate_TR,
                    #           #~ 100,
                    #           
                    #           TRUE ~ 100 #- (NonPreferential_share + Exempted_share) # If not preferential, set it to 0
                    #         ),
                    #       )
                    # 
                    
                    FreeTradeAgreement_TR <- FreeTradeAgreement_TR %>%
                      dplyr::mutate(
                        Exempted_share = case_when(
                          CustomsRevenue == 0 & CustomsRate_MFN > 0 & CustomsRate_TR > 0 ~ 100,
                          TRUE ~ 0  # If not exempt, set it to 0
                        ),
                        NonPreferential_share = case_when(
                          CustomsRevenue > 0 & Effective_Customs_rate > 0 & CustomsRate_MFN == ceiling(Effective_Customs_rate) ~ 100,
                          TRUE ~ NA_real_  # Let's set NA here to check what rows are problematic
                        ),
                        Preferential_share = case_when(
                          CustomsRevenue > 0 & Effective_Customs_rate > 0 & CustomsRate_TR == ceiling(Effective_Customs_rate) ~ 100,
                          TRUE ~ NA_real_  # Let's set NA here to check what rows are problematic
                        )
                      )
                    
                    
                    
                    
                    
                    # Cross checking !!!
                    FreeTradeAgreement_TR <- FreeTradeAgreement_TR %>%
                      dplyr::mutate(
                        Preferential_share = 100 - NonPreferential_share,
                        cross_check=NonPreferential_share+Exempted_share+Preferential_share,
                        #NonPreferential_share=if_else(cross_check == 200, 0, NonPreferential_share),
                        # Preferential_share = if_else(cross_check == 200, 0, Preferential_share),  # Use if_else for vectorized operation
                        # NonPreferential_share = if_else(cross_check == 200, 0, NonPreferential_share),
                        cross_check1=NonPreferential_share+Exempted_share+Preferential_share,
                      )
                    
                    
                    
                    View(FreeTradeAgreement_TR)
                    
                    
                    total_non_zero_counts <- data.frame(
                      Exempted_share_count = sum(FreeTradeAgreement_TR$Exempted_share != 0, na.rm = TRUE),
                      Preferential_share_count = sum(FreeTradeAgreement_TR$Preferential_share != 0, na.rm = TRUE),
                      NonPreferential_share_count = sum(FreeTradeAgreement_TR$NonPreferential_share != 0, na.rm = TRUE)
                    )
                    
                    total_non_zero_counts
                    
                    # Calculate the sum of the counts in the row
                    total_sum <- sum(total_non_zero_counts)
                    
                    total_sum
                   
                    
                    write.csv(FreeTradeAgreement_TR,"FreeTradeAgreement_TR5.csv")
                    
                    
                    
                    
# II.Preparing data for charts and for the main table --------------------------------------------

      # 1.Preparing data for Charts ---------------------------------------
            # 1.1 Regular Import ------------------------------------------
                    
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
      
              actual_year_simulation <- unique(Import_raw_monthly$Year)
              
              # Multilateral Trade Negotiations (MTN) Categories, HS 2017
              # Multilateral Trade Negotiations (MTN) categories were first defined in the Tokyo Round and adapted for the Harmonized System in the Uruguay Round.
              # The product group breakdown in this publication deviates slightly from the previous definition, which was based on the HS 1992 nomenclature.
              
              CustomsDuties_TE_MTN<-CustomsDuties_TE_agg_HS%>%
                dplyr::select(Six_digit,Treatment,CustomsDuties_TE)%>%
                dplyr::group_by(Six_digit,Treatment)%>%
                dplyr::summarise(CustomsDuties_TE=sum(CustomsDuties_TE,na.rm = TRUE))

              # New
              WTO_MTN_subset <- WTO_MTN %>%
                filter(
                  (actual_year_simulation >= 2022 & str_detect(HS_year, ">2022")) |
                    (actual_year_simulation < 2022 & str_detect(HS_year, "<2022"))
                )
              
              #CustomsDuties_TE_MTN<-left_join(CustomsDuties_TE_MTN,WTO_MTN,by =c("Six_digit"))%>%
              CustomsDuties_TE_MTN<-left_join(CustomsDuties_TE_MTN,WTO_MTN_subset,by =c("Six_digit"))%>%
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
                
                Estimation_TE <- data.frame(
                  DataSet = character(),  # Replace with appropriate data type if not character
                  Value = numeric(),
                  ExciseRevenue=numeric()# Replace with appropriate data type if not numeric
                )
                
                Excise_TE_Chapters<- data.frame(
                  Chapter = character(),  # Replace with appropriate data type if not character
                  Excise_TE = numeric(),
                  ExciseRevenue=numeric()# Replace with appropriate data type if not numeric
                )
                
                Excise_TE_MTN<- data.frame(
                  Product_group = character(),  # Replace with appropriate data type if not character
                  Excise_TE = numeric(),
                  ExciseRevenue=numeric()# Replace with appropriate data type if not numeric
                )
               
                
                



# CustomsRevenue Tab

# Charts 1 

MacroFiscalData$ImportDuties_PctOfGDP<-round(MacroFiscalData$ImportDuties_PctOfGDP,2)

df_plt <- MacroFiscalData %>%
  dplyr::select(Year, GDP, ImportDuties_PctOfGDP)


ImportDuties_PctOfGDP <- plot_ly(df_plt)
ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% add_trace(x = ~Year, y = ~GDP, type = 'bar', name = 'GDP')
ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% add_trace(x = ~Year, y = ~ImportDuties_PctOfGDP, type = 'scatter', mode = 'lines+markers', name = 'Share of customs revenue in GDP ',
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

df_plt <- MacroFiscalData %>%
  dplyr::select(Year, GDP, ImportDuties_PctOfGDP)
ImportDuties_PctOfGDP <- plot_ly(df_plt)
ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% add_trace(x = ~Year, y = ~GDP, type = 'bar', name = 'GDP')
ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% add_trace(
  x = ~Year, y = ~ImportDuties_PctOfGDP, type = 'scatter', mode = 'lines+markers', name = 'Share of customs revenue in GDP ',
  yaxis = 'y2', line = list(dash = 'dot', color = "#FFA500", width = 4)  # Set width to 4 for a bold dotted line
)

ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>% layout(
  title = 'Nominal GDP and share of customs revenues in GDP,2015-2022',font = t_11,
  xaxis = list(title = ""),
  yaxis = list(side = 'left', title = 'In million LCU', showgrid = FALSE, zeroline = FALSE),
  yaxis2 = list(side = 'right', overlaying = "y", title = 'Percentage', showgrid = FALSE, zeroline = FALSE, font = list(size = 8)), 
  annotations = list(
    x = 0, y = -0.05,
    text = "Source: National authorities",font = t_8,
    showarrow = FALSE,
    xref = 'paper',
    yref = 'paper',
    align = 'left'
  )
)


ImportDuties_PctOfGDP <- ImportDuties_PctOfGDP %>%
  layout(
    images = list(
     # source = base64_image,  # Use the base64-encoded image data
      xref = "paper",
      yref = "paper",
      x = 0,
      y = 1,
      sizex = 0.05,  
      sizey = 0.05,  
      opacity = 0.8
    )
  )

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


# Chart 4

RegularImport<-plot_ly(data =CustomsValue, type = "treemap", values = ~round(Value/1e06,1), labels = ~Chapter,
                       parents = ~HS,
                       name = " ",
                       text = ~Chapters_description   ,
                       textinfo="label+value+percent parent")%>%
  layout(title=paste("Structure of Total Regular Import by HS Chapters in LCU(Millions),", actual_year_simulation),font =t_11)


# Chart 5

ImportStructure <- CustomsDuties_base_pie %>% 
  plot_ly(labels = ~Treatment, values = ~value)

ImportStructure <- ImportStructure %>% add_pie(hole = 0.6)
ImportStructure <- ImportStructure %>% layout(
  title = paste("Structure of Import by Treatment of Goods (preferential vs non-preferential)", actual_year_simulation),
  font = t_11, 
  showlegend = TRUE,
  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  annotations = list(
    x = 0, y = -0.1,
    title = "Additional Information:",  # New subtitle
    text = "Preferential treatment includes goods covered by Free Trade Agreements, while non-preferential treatment includes only imports subject to customs tariffs.",
    showarrow = FALSE,
    xref = 'paper',
    yref = 'paper',
    align = 'left'
  ),
  font = t_8
)



# TaxExpenditures Tab 

# Chart 1

GroupOfProducts_HS <- plot_ly(CustomsDuties_TE_type_products, x = ~type_products , y = ~value, type = 'bar', text = ' ', hoverinfo = 'y+text', color = ~type_products, colors = colors) %>% 
  layout(
    title = paste("Tax expenditures by Group of products by HS,", actual_year_simulation),
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


# Charts 2

Sections_HS <- plot_ly(
  CustomsDuties_TE_Sections, 
  x = ~reorder(Sections, -CustomsDuties_TE), 
  y = ~CustomsDuties_TE, 
  type = 'bar', 
  text = ' ', 
  hoverinfo = 'y+text', 
  hovertext = ~Section_description,
  marker = list(color = '#ff7f0e')  # Set the color of all bars to orange
) %>%
  layout(
    title = paste("Tax expenditures by HS Sections,", actual_year_simulation),
    font = t_11,
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


# Charts 3


Chapters_HS <- plot_ly(CustomsDuties_TE_Chapters, x = ~reorder(Chapter, -CustomsDuties_TE), y = ~CustomsDuties_TE, type = 'bar', text = ' ', hoverinfo = 'y+text',#color = ~Treatment, colors = colors,
                       hovertext = ~Chapters_description) %>%
  layout(
    title = paste("Tax expenditures by HS Chapters,", actual_year_simulation),font = t_11,
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



# Chart 4

ProductGroups_MTN <- plot_ly(
  CustomsDuties_TE_MTN, 
  y = ~reorder(Product_group, CustomsDuties_TE), 
  x = ~CustomsDuties_TE, 
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

#  Chart 5

Sectors_CPA <- plot_ly(CustomsDuties_TE_SECTORS, y = ~reorder(CPA21_NAME , CustomsDuties_TE), x = ~CustomsDuties_TE, type = 'bar', text = ' ', hoverinfo = 'x+text',#color = ~Treatment, colors = colors,
                       hovertext = ~CPA21_NAME) %>%
  layout(
    title = paste("Tax expenditures by CPA Sectors", actual_year_simulation),font = t_11,
    font = list(size = 11),
    yaxis = list(title = ''),
    xaxis = list(title = 'In LCU'),
    barmode = 'stack',
    annotations = list(
      x = -0.1, y = -0.056,
      text = "Source: Calculations by WB staff based on data from National authorities",
      showarrow = FALSE,
      xref = 'paper',
      yref = 'paper',
      align = 'left'
    )
  )



#  Chart 5

mapdata3 <- left_join(mapdata_iso3c,CustomsDuties_TE_agg_countries,by = c("iso3"="iso3c"))

# Removing Antarctica
mapdata3<-mapdata3[!(mapdata3$region=="Antarctica"),]

map1 <- ggplot(mapdata3, aes(x= long , y=lat,group=group)) +
  geom_polygon(aes(fill=CustomsDuties_TE), color="black")



ChoroplethMap <- map1 + scale_fill_gradient(name= "In LCU million",
                                            low = "lightgreen",
                                            high = "darkgreen",
                                            na.value="grey90") +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="bottom")+
  labs(title = paste("                                                                                      Geographic distribution of tax expenditures,", actual_year_simulation))



rm(Import_raw_monthly)
        

#})