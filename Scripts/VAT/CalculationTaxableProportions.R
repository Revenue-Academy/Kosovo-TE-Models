
Import_raw_monthly



CustomsDuties_TE_agg_HS

library(tidyverse)

CPA_CN_selected<-CPA_CN%>%
  dplyr::filter(year==2023)%>%
  dplyr::select(CPA_CODE,CN_CODE)%>%
  dplyr::mutate(CPA_2_DIGIT = substr(CPA_CODE, 1, 2))
  


CustomsDuties_TE_agg_HS

test_vat<-left_join(CustomsDuties_TE_agg_HS,CPA_CN_selected,by=c("Eight_digit"="CN_CODE"))%>%
  dplyr::ungroup()%>%
  dplyr::select(CPA_2_DIGIT,Value,CustomsRevenue,ExciseRevenue,VAT_Revenue)%>%
  dplyr::mutate(vat_base=Value+CustomsRevenue+ExciseRevenue)%>%
  dplyr::group_by(CPA_2_DIGIT)%>%
  dplyr::summarise(Value =sum(Value,na.rm = TRUE),
            CustomsRevenue = sum(CustomsRevenue,na.rm = TRUE),
            ExciseRevenue =sum(ExciseRevenue,na.rm = TRUE),
            VAT_Revenue=sum(VAT_Revenue,na.rm = TRUE),
            vat_base=sum(vat_base,na.rm = TRUE),
            etr=(VAT_Revenue/vat_base)*100
            )%>%
  dplyr::select(CPA_2_DIGIT,vat_base,VAT_Revenue)%>%
  dplyr::rename("Consumption"="vat_base",
                 "VAT"="VAT_Revenue") #%>%
  # dplyr::filter(CPA_2_DIGIT %in% c("10","11","12"))%>%
  # #dplyr::group_by(CPA_2_DIGIT)%>%
  # dplyr::summarise(Consumption=sum(Consumption),
  #                  VAT=sum(VAT))
  


View(test_vat)

#

calculate_vat_portions <- function(df, vat_rate_0, vat_rate_1, vat_rate_2) {
  # Initialize new columns
  df$proportion_vat_rate_0 <- 0
  df$proportion_vat_rate_1 <- 0
  df$proportion_vat_rate_2 <- 0
  
  # Calculate the portions for each row
  for (i in 1:nrow(df)) {
    consumption <- df$Consumption[i]
    vat_paid <- df$VAT[i]
    
    # Calculate the Effective Tax Rate (ETR)
    etr <- vat_paid / consumption
    
    # Check if VAT is zero
    if (vat_paid == 0) {
      df$proportion_vat_rate_0[i] <- 100
      df$proportion_vat_rate_1[i] <- 0
      df$proportion_vat_rate_2[i] <- 0
    } else if (etr < vat_rate_1) {
      # If ETR is below vat_rate_1, it means there is a portion taxed at 0%
      # Define the system of equations
      # x + y + z = consumption
      # vat_rate_0 * x + vat_rate_1 * y + vat_rate_2 * z = vat_paid
      
      # Coefficients matrix
      A <- matrix(c(1, 1, 1, vat_rate_0, vat_rate_1, vat_rate_2), nrow = 2, byrow = TRUE)
      
      # Constants vector
      B <- c(consumption, vat_paid)
      
      # Solve the system of equations using least squares
      solution <- lm(B ~ A - 1)$coefficients
      
      # Extract the portions
      portion_x <- solution[1]
      portion_y <- solution[2]
      portion_z <- solution[3]
      
      # Calculate the proportions as percentages
      df$proportion_vat_rate_0[i] <- (portion_x / consumption) * 100
      df$proportion_vat_rate_1[i] <- (portion_y / consumption) * 100
      df$proportion_vat_rate_2[i] <- (portion_z / consumption) * 100
    } else {
      # Define the system of equations
      # x + y = consumption
      # vat_rate_1 * x + vat_rate_2 * y = vat_paid
      
      # Coefficients matrix
      A <- matrix(c(1, 1, vat_rate_1, vat_rate_2), nrow = 2, byrow = TRUE)
      
      # Constants vector
      B <- c(consumption, vat_paid)
      
      # Solve the system of equations
      solution <- solve(A, B)
      
      # Extract the portions
      portion_x <- solution[1]
      portion_y <- solution[2]
      
      # Calculate the proportions as percentages
      df$proportion_vat_rate_0[i] <- 0
      df$proportion_vat_rate_1[i] <- (portion_x / consumption) * 100
      df$proportion_vat_rate_2[i] <- (portion_y / consumption) * 100
    }
  }
  
  return(df)
}



# Define VAT rates
vat_rate_0 <- 0.00
vat_rate_1 <- 0.08
vat_rate_2 <- 0.18

# Calculate the portions and update the data frame
df <- calculate_vat_portions(test_vat, vat_rate_0, vat_rate_1, vat_rate_2)

numerical_cols <- sapply(df, is.numeric)

# Round numerical columns to 2 decimal places
df[numerical_cols] <- lapply(df[numerical_cols], round, 1)

view(df)


# Test --------------------------------------------------------------------


library(readxl)
VAT_2021_2022 <- read_excel("Data/VAT/VAT_2021_2022.xlsx", 
                            sheet = "TVSH 2022")%>%
                dplyr::select(ENT_ACTIVITY_CODE,D12,D14)%>%
                dplyr::mutate(CPA_2_DIGIT = substr(ENT_ACTIVITY_CODE, 1,2))%>%
                dplyr::select(CPA_2_DIGIT,D12,D14)%>%
                dplyr::group_by(CPA_2_DIGIT)%>%
                dplyr::summarise(
                                  VAT_18=sum(D12,na.rm = TRUE),     
                                  VAT_8=sum(D14,na.rm = TRUE),
                                  VAT_8_18=VAT_8+VAT_18
                                  
                                  )


#view(VAT_2021_2022)

#

NACE_SUT_table <- read_excel("Data/VAT/NACE_SUT_table.xlsx", 
                             sheet = "NACE")



VAT_2022<-left_join(VAT_2021_2022,NACE_SUT_table, by=c("CPA_2_DIGIT"="nace_num"))%>%
  dplyr::select(divisions_sut,VAT_18,VAT_8,VAT_8_18)%>%
  dplyr::ungroup()%>%
  dplyr::group_by(divisions_sut)%>%
  dplyr::summarise(
                  VAT_18=sum(VAT_18,na.rm = TRUE),   
                  VAT_8=sum(VAT_8,na.rm = TRUE))
                  

VAT_2022<-VAT_2022%>%
   dplyr::mutate(VAT_8_18=VAT_18+VAT_8,
     VAT_prop_18=VAT_18/VAT_8_18)
  

View(VAT_2022)

