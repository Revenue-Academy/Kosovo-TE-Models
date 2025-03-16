'Preparation of synthetic data'
options(scipen = 999)
set.seed(40)                       

# Determine size of sample
size_sample<-10000


library(tidyverse)
# 
#                           dplyr::mutate(
#                                           ssc = gross_i*0.275,
#                                           pit = (gross_i-(ssc+personal_allowance))*0.10)
#                         # Introduce TE's in data
#                         SOURCE_DATA_WAGES$pit[sample(nrow(SOURCE_DATA_WAGES),500)] <- 0
#                         SOURCE_DATA_WAGES$ssc[sample(nrow(SOURCE_DATA_WAGES),500)] <- 0
#                         SOURCE_DATA_WAGES<-SOURCE_DATA_WAGES%>%
#                           dplyr::mutate( net_i = gross_i-(ssc+pit))%>%
#                           data.table()


# 1. Functions
                        
                        # 1. Function to sum all numeric columns and add the result as a new column ----------------------------
                        add_numeric_sum_column <- function(df) {
                          # Select only numeric columns
                          numeric_columns <- df[sapply(df, is.numeric)]
                          
                          # Calculate the row-wise sum of numeric columns, ignoring NA values
                          df$g_total_gross <- rowSums(numeric_columns, na.rm = TRUE)
                          
                          # Return the updated data frame with the new 'total_sum' column
                          return(df)
                        }
                        
                        # 2. Labor sum -----------------------------------------------------

                        # Function to sum all columns ending with "_l" and add as total_gross_l
                        add_total_gross_l <- function(df) {
                          # Select columns that end with "_l"
                          l_columns <- df[, grepl("_l$", colnames(df)) & sapply(df, is.numeric)]
                          
                          # Calculate the row-wise sum for "_l" columns
                          df$total_gross_l <- rowSums(l_columns, na.rm = TRUE)
                          
                          # Return the updated data frame
                          return(df)
                        }
                        
                        
                        # 3. Capital sum -------------------------------------------------------------
                        
                        # Function to sum all columns ending with "_c" and add as total_gross_c
                        add_total_gross_c <- function(df) {
                          # Select columns that end with "_c"
                          c_columns <- df[, grepl("_c$", colnames(df)) & sapply(df, is.numeric)]
                          
                          # Calculate the row-wise sum for "_c" columns
                          df$total_gross_c <- rowSums(c_columns, na.rm = TRUE)
                          
                          # Return the updated data frame
                          return(df)
                        }
                        
                        
                        
                        
                        
                        
                        
                        # 4. Deductions ------------------------------------------------------------
                        
                        # Function to sum all columns starting with "d_" and add as total_deductions
                        add_total_deductions <- function(df) {
                          # Select columns that start with "d_"
                          d_columns <- df[, grepl("^d_", colnames(df)) & sapply(df, is.numeric)]
                          
                          # Calculate the row-wise sum for "d_" columns
                          df$total_deductions <- rowSums(d_columns, na.rm = TRUE)
                          
                          # Return the updated data frame
                          return(df)
                        }
                        
                        # pit= (g_Wages_l-(g_total_personal_allowance_l+total_ssc))*0.1
                        
                        
                        
                        
                        
                        
                        
                        
# 2. Creating a random sample with synthetic data ----------------------------------------------------------------
                        # NACE
                        # List of all categories
                        categories <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U")
                        
                        # Assign specific probabilities and evenly distribute the rest
                        probabilities <- ifelse(categories %in% c("C", "G", "O", "Q", "J"), 
                                                c(0.18, 0.19, 0.09, 0.08, 0.06)[match(categories, c("C", "G", "O", "Q", "J"))], 
                                                (1 - sum(c(0.18, 0.19, 0.09, 0.08, 0.06))) / (length(categories) - 5))


                        # Create the synthetic data frame
                        pit_data_synthetic_data <- data.frame(stringsAsFactors = FALSE, # Prevent factors
                                                              id_n = as.character(seq(1, size_sample)),
                                                              nace_section=sample(categories, size = size_sample, replace = TRUE, prob = probabilities),
                                                              year_birth = as.character(as.integer(runif(size_sample, 1957, 2003))),
                                                              gender = sample(x=c("M","F"), prob = c(.6, .4), size=size_sample, replace=TRUE),
                                                              g_Wages_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.9, 0.1)), 
                                                                                 round(abs(rchisq(size_sample, df = 10) * 950000/3.1), 0), NA),
                                                              g_WagesDiplomaticConsular_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.01, 0.99)), 
                                                                                                   round(abs(rchisq(size_sample, df = 10) * 5000/3), 0), NA),
                                                              g_TemporaryContracts_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.45, 0.55)), 
                                                                                              round(abs(rchisq(size_sample, df = 10) * 850000/3.1), 0), NA),
                                                              g_AgriculturalProductsOwn_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.15, 0.85)), 
                                                                                                   round(abs(rchisq(size_sample, df = 10) * 840000/3), 0), NA),
                                                              g_AgriculturalProducts_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.15, 0.85)), 
                                                                                                round(abs(rchisq(size_sample, df = 10) * 2070000/3.1), 0), NA),
                                                              g_IndependentActivity_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.05, 0.95)), 
                                                                                               round(abs(rchisq(size_sample, df = 10) * 550000/3), 0), NA),
                                                              g_CopyrightIncomeArtisticPhotography_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.05, 0.95)), 
                                                                                                              round(abs(rchisq(size_sample, df = 10) * 54000/3), 0), NA),
                                                              g_CopyrightIncomeMusicBallet_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.05, 0.95)), 
                                                                                                      round(abs(rchisq(size_sample, df = 10) * 63000/3), 0), NA),
                                                              g_CopyrightIncomePaintingsSculptural_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.05, 0.95)), 
                                                                                                              round(abs(rchisq(size_sample, df = 10) * 31500/3), 0), NA),
                                                              g_CopyrightIncomeSuccessor_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.05, 0.95)), 
                                                                                                    round(abs(rchisq(size_sample, df = 10) * 31000/3), 0), NA),
                                                              g_CopyrightIncomeTranslationsLectures_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.05, 0.95)), 
                                                                                                               round(abs(rchisq(size_sample, df = 10) * 30000/3), 0), NA),
                                                              g_WorkIncome_l = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.1, 0.9)), 
                                                                                      round(abs(rchisq(size_sample, df = 10) * 100000/3), 0), NA),
                                                              g_CapitalGains_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.01, 0.99)), 
                                                                                        round(abs(rchisq(size_sample, df = 10) * 6000000/3.1), 0), NA),
                                                              g_CapitalIncome_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.05, 0.95)), 
                                                                                         round(abs(rchisq(size_sample, df = 10) * 4000000/3.1), 0), NA),
                                                              g_IndustrialPropertyRights_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.02, 0.98)), 
                                                                                                    round(abs(rchisq(size_sample, df = 10) * 500000/3), 0), NA),
                                                              g_Insurance_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.02, 0.98)), 
                                                                                     round(abs(rchisq(size_sample, df = 10) * 400000/3), 0), NA),
                                                              g_Interests_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.02, 0.98)), 
                                                                                     round(abs(rchisq(size_sample, df = 10) * 1000000/3.1), 0), NA),
                                                              g_Lease_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.02, 0.98)), 
                                                                                 round(abs(rchisq(size_sample, df = 10) * 1000000/3.1), 0), NA),
                                                              g_LeaseBusiness_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.02, 0.98)), 
                                                                                         round(abs(rchisq(size_sample, df = 10) * 2000000/3.1), 0), NA),
                                                              g_Sublease_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.02, 0.98)), 
                                                                                    round(abs(rchisq(size_sample, df = 10) * 2000000/3), 0), NA),
                                                              g_SolidWaste_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob =c(0.1, 0.9)), 
                                                                                      round(abs(rchisq(size_sample, df = 10) * 1000000/3.1), 0), NA),
                                                              g_GamesofChanceSpecific_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.02, 0.98)), 
                                                                                                 round(abs(rchisq(size_sample, df = 10) * 3000/3), 0), NA),
                                                              g_GamesofChanceBettingHouse_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.02, 0.98)), 
                                                                                                     round(abs(rchisq(size_sample, df = 10) * 3000/3), 0), NA),
                                                              g_GamesofChanceGeneral_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.02, 0.98)), 
                                                                                                round(abs(rchisq(size_sample, df = 10) * 2000/3), 0), NA),
                                                              g_OtherIncome_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.02, 0.98)), 
                                                                                       round(abs(rchisq(size_sample, df = 10) * 300000/3), 0), NA),
                                                              g_CapitalGainsSellsRealEstateFiveYear_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob =c(0.02, 0.98)), 
                                                                                      round(abs(rchisq(size_sample, df = 10) * 150000/3), 0), NA),
                                                              g_CapitalGainsSaleShareCapital_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob =c(0.02, 0.98)), 
                                                                                      round(abs(rchisq(size_sample, df = 10) * 750000/3), 0), NA),
                                                              g_CapitalGainsRealEstateThreeYear_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob =c(0.02, 0.98)), 
                                                                                      round(abs(rchisq(size_sample, df = 10) * 50000/3), 0), NA),
                                                              g_CapitalGainssaleOtherMovableAssets_c = ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob =c(0.02, 0.98)), 
                                                                                      round(abs(rchisq(size_sample, df = 10) * 100000/3), 0), NA),
                                                              g_d_total_tax_reduction_l=ifelse(sample(c(TRUE, FALSE), size = size_sample, replace = TRUE, prob = c(0.01, 0.99)), 
                                                                     round(abs(rchisq(size_sample, df = 10) * 24000), 0), NA)
                                                              
                                                              )%>%
                                                dplyr::mutate(g_total_personal_allowance_l=ifelse(g_Wages_l>0,round(abs(rnorm(runif(size_sample, 8333, 100000), mean = 100000, sd = 45000)),0),NA),
                                                                #total_ssc = ifelse (g_Wages_l>0,g_Wages_l*0.28,NA),
                                                              total_ssc = ifelse(g_Wages_l > 0, 
                                                                                 ifelse(g_Wages_l > 3500000, 3500000 * 0.28, g_Wages_l * 0.28), 
                                                                                 NA),
                                                                g_d_WagesDiplomaticConsular_l          = ifelse (g_WagesDiplomaticConsular_l>0,g_WagesDiplomaticConsular_l*0.1,NA),
                                                                d_AgriculturalProductsOwn_l            = ifelse (g_AgriculturalProductsOwn_l>0,g_AgriculturalProductsOwn_l*0.8,NA),
                                                                d_CopyrightIncomeArtisticPhotography_l = ifelse (g_CopyrightIncomeArtisticPhotography_l>0,g_CopyrightIncomeArtisticPhotography_l*0.5,NA),
                                                                d_CopyrightIncomeMusicBallet_l         = ifelse (g_CopyrightIncomeMusicBallet_l>0,g_CopyrightIncomeMusicBallet_l*0.3,NA),
                                                                d_CopyrightIncomePaintingsSculptural_l = ifelse (g_CopyrightIncomePaintingsSculptural_l>0,g_CopyrightIncomePaintingsSculptural_l*0.5,NA),
                                                                d_CopyrightIncomeSuccessor_l           = ifelse (g_CopyrightIncomeSuccessor_l>0,g_CopyrightIncomeSuccessor_l*0.1,NA),
                                                                d_CopyrightIncomeTranslationsLectures_l= ifelse (g_CopyrightIncomeTranslationsLectures_l>0,g_CopyrightIncomeTranslationsLectures_l*0.2,NA),
                                                                d_WorkIncome_l                         = ifelse (g_WorkIncome_l>0,g_WorkIncome_l*0,NA),
                                                                d_CapitalGains                         = ifelse (g_CapitalGains_c>0,g_CapitalGains_c*0.1,NA),
                                                                d_IndustrialPropertyRights_c           = ifelse (g_IndustrialPropertyRights_c>0,g_IndustrialPropertyRights_c*0.1,NA),
                                                                d_Insurance_c                          = ifelse (g_Insurance_c>0,g_Insurance_c*0,NA),
                                                                d_Lease_c                              = ifelse (g_Lease_c>0,g_Lease_c*0.1,NA),
                                                                d_LeaseBusiness_c                      = ifelse (g_LeaseBusiness_c>0,g_LeaseBusiness_c*0.15,NA),
                                                                d_SolidWaste_c                         = ifelse (g_SolidWaste_c>0,g_SolidWaste_c*0.5,NA),
                                                                d_GamesofChanceSpecific_c              = ifelse (g_GamesofChanceSpecific_c>0,g_GamesofChanceSpecific_c*0.2,NA),
                                                                g_d_GamesofChanceBettingHouse_c        = ifelse (g_GamesofChanceBettingHouse_c>0,g_GamesofChanceBettingHouse_c*0.2,NA),
                                                                d_OtherIncome_c                        = ifelse (g_OtherIncome_c>0,g_OtherIncome_c*0,NA),
                                                                d_CapitalGainsSellsRealEstateFiveYear_c=ifelse (g_CapitalGainsSellsRealEstateFiveYear_c>0,g_CapitalGainsSellsRealEstateFiveYear_c*0.1,NA),
                                                                d_CapitalGainsSaleShareCapital_c       = ifelse (g_CapitalGainsSaleShareCapital_c>0,g_CapitalGainsSaleShareCapital_c*0.1,NA),
                                                                d_CapitalGainsRealEstateThreeYear_c    = ifelse (g_CapitalGainsRealEstateThreeYear_c>0,g_CapitalGainsRealEstateThreeYear_c*0.1,NA),
                                                                d_CapitalGainssaleOtherMovableAssets_c = ifelse (g_CapitalGainsRealEstateThreeYear_c>0,g_CapitalGainsRealEstateThreeYear_c*0.1,NA),
                                                                g_total_net_l=0,
                                                                g_IndustrialPropertyRightsSuccessor_c=0,
                                                                #g_d_total_tax_reduction_l=0,
                                                                weight=1,
                                                                Year=2021
                                                                # pit1= 
                                                                #   
                                                                #   g_WagesDiplomaticConsular_l + g_TemporaryContracts_l + g_AgriculturalProductsOwn_l + g_AgriculturalProducts_l + g_IndependentActivity_l + g_CopyrightIncomeArtisticPhotography_l + g_CopyrightIncomeMusicBallet_l + g_CopyrightIncomePaintingsSculptural_l + g_CopyrightIncomeSuccessor_l + g_CopyrightIncomeTranslationsLectures_l + g_WorkIncome_l + g_CapitalGains_c + g_CapitalIncome_c + g_IndustrialPropertyRights_c + g_IndustrialPropertyRightsSuccessor_c + g_Insurance_c + g_Interests_c + g_Lease_c + g_LeaseBusiness_c + g_Sublease_c + g_SolidWaste_c + g_GamesofChanceSpecific_c + g_GamesofChanceBettingHouse_c + g_GamesofChanceGeneral_c + g_OtherIncome_c + g_CapitalGainsSellsRealEstateFiveYear_c + g_CapitalGainsSaleShareCapital_c + g_CapitalGainsRealEstateThreeYear_c + g_CapitalGainssaleOtherMovableAssets_c
                                                                # 
                                                                #   
                                                                #   
                                                                #   
                                                                #   
                                                                # 
                                                                # g_total_net_l = g_Wages_l-(g_total_personal_allowance_l-total_ssc
                                                              )
                                
                                              
                                              #total_deductions                       =

                        

                    # 3. Implementing function for calculation  ---------------------------------------------------------------------

                          # Apply both functions to pit_data_synthetic_data
                        pit_data_synthetic_data <- add_numeric_sum_column(pit_data_synthetic_data)
                        pit_data_synthetic_data <- add_total_gross_l(pit_data_synthetic_data)
                        pit_data_synthetic_data <- add_total_gross_c(pit_data_synthetic_data)
                        pit_data_synthetic_data <- add_total_deductions(pit_data_synthetic_data)
                        
                        # Check the first few rows to verify the new columns
                        head(pit_data_synthetic_data)
                        
                        
                        summary(pit_data_synthetic_data)
                        
                        
                      
                        
                        

                        
# II.PLOTING DATA ------------------------------------------------------------

                        # Function to automatically select numerical columns from the data frame
                        select_numeric_columns <- function(df) {
                          df[sapply(df, is.numeric)]
                        }
                        
                        # Apply the function to select numeric columns from pit_data_synthetic_data
                        data_to_plot <- select_numeric_columns(pit_data_synthetic_data)
                        
                        # Melt the data frame to long format for plotting
                        data_long <- reshape2::melt(data_to_plot, variable.name = "IncomeType", value.name = "Value")
                        
                        # Plot using ggplot2
                        
                        ggplot(data_long, aes(x = Value)) +
                          geom_histogram(bins = 50, fill = "blue", color = "black", alpha = 0.7) +
                          facet_wrap(~IncomeType, scales = "free_x") +  # Facet by income type
                          theme_minimal() +
                          labs(title = "Distribution of Income Types", x = "", y = "")
                        
                        
  
                        pit_data_synthetic_data[is.na(pit_data_synthetic_data)] <- 0
                        write.csv(pit_data_synthetic_data,"pit_synthetic_sample.csv")                    
   

# III.ADDITIONAL CROSS-CHECKING FUNCTIONS ---------------------------------

                    
                        # Check unique ID's 
                        length(unique(pit_data_synthetic_data$id_n))
                        
                        # Check Standard Deviations
                        
                        
                        std_devs <- sapply(pit_data$sample, sd, na.rm = TRUE)
                        
                        # Print the result
                        print(std_devs)
                        
