

taric_data_sub <- taric_data %>%
  dplyr::select(HS6_COD, CustomsRate_MFN, CustomsRate_CEFTA, CustomsRate_MSA, CustomsRate_TR)
# %>%
#   dplyr::group_by(HS6_COD) %>%
#   dplyr::summarize(
#     CustomsRate_MFN = mean(CustomsRate_MFN, na.rm = TRUE),
#     CustomsRate_CEFTA = mean(CustomsRate_CEFTA, na.rm = TRUE),
#     CustomsRate_MSA = mean(CustomsRate_MSA, na.rm = TRUE),
#     CustomsRate_TR = mean(CustomsRate_TR, na.rm = TRUE)
#   )

taric_data_sub <- taric_data_sub %>%
  dplyr::mutate(Six_digit = paste0(substr(HS6_COD, 1, 4), " ", substr(HS6_COD, 5, nchar(HS6_COD))))

SimulationYear<-2023

WTO_MTN_subset <- WTO_MTN %>%
  filter(
    (SimulationYear >= 2022 & str_detect(HS_year, ">2022")) |
      (SimulationYear < 2022 & str_detect(HS_year, "<2022"))
  )


MTN_customs_data_agg<-left_join(taric_data_sub,WTO_MTN_subset,by =c("Six_digit"))%>%
  select(-c("HS_year","Four_digit","Chapter"))

# MTN_customs_data_agg<-MTN_customs_data_agg%>%
#   dplyr::group_by(HS6_COD,Product_group) %>%
#   dplyr::summarize(
#    MFN = mean(CustomsRate_MFN, na.rm = TRUE),
#    CEFTA = mean(CustomsRate_CEFTA, na.rm = TRUE),
#    MSA = mean(CustomsRate_MSA, na.rm = TRUE),
#    TR = mean(CustomsRate_TR, na.rm = TRUE)
#   )


BOX_PLOT_MFN<-ggplot(MTN_customs_data_agg, aes(x = Product_group, y =  CustomsRate_MFN, fill = Product_group),palette = c("#0073C2FF", "#FC4E07")) +
  geom_boxplot()+theme(axis.title.x=element_blank(),
                       axis.text.x=element_blank(),
                       #legend.position = "none",
                       # axis.ticks.x=element_blank())+ theme(legend.position = "none")+labs(title = "MFN", x = " ", y = "Tariff rate")
                       axis.ticks.x=element_blank())+labs(title = "MFN", x = " ", y = "Tariff rate")





BOX_PLOT_MFN <- plot_ly(data = MTN_customs_data_agg, 
                        x = ~Product_group, 
                        y = ~MFN, 
                        type = "box", 
                        color = ~Product_group, 
                        colors = c("#0073C2FF", "#FC4E07"), 
                        boxpoints = "all") %>%
  layout(title = "MFN",
         xaxis = list(title = "", showticklabels = FALSE, ticks = ""),
         yaxis = list(title = "Tariff rate"),
         showlegend = TRUE)

BOX_PLOT_MFN







# %>%
#   dplyr::select(Product_group,Treatment,CustomsDuties_TE)
# 
MTN_customs_data_agg <- distinct(MTN_customs_data_agg)

MTN_customs_data_agg<-MTN_customs_data_agg%>%
  dplyr::group_by(year,Product_group)%>%
  dplyr::summarise(
    Baseline =sum(calc_customs_duties_bu ,na.rm = TRUE),
    Simulation =sum(calc_customs_duties_sim ,na.rm = TRUE)
  )

MTN_customs_data_agg$Product_group <- ifelse(is.na( MTN_customs_data_agg$Product_group), "Other",  MTN_customs_data_agg$Product_group)

MTN_customs_data_agg$Product_group <- factor( MTN_customs_data_agg$Product_group)



# TESTING EFFECTIVE CUSTOMS RATE ------------------------------------------

library(AMR)
library(dplyr)
library(ggplot2)
library(gridExtra)

customs_simulation_parameters_raw

customs_simulation_parameters_updated 


View(customs_simulation_parameters_raw)


cummulative_mfn_bu<-customs_simulation_parameters_raw%>%
  select(Effective_Customs_rate)%>%
  mutate(cum = cumsum(rep(1/n(), n())))



# Group by 'Effective_Customs_rate' and count occurrences
result <- customs_simulation_parameters_raw %>%
  group_by(Effective_Customs_rate) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(Effective_Customs_rate)

# Calculate cumulative frequency
# result <- result %>%
#   mutate(cumulative_frequency = cumsum(count))

# Calculate cumulative frequency as a percentage
total_count <- sum(result$count)  # Calculate total count
result <- result %>%
  mutate(cumulative_frequency = cumsum(count) / total_count * 100)  # Convert to percentage




AGRI_MFN<-ggplot()+
  geom_smooth(data=result, aes( x=Effective_Customs_rate,y=cumulative_frequency),level = NA,color="red")+labs(title = "Agricultural products", x = "Range of tariff rates", y = "Cumulative Percent")+ xlim(0,10)




cummulative_mfn_bu_plot<-data.frame(
  Tariff_rates=cummulative_mfn_bu$item,
  Range_of_tariff_rates=cummulative_mfn_bu$count,
  Cumulative_freqency=cummulative_mfn_bu$cum_percent
)




NON_AGRI_MFN<-ggplot()+
  geom_smooth(data=CUMULATIVE_PLOT_NON_AGRI_MFN, aes( x=Tariff_rates,y=Cumulative_freqency),level = NA,color="blue")+labs(title = "Non-Agricultural products", x = "Range of tariff rates", y = "Cumulative Percent")


