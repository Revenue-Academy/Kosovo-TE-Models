

summary_TE <- read_excel("Summary.xlsx")
  
  

summary_TE$Base<-c('Final consumption,
Regular import and 
Gross income')

summary_TE$TE<-c('Tax Expenditures')

DistributionOfTE_sankey<-sankey_ly(summary_TE,cat_cols = c('Base',"Tax","TypeOfMeasure",
                                                           "Objective","TE"), 
                                   num_col = "TaxExpenditures",
                                   title = " ")


DistributionOfTE_sankey<-plotly::add_annotations(DistributionOfTE_sankey,
                                                 text= c("Type of tax", "Type of measure","Goal","" ), 
                                                 x = c(0.20, 0.44, 0.72, 0.97), 
                                                 y = c(1, 1, 1, 0.5), showarrow = FALSE)%>%
  
  layout(font=list(family="Arial", size=18, color="black"), margin=list(t=100))

DistributionOfTE_sankey

