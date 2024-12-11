'Preparation of Aggregated Revenue Tables
'



# 1. Type of Products ----------------------------------------------------------------------

TypeRev_import_duties_data<-rbind(TypeRev_customs_data,TypeRev_Excise_data,TypeRev_VAT_data)


TypeRev_import_duties_data<-TypeRev_import_duties_data%>%
  group_by(year,TypeOfProducts) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))


TypeRev_import_duties_data<-as.data.table(TypeRev_import_duties_data)


# 2. Chapter ----------------------------------------------------------------------

Chapter_import_duties_agg<-rbind(Chapter_customs_data_agg,Chapter_Excise_data_agg,Chapter_VAT_data_agg)


Chapter_import_duties_agg<-Chapter_import_duties_agg%>%
  group_by(year,Chapter,Chapters_description) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))


Chapter_import_duties_agg<-as.data.table(Chapter_import_duties_agg)

# 3. MTN ----------------------------------------------------------------------

MTN_import_duties_agg<-rbind(MTN_customs_data_agg,MTN_Excise_data_agg,MTN_VAT_data_agg)


MTN_import_duties_agg<-MTN_import_duties_agg%>%
  group_by(year,Product_group) %>%
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE)))


MTN_import_duties_agg<-as.data.table(MTN_import_duties_agg)




# 4 -----------------------------------------------------------------------



