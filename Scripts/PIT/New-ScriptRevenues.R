
' DO OVDE .TREBA DA SE NAPRAVAAAT NOVI FUNKCII ILI DA SE KOREGIRAAT STARITE FUNKCII ZA DA MOZE DA VADI

 STATITIKA ZA GENDER,
'


columns_to_extract_fun <- c(
                            "Year",
                            "Gender" , 
                            "section" , 
                            "description",
                            "companytype_en",
                             "pitax"
                              )

# Select columns 
selected_columns_tbl <- select(PIT_BU_list$t0, all_of(columns_to_extract_fun))



# BU ----------------------------------------------------------------------


