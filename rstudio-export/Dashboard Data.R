# install.packages('readxl')
library(dplyr)
library(readxl)
rm(list = ls())
raw_data = 
  readxl::read_xlsx(path = 'Dummy RWA Data.xlsx',
                    sheet = 'NewData1')

length(names(raw_data))

types = rep('text',12)

mrtg_data = 
  data.frame(readxl::read_xlsx(path = 'Dummy RWA Data.xlsx',
                               sheet = 'NewData1',
                               col_types = types),
             stringsAsFactors = FALSE)

dataset_types = data.frame(sapply(mrtg_data, class))

fix_types = c("i","j","Lookup",'Month',"Stage.Mapper","Exposure","ECL","PD","LGD")
keep_type = names(mrtg_data)[!(names(mrtg_data) %in% fix_types)]
fix_months = distinct(data.frame(mrtg_data$Month,stringsAsFactors = FALSE))


mrtg_data_fixed = 
  data.frame(
    lapply(names(mrtg_data), function(x){
    if (x %in% fix_types) {
      print('1')
      print(x)
      x = as.numeric(mrtg_data[,x])
      }else{
        print('2')
        print(x)
        x = as.character(mrtg_data[,x])
        }}),
    stringsAsFactors = FALSE)

names(mrtg_data_fixed) = names(mrtg_data)

mrtg_data_fixed = 
  mrtg_data_fixed %>% 
  mutate(Month = factor(mrtg_data$Month, levels = fix_months$mrtg_data.Month, ordered = TRUE))
View(mrtg_data_fixed)

historical_data = mrtg_data_fixed
save(historical_data, file = 'historical_data.rdata')


