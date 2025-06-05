### PART 1
### from the original csv file to the Rdata format file (UZISdata.Rdata)
###############################################################################
### new colums:
# 1) Month.Death (month of death)
# 2) Cat.vac.when.died (vaccination status when died)
# 3) Age
###############################################################################
# author: Ondrej Vencalek
# date: 2024-09-05
###############################################################################
library(dplyr)

x = read.csv("data/Vesely_106_202403141131.csv")

### DATES (format):
### Date of the 1st, 2nd, ... 7th dose - format "date"
  x$Datum1 = as.Date(x$Datum_1,format="%Y-%m-%d")
  x$Datum2 = as.Date(x$Datum_2,format="%Y-%m-%d")
  x$Datum3 = as.Date(x$Datum_3,format="%Y-%m-%d")
  x$Datum4 = as.Date(x$Datum_4,format="%Y-%m-%d")
  x$Datum5 = as.Date(x$Datum_5,format="%Y-%m-%d")
  x$Datum6 = as.Date(x$Datum_6,format="%Y-%m-%d")
  x$Datum7 = as.Date(x$Datum_7,format="%Y-%m-%d")
### date of death 
  x$Datum.Death = as.Date(x$DatumUmrti,format="%Y-%m-%d")

### month of death  
  x$Month.Death = strftime(x$Datum.Death,"%y/%m")

### vaccination status when died (NA if is alive)
  x$Cat.vac.when.died = pmax(0,
                1*(x$Datum1 <= x$Datum.Death),
                2*(x$Datum2 <= x$Datum.Death), 
                3*(x$Datum3 <= x$Datum.Death), 
                4*(x$Datum4 <= x$Datum.Death), 
                5*(x$Datum5 <= x$Datum.Death),
                6*(x$Datum6 <= x$Datum.Death), 
                7*(x$Datum7 <= x$Datum.Death), na.rm = TRUE ) 
  x$Cat.vac.when.died[is.na(x$Datum.Death)] = NA

### age at the beginning of 2020 
  x$Age = 2019-x$Rok_narozeni

  
### SPLIT OF THE DATA (for faster computation)  
  x.male   = x %>% filter(Pohlavikod=="M")
  x.female = x %>% filter(Pohlavikod=="F")

   # print(dim(x)[1])
   # print(c(dim(x.male)[1],dim(x.female)[1],dim(x.male)[1]+dim(x.female)[1],dim(x)[1]-dim(x.male)[1]-dim(x.female)[1]))
  rm(x)
  
  save.image("UZISdata.RData")
