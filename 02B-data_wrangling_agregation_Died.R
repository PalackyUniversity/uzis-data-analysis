### PART 2B
### from UZISdata.Rdata to Died.csv
###############################################################################
### for each Day (1096) Age (1-103) and Sex (2), i.e. 1096*103*2 = 225 776 group
### actual number of people who died in these group are recorded in 6 vaccine-status groups (corresponding to columns) 
### the 6 groups are 0-4 doses (0~unvaccinated) + one group of people who died before
###############################################################################
# author: Ondrej Vencalek
# date: 2024-09-05
###############################################################################

library(dplyr)

### auxiliary value  
  # date 1 day before start of the period  
  start = as.Date("2019-12-31",format="%Y-%m-%d")

### loading the data  
  load("UZISdata.RData")

### Excluding those vaccinated by Janssen
  # print(sum(x.male$OckovaciLatka_1=="COVID-19 Vaccine Janssen"))
  # print(sum(x.female$OckovaciLatka_1=="COVID-19 Vaccine Janssen"))

  x.male   = x.male %>% filter (OckovaciLatka_1!="COVID-19 Vaccine Janssen")
  x.female = x.female %>% filter (OckovaciLatka_1!="COVID-19 Vaccine Janssen")
  
################################################################################        

died.M = x.male   %>% filter(!is.na(Datum.Death) ) 
died.F = x.female %>% filter(!is.na(Datum.Death) ) 

died.list = list()

k = 1
for (sex in c("M","F"))
for (age in 1:103)  
{
  ### filtering of the subgroup from the data
    if (sex=="M") xsub = died.M %>% filter(Age==age)    
    if (sex=="F") xsub = died.F %>% filter(Age==age)    
    
  ### information about the subgroup
    # 1096 rows ~ 1096 individual days (2020-2022)
    # 6 columns ~ 6 vaccination groups (0-4 doses + the last column corresponding to dead people)
  
    M = matrix(ncol=6,nrow=1096)
    for (i in 1:1096)
    {
      h = start+i   # considered day
      
      # nr. of unvaccinated who died:
      pt0 = sum( is.na(xsub$Datum1)&                        ## unvaccinated  
                 (xsub$Datum.Death==h) ,na.rm=TRUE)         ## died that day
      
      # nr. of "1-time vaccinated" who died 
      pt1 = sum( (xsub$Datum1<=h)      &                    ## already vaccinated by 1 dose
                 (is.na(xsub$Datum2))  &                    ## not vaccinated by the 2nd dose
                 (xsub$Datum.Death==h) ,na.rm=TRUE)         ## died that day
      
      # nr. of "2-times vaccinated" who died
      pt2 = sum( (xsub$Datum2<=h)      &                    ## already vaccinated by 2 doses
                 (is.na(xsub$Datum3))  &                    ## not vaccinated by the 3rd dose
                 (xsub$Datum.Death==h) ,na.rm=TRUE)         ## died that day
      
      # nr. of "3-times vaccinated" who died
      pt3 = sum( (xsub$Datum3<=h)      &                    ## already vaccinated by 3 doses 
                 (is.na(xsub$Datum4 )) &                    ## not vaccinated by the 4th dose
                 (xsub$Datum.Death==h) ,na.rm=TRUE)         ## died that day
      
      # nr. of "more than 3-times vaccinated" who died
      pt4 = sum( (xsub$Datum4<=h) &                         ## already vaccinated by 4 doses
                 (xsub$Datum.Death==h) ,na.rm=TRUE)         ## died that day
      
      # nr. of people died before this day (day h)
      pt.d = sum(xsub$Datum.Death<h,na.rm=TRUE )
      
      M[i,] = c(pt0,pt1,pt2,pt3,pt4,pt.d)
    }


died.list[[k]] = M
k = k+1
print(k)
  }

################################################################################        


### Joining data - all age groups (1-103) & both Males and Females
  Died = rbind(died.list[[1]],died.list[[2]])
  for (i in 3:206)
    Died = rbind(Died,died.list[[i]])
  Died = as.data.frame(Died)
  names(Died) = c("Doses_0","Doses_1","Doses_2","Doses_3","Doses_4","Died")

  # day (month), age, sex identifiers:
    Died$Day = rep(seq(as.Date("2020/1/1"), by = "day", length.out = 1096),103*2)
    Died$Mon = strftime(Died$Day,"%Y-%m")
    Died$Age = rep(rep(1:103,each=1096),2)
    Died$Sex = rep(c("M","F"),each=1096*103)


  # reordering of columns:  
    Died = Died[,c(7:10,1:6)]
    
write.csv2(Died,"Died.csv",row.names = FALSE)
 
