### PART 2A
### from UZISdata.Rdata to At_Risk.csv
###############################################################################
### for each Day (1096) Age (1-103) and Sex (2), i.e. 1096*103*2 = 225 776 group
### actual number of people in these group are recorded in 6 vaccine-status groups (corresponding to columns) 
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

at.risk.list = list()

k = 1
for (sex in c("M","F"))
for (age in 1:103)  
{
  ### filtering of the subgroup from the data
    if (sex=="M") xsub = x.male   %>% filter(Age==age)    
    if (sex=="F") xsub = x.female %>% filter(Age==age)    
    
  ### information about the subgroup
    # 1096 rows ~ 1096 individual days (2020-2022)
    # 6 columns ~ 6 vaccination groups (0-4 doses + the last column corresponding to dead people)
  
    M = matrix(ncol=6,nrow=1096)
    for (i in 1:1096)
    {
      h = start+i   # considered day
      
      # nr. of unvaccinated at risk:
      pt0 = sum( (is.na(xsub$Datum1)|(xsub$Datum1>h))  &                        ## unvaccinated at all or vaccinated later 
                 (is.na(xsub$Datum.Death )| (xsub$Datum.Death>=h)),na.rm=TRUE)  ## alive at the end or died later
      
      # nr. of "1-time vaccinated" at risk 
      pt1 = sum( (xsub$Datum1<=h) &                                             ## already vaccinated by 1 dose
             (is.na(xsub$Datum2 )|(xsub$Datum2>h ))&                            ## not vaccinated by the 2nd dose at all or later
             (is.na(xsub$Datum.Death )|(xsub$Datum.Death>=h )),na.rm=TRUE)      ## alive at the end or died later
      
      # nr. of "2-times vaccinated" at risk
      pt2 = sum( (xsub$Datum2<=h) &                                             ## already vaccinated by 2 doses
             (is.na(xsub$Datum3 )|(xsub$Datum3>h ))&                            ## not vaccinated by the 3rd dose at all or later
             (is.na(xsub$Datum.Death )|(xsub$Datum.Death>=h )),na.rm=TRUE)      ## alive at the end or died later
      
      # nr. of "3-times vaccinated" at risk
      pt3 = sum( (xsub$Datum3<=h) &                                             ## already vaccinated by 3 doses 
             (is.na(xsub$Datum4 )|(xsub$Datum4>h ))&                            ## not vaccinated by the 4th dose at all or later
             (is.na(xsub$Datum.Death )|(xsub$Datum.Death>=h )),na.rm=TRUE)      ## alive at the end or died later
      
      # nr. of "more than 3-times vaccinated" at risk
      pt4 = sum( (xsub$Datum4<=h) &                                             ## already vaccinated by 4 doses
             (is.na(xsub$Datum.Death )|(xsub$Datum.Death>=h )),na.rm=TRUE)      ## alive at the end or died later
      
      # nr. of people died before this day (day h)
      pt.d = sum(xsub$Datum.Death<h,na.rm=TRUE )
      
      M[i,] = c(pt0,pt1,pt2,pt3,pt4,pt.d)
    }

#Mdf = as.data.frame(M)
#names(Mdf) = c("Doses_0","Doses_1","Doses_2","Doses_3","Doses_4","Died")

at.risk.list[[k]] = M
k = k+1
print(k)
  }

################################################################################        


### Joining data - all age groups (1-103) & both Males and Females
  At.risk = rbind(at.risk.list[[1]],at.risk.list[[2]])
  for (i in 3:206)
    At.risk = rbind(At.risk,at.risk.list[[i]])
  At.risk = as.data.frame(At.risk)
  names(At.risk) = c("Doses_0","Doses_1","Doses_2","Doses_3","Doses_4","Died")

  # day (month), age, sex identifiers:
    At.risk$Day = rep(seq(as.Date("2020/1/1"), by = "day", length.out = 1096),103*2)
    At.risk$Mon = strftime(At.risk$Day,"%Y-%m")
    At.risk$Age = rep(rep(1:103,each=1096),2)
    At.risk$Sex = rep(c("M","F"),each=1096*103)


  # reordering of columns:  
    At.risk = At.risk[,c(7:10,1:6)]
    
write.csv2(At.risk,"At_Risk.csv",row.names = FALSE)
 
