library(readxl)

##########################################################################
### Life tables 2019
##########################################################################

  # Male
    UT_M = read_xlsx("data/DEMD003-CR-M.xlsx",skip=6,n_max=106 )
    names(UT_M) = c("age","Dx","Px","mx","qx","lx","dx","Lx","Tx","ex")
    UT_M$age = as.numeric(UT_M$age)
    UT_M = as.data.frame(UT_M)

  # Female  
    UT_F = read_xlsx("data/DEMD003-CR-F.xlsx",skip=6,n_max=106 )
    names(UT_F) = c("age","Dx","Px","mx","qx","lx","dx","Lx","Tx","ex")
    UT_F$age = as.numeric(UT_F$age)
    UT_F = as.data.frame(UT_F)

##########################################################################
### Data on monthly numbers of deaths in 1950-2022
##########################################################################

  ### Reading data  
    deaths0 = as.data.frame(read_xlsx("data/130055230807.xlsx",skip=3))

  ### names of columns    
    names(deaths0) = unlist(lapply(strsplit(names(deaths0),split = "\n"),last))
    names(deaths0)[1:2] = c("Year","Total") 
  
  ### filtering data from years < 2020  
    deaths = deaths0 %>% filter(Year<2020)
    rm(deaths0)
    ### February counts corresponding to not leap year
      deaths$February = deaths$February*(1-1/29*(deaths$Year %%4==0))

    
##########################################################################
  ### Computing proportions of deaths corresponding to months
##########################################################################
  
  ### Raw data:      
  deaths.long = gather(deaths[,-2], key="Month",value = "died",2:13)
  deaths.long$Month = factor(deaths.long$Month,levels=names(deaths)[3:14])  
  # column Total ... Total nr. of deaths in a year
    deaths.long = deaths.long %>% group_by(Year) %>% mutate(Total=sum(died))  
  # column percentage ... percentage of deaths occuring in a given month  
    deaths.long$percentage = deaths.long$died / deaths.long$Total

  ### Estimation of seasonal effect (median and quartiles)  
    prop = deaths.long %>% group_by(Month) %>% summarise(Expected   = median(percentage), 
                                                         Upper = quantile(percentage,0.75),
                                                         Lower = quantile(percentage,0.25) )
    
  ### Estimation of seasonal effect (median and quartiles) in a leap year  
    correction.leap = matrix(1,ncol=3,nrow=12)
    correction.leap[2,] = 29/28 
    corrected = correction.leap * prop[,-1]
    corrected = t(t(corrected)/c(sum(corrected[,1]),1,1))
    prop      = t(t(prop[,-1])/c(sum(prop[,2]),1,1))
    
    ### matrix of seasonal effects for 2020, 2021, and 2022  
    percentages = rbind(corrected,prop,prop)
    
    percentages = data.frame(Year = rep(2020:2022,each=12),
                             mon  = rep(sprintf("%02d", seq(1,12)),3), 
                             percentages)
  
#### VISUALISATIONS:
  #deaths.long$t = (deaths.long$Year-1950)*12+rep(1:12,each=70)
    
  #ggplot(deaths.long, aes(x= t,y=died) )+
  #  geom_line()
  #ggplot(deaths.long, aes(x= t,y=percentage) )+
  #  geom_line() 
  
  