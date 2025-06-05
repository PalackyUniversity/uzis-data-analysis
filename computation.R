

### derived parameters  
  age.max = age.min+9  
  if(age.min==90) age.max = age.min+4
  
  born.min = 2019-age.max
  born.max = 2019-age.min

  if (sex == "M") UT = UT_M[,c("age","mx")]
  if (sex == "F") UT = UT_F[,c("age","mx")]
  names(UT)[1] = "Age.actual"  

##############################################################################
## 1) At_Risk
##############################################################################

### filtering
  # age
    At_risk_sub = At_risk_all %>% filter((Age>=age.min)&(Age<=age.max))
  # sex
    if (sex == "M")   At_risk_sub = At_risk_sub %>% filter(Sex=="M")
    if (sex == "F")   At_risk_sub = At_risk_sub %>% filter(Sex=="F")

### agregation: whole months
  At_risk = At_risk_sub %>% group_by(Mon) %>%
    summarise(Doses_0  = sum(Doses_0),
              Doses_1  = sum(Doses_1),
              Doses_2  = sum(Doses_2),
              Doses_3p = sum(Doses_3) + sum(Doses_4),
              Died     = sum(Died))

##############################################################################
## 2) Deaths
##############################################################################

### filtering
  # age
    Died_sub = Died_all %>% filter((Age>=age.min)&(Age<=age.max))
  # sex
    if (sex == "M")   Died_sub = Died_sub %>% filter(Sex=="M")
    if (sex == "F")   Died_sub = Died_sub %>% filter(Sex=="F")

### agregation: whole months
  Died = Died_sub %>% group_by(Mon) %>%
    summarise(Doses_0  = sum(Doses_0),
              Doses_1  = sum(Doses_1),
              Doses_2  = sum(Doses_2),
              Doses_3p = sum(Doses_3) + sum(Doses_4),
              Died     = sum(Died))
  
##############################################################################
### 3) Joining data and finalizing data.frame Data
##############################################################################

### long format of the data:
  At_risk_long = gather(At_risk,key="status",value="person_days",2:6)
  Died_long    = gather(Died,   key="status",value="Deaths",2:6)

### join information on Risk and Death:
  Data                                 = left_join(At_risk_long,Died_long)
  Data                                 = Data %>% filter(status!="Died")
  
### computing Mortality Rate  
  Data$MortalityRate                   = Data$Deaths / (Data$person_days/365.25) *1000 
  Data$MortalityRate[Data$Deaths<=small.numbers.threshold]    = NaN
  
### foundations for - computing average MR over vaccination groups 
###                 - comparison of MR to MR of unvaccinated  
  Data = Data %>% group_by(Mon) %>% mutate(MR_unvac              = first(MortalityRate),
                                           person_days_AllGroups = sum(person_days),
                                           Deaths_AllGroups      = sum(Deaths) )
  
### computation of - Average MR (over all vaccination groups)
###                - Ratio of MR in a given group to MR of unvaccinated
  Data$Average_mortality_rate = Data$Deaths_AllGroups  /(Data$person_days_AllGroups/365.25) *1000
  Data$MR_ComparedToUnvacc    = Data$MortalityRate     / Data$MR_unvac

### Proportions of groups  
  Data$GroupSize_Share        = Data$person_days       / Data$person_days_AllGroups
  
##############################################################################
  ### 4) Expected mortality rate
##############################################################################

  ### PART 1 - predictions for each age group
    # Computing counts at the New Year 2020, 2021, and 2022
      NewYear     = filter(At_risk_sub,grepl("01-01",Day))
      Predictions = NewYear %>% group_by(Mon,Age) %>% summarise(At_risk_NY = Doses_0+Doses_1+Doses_2+Doses_3+Doses_4)
      di          = dim(Predictions)[1]
      Predictions = add_column(Predictions, Year = rep(2020:2022,each=di/3), .after = 1)
      
    # Age at the New Year 2020, 2021, and 2022   
      Predictions = add_column(Predictions, Age.actual = Predictions$Age+rep(0:2,each=di/3), .after="Age")
    
    # Addition of information on mortality rates from life tables 2019  
      Predictions = left_join(Predictions,UT)
      
    # Computing expected number of deaths  
      Predictions$Expected = Predictions$At_risk_NY * Predictions$mx
    
    # computing 2.5% and 97.5% quantile of 
      Predictions$Lower = qbinom(0.025,Predictions$At_risk_NY,Predictions$mx)
      Predictions$Upper = qbinom(0.975,Predictions$At_risk_NY,Predictions$mx)
  
  ### PART 2 - totals over all age groups (in the given cohort)
      Pred.totals = Predictions %>% group_by(Year) %>% summarise_at(vars(At_risk_NY,Expected,Lower,Upper), sum) 
      Pred.totals.long = gather(Pred.totals[,-2],key="quantity",value="year.total",2:4)
  
  ### PART 3 - predictions for months (expected, lower, upper)    
      Months.predictions = gather( percentages ,key="quantity",value="p",3:5)
      Months.predictions = left_join(Months.predictions,Pred.totals.long)
      Months.predictions = left_join(Months.predictions,Pred.totals[,1:2])
      Months.predictions$count = Months.predictions$year.total*Months.predictions$p

      my.cumsum = function(vec)
        cumsum(c(0,vec))[1:length(vec)]
      
      Months.predictions = Months.predictions %>% group_by(Year,quantity) %>% mutate(died.before=my.cumsum(count))
    
      Months.predictions$At_risk = Months.predictions$At_risk_NY - Months.predictions$died.before
      Months.predictions$Days    = as.numeric(days_in_month(as.Date(paste(Months.predictions$Year,"-",Months.predictions$mon,"-01",sep=""), "%Y-%m-%d")))
      
      Months.predictions$MR = Months.predictions$count / 
                              (Months.predictions$Days*(Months.predictions$At_risk-Months.predictions$count/2)/365.25)*1000
      Months.predictions = add_column(Months.predictions, Mon = factor(paste(Months.predictions$Year,Months.predictions$mon,sep = "-")) ,.after="mon") 

      Months.pred = spread(Months.predictions[,c("Mon","quantity","MR")], quantity,MR)
      
##############################################################################
### 5) Graphics
##############################################################################
  
  columns = grepl("Dose",names(At_risk_sub))|grepl("Died",names(At_risk_sub))    
  Total.nr = sum(At_risk_sub[At_risk_sub$Day=="2020-01-01",columns])
  Died.nr  = sum(   Died_sub[   Died_sub$Day=="2022-12-31",columns])
      
  g2 = ggplot(Data, aes(x=Mon,y=MortalityRate, fill = status))+
    geom_bar(position = "dodge",stat = "identity")+
    ggtitle(paste("All-cause Mortality Rate (Born: ",born.min,"\u2013",born.max,", Sex: ",sex ,")", sep=""))+
    scale_fill_manual(values=color4tuple)+
    xlab("")+
    ylab("Mortality Rate\n Deaths per 1,000 PY")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.1))+
    geom_crossbar( aes(x=Mon, y=Expected , ymin=Lower, ymax=Upper),
                   data=Months.pred, width=1, colour="green3", fill="green3",alpha=0.1, size=0.5)+
    geom_vline(xintercept=c(12.5,24.5), linetype="dashed", color = "black")+
    geom_errorbar(aes(x=Mon,ymin=Average_mortality_rate,ymax=Average_mortality_rate),
                  data=Data,stat="identity", group = 1, linewidth=1)#+

  g1 = ggplot(Data, aes(x=Mon,y=GroupSize_Share, fill = status))+
    geom_bar(position = "stack",stat = "identity")+
    scale_fill_manual(values=color4tuple)+
    scale_y_continuous(labels = scales::percent)+
    xlab("")+
    ylab("Relative Group Size [%]")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.1))+
    geom_vline(xintercept=c(12.5,24.5), linetype="dashed", color = "black")+
    ggtitle(paste("Composition of the cohort (Born: ",born.min,"\u2013",born.max ,", Sex: ",sex,", Size: ",Total.nr,", Died: ",Died.nr,")", sep=""))+
    guides(fill=guide_legend(title="Vaccination status:"))
  
  
  g3 = ggplot(Data, aes(x=Mon,y=MR_ComparedToUnvacc, fill = status))+
    geom_bar(position = "dodge",stat = "identity")+
    scale_fill_manual(values=color4tuple) +
    xlab("")+
    ylab("Mortality Ratio")+
    ggtitle(paste("Relative Mortality Rate Compared to Unvaccinated (Born: ",born.min,"\u2013",born.max,", Sex: ",sex ,")", sep=""))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.1))+
    geom_vline(xintercept=c(12.5,24.5), linetype="dashed", color = "black")
  
  
  Final = ggarrange(g1,g2,g3,
                    ncol=1,nrow=3,
                    common.legend = TRUE, legend="bottom",
                    align ="v")

 
  
