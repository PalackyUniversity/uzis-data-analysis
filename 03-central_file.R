### used packages:
library(tidyverse)
library(ggplot2)
library(ggpubr)

### reading data 
At_risk_all = read.csv2("At_Risk.csv")
Died_all    = read.csv2("Died.csv")

source("umrtnostni_tabulky.R")

### input parameters  
color4tuple = rev(c("deepskyblue1","blue","blue4","red"))
small.numbers.threshold = 10   # musi byt > small.numbers.threshold

for (age.min in seq(from=40,to=90,by=10))
for (sex in c("F", "M"))
     {

source("computation.R")

if (sex=="F")
  {name.of.file.png = paste("ACM-figures/group_born",born.min,"-",born.max,"-F.png",sep="")
   name.of.file1    = paste("Source-data/group_born",born.min,"-",born.max,"-F-data.csv",sep="")
   name.of.file2    = paste("Source-data/group_born",born.min,"-",born.max,"-F-pred.csv",sep="")
  }
if (sex=="M")
  {name.of.file.png = paste("ACM-figures/group_born",born.min,"-",born.max,"-M.png",sep="")
   name.of.file1    = paste("Source-data/group_born",born.min,"-",born.max,"-M-data.csv",sep="")
   name.of.file2    = paste("Source-data/group_born",born.min,"-",born.max,"-M-pred.csv",sep="")
  }

write.csv(Data,               name.of.file1,row.names = FALSE)
write.csv(Months.predictions, name.of.file2,row.names = FALSE)
write.csv2(Data,               paste("Source-data/CZ-",name.of.file1,sep=""),row.names = FALSE)
write.csv2(Months.predictions, paste("Source-data/CZ-",name.of.file2,sep=""),row.names = FALSE)
#png(name.of.file.png,res=1920,pointsize=12,height=20000,width=22000)
#Final
#dev.off()
}


