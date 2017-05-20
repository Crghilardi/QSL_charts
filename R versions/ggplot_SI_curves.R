library(ggplot2)

 
#Red Maple - Carmean 
Age=seq(20,100,10)
Ht=seq(20,120,10)
site=seq(20,120,10)
BH=0

all_parms=expand.grid(site,Age)
colnames(all_parms)=c("SI","Age")
all_parms=as.data.frame(all_parms)

SI=function(ht,age){
  si=(0+(0.3263*(ht^1.0634))*(1-(exp(-0.0106*age)))^(-1.2573*(ht^-0.0646)))
  return(si)
}

H=function(si,age){
 h=(0+(2.9435*(si^0.9132))*(1-(exp(-0.0141*age)))^(1.6580*(si^-0.1095))) 
}

SI(50,50)
SI(25,50)

rm=sapply(all_parms,H,age=all_parms$Age)


colnames(rm)=c("SI_ht","SI_age")
rm2=cbind(all_parms,rm)


ggplot(data=rm2,aes(x=SI_age,y=SI_ht,group=SI))+
  geom_point()+
  geom_line() +
  scale_x_continuous(breaks=Age)+
  scale_y_continuous(breaks=Ht)



