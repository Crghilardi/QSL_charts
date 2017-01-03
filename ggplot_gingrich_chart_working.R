
library(ggplot2)
#Stocking chart
a =c(-0.0507, 0.1698, 0.0317)
b =c(0.175, 0.205, 0.06)

ba_ticks=seq(0,160,20)
tpa_ticks=seq(0,450,50)

#vertical diameter lines
qmd = c(7, 8, 10, 12, 14, 16, 18, 20, 22)

#stocking percent horizontal lines
stk_percent=seq(20,110,10)

amd_convert=function(qmd){
  amd=-0.259+0.973*qmd
  return(amd)
}


all_parms=expand.grid(qmd,stk_percent)
colnames(all_parms)=c("qmd","stk")
all_parms=as.data.frame(all_parms)




make_stklines=function(stk,qmd){ 
  tpa=(1*stk*10)/(a[1]+a[2]*amd_convert(qmd)+a[3]*qmd^2)
  ba=pi*(qmd/24)^2*tpa
return(data.frame(tpa=c(tpa),ba=c(ba)))
} 

sapply(all_parms,make_stklines,stk=all_parms$stk)


horiz_lines=make_stklines(stk=all_parms$stk,qmd=all_parms$qmd)

stk_lines=cbind(all_parms,horiz_lines)

all_parms2=expand.grid(qmd,stk_percent)
colnames(all_parms2)=c("qmd","stk")
all_parms2=as.data.frame(all_parms2)

make_qmdlines=function(qmd,stk){
  tpa= 1*(stk*10)/(a[1]+a[2]*amd_convert(qmd)+a[3]*qmd^2)
  ba=pi*(qmd/24)^2*tpa
  return(data.frame(tpa=c(tpa),ba=c(ba)))
}

vert_lines=make_qmdlines(stk=all_parms2$stk,qmd=all_parms2$qmd)
qmd_lines=cbind(all_parms2,vert_lines)

A_line=stk_lines[stk_lines$stk==100,]




all_parms3=expand.grid(qmd,stk_percent)
colnames(all_parms3)=c("qmd","stk")
all_parms3=as.data.frame(all_parms3)


make_bline=function(qmd){
  tpa= 1*(100*10)/(b[1]+b[2]*amd_convert(qmd)+b[3]*qmd^2)
  ba=pi*(qmd/24)^2*tpa
  return(data.frame(tpa=c(tpa),ba=c(ba)))
}

b_line=make_bline(qmd=qmd)
b_line=cbind(qmd,b_line)


##actual plotting function
GingrichChart=function(TPA=100,BA=60,group=""){
  
  outplot=
    
  ggplot(data=stk_lines,aes(x=tpa,y=ba))+
  geom_path(data=stk_lines,aes(x=tpa,y=ba,group=stk))+
  geom_path(data=qmd_lines,aes(x=tpa,y=ba,group=qmd))+
  geom_path(data=A_line,aes(x=tpa,y=ba,group=stk),color="red")+
  geom_path(data=b_line,aes(x=tpa,y=ba),color="blue")+
  geom_point(x=TPA,y=BA)+
  theme_bw()+
  labs(x="Trees/Acre",y="Basal Area")
  
return(outplot)
}




