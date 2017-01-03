SDI.plot=function(tpa=10, qmd=2,maxSDI=230,group="",color="",type="p",tpa_range=c(1,1200),
dia_range=c(1,40),rdi_lines=c(35,55),units="imperial",psize=1,lsize=1){

maxSDI_intercept=function(maxSDI){
  max_intercept=0.6228898 * log(maxSDI)+ 2.3037567
  return (max_intercept)
}

RDI_intercept=function(rdi,sdi){
  rdi_inter=0.622968800*log(rdi)+0.001697917*(sdi)+2.431415231
  return (rdi_inter)
}

rdi_inters=sapply(rdi_lines,maxSDI,FUN=RDI_intercept)

base_breaksx <- function(n = 10){
    function(x) {
        axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }}

base_breaksy <- function(n = 10){
    function(x) {
        axisTicks(log10(range(x, na.rm = TRUE)), log = TRUE, n = n)
    }}

data=data.frame()#empty df to pass to make aes work

main_text=paste("Max SDI is",maxSDI)

#check length of rdi vector, return a blank if no rdi lines wanted
sub_text=ifelse (length(rdi_lines)>1,
paste("RDI line(s) at",paste("",as.character(rdi_lines),"%",collapse=",",sep="")),"")

#actual plotting function

rdi_trigger=length(rdi_lines)>1
point_trigger=(type=="p")
line_trigger=(type=="l")
both_trigger=(type=="b")

outplot=
  
ggplot(data=data,aes(x=tpa,y=qmd,group=group,color=color))+
geom_blank()+
geom_abline(intercept=maxSDI_intercept(maxSDI),slope=-0.623053,color="red",size=1.3)+
{if (rdi_trigger) geom_abline(intercept=rdi_inters,slope=-0.623053,color="black",size=1.3)}+ #only add rdi_lines layer if rdi_lines vector is populated
{if (point_trigger) geom_point(size=psize)}+ #check for what type of plot wanted: point,line or both
{if (line_trigger) geom_path(size=lsize)}+
{if (both_trigger) c(geom_point(size=psize),geom_path(size=lsize))}+
  scale_x_continuous(trans=log_trans(),breaks=base_breaksx(),limits=c(tpa_range[1],(tpa_range[2]+tpa_range[2]*0.10)))+
scale_y_continuous(trans=log_trans(),breaks=base_breaksy(),limits=c(dia_range[1],(dia_range[2]+dia_range[2]*0.10)))+
theme_bw()+
labs(x="Tree/Acre",y="QMD",title=main_text,subtitle=sub_text)


#spit out finished plot
return(outplot)
}


testing_stand=data.frame(Stand=c(101,101,101,101,101,101,101,102,102,102,102,102,102,102),
  Year=c(1999,
2004,2009,2014,2019,2024,2029,1999,
2004,2009,2014,2019,2024,2029),
  TPA=c(300,300,300,250,175,120,80,700,
700,700,450,375,320,120),
  QMD=c(2.9,4.2,5.9,7.6,8.8,10.2,11.3,
    1.9,2.9,3.4,4.4,5.8,7.2,9.3)
  )

#testing_stand=testing_stand[testing_stand$Stand=="101",]

testing_stand$Stand=as.factor(testing_stand$Stand)
testing_stand$Year=as.factor(testing_stand$Year)


SDI.plot(tpa=testing_stand$TPA,qmd=testing_stand$QMD,type="b",group=testing_stand$Stand,color=testing_stand$Stand,psize=4,lsize=4)
