setwd("C:/Users/Mike/Dropbox/R")
rm(list=ls())

#install.packages("gridExtra")
library(ggplot2)
library(gridExtra)
library(graphics)

data<-read.csv("dat.csv")

#46 m height

subset(data, subset=(h =="46m")) -> data.46
v46<-ggplot(data.46, aes(x=v)) + geom_histogram(aes(y=..density..),alpha=0.3,binwidth=1.0,colour="black",fill="red") 
v46<-v46+scale_y_continuous(limits = c(0, 0.2))
data.46dlist <- ggplot_build(v46)
data.46d<-data.46dlist$data[[1]]
v46d=data.frame(data.46d$x,data.46d$y)
names(v46d)[names(v46d) == 'data.46d.x'] <- 'v'
names(v46d)[names(v46d) == 'data.46d.y'] <- 'y'
rm(data.46dlist,data.46d)


A=6.8
k=2.6
fit=nls(data=v46d,y~dweibull(v,scale=A46,shape=k46),start=list(A46=A,k46=k),trace=T)
summary(fit)

A46<-summary(fit)$parameters[1,1]
k46<-summary(fit)$parameters[2,1]
sdA46<-summary(fit)$parameters[1,2]
sdk46<-summary(fit)$parameters[2,2]

meanv46=A46*gamma(1/k46+1)

meanv46<-signif(meanv46,digits=3)
A46<-signif(A46,digits=3)
k46<-signif(k46,digits=3)
sdA46<-signif(sdA46,digits=2)
sdk46<-signif(sdk46,digits=2)

v46<-v46 + stat_function(fun = dweibull, arg = list(scale = A46,shape=k46))
v46<-v46+xlab(expression(italic(v)~"(m/s)")) + ylab("probability") + ggtitle(expression(italic(h)~" = 46 m"))

meanv46lab<-paste0(substitute(expression(bar(italic(v))==A~"m/s"),list(A=meanv46)))[2]
A46lab<-paste0(substitute(expression(italic(A)==B~"\u00b1"~C),list(B=A46,C=sdA46)))[2]
k46lab<-paste0(substitute(expression(italic(k)==D~"\u00b1"~E),list(D=k46,E=sdk46)))[2]

v46<-v46+annotate("text",x=12,y=0.17,parse=T,label=as.character(meanv46lab),hjust=0)
v46<-v46+annotate("text",x=12,y=0.16,parse=T,label=as.character(A46lab),hjust=0)
v46<-v46+annotate("text",x=12,y=0.15,parse=T,label=as.character(k46lab),hjust=0)


#32 m height

subset(data, subset=(h =="32m")) -> data.32
v32<-ggplot(data.32, aes(x=v)) + geom_histogram(aes(y=..density..),alpha=0.3,binwidth=1.0,colour="black",fill="blue") 
v32<-v32+scale_y_continuous(limits = c(0, 0.2))
data.32dlist <- ggplot_build(v32)
data.32d<-data.32dlist$data[[1]]
v32d=data.frame(data.32d$x,data.32d$y)
names(v32d)[names(v32d) == 'data.32d.x'] <- 'v'
names(v32d)[names(v32d) == 'data.32d.y'] <- 'y'
rm(data.32dlist,data.32d)


A=6
k=2.5
fit=nls(data=v32d,y~dweibull(v,scale=A32,shape=k32),start=list(A32=A,k32=k),trace=T)
summary(fit)

A32<-summary(fit)$parameters[1,1]
k32<-summary(fit)$parameters[2,1]
sdA32<-summary(fit)$parameters[1,2]
sdk32<-summary(fit)$parameters[2,2]

meanv32=A32*gamma(1/k32+1)

meanv32<-signif(meanv32,digits=3)
A32<-signif(A32,digits=3)
k32<-signif(k32,digits=3)
sdA32<-signif(sdA32,digits=2)
sdk32<-signif(sdk32,digits=2)

v32<-v32 + stat_function(fun = dweibull, arg = list(scale = A32,shape=k32))
v32<-v32+xlab(expression(italic(v)~"(m/s)")) + ylab("probability") + ggtitle(expression(italic(h)~" = 32 m"))

meanv32lab<-paste0(substitute(expression(bar(italic(v))==A~"m/s"),list(A=meanv32)))[2]
A32lab<-paste0(substitute(expression(italic(A)==B~"\u00b1"~C),list(B=A32,C=sdA32)))[2]
k32lab<-paste0(substitute(expression(italic(k)==D~"\u00b1"~E),list(D=k32,E=sdk32)))[2]

v32<-v32+annotate("text",x=12,y=0.17,parse=T,label=as.character(meanv32lab),hjust=0)
v32<-v32+annotate("text",x=12,y=0.16,parse=T,label=as.character(A32lab),hjust=0)
v32<-v32+annotate("text",x=12,y=0.15,parse=T,label=as.character(k32lab),hjust=0)

#20 m height

subset(data, subset=(h =="20m")) -> data.20
v20<-ggplot(data.20, aes(x=v)) + geom_histogram(aes(y=..density..),alpha=0.3,binwidth=1.0,colour="black",fill="green") 
v20<-v20+scale_y_continuous(limits = c(0, 0.2))
data.20dlist <- ggplot_build(v20)
data.20d<-data.20dlist$data[[1]]
v20d=data.frame(data.20d$x,data.20d$y)
names(v20d)[names(v20d) == 'data.20d.x'] <- 'v'
names(v20d)[names(v20d) == 'data.20d.y'] <- 'y'
rm(data.20dlist,data.20d)


A=5.5
k=2.4
fit=nls(data=v20d,y~dweibull(v,scale=A20,shape=k20),start=list(A20=A,k20=k),trace=T)
summary(fit)

A20<-summary(fit)$parameters[1,1]
k20<-summary(fit)$parameters[2,1]
sdA20<-summary(fit)$parameters[1,2]
sdk20<-summary(fit)$parameters[2,2]

meanv20=A20*gamma(1/k20+1)

meanv20<-signif(meanv20,digits=3)
A20<-signif(A20,digits=3)
k20<-signif(k20,digits=3)
sdA20<-signif(sdA20,digits=2)
sdk20<-signif(sdk20,digits=2)

v20<-v20 + stat_function(fun = dweibull, arg = list(scale = A20,shape=k20))
v20<-v20+xlab(expression(italic(v)~"(m/s)")) + ylab("probability") + ggtitle(expression(italic(h)~" = 20 m"))

meanv20lab<-paste0(substitute(expression(bar(italic(v))==A~"m/s"),list(A=meanv20)))[2]
A20lab<-paste0(substitute(expression(italic(A)==B~"\u00b1"~C),list(B=A20,C=sdA20)))[2]
k20lab<-paste0(substitute(expression(italic(k)==D~"\u00b1"~E),list(D=k20,E=sdk20)))[2]

v20<-v20+annotate("text",x=12,y=0.17,parse=T,label=as.character(meanv20lab),hjust=0)
v20<-v20+annotate("text",x=12,y=0.16,parse=T,label=as.character(A20lab),hjust=0)
v20<-v20+annotate("text",x=12,y=0.15,parse=T,label=as.character(k20lab),hjust=0)


grid.arrange(v20,v32,v46,ncol=3)

rm(data)

