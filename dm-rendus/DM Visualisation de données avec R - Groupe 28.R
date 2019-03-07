rm(list=ls())
library(dplyr)
library(lazyeval)
library(ggplot2)
library(gridExtra)
library(cowplot)
#Création des données
data <- data.frame(a=c(4855000,1594000),b=1:2,legend1=c('Never or Physically unable', 'At least once'))
data2<-data.frame(l=c(778000,272000,544000),b=1:3,legend2=c('A few times per year','At least once a month','At least several times a month'))
x1<-data[1,1]/(data[1,1]+data[2,1])
x2<-data[2,1]/(data[1,1]+data[2,1])
x3<-data2[1,1]/(data2[1,1]+data2[2,1]+data2[3,1])
x4<-data2[2,1]/(data2[1,1]+data2[2,1]+data2[3,1])
x5<-data2[3,1]/(data2[1,1]+data2[2,1]+data2[3,1])
per<-c(x1,x2)
data<-data.frame(data,per)
data$label <- scales::percent(data$per)
per2<-c(x3,x4,x5)
data2<-data.frame(data2,per2)
data2$label <- scales::percent(data2$per)

#Graphe
#Premier pie
p1<-ggplot(data=data)+
  geom_bar(aes(x="", y=per, fill=legend1),stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x = 1, 
                y = cumsum(per)-per/2, label=paste(legend1,"\n",label)),color="white")
p1<-p1 + theme(legend.position = "none")#Masquer la légende
p1<-p1+ggtitle("Population of adult New Yorkers")
#Deuxième pie
p2<-ggplot(data=data2)+
  geom_bar(aes(x="", y=per2, fill=l), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x = 1, 
                y = cumsum(per2)-per2/3 , label=paste(legend2,"\n",label)), color="white")#Mettre la légende sur le graphe+la mettre en une couleur évidente
p2<-p2 + theme(legend.position = "none")#Masquer la légende
p2<-p2+ggtitle("Population of adult New Yorkers who ride a Bike")

#
a<-plot_grid(p1,p2, nrow=1, align="h")#Cumuler les deux graphes
#Ajouter titre+sous titre en utilisant la fonction plot_grid aussi
title_gg <- ggplot() + 
  labs(title = "Number of cyclists in 2014", subtitle = "Percent of Adult New Yorkers who ride a Bike(NYC DOHMH)") + 
  theme_void()
b<-plot_grid(title_gg, a, ncol = 1, rel_heights = c(0.2, 2))
d<- add_sub(b, "Community health survey population  estimate 6.45 millions adult New Yorkers")
ggdraw(d)
