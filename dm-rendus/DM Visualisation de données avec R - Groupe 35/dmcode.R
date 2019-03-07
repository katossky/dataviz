library(xlsx) #import des packages
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(scales)


unempl <- read.csv2("P:/dataviz/unempl.csv",header = T)  #import des données
row.names(unempl) <- unempl$Year #changement des index des lignes
unempl<- unempl[-61,]   #suppression d'une ligne parasite

unempl$Year <- mdy(unempl$Year) #conversion au format date mois/jours/annee



options(scipen=999)  #  desactivation de la notation scientifique


a<- ggplot(unempl, aes(x = Year, y = Unemployment,group=1))+
  geom_point(aes(y=Unemployment))+ #affichage des points
  geom_line(aes(y=Unemployment))+ #affichage de la courbe associee aux points
  theme_minimal()+ #modification du theme
  ylim(8000,17000)+ #initialisation de l'echelle d'un nombre de chomeurs pour eviter une vision 
  #trop grande des variation 
  scale_x_date(date_breaks = "4 month",labels=date_format("%m-%Y"))+ #modification de l'echelle des mois 
  #pour afficher tout les 4 mois au format mois-annee
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ #affichage des dates avec un leger angle 
  #pour meilleure visibilite
  labs(x = "Mois", y = "Nombre de chomeurs en milliers", 
       title = "Aux Etats-Unis, le nombre de chomeurs est en diminution depuis 2008 ",
       subtitle = 'Evolution du nombre de chomeurs aux Etats-Unis de 2007 à 2012 ',
       caption="Sources : Bureau of Labor Statistics (Current Population Survey - CPS)"
       ) #formatage du titre, sous titre, nom des axes et sources
  
print(a)  #affichage


