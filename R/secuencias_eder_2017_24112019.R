

rm(list=ls(all=TRUE)); options(warn=-1)
options(survey.lonely.psu="adjust")

library(devtools)
library(stats)
library(devtools)
library(RCurl)
library(foreign)
library(survey)
library(questionr)
library(TraMineR)
library(cluster)
library(dplyr)
library(foreign)

setwd("C:/Users/jmartinez/Desktop/JC/OneDrive - El Colegio de México A.C/2. CM/CM_1/EDER_2017/eder2017_bases_csv") 
eder <- read.csv("historiavida.csv", header = TRUE)
persona <- read.csv("persona.csv", header = TRUE)
viv <- read.csv("vivienda.csv", header = TRUE)

# Se crea la variable "sect" en donde se capta el sector de actividad
# de acuerdo al SCIAN

eder$scian<-as.numeric(as.character(eder$scian))
eder$scian[is.na(eder$scian)]<- 0
eder$scian_cat<-substr(eder$scian,1,1)
prop.table(table(eder$scian_cat,exclude = NULL ))

eder$sect <- ifelse(eder$scian_cat==1, 1, 
                    ifelse(eder$scian_cat==2, 2, 
                           ifelse(eder$scian_cat>2, 3, 0)))

prop.table(table(eder$sect,exclude = NULL ))


# TRABAJO
# Variable: pos_tra       Puesto en trabajo.
#       0 No laboraba
#       1 Patrón
#       2 Trabajador por su cuenta
#       3 Trabajador a sueldo fijo, salario o jornal
#       4 Trabajador a destajo, porcentaje o comisión
#       5 Trabajador sin pago
#       6 Otro
#       9 No sabe

eder$trabajo<-as.numeric(as.character(eder$trabajo))
eder$pos_tra<-as.numeric(as.character(eder$pos_tra))
eder$tra <- ifelse(eder$pos_tra==0 | eder$pos_tra==9,0,ifelse(eder$pos_tra==1,1,ifelse(eder$pos_tra==2,2,ifelse(eder$pos_tra>2 & eder$pos_tra<9,3,0))))
prop.table(table(eder$tra,exclude = NULL))

#Rangos:
#0<-no trabaja
#1<-patrón 
#2<-cuenta propia
#3<-subordinado

#Creación de las trayectorias

eder$tray <- ifelse(eder$tra==0,1,
                    ifelse(eder$tra==1 & eder$sect==1,2,
                           ifelse(eder$tra==2 & eder$sect==1,3,
                                  ifelse(eder$tra==3 & eder$sect==1,4,
                                    ifelse(eder$tra==1 & eder$sect==2,5,
                                         ifelse(eder$tra==2 & eder$sect==2,6,
                                                ifelse(eder$tra==3 & eder$sect==2,7,
                                                       ifelse(eder$tra==1 & eder$sect==3,8,
                                                              ifelse(eder$tra==2 & eder$sect==3,9,
                                                                     ifelse(eder$tra==3 & eder$sect==3,10,0))))))))))



                    

# Vamos a quedarnos con aquellos que entraron al trabajo antes de los 15 años
# Base nueva: eder.micro

#Identificamos la edad mínima a la que ingresó

eder.m<-eder[which(eder$tray>1),] %>% group_by(ï..folioviv,foliohog,id_pobla)%>% summarize(min_edad = min(edad_retro))
eder.m<-eder.m[which(eder.m$min_edad<=15),]
eder.m$select<-1
     

eder.15<-merge(eder, eder.m, all.x=TRUE,by=c("ï..folioviv","foliohog","id_pobla"))
eder.15 <- eder.15[order(eder.15$ï..folioviv,eder.15$foliohog,eder.15$id_pobla,eder.15$edad_retro),]
View( eder.15[which(eder.15$select==1), c("ï..folioviv","foliohog","id_pobla","edad_retro","tra","sect","tray", "min_edad","select")]) 

var<-c("ï..folioviv","foliohog","id_pobla","edad_retro","tray")
eder.micro<-eder.15[which(eder.15$select==1 & eder.15$edad_act>=35),var]

#***** Trayectorias                                                                                                         

attach(eder.micro)
eder.micro <- eder.micro[order(ï..folioviv, foliohog,id_pobla,edad_retro),] 
detach(eder.micro)

eder.micro.wide<-reshape(eder.micro, idvar = c("ï..folioviv","foliohog","id_pobla"), timevar = "edad_retro", direction = "wide")
View(eder.micro.wide)
mvad.labels <- c("No Trabaja","PRIM-1","PRIM-2","PRIM-3","SEC-1","SEC-2","SEC-3","TER-1","TER-2","TER-3")
mvad.scode <- c(1,2,3,4,5,6,7,8,9,10)
mvad.seq <- seqdef(eder.micro.wide, 4:34, states = mvad.scode,labels = mvad.labels, xtstep=10)
seqdplot(mvad.seq, withlegend = F, border = NA, title = "Gráfica")

seqlegend(mvad.seq, cex = 1,position = "topleft")
seqHtplot(mvad.seq, title = "Entropy index")

submat <- seqsubm(mvad.seq, method = "TRATE", with.missing=FALSE)
dist.om1 <- seqdist(mvad.seq, method = "OM", indel = 1,sm = submat, with.missing=TRUE)

clusterward1 <- agnes(dist.om1, diss = TRUE, method = "ward")

cl1.4 <- cutree(clusterward1, k = 4)
cl1.4fac <- factor(cl1.4, labels = paste("Type", 1:4))
seqdplot(mvad.seq, group = cl1.4fac)
seqfplot(mvad.seq, group = cl1.4fac, border = NA)

eder.cl4<-data.frame(eder.micro.wide[,1:3],cl1.4)
eder.cl4.all<-merge(eder.cl4, persona, all.x=TRUE,by=c("ï..folioviv","foliohog","id_pobla"))


#Precisiones estadísticas


v1<-c("ï..folioviv","ubica_geo", "tam_loc","est_socio","est_dis","upm","factor")

eder.cl4.all<-merge(eder.cl4.all,viv[,v1],all.x=TRUE,by=c("ï..folioviv"))
names(eder.cl4.all)
write.csv(eder.cl4.all,"C:/Users/jmartinez/Desktop/JC/OneDrive - El Colegio de México A.C/2. CM/CM_1/EDER_2017/eder2017_bases_csv/eder_4_all.csv",sep=",")


