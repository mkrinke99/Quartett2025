
#Erstellen von Ligen & Sonderligen, Farben für Länder

#Daten----------
library(readxl)
df <- read_excel("C:/Nerdzeug_backup/quartett/luxusklasse.xlsx")







#Länderfarben
dx$natcol1<- "black"
dx$natcol2<- "black"
#
dx$natcol1[dx$land==   "A"]<- "red2" 
dx$natcol2[dx$land==   "A"]<- "white"
dx$natcol1[dx$land==   "AUS"]<- "darkgreen" 
dx$natcol2[dx$land==   "AUS"]<- "yellow"
dx$natcol1[dx$land==   "CH"]<- "indianred3" 
dx$natcol2[dx$land==   "CH"]<- "indianred1"
dx$natcol1[dx$land==   "CR"]<- "navy" 
dx$natcol2[dx$land==   "CR"]<- "firebrick3"
dx$natcol1[dx$land==   "D"]<- "black" 
dx$natcol2[dx$land==   "D"]<- "grey44"
dx$natcol1[dx$land==   "DK"]<- "tomato2" 
dx$natcol2[dx$land==   "DK"]<- "grey90"
dx$natcol1[dx$land==   "E"]<- "red3" 
dx$natcol2[dx$land==   "E"]<- "yellow"
dx$natcol1[dx$land==   "F"]<- "navy" 
dx$natcol2[dx$land==   "F"]<- "blue3"
dx$natcol1[dx$land==   "FIN"]<- "lightskyblue3" 
dx$natcol2[dx$land==   "FIN"]<- "white"
dx$natcol1[dx$land==   "GB"]<- "tomato4" 
dx$natcol2[dx$land==   "GB"]<- "tomato2"
dx$natcol1[dx$land==   "I"]<- "green4" 
dx$natcol2[dx$land==   "I"]<- "springgreen3"
dx$natcol1[dx$land==   "J"]<- "red3" 
dx$natcol2[dx$land==   "J"]<- "grey92"
dx$natcol1[dx$land==   "LV"]<- "chocolate4" 
dx$natcol2[dx$land==   "LV"]<- "white"
dx$natcol1[dx$land==   "MLY"]<- "firebrick4" 
dx$natcol2[dx$land==   "MLY"]<- "orangered2"
dx$natcol1[dx$land==   "MRK"]<- "darkgreen" 
dx$natcol2[dx$land==   "MRK"]<- "red"
dx$natcol1[dx$land==   "NL"]<- "orange4" 
dx$natcol2[dx$land==   "NL"]<- "orange"
dx$natcol1[dx$land==   "NZ"]<- "navy" 
dx$natcol2[dx$land==   "NZ"]<- "tomato2"
dx$natcol1[dx$land==   "RC"]<- "firebrick3" 
dx$natcol2[dx$land==   "RC"]<- "yellow"
dx$natcol1[dx$land==   "RO"]<- "blue4" 
dx$natcol2[dx$land==   "RO"]<- "darkgoldenrod2"
dx$natcol1[dx$land==   "RF"]<- "dodgerblue2" 
dx$natcol2[dx$land==   "RF"]<- "white"
dx$natcol1[dx$land==   "SLE"]<- "purple" 
dx$natcol2[dx$land==   "SLE"]<- "dodgerblue1"
dx$natcol1[dx$land==   "SW"]<- "lightskyblue4" 
dx$natcol2[dx$land==   "SW"]<- "yellow"
dx$natcol1[dx$land==   "UAE"]<- "black" 
dx$natcol2[dx$land==   "UAE"]<- "darkgreen"
dx$natcol1[dx$land==   "US"]<- "navy" 
dx$natcol2[dx$land==   "US"]<- "dodgerblue2"


#Liga subsets------
q1<- dx[dx$league== 1 & dx$exp %in% c(0.2,0.5,0.7),]
q2a<- dx[dx$league== 2 & dx$exp %in% c(0.2,0.5,0.7),]
q2<- q2a[!(q2a$name %in% q1$name),]
q3a<- dx[dx$league== 3 & dx$exp %in% c(0.2,0.5,0.7),]
q3<- q3a[!(q3a$name %in% q2$name),]
q2e<- dx[dx$league== 2 & dx$exp > 0.4,]
d<- rbind(q1, q2, q3)



