
#packages----------
library(tictoc)
library(ggpubr)
library(factoextra)
library(vegan)
library(stats)


#win percentage für Liga 2
q2w1<- wrel(q2a)

#optimiert
q2w<- wrel_opt(q2w1)

#doppelt optimiert
q2w2<- wrel_opt2(q2w)



#Liga 2
#50 Turniere simulieren
t50<- turnier64(q2, 50, 1238)
t50nat<- merge(t50, q2[c("name","land")], by= "name")
t50by_nat<-  aggregate(t50nat$wins, list(t50nat$land), sum)
t50by_nat<- t50by_nat[order(t50by_nat[,2], decreasing = T),]
t50by_nat



q2ww<- merge(q2w2, t5000, by= "name")
q2ww<- q2ww[order(q2ww$wrel_opt2, decreasing= T),]
q2ww


lo1<- loess(q2ww$wins ~ q2ww$wrel)
lo2<- loess(q2ww$wins ~ q2ww$wrel_opt)
lo3<- loess(q2ww$wins ~ q2ww$wrel_opt2)

