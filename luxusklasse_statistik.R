
#packages----------
library(tictoc)
library(ggpubr)
library(factoextra)
library(vegan)
library(stats)


#plotte Siegesquote x Wins
par(mfrow= c(1,1), lwd=1)
plot(q2ww$wrel, q2ww$wins,
     col= q2ww$natcol1, pch= 16, cex= 1.5,
     main= "relwin_opt2 ~ wins", xlab= "relwin", ylab= "wins [5000 runs]",
     las= 1, ylim= c(0, 600), xlim= c(0,1))
lines(sort(q2ww$wrel), predict(lo1)[order(q2ww$wrel)],
      col='grey66', lwd=2)
points(q2ww$wrel, q2ww$wins,col= q2ww$natcol2, pch= 16, cex= 0.8)
text(q2ww$wrel, q2ww$wins+5, q2ww$name, col= q2ww$natcol1, cex= 0.65)

plot(q2ww$wrel_opt, q2ww$wins,
     col= q2ww$natcol1, pch= 16, cex= 1.5,
     main= "relwin_opt ~ wins", xlab= "relwin_opt", ylab= "wins [5000 runs]",
     las= 1,  ylim= c(0, 600), xlim= c(0,1))
lines(q2ww$wrel_opt, predict(lo2), col='grey66', lwd=2)
points(q2ww$wrel_opt, q2ww$wins,col= q2ww$natcol2, pch= 16, cex= 0.8)
#text(q2ww$wrel_opt, q2ww$wins+5, q2ww$name, col= q2ww$natcol1, cex= 0.65)

plot(q2ww$wrel_opt2, q2ww$wins,
     col= q2ww$natcol1, pch= 16, cex= 1.5,
     main= "relwin_opt2 ~ wins", xlab= "relwin_opt2", ylab= "wins [5000 runs]",
     las= 1, ylim= c(0, 600), xlim= c(0,1))
lines(q2ww$wrel_opt2, predict(lo3), col='grey66', lwd=2)
points(q2ww$wrel_opt2, q2ww$wins,col= q2ww$natcol2, pch= 16, cex= 0.8)
#text(q2ww$wrel_opt2, q2ww$wins+5, q2ww$name, col= q2ww$natcol1, cex= 0.65)






#barplot win percentage-----
par(mar= c(8,4,4,1), lwd= 3)
b2<-barplot(d$winp, names = d$name, 
            las= 2, cex.names= 0.8, ylim= c(0,max(d$winp*1.1)), 
            col= d$natcol2, border= d$natcol1)
text(b2, d$winp +0.01, d$winp, pos= 3, cex= 0.6)



#optimized win_perc----

#function taking opponent strength into account


relwin_opt1<- vector()
for(i in 1:nrow(d)){
    w= data.frame(name= d$name[i], win= 0)
    opp<- 1:nrow(d)
    opp<- opp[!opp %in% i]
    for(j in opp){
      w<- rbind(w, fight3(i,j))
    }
    relwin_opt1[i]<- round(mean(w[,2]),3)
  }
relwin_opt<- relwin_opt1* 0.5/mean(relwin_opt1)
d$winp_opt<- relwin_opt




#wins~winp----
lo <- loess(d$wins ~ d$winp)
d_opt<- d[order(d$winp_opt, decreasing = T),]
lo2<- loess(d_opt$wins~d_opt$winp_opt)

par(lwd=1, mfrow= c(1,2))
#plot1: relperc~win
plot(d$winp, d$wins, col= d$natcol1, pch= 16, cex= 1.5,
     las=1, xlab= "win percentage", ylab= "wins", 
     xlim= c(min(d$winp, d$winp_opt)*0.95, max(d$winp, d$winp_opt)*1.05),
     ylim= c(0, max(d$wins)*1.1), main= "win percentage~wins")
points(d$winp, d$wins, col= d$natcol2, pch= 16, cex= 0.8)
lines(d$winp, predict(lo), col='grey66', lwd=2)
text(d$winp, d$wins+3.2, d$name, cex= 0.6, col= "grey33")
text(0.3, max(d$wins)-9, paste0("mean error: ", 
     round(mean(abs(d$wins - predict(lo))), 2)), cex= 0.7)
#plot2: with optimized winp
plot(d_opt$winp_opt, d_opt$wins, col= d_opt$natcol1, pch= 16, cex= 1.5,
     las=1, xlab= "optimized win percentage", ylab= "wins",
     xlim= c(min(d$winp, d$winp_opt)*0.95, max(d$winp, d$winp_opt)*1.05),
     ylim= c(0, max(d$wins)*1.1),  main= "optimized win percentage~wins")
points(d_opt$winp_opt, d_opt$wins, col= d_opt$natcol2, pch= 16, cex= 0.8)
lines(d_opt$winp_opt, predict(lo2), col='grey66', lwd=2)
text(d_opt$winp_opt, d_opt$wins+3.2, d_opt$name, cex= 0.6, col= "grey33")
text(0.3, max(d$wins)-9, paste0("mean error: ", 
                      round(mean(abs(d_opt$wins - predict(lo2))), 2)), cex= 0.7)
#mean error 7.42 -> 3.59 (5.5 when winp not squarred)


#q2 elite-----
q2ee<- q2e[!(q2e$name %in% c("Bentley Nokian",
                             "Bentley Speed",
                             "Ferrari 599",
                             "Mercedes SLS GT3",
                             "Porsche Carrera",
                             "A-Ma DB11",
                             "Lambo Diablo",
                             "McLaren MP4")),]
el<- wrel(q2ee)
el$expcol<- "navy"
el[el$exp== 0.7,]$expcol<- "dodgerblue3"
el[el$exp== 0.5,]$expcol<- "lightskyblue2"

b1<-barplot(el$wrel[1:40], col= el$expcol[1:40],
            las= 1, ylab= "win%", ylim= c(0,1.1))
text(b1+0.3, el$wrel[1:40]+0.066, el$name[1:40], 
     srt= 69, cex= 0.6)
el2<- wrel_opt(el)
elite<- el2[1:32,]
t32<- turnier32(elite, 100, 2)

elite50<- el2[1:50,]
t50<- turnier64(elite64, 500, 123)




#d<- dx[dx$league== 2  & dx$exp %in% c(0.5,0.7),]
#t<- turnier64a(d, 100, 1)
#top32<- t[1:32,]
#t32<- turnier32(top32, 1000, 1)
#t32




#NMDS-------------


m2<- dx[,c(2:6, 8)]

example_NMDS2=metaMDS(m2,  k=2)

plot(example_NMDS2)
ordiplot(example_NMDS2,type="n")
orditorp(example_NMDS2,display="species",col="red",air=0.01)
orditorp(example_NMDS2,display="sites",cex=1.25,air=0.01)




#d ranks-----
#all without exp= 1 and duplikate
d<- rbind(q1, q2, q3)
divcolvec<- c("red3", "forestgreen", "dodgerblue3")
d$divcol<- divcolvec[1]
d[d$league== 2,]$divcol<- divcolvec[2]
d[d$league== 3,]$divcol<- divcolvec[3]

drank<- data.frame(name= d$name,
                   hubraum= rank(-d$hubraum), leistung= rank(-d$leistung),
                   speed= rank(-d$speed), weight= rank(d$weight),
                   length= rank(-d$length),
                   div= d$league, divcol= d$divcol,
                   natcol1= d$natcol1,natcol2= d$natcol2,
                   dpch= 16,dcex= 0.8)
q1rank<- data.frame(name= q1$name,
                   hubraum= rank(-q1$hubraum), leistung= rank(-q1$leistung),
                   speed= rank(-q1$speed), weight= rank(q1$weight),
                   length= rank(-q1$length),
                   natcol1= q1$natcol1, natcol2= q1$natcol2,
                   div= q1$league, dpch= 16,dcex= 0.8)
q2rank<- data.frame(name= q2a$name,
                    hubraum= rank(-q2a$hubraum), leistung= rank(-q2a$leistung),
                    speed= rank(-q2a$speed), weight= rank(q2a$weight),
                    length= rank(-q2a$length),
                    natcol1= q2a$natcol1, natcol2= q2a$natcol2,
                    div= q2a$league, dpch= 16,dcex= 0.8)
q3rank<- data.frame(name= q3a$name,
                    hubraum= rank(-q3a$hubraum), leistung= rank(-q3a$leistung),
                    speed= rank(-q3a$speed), weight= rank(q3a$weight),
                    length= rank(-q3a$length),
                    natcol1= q3a$natcol1, natcol2= q3a$natcol2,
                    div= q3a$league, dpch= 16,dcex= 0.8)


drank$sum<- rowSums(drank[,2:6])
drank$minrank<- apply(drank[,2:6], 1, FUN = min)
drank$minpos<-  apply(drank[,2:6], 1, FUN = which.min)
drank$maxrank<- apply(drank[,2:6], 1, FUN = max)
q1rank$qsum<- rowSums(q1rank[,2:6])
q1rank$qminrank<- apply(q1rank[,2:6], 1, FUN = min)
q1rank$qminpos<-  apply(q1rank[,2:6], 1, FUN = which.min)
q1rank$qmaxrank<- apply(q1rank[,2:6], 1, FUN = max)
q2rank$qsum<- rowSums(q2rank[,2:6])
q2rank$qminrank<- apply(q2rank[,2:6], 1, FUN = min)
q2rank$qminpos<-  apply(q2rank[,2:6], 1, FUN = which.min)
q2rank$qmaxrank<- apply(q2rank[,2:6], 1, FUN = max)
q3rank$qsum<- rowSums(q3rank[,2:6])
q3rank$qminrank<- apply(q3rank[,2:6], 1, FUN = min)
q3rank$qminpos<-  apply(q3rank[,2:6], 1, FUN = which.min)
q3rank$qmaxrank<- apply(q3rank[,2:6], 1, FUN = max)



#means
dmeans<- aggregate(drank[,2:6], list(drank$div), mean)
colnames(dmeans)[1]<- "name"
dmeans$div= 1:3
dmeans$divcol<- c("red4", "darkgreen", "dodgerblue3")
dmeans$dpch= 17
dmeans$dcex= 2.5

drank2<- rbind(drank[,c(c(1:8, 11:12))], dmeans)
  
pairs(drank2[,2:6], 
      pch = drank2$dpch, col= drank2$divcol, cex= drank2$dcex)




#cluster--------
df<- drank[,c(13,14,15,16)]
rownames(df)<- drank$name
iris.scaled <- scale(df)
km.res <- kmeans(iris.scaled, 4, nstart = 10)
fviz_cluster(km.res, df, show.clust.cent= F, 
             labelsize= 9, pointsize= 2,
             ellipse.type = "norm",
             pch= drank$dpch)



#pca-----
# Graph of the variables
library('corrr')
library(ggcorrplot)
library("FactoMineR")

d$d1<- 0
d$d2<- 0
d$d3<- 0
d[d$league==1,]$d1<- 1
d[d$league==2,]$d2<- 1
d[d$league==3,]$d3<- 1

numerical_data <- d[,c(2:6, 13:15)]
data_normalized <- scale(numerical_data)
head(data_normalized)
#correl matrix
corr_matrix <- cor(data_normalized)
ggcorrplot(corr_matrix)
#PCA
data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:2]
fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, 
             col.var = "grey11")



library(hrbrthemes)
library(viridis)

# create a dataset
data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

# Plot
data %>%
  ggplot( aes(x=name, y=value, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")



data2 <- data.frame(
  name=  as.character(dx$league),
  value1=dx$hubraum,
  value2=dx$leistung,
  value3=dx$speed,
  value4=dx$weight,
  value5=dx$length)


#BoxPlot-----

data2 %>%
  ggplot( aes(x=name, y= value1, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Hubraum") +
  xlab("division")+
  ylab("Hubraum [ccm]")+
  ylim(1500,9200)


#Boxplot Ranks-----

#rang sum per div
data3<-  data.frame(name=  as.character(drank$div),
                    value1= drank$sum)
data3 %>%
  ggplot( aes(x=name, y= value1, fill=name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Rangsumme") +
  xlab("division")+
  ylab("sum of all ranks")+
  ylim(300,1000)


#minimum rank of each card
dmin1<- drank[,c("name", "minrank", "minpos", 
                "maxrank","natcol1", "natcol2", "div", "divcol")]
q1min<- q1rank[,c("name","qminrank","qminpos","qmaxrank")]
q2min<- q2rank[,c("name","qminrank","qminpos","qmaxrank")]
q3min<- q3rank[,c("name","qminrank","qminpos","qmaxrank")]

dmin<- merge(dmin1, rbind(q1min, q2min, q3min), by= "name")
dmin<- dmin[order(dmin$qminrank),]
dmax<- dmin[order(dmin$qmaxrank),]

dmin1<- dmin[dmin$div==1,]
dmax1<- dmax[dmax$div==1,]

dmin2<- merge(q2min, q2a, by= "name")
dmin2<- dmin2[order(dmin2$qminrank),]
dmax2<- dmax[dmax$div==2,]

dmin3<- merge(q3min, q3a, by= "name")
dmin3<- dmin3[order(dmin3$qminrank),]
dmax3<- dmax[dmax$div==3,]


par(mfrow= c(2,1), lwd= 2)
b11<- barplot(dmin3$qminrank, 
             ylim= c(0,80), axes=F,
             main= "Minimum Rank (Div)", 
             col= dmin3$natcol2, border= dmin3$natcol1)
axis(2, las=2, cex.axis= 0.8)
text(b11+0.4, dmin3$qminrank+10, pos=3,  dmin3$name, cex= 0.62, srt=69)
b12<- barplot(dmax3$qmaxrank-50, main= "Maximum Rank (Div)",
              ylim= c(0,80), axes= F,
              col= dmax3$natcol2, border= dmax3$natcol1)
axis(2,seq(0,60,10), seq(50,110,10),las=2, cex.axis= 0.8)
text(b12+0.4, dmax3$qmaxrank-39, pos=3,  dmax3$name, cex= 0.62, srt=69)


par(mfrow= c(1,1), lwd= 1)
plot(dmin1$minrank, dmin1$maxrank, 
     main= "Minrank ~ Maxrank \n Div.1", xlab= "Minrank", ylab= "Maxrank",
     col= dmin1$natcol1, pch= 16, cex= 1.5, las= 1)
points(dmin1$minrank, dmin1$maxrank, col= dmin1$natcol2, pch= 16)
text(dmin1$minrank, dmin1$maxrank, dmin1$name, pos= 3, cex= 0.6, col= dmin1$natcol1)
abline(lm(dmin1$maxrank~dmin1$minrank), col= "grey78", lty= 2)

par(mfrow= c(1,1), lwd= 1)
plot(dmin2$qminrank, dmin2$qmaxrank, 
     main= "qMinrank ~ qMaxrankk \n Div.2", xlab= "qMinrank", ylab= "qMaxrank",
     col= dmin2$natcol1, pch= 16, cex= 1.5, las= 1)
points(dmin2$qminrank, dmin2$qmaxrank, col= dmin2$natcol2, pch= 16)
text(dmin2$qminrank, dmin2$qmaxrank, dmin2$name, pos= 3, cex= 0.6, col= dmin2$natcol1)
abline(lm(dmin2$qmaxrank~dmin2$qminrank), col= "grey78", lty= 2)

par(mfrow= c(1,1), lwd= 1)
plot(dmin3$qminrank, dmin3$qmaxrank, 
     main= "qminrank ~ Maxrankk \n Div.3", xlab= "qminrank", ylab= "Maxrank",
     col= dmin3$natcol1, pch= 16, cex= 1.5, las= 1)
points(dmin3$qminrank, dmin3$qmaxrank, col= dmin3$natcol2, pch= 16)
text(dmin3$qminrank, dmin3$qmaxrank, dmin3$name, pos= 3, cex= 0.6, col= dmin3$natcol1)
abline(lm(dmin3$qmaxrank~dmin3$qminrank), col= "grey78", lty= 2)


#Best Var per car
dmin<- dmin[order(dmin$minrank),]
hub<- dmin[dmin$minpos==1,]
lei<- dmin[dmin$minpos==2,]
ges<- dmin[dmin$minpos==3,]
gew<- dmin[dmin$minpos==4,]
lae<- dmin[dmin$minpos==5,]


par(mfrow= c(1,1), lwd= 2)
bb1<- barplot(hub$minrank,
              #col= hub$natcol2, border= hub$natcol1, 
               col= hub$divcol, border= "grey20",
              las=2, main= "Hubraum mains", ylim= c(0,170),) 
text(bb1 +0.25, hub$minrank +9, pos=3, hub$name, cex= 0.62, srt=69)        

bb2<- barplot(lei$minrank,
              #col= lei$natcol2, border= lei$natcol1, 
              col= lei$divcol, border= "grey20",
              las=2, main= "leistung mains", ylim= c(0,177),) 
text(bb2 +0.25, lei$minrank +9, pos=3, lei$name, cex= 0.62, srt=69)  

bb3<- barplot(ges$minrank,
              #col= ges$natcol2, border= ges$natcol1, 
              col= ges$divcol, border= "grey20",
              las=2, main= "speed mains", ylim= c(0,170),) 
text(bb3, ges$minrank +9, pos=3, ges$name, cex= 0.62, srt=69)  

bb4<- barplot(gew$minrank,
              #col= gew$natcol2, border= gew$natcol1, 
              col= gew$divcol, border= "grey20",
              las=2, main= "Gewicht mains", ylim= c(0,180),) 
text(bb4, gew$minrank +9, pos=3, gew$name, cex= 0.62, srt=69)  

bb5<- barplot(lae$minrank,
              #col= lae$natcol2, border= lae$natcol1, 
              col= lae$divcol, border= "grey20",
              las=2, main= "länge mains", ylim= c(0,170),) 
text(bb5, lae$minrank +9, pos=3, lae$name, cex= 0.62, srt=69)  


#rank summe länder----
dland<- merge(drank, d[,c("name", "land")], by= "name")

de<-  dland[dland$land== "D",]
en<-  dland[dland$land== "GB",]
it<-  dland[dland$land== "I",]
us<-  dland[dland$land== "US",]
rdw<- dland[!(dland$land %in% c("D","GB","I","US")),]

plotland<- function(land, divcols){
  la= land
  la<- la[order(la$sum),]
  co1<- la$natcol1
  co2<- la$natcol2
  if(divcols== T){
    co1<- "black"
    co2<- la$divcol
  }
  par(mfrow= c(1,1), lwd= 2)
  l1<-barplot(la$sum, las= 1, 
          col= co2, border= co1,
          ylim= c(0, max(la$sum)*1.2))
  text(l1 +0.25, la$sum +60, pos=3, la$name, cex= 0.62, srt=69) 
}

#barplot vars-------
d$lcol<- "red"
d[d$league== 2,]$lcol<- "royalblue"
d[d$league== 3,]$lcol<- "thistle"

bpvar<- function(var, rank){
dv<- d[order(d[,var]),] 
if(rank== T){dv[,var]<- rank(dv[,var])}
barplot(dv[,var], col= dv$lcol, las=1, 
        cex.axis= 0.8, main= colnames(dv)[var])
}
bpvar(2,F)


#.-----------
#Länder Turniere--------

#DE-----
xde_all<-  d[d$land==  "D",]
xde1<-  xde_all[!(xde_all$league== 3 & xde_all$exp < 0.6),]
xde<-  xde1[!(xde1$name %in% c("Gumpert Apo Sp",
                             "Porsche GTstr RS",
                             "Porsche Carrera",
                             "Gumpert Apollo",
                             "Mercedes SLS GT3",
                             "Audi R8 Spy", 
                             "Audi R8 V10")),]
turnier32(xde, 100,1)
#P1: Lotec (77), P2: Gumpert (9), P3: Exe (5)
xde2<- xde_all[xde_all$league >1,]
turnier32(xde2, 100, 1)
#P1: Isdera (25), P2: SLR (20), P3: SLS GT3 (16)

#EN-------
xen_all<-  d[d$land== "GB",]
xen1<- xen_all[!(xen_all$league== 3 & xen_all$exp < 0.6),]
xen0<-  xen1[!(xen1$name %in% c("A-Ma Vulcan",
                               "A-Ma DB11",
                               "Bentley Conti",
                               "Lister Storm",
                               "A-Ma Vanq",
                               "A-Ma DBS")),]
xen<- rbind(xen0, d[d$name== "Jaguar XF10",])
turnier32(xen, 100,1)
#P1: Lister Storm R (25), P2: One-77 (16), P3: Senna (12)

xen2<- xen_all[xen_all$league >1,]
xen2[32,]<- d[d$name== "Ascari KZ1",] 
turnier32(xen2, 100, 1)
#P1: XJ220 (30), P2: Artura (15), P3: LFT-666 (13)

#ITA------
xit_all<-  d[d$land== "I",]
xit1<- xit_all[!(xit_all$league== 3 & xit_all$exp < 0.6),]
xit0<-  xit1[!(xit1$name %in% c("Maserati Birdcage",
                                "Lambo Diablo",
                                "Ferrari F458",
                                "Ferrari GG50",
                                "Ferrari Zagato")),]
turnier32(xit0, 100,1)
#P1: Ferrari LaF (33), P2: Ferrari 812 (16), P3: Spada (10)

xit2a<- xit_all[xit_all$league >1,] 
xit2<- rbind(xit2a, dx[dx$name %in% c("Ferrari 575 GTC",
                                      "Soleil Anadi",
                                      "Lotus F1",
                                      "Mega MC"),]) 
turnier32(xit2, 100, 1)
#P1: Fer 575 GTC (31), P2: Fer 599 (24), P3: Soleil (17)


#US------
xus_all<-  d[d$land== "US",]
xus1<- xus_all[!(xus_all$league== 3 & xus_all$exp < 0.4),]
xus0a<-  xus1[!(xus1$name %in% c("Dodge Vip Rek",
                                "Dodge Viper Tune",
                                "Dodge Viper SV1",
                                "Hummer Geiger")),]
xus0<- rbind(xus0a, dx[dx$name %in% c("Hummer H2", "Dodge Vip SRT-10"),]) 
turnier32(xus0, 100,1)
#P1: Ford GT Mansory (26), P2: S7 (24), P3: S7R (18)

xus2a<- xus_all[xus_all$league >1,] 
xus2<- rbind(xus2a, dx[dx$ID %in% c(299, 278, 300, 290, 297),]) 
turnier32(xus2, 100, 1)
#P1: Ford GT(38) P2: C3(22), Viper (11)

#RDW----
xrdw_all<-  d[!(d$land %in% c("D", "I", "GB", "US")),]
xrdw1<- xrdw_all[!(xrdw_all$league== 3 & xrdw_all$exp < 0.6),]
xrdw0<-  xrdw1[!(xrdw1$ID %in% c(33, 39, 141, 171, 186)),]
turnier32(xrdw0, 100,1)
#P1: Zenvo (25), P2: Tramo R (22), P3: Toyota TS050 (17)

xrdw2a<- xrdw_all[xrdw_all$league >1,] 
xrdw2<- xrdw2a[!(xrdw2a$league==3 & xrdw2a$exp <0.4),]
turnier32(xrdw2, 100, 1)
#P1: Peg 908 (26), P2: Holden Commo (25), P3: Austro (11)



#.-------
#cluster--------

library("dendextend")
x <- q3[,2:6]
dend <- as.dendrogram(hclust(dist(x), method= "complete"))
labels(dend) <- q3$name[order.dendrogram(dend)]

dend <- dend %>%
  color_labels(k =  8) %>%
  color_branches(k= 8) %>%
  set("branches_lwd", 2)

par(cex= 0.67, mar= c(9,5,4,1))
p1<- plot(dend, las= 1)


