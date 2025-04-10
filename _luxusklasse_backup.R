
#packages----------
library(tictoc)
library(ggpubr)
library(factoextra)
library(vegan)
library(stats)


#.--------
#Funtionen----


#fight: output: winner--------
fight<- function(car1, car2){
  if(typeof(car1)== "list")   {d1<- car1}
  if(typeof(car1)== "double") {d1<- d[car1,]}
  if(typeof(car2)== "list")   {d2<- car2}
  if(typeof(car2)== "double") {d2<- d[car2,]}
  # print(paste0(d1[1], " vs ", d2[1]))
  res<- 0
  if (d1[2]>d2[2]) {res= res+1}
  if (d1[2]<d2[2]) {res= res-1}
  if (d1[3]>d2[3]) {res= res+1}
  if (d1[3]<d2[3]) {res= res-1}
  if (d1[4]>d2[4]) {res= res+1}
  if (d1[4]<d2[4]) {res= res-1}
  if (d1[5]<d2[5]) {res= res+1}
  if (d1[5]>d2[5]) {res= res-1}
  if (d1[6]>d2[6]) {res= res+1}
  if (d1[6]<d2[6]) {res= res-1}
  #if(res >0)  {print(paste0("Winner: ", d1[1]))}
  #if(res==0)  {print("draw")}
  #else         print(paste0("Winner: ", d2[1]))
  #print("")
  if(res >= 0) {return(d1)}
  if(res  < 0) {return(d2)}
}

#fight2: output: 0, 0.5, 1------
fight2<- function(d1, d2, d){
  d1<- d[d1,]
  d2<- d[d2,]
  res<- 0
  if (d1[2]>d2[2]) {res= res+1}
  if (d1[2]<d2[2]) {res= res-1}
  if (d1[3]>d2[3]) {res= res+1}
  if (d1[3]<d2[3]) {res= res-1}
  if (d1[4]>d2[4]) {res= res+1}
  if (d1[4]<d2[4]) {res= res-1}
  if (d1[5]<d2[5]) {res= res+1}
  if (d1[5]>d2[5]) {res= res-1}
  if (d1[6]>d2[6]) {res= res+1}
  if (d1[6]<d2[6]) {res= res-1}
  pts<- data.frame(name= c(as.character(d1[1])),
                   win= 0)
  if(res >0){pts$win<-   1}
  if(res==0){pts$win<- 0.5}
  return(pts)
}


fight3<- function(d1, d2, d){
  d1<- d[d1,]
  d2<- d[d2,]
  res<- 0
  pts<- data.frame(name= c(as.character(d1[1])),
                   win= 0)
  if (d1[2]>d2[2]) {res= res+1}
  if (d1[2]<d2[2]) {res= res-1}
  if (d1[3]>d2[3]) {res= res+1}
  if (d1[3]<d2[3]) {res= res-1}
  if (d1[4]>d2[4]) {res= res+1}
  if (d1[4]<d2[4]) {res= res-1}
  if (d1[5]<d2[5]) {res= res+1}
  if (d1[5]>d2[5]) {res= res-1}
  if (d1[6]>d2[6]) {res= res+1}
  if (d1[6]<d2[6]) {res= res-1}
#new weighted wins 
  if(res >0){pts$win<-     d2$wrel^2}
  if(res==0){pts$win<- 0.5*d2$wrel^2}
  return(pts)
}

fight4<- function(d1, d2, d){
  d1<- d[d1,]
  d2<- d[d2,]
  res<- 0
  pts<- data.frame(name= c(as.character(d1[1])),
                   win= 0)
  if (d1[2]>d2[2]) {res= res+1}
  if (d1[2]<d2[2]) {res= res-1}
  if (d1[3]>d2[3]) {res= res+1}
  if (d1[3]<d2[3]) {res= res-1}
  if (d1[4]>d2[4]) {res= res+1}
  if (d1[4]<d2[4]) {res= res-1}
  if (d1[5]<d2[5]) {res= res+1}
  if (d1[5]>d2[5]) {res= res-1}
  if (d1[6]>d2[6]) {res= res+1}
  if (d1[6]<d2[6]) {res= res-1}
  #new weighted wins 
  if(res >0){pts$win<-     d2$wrel_opt^2}
  if(res==0){pts$win<- 0.5*d2$wrel_opt^2}
  return(pts)
}




#32er Turnier---------
turnier32<- function(grid, rep, seed){
  #seed for tournaments
  if(is.na(seed)) {set.seed(floor(runif(1,1,10000000)))}
  else {set.seed(seed)}
  w<- vector()
  l16<- list()
  l8<- list()
  l4<- list()
  set.seed(floor(runif(1,1,1000000)))
  for(i in 1:rep){
    print(i)
    grid<- grid[sample(32),]
    #tournament
    for(j in 1:16){
      l16[[j]]<- fight(grid[(2*j-1),], grid[(2*j),])
    }
    for(j in 1:8){
      l8[[j]]<- fight(l16[[2*j-1]], l16[[2*j]])
    }
    for(j in 1:4){
      l4[[j]]<- fight( l8[[2*j-1]],  l8[[2*j]])
    }
    s1<- fight(l4[[1]], l4[[2]])
    s2<- fight(l4[[3]], l4[[4]])
    print(paste("Finale:", s1[1], "vs", s2[1]))
    fi<- fight(s1, s2)
    print(as.character(fi[1]))
    print(" ")
    w[i]<- fi[1]
  }
  res<- as.data.frame(table(unlist(w)))
  colnames(res)<- c("name", "wins")
  res<- res[order(res$wins, decreasing = T),]
  d7<- merge(grid, res, by= "name", all.x= F)
  d7$wins[is.na(d7$wins)]<- 0
  d<- d7[order(d7$wins, decreasing = T),]
  par(mar= c(8,4,4,1), lwd= 3, mfrow= c(1,1))
  b1<- barplot(d$wins, names = d$name, 
               las= 2, cex.names= 0.8, ylim= c(0,max(d$wins)*1.1), 
               col= d$natcol2, border= d$natcol1)
  text(b1, d$wins, d$wins, pos= 3, cex= 0.7)
  return(d)
}



#64er Turnier--------
turnier64<- function(grid, rep, seed){
  #seed for tournaments
  if(is.na(seed)) {set.seed(floor(runif(1,1,10000000)))}
  else {set.seed(seed)}
  w<- vector()
  l32<- list()
  l16<- list()
  l8<- list()
  l4<- list()
  #tournaments
  for(i in 1:rep){
    print(i)
    grid<- grid[sample(nrow(grid)),]
    #tournament
    for(j in 1:32){
      l32[[j]]<- fight(grid[(2*j-1),], grid[(2*j),])
    }
    for(j in 1:16){
      l16[[j]]<- fight(l32[[2*j-1]], l32[[2*j]])
    }
    for(j in 1:8){
      l8[[j]]<- fight(l16[[2*j-1]], l16[[2*j]])
    }
    for(j in 1:4){
      l4[[j]]<- fight( l8[[2*j-1]],  l8[[2*j]])
    }
    s1<- fight(l4[[1]], l4[[2]])
    s2<- fight(l4[[3]], l4[[4]])
    fi<- fight(s1, s2)
    w[i]<- fi[1]
  }
  res<- as.data.frame(table(unlist(w)))
  colnames(res)<- c("name", "wins")
  res<- merge(res, grid[,c("name", "natcol1", "natcol2")], by= "name", all.y=T)
  res[is.na(res$wins),]$wins<- 0
  res<- res[order(res$wins, decreasing = T),] |> filter(wins >0)
  #plot
  par(mar= c(8,4,4,1), lwd= 3, mfrow= c(1,1))
  b1<- barplot(res$wins, names = res$name, 
               las= 2, cex.names= 0.8, ylim= c(0,max(res$wins)*1.1), 
               col= res$natcol2, border= res$natcol1)
  text(b1, res$wins, res$wins, pos= 3, cex= 0.7)
  return(res)
}



#turnier für x Teilnehmer-----
turnierx<- function(grid, rep, seed){
  #seed for tournaments
  if(is.na(seed)) {set.seed(floor(runif(1,1,10000000)))}
  else set.seed(seed)
  w<- vector()
  l256<- list()
  l128<- list()
  l64<- list()
  l32<- list()
  l16<- list()
  l8<- list()
  l4<- list()
  #tournaments
  for(i in 1:rep){
    print(i)
    grid<- grid[sample(nrow(grid)),]
    #tournament
  if(potnr== 512){
    for(j in 1:256){
      l256[[j]]<- fight(grid[(2*j-1),], grid[(2*j),])
    }
  }  else l256<- grid
    for(j in 1:128){
      l128[[j]]<- fight(l256[(2*j-1),], l256[(2*j),])
    }
    for(j in 1:64){
      l64[[j]]<- fight(l128[(2*j-1),], l128[(2*j),])
    }
    for(j in 1:32){
      l32[[j]]<- fight(l64[(2*j-1),], l64[(2*j),])
    }
    for(j in 1:16){
      l16[[j]]<- fight(l32[[2*j-1]], l32[[2*j]])
    }
    for(j in 1:8){
      l8[[j]]<- fight(l16[[2*j-1]], l16[[2*j]])
    }
    for(j in 1:4){
      l4[[j]]<- fight( l8[[2*j-1]],  l8[[2*j]])
    }
    s1<- fight(l4[[1]], l4[[2]])
    s2<- fight(l4[[3]], l4[[4]])
    fi<- fight(s1, s2)
    w[i]<- fi[1]
  }
  colnames(t2)<- c("name", "wins")
  d7<- merge(d, t2, by= "name", all.x= T)
  d7$wins[is.na(d7$wins)]<- 0
  d<- d7[order(d7$wins, decreasing = T),]
  
  par(mar= c(8,4,4,1), lwd= 3, mfrow= c(1,1))
  b1<-barplot(d$wins, names = d$name, 
             las= 2, cex.names= 0.8, ylim= c(0,max(d$wins)*1.1), 
            col= d$natcol2, border= d$natcol1)
  text(b1, d$wins, d$wins, pos= 3, cex= 0.7)
}



#Turnier mit punkten------
turnier64a<- function(grid, rep, seed){
  #seed for tournaments
  if(is.na(seed)) {set.seed(floor(runif(1,1,10000000)))}
  else set.seed(seed)
  grid$points<- 0
  l32<- list()
  l16<- list()
  l8<- list()
  l4<- list()
  for(i in 1:rep){
    print(i)
    grid<- grid[sample(nrow(grid)),]
    #tournament
    for(j in 1:32){
      l32[[j]]<- fight(grid[(2*j-1),], grid[(2*j),])
    }
    for(j in 1:16){
      l16[[j]]<- fight(l32[[2*j-1]], l32[[2*j]])
    }
    for(j in 1:8){
      l8[[j]]<- fight(l16[[2*j-1]], l16[[2*j]])
    }
    for(j in 1:4){
      l4[[j]]<- fight( l8[[2*j-1]],  l8[[2*j]])
    }
    s1<- fight(l4[[1]], l4[[2]])
    s2<- fight(l4[[3]], l4[[4]])
    fi<- fight(s1, s2)
    
    grid[grid$name %in% as.character(unlist(l4)),]$points= 
      grid[grid$name %in% as.character(unlist(l4)),]$points +1
    grid[grid$name== as.character(s1[1]),]$points= 
      grid[grid$name== as.character(s1[1]),]$points +1
    grid[grid$name== as.character(s2[1]),]$points= 
      grid[grid$name== as.character(s2[1]),]$points +1
    grid[grid$name== as.character(fi[1]),]$points= 
      grid[grid$name== as.character(fi[1]),]$points +1
  }
  pt<- grid[order(grid$points, decreasing= T),]
  par(mar= c(8,4,4,1), lwd= 3, mfrow= c(1,1))
  b1<-barplot(pt$points, names = pt$name, 
              las= 2, cex.names= 0.8, ylim= c(0,max(pt$points)*1.1), 
              col= pt$natcol2, border= pt$natcol1)
  text(b1, pt$points, pt$points, pos= 3, cex= 0.7)
  grid<- grid[order(grid$points, decreasing = T),]
  return(grid)
}




#.----------------
#Daten---------
setwd("C:/Nerdzeug_backup/quartett")
dx<- read.csv("luxusklasse.csv", sep= ";")
colnames(dx)[1]<- "name"


dx$natcol1<- "black"
dx$natcol2<- "black"
#Länder Farben------
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





#.------
#.----
#.----
#win percentage------

wrel<- function(df){
  relwin<- vector()
  for(i in 1:nrow(df)){
    w= data.frame(name= df$name[i], win= 0)
    opp<- 1:nrow(df)
    opp<- opp[!opp %in% i]
    for(j in opp){
      w<- rbind(w, fight2(i,j, df))
    }
    relwin[i]<- round(mean(w[,2]),4)
  }
  df$wrel<- relwin
  df<- df[order(df$wrel, decreasing= T),]
  df$pos<- 1:nrow(df)
  return(df)
}
#Win percentage für Liga 2
q2w1<- wrel(q2a)

wrel_opt<- function(df){
  relwin_opt<- vector()
  for(i in 1:nrow(df)){
    w= data.frame(name= df$name[i], win= 0)
    opp<- 1:nrow(df)
    opp<- opp[!opp %in% i]
    for(j in opp){
      w<- rbind(w, fight3(i,j, df))
    }
    relwin_opt[i]<- round(mean(w[,2]),4)
  }
  df$wrel_opt<- relwin_opt
  df$wrel_opt<- round(df$wrel_opt* (0.5/mean(df$wrel_opt)),4)
  df<- df[order(df$wrel_opt, decreasing= T),]
  df$pos_opt<- 1:nrow(df)
  return(df)
}
q2w<- wrel_opt(q2w1)

wrel_opt2<- function(df){
  relwin_opt2<- vector()
  for(i in 1:nrow(df)){
    w= data.frame(name= df$name[i], win= 0)
    opp<- 1:nrow(df)
    opp<- opp[!opp %in% i]
    for(j in opp){
      w<- rbind(w, fight4(i,j, df))
    }
    relwin_opt2[i]<- round(mean(w[,2]),4)
  }
  df$wrel_opt2<- relwin_opt2
  df$wrel_opt2<- round(df$wrel_opt2* (0.5/mean(df$wrel_opt2)),4)
  df<- df[order(df$wrel_opt2, decreasing= T),]
  df$pos_opt<- 1:nrow(df)
  return(df)
}
q2w2<- wrel_opt2(q2w)



#50 Turniere simulieren
t5000<- turnier64(q2, 50, 2)
t5000by_nat<-  aggregate(q2ww$wins, list(q2ww$land), sum)
t5000by_nat<- t5000by_nat[order(t5000by_nat[,2], decreasing = T),]


q2ww<- merge(q2w2, t5000, by= "name")
q2ww<- q2ww[order(q2ww$wrel_opt2, decreasing= T),]
q2ww


lo1<- loess(q2ww$wins ~ q2ww$wrel)
lo2<- loess(q2ww$wins ~ q2ww$wrel_opt)
lo3<- loess(q2ww$wins ~ q2ww$wrel_opt2)

#Siegesquote x Wins
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




insert<- function(d, new, new.league){
  if(is.numeric(new)){new<- dx[dx$league== new.league,][new,]}
  a<- turnier64a(d, 10)
  a[a$points== 0,][1,]<- new
  a$points<- 0
  anew<- as.data.frame(turnier32(a, 100))
  colnames(anew)<-  c("name", "wins")
  anew<- anew[order(anew$wins, decreasing = T),]
  anew$colvec1<- "grey22"
  anew$colvec2<- "grey77"
  anew[anew$name== new$name,]$colvec1<- "firebrick4"
  anew[anew$name== new$name,]$colvec2<- "firebrick1"
  par(mar= c(8,4,4,1), lwd= 3, mfrow= c(1,1))
  bar<-barplot(anew$wins, names= anew$name, 
              las= 2, cex.names= 0.8, ylim= c(0,max(anew$wins)*1.1), 
              col= anew$colvec2, border= anew$colvec1)
  text(bar, anew$wins, anew$wins, pos= 3, cex= 0.7)
  return(bar)
}


#insert(d, 3, 1)


#d<- dx[dx$league== 2  & dx$exp %in% c(0.5,0.7),]
#t<- turnier64a(d, 100, 1)
#top32<- t[1:32,]
#t32<- turnier32(top32, 1000, 1)
#t32






#.-------------
#plots-----------


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


