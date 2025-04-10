
#Alle Funktionen für spätere Schritte

#packages----------
library(tictoc)
library(ggpubr)
library(factoextra)
library(vegan)
library(stats)
library(dplyr)

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


#fight2: output: Punkte (0, 0.5, 1)------
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


#Fight3: Output: gewichtete Punkte in relation zur Siegesrate
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

#Fight4 (theoretisch: gewichtet mit gewichteten Ratings)
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


#Turniere


#32er Turnier klassisch, Output: Turniersieger---------
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



#64er Turnier klassisch, Output: Turniersieger--------
turnier64<- function(grid, rep, seed){
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




#Hilfsfunktion für Teilnehmerzahl als Zweierpotenz 32,64,128,...
pot2 <- function(x) {
  if (x < 1) return(1)
  return(2^ceiling(log2(x)))
}
#Hilfsfunktion: Freilose einfügen, um auf Zweierpotenz zu kommen
getfreilos<- function(n){
  freilos <- data.frame(
    name = rep("Freilos", n),
    hubraum = rep(0, n),
    leistung = rep(0, n),
    speed = rep(0, n),
    weight = rep(0, n),
    length = rep(0, n),
    stringsAsFactors = FALSE
  )
  return(freilos)
}

#turnier für x Teilnehmer-----
turnier128<- function(grid, rep, seed){
  if(is.na(seed)) {set.seed(floor(runif(1,1,10000000)))}
  else {set.seed(seed)}
  w<- vector()
  l512<- list()
  l256<- list()
  l128<- list()
  l64<- list()
  l32<- list()
  l16<- list()
  l8<- list()
  l4<- list()
  
  tnr<- pot2(nrow(grid))
  freinr<- tnr - nrow(grid)
  df2er<- bind_rows(grid, getfreilos(freinr))
  
  
  #tournaments
  for(i in 1:rep){
    print(i)
    grid<- grid[sample(nrow(grid)),]
    #tournament
    
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
  res<- as.data.frame(table(unlist(w)))
  colnames(res)<- c("name", "wins")
  res<- merge(res, grid[,c("name", "natcol1", "natcol2")], by= "name", all.y=T)
  res[is.na(res$wins),]$wins<- 0
  res<- res[order(res$wins, decreasing = T),]  |> filter(wins >0)
  #plot
  par(mar= c(8,4,4,1), lwd= 3, mfrow= c(1,1))
  b1<- barplot(res$wins, names = res$name, 
               las= 2, cex.names= 0.8, ylim= c(0,max(res$wins)*1.1), 
               col= res$natcol2, border= res$natcol1)
  text(b1, res$wins, res$wins, pos= 3, cex= 0.7)
  return(res)
}


turnier128a<- function(grid, rep, seed){
  if(is.na(seed)) {set.seed(floor(runif(1,1,10000000)))}
  else {set.seed(seed)}
  w<- vector()
  l128<- list()
  l64<- list()
  l32<- list()
  l16<- list()
  l8<- list()
  l4<- list()
  #tournament
  for(i in 1:rep){
    print(i)
    grid<- grid[sample(nrow(grid)),]
    #tournament
    for(j in 1:64){
      l64[[j]]<- fight(grid[(2*j-1),], grid[(2*j),])
    }
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
  res<- res[order(res$wins, decreasing = T),]  |> filter(wins >0)
  #plot
  par(mar= c(8,4,4,1), lwd= 3, mfrow= c(1,1))
  b1<- barplot(res$wins, names = res$name, 
               las= 2, cex.names= 0.8, ylim= c(0,max(res$wins)*1.1), 
               col= res$natcol2, border= res$natcol1)
  text(b1, res$wins, res$wins, pos= 3, cex= 0.7)
  return(res)
}




#Turnier mit Freilosen 2. Versuch
turnierx2<- function(grid, rep, seed){
  tnr<- pot2(nrow(grid))      #Zweierpotenz für Turnierzahl, zB 32,64,...
  freinr<- tnr - nrow(grid)   #Anzahl benötigter Freilose, um auf tnr zu kommen
  
  gesamt <- grid[sample(nrow(grid)),]
  grid_gemischt <- grid[sample(nrow(grid)),]    #Mischen
  gesamt <- data.frame(matrix(NA, nrow = tnr, ncol = ncol(grid)))  
  colnames(gesamt) <- colnames(grid)    #Erstelle leeren df mit labels
  freilos_pos <- 2 * (1:freinr)   
  gesamt[freilos_pos, ] <- getfreilos(1)    #Platziere erst Freilose
  rest_pos <- setdiff(1:tnr, freilos_pos)  #Und dann gemischte Karten 
  gesamt[rest_pos, ] <- grid_gemischt      #Auf restlichen Positiionen
  
  if(tnr== 128)  {output<- turnier128(gesamt, rep, seed)}
  if(tnr== 64)  {output<- turnier64(gesamt, rep, seed)}
  if(tnr== 32)  {output<- turnier32(gesamt, rep, seed)}
  return(output)
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



#Optimierte win percentage (mit fight3 statt fight2)
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

#Doppelt optimierte win percentage (mit fight4 statt fight2), in der
#Praxis zu kompliziert und unnötig, aber nette Idee
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


#Insert new cars-----
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


#turnier mit Untergruppe (Land, Marken,...)
turnierselect <- function(dat, kat, crit) {
  dfsub <- dat %>%
    filter(!!sym(kat) %in% crit)
}

#Beispiel
df<- turnierselect(d, "land", "GB")
turnierx2(df, 100,111)

tn<- turnierx2(q2, 50, 123)

#Beispiel 2
tn4<- turnierselect(d, "marke", c("Mercedes", "Ford",
                                  "Ferrari", "Jaguar",
                                  "Audi", "Aston Martin"))
turnierx2(tn4, 50, 123)

