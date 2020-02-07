library(MASS)

get.data.MAR <- function(N = 300, prop.NA = 0.1, moc.mehanizma = 4, stevilo.skupin= 3, plot=F){
  #INPUT
  #N - stevilo enot
  #prop.NA - razmerje NA vrednosti
  #moc.mehanizma - moc mehanizma
  #OUTPUT:
  #podatki - dataset s NA vrednosti
  #podatki.plot - perfect dataset
  
  #Generiranje podatkov
  n.skupin <- round(N/stevilo.skupin, 0)
  podatki <- NULL
  mi <- c(3, 5, 7)
  r <- 0.3 #smisel življenja
  Sigma <- rbind(c(1, r, r, r),
                 c(r, 1.0, r, r),
                 c(r, r, 1.0, r),
                 c(r, r, r, 1.0)
                 )
  
  for(i.mi in mi){
    dataset <- as.data.frame(mvrnorm(n = n.skupin, mu = rep(i.mi, 4), Sigma = Sigma))
    dataset$skupina <- rep(which(mi == i.mi), n.skupin)
    podatki <- rbind(podatki, dataset)
  }
  podatki <- as.data.frame(podatki)
  colnames(podatki) <- c("X1", "X2", "X3", "X4", "skupina")
  podatki.plot <- podatki
  
  #Manjkajoče vrednosti
  nMiss <- round(prop.NA *N)
  verjetnosti.na <- abs((podatki[,1])^moc.mehanizma/(sum(podatki[,1]))^moc.mehanizma)
  
  set.na.2 <- sample(length(verjetnosti.na), size = nMiss, prob = verjetnosti.na)
  podatki[set.na.2, 2] <- NA #X1 vpliva na X2
  
  set.na.3 <- sample(length(verjetnosti.na), size = nMiss, prob = verjetnosti.na)
  podatki[set.na.3, 3] <- NA #X1 vpliva na X3
  
  set.na.4 <- sample(length(verjetnosti.na), size = nMiss, prob = verjetnosti.na)
  podatki[set.na.4, 4] <- NA #X1 vpliva na X4
  
  
  #za risanje slik
  if(plot){
    podatki.plot$na.2<- 1 #za risanje grafa
    podatki.plot[set.na.2, "na.2"] <- 4 
    podatki.plot$na.3<- 1 #za risanje grafa
    podatki.plot[set.na.3, "na.3"] <- 4
    podatki.plot$na.4<- 1 #za risanje grafa
    podatki.plot[set.na.4, "na.4"] <- 4
    #return(podatki.plot)
    p1 <- plot(podatki.plot$X1, podatki.plot$X2, col = podatki.plot$na.2,
              main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X2",
              xlab = "X1", ylab = "X2")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    p2 <- plot(podatki.plot$X1, podatki.plot$X3, col = podatki.plot$na.3,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X3",
               xlab = "X1", ylab = "X3")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    p3 <- plot(podatki.plot$X1, podatki.plot$X4, col = podatki.plot$na.4,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X4",
               xlab = "X1", ylab = "X4")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
  return(list(podatki, p1,p2,p3))
  }
  else{
    return(list(podatki, podatki.plot))
  }
}


get.data.NMAR <- function(N = 300, prop.NA = 0.1, moc.mehanizma = 4, stevilo.skupin= 3, plot=F){
  #INPUT:
  #N - stevilo enot
  #prop.NA - razmerje NA vrednosti
  #moc.mehanizma - moc mehanizma
  #OUTPUT:
  #podatki - dataset s NA vrednosti
  #podatki.plot - perfect dataset
  
  #Generiranje podatkov
  n.skupin <- round(N/stevilo.skupin, 0)
  podatki <- NULL
  mi <- c(3, 5, 7)
  r <- 0.3 #smisel življenja
  Sigma <- rbind(c(1, r, r, r),
                 c(r, 1.0, r, r),
                 c(r, r, 1.0, r),
                 c(r, r, r, 1.0)
  )
  for(i.mi in mi){
    dataset <- as.data.frame(mvrnorm(n = n.skupin, mu = rep(i.mi,4), Sigma = Sigma))
    dataset$skupina <- rep(which(mi == i.mi), n.skupin)
    podatki <- rbind(podatki, dataset)
  }
  podatki <- as.data.frame(podatki)
  colnames(podatki) <- c("X1", "X2", "X3", "X4", "skupina")
  podatki.plot <- podatki
  
  #Manjkajoče vrednosti
  nMiss <- round(prop.NA * N)
  
  verjetnosti.na <- (podatki[,2])^moc.mehanizma / (sum(podatki[,2]))^moc.mehanizma
  set.na.2 <- sample(length(verjetnosti.na), size = nMiss, prob = abs(verjetnosti.na))
  podatki[set.na.2, 2] <- NA #X2 vpliva na X2
  
  verjetnosti.na.3 <- (podatki[,3])^moc.mehanizma / (sum(podatki[,3]))^moc.mehanizma
  set.na.3 <- sample(length(verjetnosti.na.3), size = nMiss, prob = abs(verjetnosti.na.3))
  podatki[set.na.3, 3] <- NA #X3 vpliva na X3
  
  verjetnosti.na.4 <- (podatki[,4])^moc.mehanizma/(sum(podatki[,4]))^moc.mehanizma
  set.na.4 <- sample(length(verjetnosti.na.4), size = nMiss, prob = abs(verjetnosti.na.4))
  podatki[set.na.4, 4] <- NA #X4 vpliva na X4
  
  #za risanje slik
  if(plot){
    podatki.plot$na.2<- 1 #za risanje grafa
    podatki.plot[set.na.2, "na.2"] <- 4 
    podatki.plot$na.3<- 1 #za risanje grafa
    podatki.plot[set.na.3, "na.3"] <- 4
    podatki.plot$na.4<- 1 #za risanje grafa
    podatki.plot[set.na.4, "na.4"] <- 4
    #return(podatki.plot)
    p1 <- plot(podatki.plot$X1, podatki.plot$X2, col = podatki.plot$na.2,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X2",
               xlab = "X1", ylab = "X2")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    p2 <- plot(podatki.plot$X1, podatki.plot$X3, col = podatki.plot$na.3,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X3",
               xlab = "X1", ylab = "X3")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    p3 <- plot(podatki.plot$X1, podatki.plot$X4, col = podatki.plot$na.4,
               main = "Povezanost med spremenljivko X1 \n in manjkajocimi podatki X4",
               xlab = "X1", ylab = "X4")
    legend("bottomright", legend = c("obstojeci", "manjkajoci"), col= c(1,4), pch = 1)
    return(list(podatki, p1,p2,p3))
  }
  else{
    return(list(podatki, podatki.plot))
  }
}


get.data.MCAR <- function(N = 300, prop.NA = 0.1, stevilo.skupin= 3){
  #INPUT:   
  #N - stevilo enot v skupini
  #prop.NA - razmerje NA vrednosti
  #OUTPUT:
  #podatki - datasets NA vrednostmi
  
  #Generiranje podatkov
  n.skupin <- round(N/stevilo.skupin, 0)
  dataset <- NULL
  mi_1 <- rep(0,4)
  mi_2 <- rep(2,4)
  mi_3 <- rep(5,4)
  r <- 0.3 #smisel življenja
  Sigma <- rbind(c(1, r, r, r),
                 c(r, 1.0, r, r),
                 c(r, r, 1.0, r),
                 c(r, r, r, 1.0)
  )
  df_1 <- as.data.frame(mvrnorm(n = n.skupin, mu = mi_1, Sigma = Sigma))
  df_2 <- as.data.frame(mvrnorm(n = n.skupin, mu = mi_2, Sigma = Sigma))
  df_3 <- as.data.frame(mvrnorm(n = n.skupin, mu = mi_3, Sigma = Sigma))
  podatki <- as.data.frame(rbind(df_1,df_2,df_3))
  podatki$skupina <- as.factor(rep(c(1:3), each = n.skupin))
  colnames(podatki) <- c("X1", "X2", "X3", "X4", "skupina")
  podatki.plot <- podatki
  
  #Manjkajoče vrednosti
  nMiss <- round(prop.NA *N)
  podatki[sample(N, size = nMiss), 2] <- NA 
  podatki[sample(N, size = nMiss), 3] <- NA 
  podatki[sample(N, size = nMiss), 4] <- NA 
  return(list(podatki, podatki.plot))
}
