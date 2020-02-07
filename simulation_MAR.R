library(magrittr)
library(dplyr)
library(ggplot2)
library(reshape2)
source("Generation_data.r")
source("Imputation_Models.R")

pon <- 100
sampleSize <- 300
delez_na <- c(0.3, 0.4, 0.5, 0.6)
moc <- c(1, 2, 5, 8, 12)
zasnova <- expand.grid(delez_na,  moc, sampleSize)
zasnova <- do.call(rbind, replicate(pon, zasnova, simplify=FALSE)) %>% `colnames<-`(c("delez_na", "moc_mehanizma", "N"))

rez <- data.frame()
for(i in 1:nrow(zasnova)){
  moc.i <- zasnova[i, "moc_mehanizma"]
  N.i <- zasnova[i, "N"]
  delezNA.i <- zasnova[i, "delez_na"]
  
  dataMAR <- get.data.MAR(N = N.i, prop.NA = delezNA.i, moc.mehanizma = moc.i)
  data.NA <- dataMAR[[1]]
  data.perfect <- dataMAR[[2]]
  data.test <- get.data.MAR(N = 1200, prop.NA = 0, moc.mehanizma = 0)[[1]]
  
  #1.Perfect 
  perfect.rez <- lda_perfect.cases(data.perfect, data.test)
  #2.Complete
  complete.cas.rez <- lda_complete.cases(data.NA, data.test)
  #3. KNN
  knn.rez <- lda_knn(data.NA, data.test, k = 10)
  #4. EM-algoritem
  EM.rez <- lda_EM.algoritem(data.NA, data.test)
  #5. MICE
  mice.rez <-lda_mice(data.NA, data.test, m = 10)[["prop"]]
  #6. Random Forest
  rf.rez <- lda_rf(data.NA, data.test)[["prop"]]
  
  rezult <- c("perfect.data" = perfect.rez,
              "complete.data" = complete.cas.rez,
              "knn.imputation"= knn.rez, 
              "EM.imputation" = EM.rez, 
              "MICE.imputation" = mice.rez,
              "RandomForest.imputation" = rf.rez)
  #print(rezult)
  
  rez <- rbind(rez,c(zasnova[i,], rezult) )
  
}

rez.df <- as.data.frame(rez)
melt.df <- melt(rez.df, id = c("delez_na", "moc_mehanizma", "N"))


#####GRAF MAR#####

ggplot(melt.df, aes(x = variable, y = value, color = variable))+
  geom_boxplot()+
  facet_grid(cols = vars(delez_na))+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(colour = "white"))+
  ylab("Delež uspešnosti")+
  xlab("    ")

variable.df <- melt.df %>% group_by(variable, delez_na) %>%
  summarise(prop.mean = mean(value),
            prop.sd = sd(value))


summrise.df <- melt.df %>% group_by(variable, moc_mehanizma, delez_na) %>%
  summarise(prop.mean = mean(value),
            prop.sd = sd(value))

ggplot(summrise.df,aes(x = delez_na, y = prop.mean, color = variable, group =variable)) +
  geom_line()+
  geom_point()+
  ylab("Povprečni delež uspešnosti")+
  xlab("Delež NA vrednosti")+
  facet_grid(cols= vars(moc_mehanizma)) + 
  theme(legend.position = "bottom")



