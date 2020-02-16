#PARALENE SIMULACIJE
source("Generation_data.r")
source("Imputation_Models.R")
library(parallel)
library(doParallel)
library(gridExtra)
library(magrittr)
library(foreach)
library(doRNG)
rngseed(45) 

# # poglej stevilo jedr in odstej enega (za nase delo)
no_cores <- detectCores() - 1


# # doloci "skupine" za delo
cl <- makeCluster(no_cores)
# 
# # registriraj "parallel beckend"
clusterExport(cl, c("lda_mice", "lda_knn", "lda_EM.algoritem", "lda_rf",
                    "lda_complete.cases", "lda_perfect.cases", "generation.data",
                    "get.NA.MAR", "get.NA.NMAR", "get.NA.MCAR", "make.imputation"))

registerDoParallel(cl)


pon <- 2
sampleSize <- 300
delez_na <- c(0.3, 0.4, 0.5, 0.6)
moc <- c(1, 2, 5, 8, 12)
zasnova <- expand.grid(delez_na,  moc, mehanizem)
zasnova <- do.call(rbind, replicate(pon, zasnova, simplify=FALSE)) %>% `colnames<-`(c("delez_na", "moc_mehanizma"))



start_time <- Sys.time()
rez <- foreach(i = 1:nrow(zasnova), .combine = "rbind", 
               .packages = c("MASS", "mice", "missForest", 
                             "fpc", "DescTools", "DMwR", "norm")) %dorng% {
                               rngseed(45)
                               missNA.i <- zasnova[i, "delez_na"]
                               moc.i <- zasnova[i, "moc_mehanizma"]
                               data.perfect <- generation.data(N = sampleSize)
                               data.test <- generation.data(N = 1200)
                               data.NA.MAR <- get.NA.MAR(dataSet = data.perfect,
                                                         prop.NA = missNA.i,
                                                         moc.mehanizma = moc.i)
                               data.NA.NMAR <- get.NA.NMAR(dataSet =data.perfect, 
                                                           prop.NA =missNA.i,
                                                           moc.mehanizma = moc.i)
                               data.NA.MCAR <- get.NA.MCAR(dataSet = data.perfect,
                                                           prop.NA = missNA.i)
                               
                              imputation.MAR <- make.imputation(data.perfect,data.NA.MAR, data.test)
                              imputation.NMAR <- make.imputation(data.perfect,data.NA.NMAR, data.test)
                              imputation.MCAR <- make.imputation(data.perfect,data.NA.MCAR, data.test)
                              
                              trentutniDF <- NULL
                              trentutniDF <- rbind(trentutniDF, c("delez_na" = missNA.i, "moc_mehanizma" = moc.i, "mehanizem" = "MAR", imputation.MAR) )
                              trentutniDF <- rbind(trentutniDF, c("delez_na" = missNA.i, "moc_mehanizma" = moc.i, "mehanizem" = "NMAR", imputation.NMAR) )
                              trentutniDF <- rbind(trentutniDF, c("delez_na" = missNA.i, "moc_mehanizma" = 0,     "mehanizem" = "MCAR", imputation.MCAR))
                              trentutniDF
                             }

rez.d <- as.data.frame(apply(rez[,-3],2, as.numeric))
rez.d$mehanizem <- rez[,3]
end_time <- Sys.time()   
time<- end_time - start_time
time
stopCluster(cl)
registerDoSEQ()
saveRDS(object = as.data.frame(rez.d), paste("data/LDA_imputation_data", Sys.Date(),".RDS", sep = ""))
