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
                    "lda_complete.cases", "lda_perfect.cases", "get.data.MCAR"))

registerDoParallel(cl)

pon <- 1
sampleSize <- 300
delez_na <- c(0.3, 0.4, 0.5, 0.6)
zasnova <- expand.grid(delez_na, sampleSize)
zasnova <- do.call(rbind, replicate(pon, zasnova, simplify=FALSE)) %>% `colnames<-`(c("delez_na",  "N"))

start_time <- Sys.time()
rez <- foreach(i = 1:nrow(zasnova), .combine = "rbind", 
               .packages = c("MASS", "mice", "missForest", 
                             "fpc", "DescTools", "DMwR", "norm")) %dorng% {
                               rngseed(45)
                               missNA.i <- zasnova[i, "delez_na"]
                               df <- get.data.MCAR( 300, prop.NA = missNA.i)
                               df.test <- get.data.MCAR( 1200, prop.NA = 0)[[1]]
                               data.NA <- df[[1]]
                               data.perfect <- df[[2]]
                               
                               comp <- lda_perfect.cases(data.perfect, df.test)
                               pair <- lda_complete.cases(data.NA, df.test)
                               rf <- lda_rf(data.NA, df.test)
                               em <- lda_EM.algoritem(data.NA, df.test)
                               knn <- lda_knn(data.NA, df.test, k =10)
                               mice <- lda_mice(data.NA, df.test, 10)
                               cbind(zasnova[i,], 
                                      "perfect.data" = comp,
                                      "complete.data" = pair,
                                      "knn.imputation"= knn, 
                                      "EM.imputation" = em, 
                                      "MICE.imputation" = mice,
                                      "RandomForest.imputation" = rf$prop)
                             }
end_time <- Sys.time()    
time.MCAR <- end_time - start_time
time.MCAR
stopCluster(cl)
registerDoSEQ()

saveRDS(object = as.data.frame(rez), paste("data/data_MCAR_", Sys.Date(),".RDS", sep = ""))

