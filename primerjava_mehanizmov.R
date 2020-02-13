library(reshape2)

#MAR
rez.df.MAR <-readRDS("data/shraniMAR.RDS")
melt.df.MAR <- melt(rez.df.MAR, id = c("delez_na", "moc_mehanizma", "N"))
melt.df.MAR$mehanizem <- rep("MAR", nrow(melt.df.MAR))

#NMAR
rez.df.NMAR <-readRDS("data/shraniNMAR.RDS")
melt.df.NMAR <- melt(rez.df.NMAR, id = c("delez_na", "moc_mehanizma", "N"))
melt.df.NMAR$mehanizem <- rep("NMAR", nrow(melt.df.NMAR))


#MCAR
rez.df.MCAR <-readRDS("data/shraniMCAR.RDS")
melt.df.MCAR <- melt(rez.df.MCAR, id = c("delez_na", "N"))
melt.df.MCAR$mehanizem <- rep("MCAR", nrow(melt.df.MCAR))




veliki.dataset <- rbind(subset(melt.df.MAR, moc_mehanizma == 1)[,-2], subset(melt.df.NMAR, moc_mehanizma == 1)[,-2], melt.df.MCAR)

veliki.dataset$mehanizem <- factor(veliki.dataset$mehanizem, levels = c("MAR", "NMAR", "MCAR"))

veliki.data <- veliki.dataset %>% group_by(variable,mehanizem,delez_na) %>%
  summarise(mean.a = mean(value),
            sd.a = sd(value),
            cent5 = quantile(value,probs = 0.05),
            cent95 = quantile(value,probs = 0.95)
  )

ggplot(veliki.data, aes(x = variable, y = mean.a, color = variable)) +
  geom_point()+
  geom_errorbar(aes(ymin = cent5, ymax=cent95), width =0.3)+
  facet_grid(rows = vars(delez_na), cols = vars(mehanizem))+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(colour = "white"))+
  xlab("Delež uspešnosti")+
  ylab("Metoda")+
  ggtitle("Primerjava mehanizmov manjkajočih vrednosti", subtitle = "Moč mehanizma: m = 1")





veliki.dataset <- rbind(subset(melt.df.MAR, moc_mehanizma == 12)[,-2], subset(melt.df.NMAR, moc_mehanizma == 12)[,-2], melt.df.MCAR)

veliki.dataset$mehanizem <- factor(veliki.dataset$mehanizem, levels = c("MAR", "NMAR", "MCAR"))

veliki.data <- veliki.dataset %>% group_by(variable,mehanizem,delez_na) %>%
  summarise(mean.a = mean(value),
            sd.a = sd(value),
            cent5 = quantile(value,probs = 0.05),
            cent95 = quantile(value,probs = 0.95)
  )


ggplot(veliki.data, aes(x = variable, y = mean.a, color = variable)) +
  geom_point()+
  geom_errorbar(aes(ymin = cent5, ymax=cent95), width =0.3)+
  facet_grid(rows = vars(delez_na), cols = vars(mehanizem))+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(colour = "white"))+
  xlab("Delež uspešnosti")+
  ylab("Metoda")+
  ggtitle("Primerjava mehanizmov manjkajočih vrednosti", subtitle = "Moč mehanizma: m = 12")



