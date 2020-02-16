library(reshape2)
library(dplyr)
library(ggplot2)

#####GRAF MAR#####
#rez.df.MAR <-readRDS(paste("data/data_MAR_", Sys.Date(),".RDS", sep = ""))
rez.df.MAR <- readRDS("data/shraniNMAR.RDS")
melt.df.MAR <- melt(rez.df.MAR, id = c("delez_na", "moc_mehanizma", "N"))

ggplot(melt.df.MAR, aes(x = variable, y = value, color = variable))+
  geom_boxplot()+
  facet_grid(cols = vars(delez_na))+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(colour = "white"))+
  ylab("Delež uspešnosti")+
  xlab("    ")+
  labs(color = "Metoda")

variable.df.MAR <- melt.df.MAR %>% group_by(variable, delez_na) %>%
  summarise(prop.mean = mean(value),
            prop.sd = sd(value))


summrise.df.MAR <- melt.df.MAR %>% group_by(variable, moc_mehanizma, delez_na) %>%
  summarise(prop.mean = mean(value),
            prop.sd = sd(value))

ggplot(summrise.df.MAR, aes(x = delez_na, y = prop.mean, color = variable, group =variable)) +
  geom_line()+
  geom_point()+
  ylab("Povprečni delež uspešnosti")+
  xlab("Delež NA vrednosti")+
  labs(color = "Metoda")+
  facet_grid(cols= vars(moc_mehanizma)) + 
  theme(legend.position = "bottom")


#######GRAF NMAR########
rez.df.NMAR <-readRDS("data/shraniNMAR.RDS")

melt.df.NMAR <- melt(rez.df.NMAR, id = c("delez_na", "moc_mehanizma", "N"))


ggplot(melt.df.NMAR, aes(x = variable, y = value, color = variable))+
  geom_boxplot()+
  facet_grid(cols = vars(delez_na))+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(colour = "white"))+
  ylab("Delež uspešnosti")+
  xlab("    ")+
  labs(color = "Metoda")

variable.df.NMAR <- melt.df.NMAR %>% group_by(variable, delez_na) %>%
  summarise(prop.mean = mean(value),
            prop.sd = sd(value))


summrise.df.NMAR <- melt.df.NMAR %>% group_by(variable, moc_mehanizma, delez_na) %>%
  summarise(prop.mean = mean(value),
            prop.sd = sd(value))

ggplot(summrise.df.NMAR,aes(x = delez_na, y = prop.mean, color = variable, group =variable)) +
  geom_line()+
  geom_point()+
  ylab("Povprečni delež uspešnosti")+
  xlab("Delež NA vrednosti")+
  labs(color = "Metoda")+
  facet_grid(cols= vars(moc_mehanizma)) + 
  theme(legend.position = "bottom")

####GRAF MCAR ########

rez.df.MCAR <-readRDS("data/shraniMCAR.RDS")

melt.df.MCAR <- melt(rez.df.MCAR, id = c("delez_na", "N"))


ggplot(melt.df.MCAR, aes(x = variable, y = value, color = variable))+
  geom_boxplot()+
  facet_grid(cols = vars(delez_na))+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(colour = "white"))+
  ylab("Delež uspešnosti")+
  xlab("    ")+
  labs(color = "Metoda")

variable.df.MCAR <- melt.df.MCAR %>% group_by(variable, delez_na) %>%
  summarise(prop.mean = mean(value),
            prop.sd = sd(value))


summrise.df.MCAR <- melt.df.MCAR %>% group_by(variable, delez_na) %>%
  summarise(prop.mean = mean(value),
            prop.sd = sd(value))

ggplot(summrise.df.MCAR,aes(x = delez_na, y = prop.mean, color = variable, group =variable)) +
  geom_line()+
  geom_point()+
  ylab("Povprečni delež uspešnosti")+
  xlab("Delež NA vrednosti")+
  labs(color = "Metoda")+
  theme(legend.position = "bottom")






