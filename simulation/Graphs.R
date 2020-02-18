library(reshape2)
library(dplyr)
library(tidyr)
library(ggplot2)

####PODATKI######

rez.df <- readRDS("data/praviNAdata.RDS")

rez.df <- readRDS("data/LDA_imputation_data2020-02-18.RDS")
#Manjka še združevanje podatkov s Nacetovimi


#####GRAF MAR#####

rez.df.MAR <- rez.df %>% subset(mehanizem == "MAR") %>% select(-mehanizem)
melt.df.MAR <- melt(rez.df.MAR, id = c("delez_na", "moc_mehanizma"))

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
rez.df.NMAR <-rez.df %>% subset(mehanizem == "NMAR") %>% select(-mehanizem)
melt.df.NMAR <- melt(rez.df.NMAR, id = c("delez_na", "moc_mehanizma"))


ggplot(melt.df.NMAR, aes(x = variable, y = value, color = variable))+
  geom_boxplot()+
  facet_grid(cols = vars(delez_na))+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(colour = "white"))+
  ylab("Delež uspešnosti")+
  xlab("    ")+
  labs(color = "Metoda")

variable.df.NMAR <- melt.df.NMAR %>% group_by(variable, delez_na,moc_mehanizma) %>%
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

rez.df.MCAR <- rez.df %>% subset(mehanizem == "MCAR") %>% select(-mehanizem,-moc_mehanizma)

melt.df.MCAR <- melt(rez.df.MCAR, id = c("delez_na"))


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



#######PRIMERJAVA MEHANIZMOV ###############

rez.df.melt <- rez.df %>% 
  gather(metoda, vrednost, -c(delez_na,moc_mehanizma,mehanizem))


primerjava.data.mala.moc <- rez.df.melt %>% subset(moc_mehanizma %in% c(0,1)) %>%
  group_by(delez_na,mehanizem, metoda) %>%
  summarise(mean.a = mean(vrednost),
            sd.a = sd(vrednost),
            cent5 = quantile(vrednost,probs = 0.05),
            cent95 = quantile(vrednost,probs = 0.95)
  )

ggplot(primerjava.data.mala.moc, aes(x = metoda, y = mean.a, color = metoda)) +
  geom_point()+
  geom_errorbar(aes(ymin = cent5, ymax=cent95), width =0.3)+
  facet_grid(rows = vars(delez_na), cols = vars(mehanizem))+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(colour = "white"))+
  xlab("Delež uspešnosti")+
  ylab("Metoda")+
  ggtitle("Primerjava mehanizmov manjkajočih vrednosti", subtitle = "Moč mehanizma: m = 1")

#Velika moč

primerjava.data.velika.moc <- rez.df.melt %>% subset(moc_mehanizma %in% c(0,12)) %>%
  group_by(delez_na,mehanizem, metoda) %>%
  summarise(mean.a = mean(vrednost),
            sd.a = sd(vrednost),
            cent5 = quantile(vrednost,probs = 0.05),
            cent95 = quantile(vrednost,probs = 0.95)
  )

ggplot(primerjava.data.velika.moc, aes(x = metoda, y = mean.a, color = metoda)) +
  geom_point()+
  geom_errorbar(aes(ymin = cent5, ymax=cent95), width =0.3)+
  facet_grid(rows = vars(delez_na), cols = vars(mehanizem))+
  theme(legend.position = "bottom", 
        axis.text.x = element_text(colour = "white"))+
  ylab("Delež uspešnosti")+
  xlab("  ")+
  labs(color = "Metoda")+
  ggtitle("Primerjava mehanizmov manjkajočih vrednosti", subtitle = "Moč mehanizma: m = 12")





