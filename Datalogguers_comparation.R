library(readxl)
tmed_datalogers <- read_excel("B:/MARIPOSAS/SCALE_ANALOGOUS/MICROCLIMA/DATALOGUERS/HABITATS_APOLLO/MATRIZ_COMPLETA_MICROCLIMA_APOLLO_CHELSA.xlsx", 
                                                       sheet = "T_mean_dataloggers_mensual", 
                              col_types = c("text", "text", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric", "numeric", "numeric", 
                                            "numeric", "numeric", "numeric"))

tmed_chelsa <- read_excel("B:/MARIPOSAS/SCALE_ANALOGOUS/MICROCLIMA/DATALOGUERS/HABITATS_APOLLO/MATRIZ_COMPLETA_MICROCLIMA_APOLLO_CHELSA.xlsx", 
                              sheet = "T_mean_chelsa_mensual")

tmed_microclima <- read_excel("B:/MARIPOSAS/SCALE_ANALOGOUS/MICROCLIMA/DATALOGUERS/HABITATS_APOLLO/MATRIZ_COMPLETA_MICROCLIMA_APOLLO_CHELSA.xlsx", 
                          sheet = "T_mean_microclima_mensual")


averages <- as.data.frame(colMeans(tmed_datalogers[, 11:31], na.rm=TRUE))
averages <- cbind(averages,colMeans(tmed_chelsa[, 11:31], na.rm=TRUE))
averages <- cbind(averages,colMeans(tmed_microclima[, 11:31], na.rm=TRUE))
averages <- cbind(averages,
                  format(seq.Date(as.Date("2011-10-01"), as.Date("2013-06-01"), by = "month"),
                         format = "%m-%Y"))
averages <- cbind(averages,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21))

colnames(averages) <- c("Datalogguers", "Chelsa", "Microclima", "Date", "N")

library(tidyverse)
ggplot(averages)+
  geom_point(aes(x= N, y= Datalogguers), col= "brown2")+
  geom_smooth(aes(x=N, y= Datalogguers), col= "brown2", fill = "brown2", alpha = 0.2)+
  geom_smooth(aes(x=N, y= Datalogguers), col= "brown2",method = lm, se = FALSE)+
  stat_regline_equation(aes(x= N, y= Datalogguers),label.x=2, label.y=25, col = "brown2")+
  geom_point(aes(x= N, y= Chelsa), col= "deepskyblue2")+
  geom_smooth(aes(x=N, y= Chelsa), col= "deepskyblue2", fill = "deepskyblue2", alpha = 0.2)+
  geom_smooth(aes(x=N, y= Chelsa), col= "deepskyblue2",method = lm, se = FALSE)+
  stat_regline_equation(aes(x= N, y= Chelsa),label.x=2, label.y=22.5, col = "deepskyblue2")+
  geom_point(aes(x= N, y= Microclima), col= "chartreuse4")+
  geom_smooth(aes(x=N, y= Microclima), col= "chartreuse4", fill = "chartreuse4", alpha = 0.2)+
  geom_smooth(aes(x=N, y= Microclima), col= "chartreuse4",method = lm, se = FALSE)+
  stat_regline_equation(aes(x= N, y= Microclima),label.x=2, label.y=20, col = "chartreuse4")+
  labs(y = "Temperature (ºC)", x = "Date")+
  theme(
    panel.background = element_rect(fill = "white",
                                    colour = "white",
                                    size = 0.5, linetype = "solid"),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(family = "Courier", colour = "black", size = 10),
    axis.title = element_text(family = "Courier", colour = "black", size = 10),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

my.formula <- averages$Datalogguers ~ averages$N

a <- lm(averages$Datalogguers ~ averages$N)

a$coefficients[2]

my.formula <- y ~ x

ggplot(averages)+
  geom_point(aes(x= N, y= Datalogguers), col= "brown2")+
  geom_smooth(aes(x=N, y= Datalogguers), col= "brown2", fill = "brown2", alpha = 0.2)+
  stat_regline_equation(aes(x= N, y= Datalogguers),label.x=10, label.y=30, col = "blue")

library(ggpubr)





