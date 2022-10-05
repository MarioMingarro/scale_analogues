---
  title: "Figure 4"
author: "Mario"
date: "9/6/2022"
output: pdf_document
---
  
  
  ```{r}
library(readxl)
library(reshape2)
library(ggpubr)
library(PupillometryR)


H10 <- read_excel("D:/MNCN/ROB/MONICA/Paper/MODELS/Datos_AA_H1_7_10.xlsx")

#Gredos---------
G <- filter(H10, H10$Zona == "1")
G_m<- melt(G[,c(3,5,8)])



Gp<-ggplot(G_m) +
  geom_boxplot(
    data = G_m,
    aes(
      x = Habitat,
      y = value,
      fill = Habitat
    ),
    outlier.shape = NA,
    alpha = .2,
    width = 3,
    colour = "gray50",
    fill = "darkslategray1"
  ) +
  stat_summary(aes(G$Habitat, G$Porcentaje_AA, col = G$Estacion_IO_P_V, group=G$Estacion_IO_P_V),fun=mean,  geom='point', shape='-', size=10) +
  stat_summary(aes(G$Habitat, G$Porcentaje_AA, col = G$Estacion_IO_P_V, group=G$Estacion_IO_P_V),fun=mean,  geom='line', lty=2)+
  geom_point(aes(G$Habitat, G$Porcentaje_AA, col = G$Estacion_IO_P_V), size= 2, alpha = .5) +
  scale_color_manual(labels = c("Winter/Autumn", "Spring", "Summer"), values = c("dodgerblue4", "darkolivegreen4"," darkorange"))+
  scale_y_continuous("Analogous (%)",
                     c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     limits = c(0, 100)) +
  ggtitle("Gredos") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y  = element_text()
  )+
  guides(fill="none")

### Guadarrama ------------------
#Gredos---------
Gu <- filter(H10, H10$Zona == "2")
Gu_m<- melt(Gu[,c(3,5,8)])



Gup<-ggplot(Gu_m) +
  geom_boxplot(
    data = Gu_m,
    aes(
      x = Habitat,
      y = value,
      fill = Habitat
    ),
    outlier.shape = NA,
    alpha = .2,
    width = 3,
    colour = "gray50",
    fill = "darkslategray1"
  ) +
  stat_summary(aes(Gu$Habitat, Gu$Porcentaje_AA, col = Gu$Estacion_IO_P_V, group=Gu$Estacion_IO_P_V),fun=mean,  geom='point', shape='-', size=10) +
  stat_summary(aes(Gu$Habitat, Gu$Porcentaje_AA, col = Gu$Estacion_IO_P_V, group=Gu$Estacion_IO_P_V),fun=mean,  geom='line', lty=2)+
  geom_point(aes(Gu$Habitat, Gu$Porcentaje_AA, col = Gu$Estacion_IO_P_V), size= 2, alpha = .5) +
  scale_color_manual(labels = c("Winter/Autumn", "Spring", "Summer"), values = c("dodgerblue4", "darkolivegreen4", "darkorange"))+
  scale_y_continuous("Analogous (%)",
                     c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     limits = c(0, 100)) +
  ggtitle("Guadarrama") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )+
  guides(fill="none")

### Albarracin ------------------

A <- filter(H10, H10$Zona == "3")
A_m<- melt(A[,c(3,5,8)])



Ap<-ggplot(A_m) +
  geom_boxplot(
    data = A_m,
    aes(
      x = Habitat,
      y = value,
      fill = Habitat
    ),
    outlier.shape = NA,
    alpha = .2,
    width = 3,
    colour = "gray50",
    fill = "darkslategray1"
  ) +
  stat_summary(aes(A$Habitat, A$Porcentaje_AA, col = A$Estacion_IO_P_V, group=A$Estacion_IO_P_V),fun=mean,  geom='point', shape='-', size=10) +
  stat_summary(aes(A$Habitat, A$Porcentaje_AA, col = A$Estacion_IO_P_V, group=A$Estacion_IO_P_V),fun=mean,  geom='line', lty=2)+
  geom_point(aes(A$Habitat, A$Porcentaje_AA, col = A$Estacion_IO_P_V), size= 2, alpha = .5) +
  scale_color_manual(labels = c("Winter/Autumn", "Spring", "Summer"), values = c("dodgerblue4", "darkolivegreen4", "darkorange"))+
  scale_y_continuous("Analogous (%)",
                     c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     limits = c(0, 100)) +
  ggtitle("Abarracín") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(),
    axis.ticks.x = element_blank(),
    axis.text.y  = element_text()
  )+
  guides(fill="none")



### Javalambre ------------------

J <- filter(H10, H10$Zona == "4")
J_m<- melt(J[,c(3,5,8)])



Jp <- ggplot(J_m) +
  geom_boxplot(
    data = J_m,
    aes(x = Habitat,
        y = value,
        fill = Habitat),
    outlier.shape = NA,
    alpha = .2,
    width = 3,
    colour = "gray50",
    fill = "darkslategray1"
  ) +
  geom_point(
    aes(J$Habitat, J$Porcentaje_AA, col = J$Estacion_IO_P_V),
    size = 2,
    alpha = .5
  ) +
  stat_summary(aes(J$Habitat, J$Porcentaje_AA, col = J$Estacion_IO_P_V, group=J$Estacion_IO_P_V),fun=mean,  geom='point', shape='-', size=10) +
  stat_summary(aes(J$Habitat, J$Porcentaje_AA, col = J$Estacion_IO_P_V, group=J$Estacion_IO_P_V),fun=mean,  geom='line', lty=2)+
  scale_color_manual(
    labels = c("Winter/Autumn", "Spring", "Summer"),
    values = c("dodgerblue4", "darkolivegreen4", "darkorange")
  ) +
  scale_y_continuous("Analogous (%)",
                     c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     limits = c(0, 100)) +
  ggtitle("Javalambre") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

ggarrange(Gp, Gup, Ap, Jp,  common.legend = TRUE, legend = "bottom")


```

