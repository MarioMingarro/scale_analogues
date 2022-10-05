library(readxl)
library(reshape2)
library(ggpubr)
library(PupillometryR)
#Gruesa---------
H10 <-
  read_excel("D:/MNCN/ROB/MONICA/Paper/MODELS/H10_H1_AA_Tmed_DifEscalas.xlsx",
             sheet = "H10_AA")
H10_G <- filter(H10, H10$Escala == "Gruesa")
H10_G_m <- melt(H10_G[, c(5, 6)])
H10_G_p <- ggplot(H10_G) +
  geom_boxplot(
    data = H10_G_m,
    aes(x = Escala,
        y = value,
        colour = variable),
    outlier.shape = NA,
    alpha = .2,
    width = 5,
    colour = "gray50",
    fill = "darkslategray1",
    position = position_nudge(x = -2.5)
  ) +
  scale_color_manual(
    labels = c("Autumn/Winter", "Spring", "Summer"),
    values = c("dodgerblue4", "darkolivegreen4", " darkorange")
  ) +
  geom_point(
    aes(Cuadricula, Porcentaje, col = Estacion_IO_P_V),
    size = 2,
    alpha = .5
  ) +
  stat_summary(
    aes(
      Cuadricula,
      Porcentaje,
      col = Estacion_IO_P_V,
      group =
        H10_G$Estacion_IO_P_V
    ),
    fun = mean,
    geom = 'point',
    shape = '-',
    size = 10
  ) +
  scale_x_discrete(
    labels = c(
      "30TUK15B" = "Gre",
      "30TVL11C" = "Gua",
      "30TXK17D" = "Alb",
      "30TXK64D" = "Jav",
      "Gruesa" = " "
    )
  ) +
  scale_y_continuous("Analogous (%)",
                     c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     limits = c(0, 100)) +
  ggtitle("Mesoclimate") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text()
  ) +
  guides(fill =
           "none")


H10_F <-
  filter(H10, H10$Escala == "Fina")
H10_F_m <-
  melt(H10_F[, c(5, 6)])
H10_F_p <-
  ggplot(H10_F) +
  geom_boxplot(
    data = H10_F_m,
    aes(x = Escala,
        y = value,
        colour = variable),
    outlier.shape = NA,
    alpha = .2,
    width = 5,
    colour = "gray50",
    fill = "darkslategray1",
    position = position_nudge(x = -2.5)
  ) +
  scale_color_manual(
    labels = c("Autumn/Winter", "Spring", "Summer"),
    values = c("dodgerblue4", "darkolivegreen4", " darkorange")
  ) +
  geom_point(
    aes(Cuadricula, Porcentaje, col = Estacion_IO_P_V),
    size = 2,
    alpha = .5
  ) +
  stat_summary(
    aes(Cuadricula, Porcentaje, col = Estacion_IO_P_V,
        group =
          Estacion_IO_P_V),
    fun = mean,
    geom = 'point',
    shape = '-',
    size = 10
  ) +
  scale_x_discrete(
    labels = c(
      "30TUK15B" = "Gre",
      "30TVL11C" = "Gua",
      "30TXK17D" = "Alb",
      "30TXK64D" = "Jav",
      "Fina" = " "
    )
  ) +
  scale_y_continuous("Analogous (%)",
                     c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     limits = c(0, 100)) +
  ggtitle("H10 Microclimate") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  guides(fill =
           "none")
#Fina ---------
H1 <-
  read_excel("D:/MNCN/ROB/MONICA/Paper/MODELS/H10_H1_AA_Tmed_DifEscalas.xlsx",
             sheet = "H1_AA")
H1_G <-
  filter(H1, H1$Escala == "Gruesa")
H1_G_m <-
  melt(H1_G[, c(5, 6)])


H1_F <-
  filter(H1, H1$Escala == "Fina")
H1_F_m <-
  melt(H1_F[, c(5, 6)])
H1_F_p <-
  ggplot(H1_F) +
  geom_boxplot(
    data = H1_F_m,
    aes(x = Escala,
        y = value,
        colour = variable),
    outlier.shape = NA,
    alpha = .2,
    width = 5,
    colour = "gray50",
    fill = "darkslategray1",
    position = position_nudge(x = -2.5)
  ) +
  scale_color_manual(
    labels = c("Autumn/Winter", "Spring", "Summer"),
    values = c("dodgerblue4", "darkolivegreen4", " darkorange")
  ) +
  geom_point(
    aes(Cuadricula, Porcentaje, col = Estacion_IO_P_V),
    size = 2,
    alpha = .5
  ) +
  stat_summary(
    aes(Cuadricula, Porcentaje, col = Estacion_IO_P_V,
        group =
          Estacion_IO_P_V),
    fun = mean,
    geom = 'point',
    shape = '-',
    size = 10
  ) +
  scale_y_continuous("Analogous (%)",
                     c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     limits = c(0, 100)) +
  scale_x_discrete(
    labels = c(
      "30TUK15B" = "Gre",
      "30TVL11C" = "Gua",
      "30TXK17D" = "Alb",
      "30TXK64D" = "Jav",
      "Fina" = " "
    )
  ) +
  ggtitle("H1 Microclimate") +
  theme(
    panel.background = element_rect(fill = "white"),
    legend.title = element_blank(),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.title.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  guides(fill =
           "none")
ggarrange(
  H10_G_p,
  H1_F_p,
  H10_F_p,
  ncol = 3,
  nrow = 1,
  common.legend = TRUE,
  legend = "bottom"
)
