library(readxl)
library(tidyverse)
library(Metrics)
library(writexl)

data <- read_excel("C:/GITHUB_REP/scale_analogues/data_correlations.xlsx", sheet = "TMIN")

data <- data[,-1]
model <- unique(data$MODEL)

tabla_cor <- data.frame(
  "Variable" = character(),
  "Models" = character(),
  "Pearson" = numeric(),
  "RMSE" = numeric()
)

Datalogger <- filter(data, data$MODEL == "Datalogger")
Chelsa     <- filter(data, data$MODEL == "Chelsa")
Microclima <- filter(data, data$MODEL == "Microclima")

Datalogger <- Datalogger[,-22]
Chelsa     <- Chelsa[,-22]
Microclima <- Microclima[,-22]

Datalogger <- data.frame(sapply(Datalogger, function(x) as.numeric(as.character(x))))
Chelsa     <- data.frame(sapply(Chelsa    , function(x) as.numeric(as.character(x))))
Microclima <- data.frame(sapply(Microclima, function(x) as.numeric(as.character(x))))

Datalogger <- reshape2::melt(Datalogger)
Chelsa <- reshape2::melt(Chelsa)    
Microclima <- reshape2::melt(Microclima)

tabla_cor[1, 1] <- c("TMax")
tabla_cor[1, 2] <- c("Datalogger - Chelsa")
tabla_cor[1, 3] <- cor(Datalogger[,2], Chelsa[,2], use="complete.obs")
tabla_cor[1, 4] <- hydroGOF::rmse(Datalogger[,2], Chelsa[,2], na.rm = TRUE)
tabla_cor[2, 1] <- c("TMax")
tabla_cor[2, 2] <- c("Datalogger - Microclima")
tabla_cor[2, 3] <- cor(Datalogger[,2], Microclima[,2], use="complete.obs")
tabla_cor[2, 4] <- hydroGOF::rmse(Datalogger[,2], Microclima[,2], na.rm = TRUE)
tabla_cor[3, 1] <- c("TMax")
tabla_cor[3, 2] <- c("Chelsa - Microclima")
tabla_cor[3, 3] <- cor(Chelsa[,2], Microclima[,2], use="complete.obs")
tabla_cor[3, 4] <- hydroGOF::rmse(Chelsa[,2], Microclima[,2], na.rm = TRUE)


tabla_cor[4, 1] <- c("TMin")
tabla_cor[4, 2] <- c("Datalogger - Chelsa")
tabla_cor[4, 3] <- cor(Datalogger[,2], Chelsa[,2], use="complete.obs")
tabla_cor[4, 4] <- hydroGOF::rmse(Datalogger[,2], Chelsa[,2], na.rm = TRUE)
tabla_cor[5, 1] <- c("TMin")
tabla_cor[5, 2] <- c("Datalogger - Microclima")
tabla_cor[5, 3] <- cor(Datalogger[,2], Microclima[,2], use="complete.obs")
tabla_cor[5, 4] <- hydroGOF::rmse(Datalogger[,2], Microclima[,2], na.rm = TRUE)
tabla_cor[6, 1] <- c("TMin")
tabla_cor[6, 2] <- c("Chelsa - Microclima")
tabla_cor[6, 3] <- cor(Chelsa[,2], Microclima[,2], use="complete.obs")
tabla_cor[6, 4] <- hydroGOF::rmse(Chelsa[,2], Microclima[,2], na.rm = TRUE)


range(Datalogger[,2], na.rm = T)
range(Chelsa[,2])

write_xlsx(tabla_cor, "A:/MONICA/COR_RMSE.xlsx")

###########################################


logger <- unique(data$Logger_No_)


tabla_cor <- data.frame(
  "Logger" = character(),
  "Models" = character(),
  "Pearson" = numeric(),
  "RMSE" = numeric()
)


for (i in 1:length(logger)){
  tabla <- data.frame(
    "Logger" =  NA,
    "Models" =  NA,
    "Pearson" = NA,
    "RMSE" =    NA
  )
  
  kk <- filter(data, data$Logger_No_ == logger[i])
  kk <-   as.data.frame(t(kk))
  colnames(kk) <- c("Datalogger", "Chelsa", "Microclima")
  kk <- kk[c(-1, -23),]
  kk <- data.frame(sapply(kk, function(x) as.numeric(as.character(x))))
  
  
  tabla[1, 1] <- logger[i]
  tabla[1, 2] <- c("Datalogger - Chelsa")
  tabla[1, 3] <- cor(kk$Datalogger, kk$Chelsa, na.rm=TRUE)
  tabla[1, 4] <- rmse(kk$Datalogger, kk$Chelsa)
  tabla[2, 1] <- logger[i]
  tabla[2, 2] <- c("Datalogger - Microclima")
  tabla[2, 3] <- cor(kk$Datalogger, kk$Microclima)
  tabla[2, 4] <- rmse(kk$Datalogger, kk$Microclima)
  tabla[3, 1] <- logger[i]
  tabla[3, 2] <- c("Chelsa - Microclima")
  tabla[3, 3] <- cor(kk$Chelsa, kk$Microclima)
  tabla[3, 4] <- rmse(kk$Chelsa, kk$Microclima)
  
  tabla_cor <- rbind(tabla_cor, tabla)
}
  

write_xlsx(tabla_cor, "A:/MONICA/COR_RMSE_TMIN.xlsx")
