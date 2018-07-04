setwd("C:/Users/victo/Documents/FAO")

dat <- read.csv('BD_CO_FAO.csv', dec=",", na.strings = c("ND", "NO", "NA", "M", "HAY"))
dat <- dat[!is.na(dat$DENSIDAD_LAB),]

str(dat)

dat <- dat[, c("DENSIDAD_LAB", "ORDEN_SUELO", "NOMENCLAUR", "MO_VALOR",
                        "ARCILLA", "LIMO", "ARENA")]

set.seed("123")

## Generalizamos las nomencla
dat$NOMENCLAUR <- substr(dat$NOMENCLAUR, 0 ,1)

dat$NOMENCLAUR[dat$NOMENCLAUR=="b"] <- "B"
dat$NOMENCLAUR[dat$NOMENCLAUR=="a"] <- "A"
dat$NOMENCLAUR[dat$NOMENCLAUR=="R"] <- NA

dat$NOMENCLAUR <- as.factor(dat$NOMENCLAUR)

dat$NOMENCLAUR

dat <- dat[complete.cases(dat),]

## Code by Olmedo, GF; Guevara, MA; Ramcharan, AM
library(caret)
inTrain <- createDataPartition(y = dat$DENSIDAD_LAB, p = .75, list = FALSE)
dat.training <- dat[ inTrain,] # n 39359
dat.validation <- dat[-inTrain,] # n 13117



###################################################################################
#### Modeling #######
library(randomForestSRC)
library(caret)


dat <- dat[complete.cases(dat),]

# rf
fm <- DENSIDAD_LAB ~ ORDEN_SUELO  + MO_VALOR + ARCILLA + LIMO + ARENA
model.rf <- rfsrc(fm, data = dat, ntree = 150)



dat.validation$BLDrf <- predict(model.rf, dat.validation)$predicted


rmse(dat.validation$DENSIDAD_LAB[!is.na(dat.validation$BLDrf)], dat.validation$BLDrf[!is.na(dat.validation$BLDrf)])  #0.186

plot(dat.validation$BLDrf, dat.validation$DENSIDAD_LAB)
abline(0,1)

BLDrfCOL <- function(ORDEN_SUELO, MO_VALOR, ARCILLA, LIMO, ARENA){
  # prepare data.frame and predict
  dat <- data.frame(ORDEN_SUELO = ORDEN_SUELO,
                    MO_VALOR = as.numeric(MO_VALOR), 
                    ARCILLA = as.numeric(ARCILLA),
                    LIMO = as.numeric(LIMO), 
                    ARENA = as.numeric(ARENA))
  BLD <- predict(model.rf, dat)$predicted
  return(BLD)
}

BLDrfCOL(ORDEN_SUELO = "HISTOSOLES",  MO_VALOR = 52.1, 
         ARCILLA = 30, ARENA = 10, LIMO = 60)

save(model.rf, BLDrfCOL, file="BLDrf_COLOMBIA.RData")
