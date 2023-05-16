
############BBK################

library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

setwd('E:\\Air Quality - GAM')
Air_Covid <- read.csv("Air_Covid.csv")
Air_Covid2 <- Air_Covid[8:17]

Air_Covid3 <- subset(Air_Covid2, select = - c(COVID_BBK, PM10.BBK))         # Apply subset function
Air_Covid3 

summary(Air_Covid3)

sd(Air_Covid3$COVID_BBK_WM)
sd(Air_Covid3$PM2.5_BBK)
sd(Air_Covid3$WS.BBK)
sd(Air_Covid3$RH.BBK)
sd(Air_Covid3$AH.BBK)
sd(Air_Covid3$P_BBK)
sd(Air_Covid3$R_BBK)
sd(Air_Covid3$T.BBK)


t.test(Air_Covid[Air_Covid$Y == '2020', 'COVID_BBK_WM'], Air_Covid[Air_Covid$Y == '2021', 'COVID_BBK_WM'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'PM2.5_BBK'], Air_Covid[Air_Covid$Y == '2021', 'PM2.5_BBK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'WS.BBK'], Air_Covid[Air_Covid$Y == '2021', 'WS.BBK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'RH.BBK'], Air_Covid[Air_Covid$Y == '2021', 'RH.BBK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'AH.BBK'], Air_Covid[Air_Covid$Y == '2021', 'AH.BBK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'P_BBK'], Air_Covid[Air_Covid$Y == '2021', 'P_BBK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'R_BBK'], Air_Covid[Air_Covid$Y == '2021', 'R_BBK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'T.BBK'], Air_Covid[Air_Covid$Y == '2021', 'T.BBK'])

ggpairs(Air_Covid3)

Air_Covid$time <- as.Date(Air_Covid$date, format = "%d/%m/%Y")

Air_Covid$COVID_BBK_WM = as.numeric(Air_Covid$COVID_BBK_WM)
Air_Covid$PM2.5_BBK = as.numeric(Air_Covid$PM2.5_BBK)
Air_Covid$WS.BBK = as.numeric(Air_Covid$WS.BBK)
Air_Covid$RH.BBK = as.numeric(Air_Covid$RH.BBK)
Air_Covid$AH.BBK = as.numeric(Air_Covid$AH.BBK)
Air_Covid$P_BBK = as.numeric(Air_Covid$P_BBK)
Air_Covid$R_BBK = as.numeric(Air_Covid$R_BBK)
Air_Covid$T.BBK = as.numeric(Air_Covid$T.BBK)
Air_Covid$Season = as.factor(Air_Covid$Season)

ggplot(Air_Covid, aes(x=time, y=COVID_BBK_WM,color=Season)) + stat_smooth()

Air_Covid$time = as.numeric(Air_Covid$time)
gamint3 = gam(COVID_BBK_WM ~ s(time) ,data=Air_Covid)
summary(gamint3)
visreg(gamint3 ,"time")


gamint3 = gam(COVID_BBK_WM ~ s(time) + Season + PM2.5_BBK + WS.BBK + RH.BBK + AH.BBK + P_BBK + R_BBK + T.BBK,data=Air_Covid)
summary(gamint3)

acf(resid(gamint3), lag.max = 7, main = "ACF")
acf(resid(gamint3), lag.max = 14, main = "ACF")
acf(resid(gamint3), lag.max = 21, main = "ACF")


pacf(resid(gamint3), lag.max = 7, main = "pACF")
pacf(resid(gamint3), lag.max = 14, main = "pACF")
pacf(resid(gamint3), lag.max = 21, main = "pACF")

Air_Covid$COVID_BBK_WM_d1 <- diff(Air_Covid$COVID_BBK_WM)

gamint3 = gam(COVID_BBK_WM_d1 ~ s(time) + Season + PM2.5_BBK + WS.BBK + RH.BBK + AH.BBK + P_BBK + R_BBK + T.BBK,data=Air_Covid)
summary(gamint3)

gamint3$aic

cbind(CIlower =  656.414- 1.96 *  89.224 / sqrt(nrow(Air_Covid)), CIupper = 656.414 + 1.96 * 89.224  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  125.915 - 1.96 *  91.583 / sqrt(nrow(Air_Covid)), CIupper = 125.915 + 1.96 * 91.583  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -3.361 - 1.96 *  2.146 / sqrt(nrow(Air_Covid)), CIupper = -3.361 + 1.96 * 2.146  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -143.247 - 1.96 *  82.142 / sqrt(nrow(Air_Covid)), CIupper = -143.247 + 1.96 * 82.142  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  1.726 - 1.96 *  14.215 / sqrt(nrow(Air_Covid)), CIupper = 1.726 + 1.96 * 14.215  / sqrt(nrow(Air_Covid)))
cbind(CIlower = -5.801 - 1.96 *  51.744 / sqrt(nrow(Air_Covid)), CIupper = -5.801 + 1.96 * 51.744  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -2.556- 1.96 *  13.575 / sqrt(nrow(Air_Covid)), CIupper = -2.556 + 1.96 * 13.575  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  5.744 - 1.96 *  2.440 / sqrt(nrow(Air_Covid)), CIupper = 5.744 + 1.96 * 2.440  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  9.170 - 1.96 *  49.910 / sqrt(nrow(Air_Covid)), CIupper = 9.170 + 1.96 * 49.910  / sqrt(nrow(Air_Covid)))


############SSK################

library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

setwd('E:\\Air Quality - GAM')
Air_Covid <- read.csv("Air_Covid.csv")
Air_Covid2 <- Air_Covid[18:27]

Air_Covid3 <- subset(Air_Covid2, select = - c(COVID_SSK, PM10.SSK))         # Apply subset function
Air_Covid3 

summary(Air_Covid3)

sd(Air_Covid3$COVID_SSK_WM)
sd(Air_Covid3$PM2.5_SSK)
sd(Air_Covid3$WS.SSK)
sd(Air_Covid3$RH.SSK)
sd(Air_Covid3$AH.SSK)
sd(Air_Covid3$P_SSK)
sd(Air_Covid3$R_SSK)
sd(Air_Covid3$T.SSK)


t.test(Air_Covid[Air_Covid$Y == '2020', 'COVID_SSK_WM'], Air_Covid[Air_Covid$Y == '2021', 'COVID_SSK_WM'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'PM2.5_SSK'], Air_Covid[Air_Covid$Y == '2021', 'PM2.5_SSK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'WS.SSK'], Air_Covid[Air_Covid$Y == '2021', 'WS.SSK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'RH.SSK'], Air_Covid[Air_Covid$Y == '2021', 'RH.SSK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'AH.SSK'], Air_Covid[Air_Covid$Y == '2021', 'AH.SSK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'P_SSK'], Air_Covid[Air_Covid$Y == '2021', 'P_SSK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'R_SSK'], Air_Covid[Air_Covid$Y == '2021', 'R_SSK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'T.SSK'], Air_Covid[Air_Covid$Y == '2021', 'T.SSK'])

ggpairs(Air_Covid3)



Air_Covid$time <- as.Date(Air_Covid$date, format = "%d/%m/%Y")

Air_Covid$COVID_SSK_WM = as.numeric(Air_Covid$COVID_SSK_WM)
Air_Covid$PM2.5_SSK = as.numeric(Air_Covid$PM2.5_SSK)
Air_Covid$WS.SSK = as.numeric(Air_Covid$WS.SSK)
Air_Covid$RH.SSK = as.numeric(Air_Covid$RH.SSK)
Air_Covid$AH.SSK = as.numeric(Air_Covid$AH.SSK)
Air_Covid$P_SSK = as.numeric(Air_Covid$P_SSK)
Air_Covid$R_SSK = as.numeric(Air_Covid$R_SSK)
Air_Covid$T.SSK = as.numeric(Air_Covid$T.SSK)
Air_Covid$Season = as.factor(Air_Covid$Season)

ggplot(Air_Covid, aes(x=time, y=COVID_SSK_WM,color=Season)) + stat_smooth()

Air_Covid$time = as.numeric(Air_Covid$time)
gamint3 = gam(COVID_SSK_WM ~ s(time) ,data=Air_Covid)
summary(gamint3)
visreg(gamint3 ,"time")


gamint3 = gam(COVID_SSK_WM ~ s(time) + Season + PM2.5_SSK + WS.SSK + RH.SSK + AH.SSK + P_SSK + R_SSK + T.SSK,data=Air_Covid)
summary(gamint3)

gamint3$aic

cbind(CIlower =  189.856463- 1.96 *  33.610519 / sqrt(nrow(Air_Covid)), CIupper = 189.856463 + 1.96 * 33.610519  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  176.363331 - 1.96 *  33.984529 / sqrt(nrow(Air_Covid)), CIupper = 176.363331 + 1.96 * 33.984529  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -0.695654 - 1.96 *  0.529077 / sqrt(nrow(Air_Covid)), CIupper = -0.695654 + 1.96 * 0.529077 / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -88.018628 - 1.96 *  29.890294 / sqrt(nrow(Air_Covid)), CIupper = -88.018628 + 1.96 * 29.890294 / sqrt(nrow(Air_Covid)))
cbind(CIlower = -8.438565 - 1.96 * 2.095395 / sqrt(nrow(Air_Covid)), CIupper = -8.438565 + 1.96 * 2.095395  / sqrt(nrow(Air_Covid)))
cbind(CIlower = 32.078837 - 1.96 *  7.876408  / sqrt(nrow(Air_Covid)), CIupper = 32.078837 + 1.96 * 7.876408  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -0.062314  - 1.96 *  0.204009 / sqrt(nrow(Air_Covid)), CIupper = -0.062314 + 1.96 * 0.204009  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  0.026485 - 1.96 *  0.007104  / sqrt(nrow(Air_Covid)), CIupper = 0.026485 + 1.96 * 0.007104  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -12.373594 - 1.96 *  5.348515 / sqrt(nrow(Air_Covid)), CIupper = -12.373594 + 1.96 * 5.348515  / sqrt(nrow(Air_Covid)))




############NBR################

library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

setwd('E:\\Air Quality - GAM')
Air_Covid <- read.csv("Air_Covid.csv")
Air_Covid2 <- Air_Covid[38:47]

Air_Covid3 <- subset(Air_Covid2, select = - c(COVID_NBR, PM10.NBR))         # Apply subset function
Air_Covid3 

summary(Air_Covid3)

sd(Air_Covid3$COVID_NBR_WM)
sd(Air_Covid3$PM2.5_NBR)
sd(Air_Covid3$WS.NBR)
sd(Air_Covid3$RH.NBR)
sd(Air_Covid3$AH.NBR)
sd(Air_Covid3$P_NBR)
sd(Air_Covid3$R_NBR)
sd(Air_Covid3$T.NBR)


t.test(Air_Covid[Air_Covid$Y == '2020', 'COVID_NBR_WM'], Air_Covid[Air_Covid$Y == '2021', 'COVID_NBR_WM'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'PM2.5_NBR'], Air_Covid[Air_Covid$Y == '2021', 'PM2.5_NBR'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'WS.NBR'], Air_Covid[Air_Covid$Y == '2021', 'WS.NBR'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'RH.NBR'], Air_Covid[Air_Covid$Y == '2021', 'RH.NBR'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'AH.NBR'], Air_Covid[Air_Covid$Y == '2021', 'AH.NBR'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'P_NBR'], Air_Covid[Air_Covid$Y == '2021', 'P_NBR'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'R_NBR'], Air_Covid[Air_Covid$Y == '2021', 'R_NBR'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'T.NBR'], Air_Covid[Air_Covid$Y == '2021', 'T.NBR'])

ggpairs(Air_Covid3)



Air_Covid$time <- as.Date(Air_Covid$date, format = "%d/%m/%Y")

Air_Covid$COVID_NBR_WM = as.numeric(Air_Covid$COVID_NBR_WM)
Air_Covid$PM2.5_NBR = as.numeric(Air_Covid$PM2.5_NBR)
Air_Covid$WS.NBR = as.numeric(Air_Covid$WS.NBR)
Air_Covid$RH.NBR = as.numeric(Air_Covid$RH.NBR)
Air_Covid$AH.NBR = as.numeric(Air_Covid$AH.NBR)
Air_Covid$P_NBR = as.numeric(Air_Covid$P_NBR)
Air_Covid$R_NBR = as.numeric(Air_Covid$R_NBR)
Air_Covid$T.NBR = as.numeric(Air_Covid$T.NBR)
Air_Covid$Season = as.factor(Air_Covid$Season)

ggplot(Air_Covid, aes(x=time, y=COVID_NBR_WM,color=Season)) + stat_smooth()

Air_Covid$time = as.numeric(Air_Covid$time)
gamint3 = gam(COVID_NBR_WM ~ s(time) ,data=Air_Covid)
summary(gamint3)
visreg(gamint3 ,"time")


gamint3 = gam(COVID_NBR_WM ~ s(time) + Season + PM2.5_NBR + WS.NBR + RH.NBR + AH.NBR + P_NBR + R_NBR + T.NBR,data=Air_Covid)
summary(gamint3)

gamint3$aic

cbind(CIlower =  -17.100611 - 1.96 *  26.999043 / sqrt(nrow(Air_Covid)), CIupper = -17.100611 + 26.999043  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -33.145874 - 1.96 *  25.817622 / sqrt(nrow(Air_Covid)), CIupper = -33.145874 + 1.96 * 25.817622   / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -0.815007 - 1.96 *  0.578536 / sqrt(nrow(Air_Covid)), CIupper = -0.815007 + 1.96 * 0.578536 / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -45.976257  - 1.96 *  17.938422 / sqrt(nrow(Air_Covid)), CIupper = -45.976257  + 1.96 *   17.938422/ sqrt(nrow(Air_Covid)))
cbind(CIlower = 0.202216 - 1.96 * 1.021356 / sqrt(nrow(Air_Covid)), CIupper = 0.202216 + 1.96 * 1.021356  / sqrt(nrow(Air_Covid)))
cbind(CIlower = -1.815758 - 1.96 *  5.254280  / sqrt(nrow(Air_Covid)), CIupper = -1.815758 + 1.96 *5.254280  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  0.003767  - 1.96 *  0.035083  / sqrt(nrow(Air_Covid)), CIupper = 0.003767 + 1.96 *0.035083   / sqrt(nrow(Air_Covid)))
cbind(CIlower =  0.509326 - 1.96 *  0.657738  / sqrt(nrow(Air_Covid)), CIupper = 0.509326 + 1.96 * 0.657738  / sqrt(nrow(Air_Covid)))
cbind(CIlower = 0.937516 - 1.96 *  2.208160 / sqrt(nrow(Air_Covid)), CIupper = 0.937516 + 1.96 * 2.208160   / sqrt(nrow(Air_Covid)))





############PTN################

library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

setwd('E:\\Air Quality - GAM')
Air_Covid <- read.csv("Air_Covid.csv")
Air_Covid2 <- Air_Covid[48:57]

Air_Covid3 <- subset(Air_Covid2, select = - c(COVID_PTN, PM10.PTN))         # Apply subset function
Air_Covid3 

summary(Air_Covid3)

sd(Air_Covid3$COVID_PTN_WM)
sd(Air_Covid3$PM2.5_PTN)
sd(Air_Covid3$WS.PTN)
sd(Air_Covid3$RH.PTN)
sd(Air_Covid3$AH.PTN)
sd(Air_Covid3$P_PTN)
sd(Air_Covid3$R_PTN)
sd(Air_Covid3$T.PTN)


t.test(Air_Covid[Air_Covid$Y == '2020', 'COVID_PTN_WM'], Air_Covid[Air_Covid$Y == '2021', 'COVID_PTN_WM'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'PM2.5_PTN'], Air_Covid[Air_Covid$Y == '2021', 'PM2.5_PTN'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'WS.PTN'], Air_Covid[Air_Covid$Y == '2021', 'WS.PTN'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'RH.PTN'], Air_Covid[Air_Covid$Y == '2021', 'RH.PTN'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'AH.PTN'], Air_Covid[Air_Covid$Y == '2021', 'AH.PTN'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'P_PTN'], Air_Covid[Air_Covid$Y == '2021', 'P_PTN'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'R_PTN'], Air_Covid[Air_Covid$Y == '2021', 'R_PTN'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'T.PTN'], Air_Covid[Air_Covid$Y == '2021', 'T.PTN'])

ggpairs(Air_Covid3)



Air_Covid$time <- as.Date(Air_Covid$date, format = "%d/%m/%Y")

Air_Covid$COVID_PTN_WM = as.numeric(Air_Covid$COVID_PTN_WM)
Air_Covid$PM2.5_PTN = as.numeric(Air_Covid$PM2.5_PTN)
Air_Covid$WS.PTN = as.numeric(Air_Covid$WS.PTN)
Air_Covid$RH.PTN = as.numeric(Air_Covid$RH.PTN)
Air_Covid$AH.PTN = as.numeric(Air_Covid$AH.PTN)
Air_Covid$P_PTN = as.numeric(Air_Covid$P_PTN)
Air_Covid$R_PTN = as.numeric(Air_Covid$R_PTN)
Air_Covid$T.PTN = as.numeric(Air_Covid$T.PTN)
Air_Covid$Season = as.factor(Air_Covid$Season)

ggplot(Air_Covid, aes(x=time, y=COVID_PTN_WM,color=Season)) + stat_smooth()

Air_Covid$time = as.numeric(Air_Covid$time)
gamint3 = gam(COVID_PTN_WM ~ s(time) ,data=Air_Covid)
summary(gamint3)
visreg(gamint3 ,"time")


gamint3 = gam(COVID_PTN_WM ~ s(time) + Season + PM2.5_PTN + WS.PTN + RH.PTN + AH.PTN + P_PTN + R_PTN + T.PTN,data=Air_Covid)
summary(gamint3)

gamint3$aic

cbind(CIlower =  113.66973 - 1.96 *  18.27237 / sqrt(nrow(Air_Covid)), CIupper = 113.66973 + 18.27237  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  69.01187 - 1.96 *  18.09417 / sqrt(nrow(Air_Covid)), CIupper = 69.01187 + 1.96 * 18.09417   / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -0.36586 - 1.96 *  0.34951 / sqrt(nrow(Air_Covid)), CIupper = -0.36586 + 1.96 * 0.34951 / sqrt(nrow(Air_Covid)))
cbind(CIlower =  15.40169  - 1.96 * 10.86530 / sqrt(nrow(Air_Covid)), CIupper = 15.40169  + 1.96 *  10.86530/ sqrt(nrow(Air_Covid)))
cbind(CIlower =-0.83125  - 1.96 * 0.90084 / sqrt(nrow(Air_Covid)), CIupper = -0.83125  + 1.96 * 0.90084 / sqrt(nrow(Air_Covid)))
cbind(CIlower =  4.33406 - 1.96 *  3.39556  / sqrt(nrow(Air_Covid)), CIupper =  4.33406 + 1.96 * 3.39556  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -0.03870  - 1.96 *  0.01589  / sqrt(nrow(Air_Covid)), CIupper = -0.03870  + 1.96 * 0.01589   / sqrt(nrow(Air_Covid)))
cbind(CIlower =  0.39413 - 1.96 *  0.657738  / sqrt(nrow(Air_Covid)), CIupper = 0.39413 + 1.96 * 0.657738  / sqrt(nrow(Air_Covid)))
cbind(CIlower = 2.57652 - 1.96 *  2.208160 / sqrt(nrow(Air_Covid)), CIupper = 2.57652 + 1.96 * 2.208160   / sqrt(nrow(Air_Covid)))





############SPK################

library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

setwd('E:\\Air Quality - GAM')
Air_Covid <- read.csv("Air_Covid.csv")
Air_Covid2 <- Air_Covid[58:67]

Air_Covid3 <- subset(Air_Covid2, select = - c(COVID_SPK, PM10.SPK))         # Apply subset function
Air_Covid3 

summary(Air_Covid3)

sd(Air_Covid3$COVID_SPK_WM)
sd(Air_Covid3$PM2.5_SPK)
sd(Air_Covid3$WS.SPK)
sd(Air_Covid3$RH.SPK)
sd(Air_Covid3$AH.SPK)
sd(Air_Covid3$P_SPK)
sd(Air_Covid3$R_SPK)
sd(Air_Covid3$T.SPK)


t.test(Air_Covid[Air_Covid$Y == '2020', 'COVID_SPK_WM'], Air_Covid[Air_Covid$Y == '2021', 'COVID_SPK_WM'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'PM2.5_SPK'], Air_Covid[Air_Covid$Y == '2021', 'PM2.5_SPK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'WS.SPK'], Air_Covid[Air_Covid$Y == '2021', 'WS.SPK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'RH.SPK'], Air_Covid[Air_Covid$Y == '2021', 'RH.SPK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'AH.SPK'], Air_Covid[Air_Covid$Y == '2021', 'AH.SPK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'P_SPK'], Air_Covid[Air_Covid$Y == '2021', 'P_SPK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'R_SPK'], Air_Covid[Air_Covid$Y == '2021', 'R_SPK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'T.SPK'], Air_Covid[Air_Covid$Y == '2021', 'T.SPK'])

ggpairs(Air_Covid3)



Air_Covid$time <- as.Date(Air_Covid$date, format = "%d/%m/%Y")

Air_Covid$COVID_SPK_WM = as.numeric(Air_Covid$COVID_SPK_WM)
Air_Covid$PM2.5_SPK = as.numeric(Air_Covid$PM2.5_SPK)
Air_Covid$WS.SPK = as.numeric(Air_Covid$WS.SPK)
Air_Covid$RH.SPK = as.numeric(Air_Covid$RH.SPK)
Air_Covid$AH.SPK = as.numeric(Air_Covid$AH.SPK)
Air_Covid$P_SPK = as.numeric(Air_Covid$P_SPK)
Air_Covid$R_SPK = as.numeric(Air_Covid$R_SPK)
Air_Covid$T.SPK = as.numeric(Air_Covid$T.SPK)
Air_Covid$Season = as.factor(Air_Covid$Season)

ggplot(Air_Covid, aes(x=time, y=COVID_SPK_WM,color=Season)) + stat_smooth()

Air_Covid$time = as.numeric(Air_Covid$time)
gamint3 = gam(COVID_SPK_WM ~ s(time) ,data=Air_Covid)
summary(gamint3)
visreg(gamint3 ,"time")


gamint3 = gam(COVID_SPK_WM ~ s(time) + Season + PM2.5_SPK + WS.SPK + RH.SPK + AH.SPK + P_SPK + R_SPK + T.SPK,data=Air_Covid)
summary(gamint3)

gamint3$aic

cbind(CIlower =  147.2896 - 1.96 *  29.8587 / sqrt(nrow(Air_Covid)), CIupper = 147.2896 +29.8587  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -62.9617 - 1.96 *  30.6362 / sqrt(nrow(Air_Covid)), CIupper = -62.9617 + 1.96 * 30.6362   / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -0.4314  - 1.96 *  0.6902 / sqrt(nrow(Air_Covid)), CIupper = -0.4314  + 1.96 *0.6902 / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -8.9266  - 1.96 * 14.6243 / sqrt(nrow(Air_Covid)), CIupper = -8.9266 + 1.96 *  14.6243/ sqrt(nrow(Air_Covid)))
cbind(CIlower =2.4703   - 1.96 * 5.4862 / sqrt(nrow(Air_Covid)), CIupper = 2.4703  + 1.96 * 5.4862 / sqrt(nrow(Air_Covid)))
cbind(CIlower =   -0.2286 - 1.96 *  20.4724  / sqrt(nrow(Air_Covid)), CIupper =   -0.2286 + 1.96 * 20.4724  / sqrt(nrow(Air_Covid)))
cbind(CIlower =  8.6167   - 1.96 *  4.4864  / sqrt(nrow(Air_Covid)), CIupper = 8.6167  + 1.96 * 4.4864   / sqrt(nrow(Air_Covid)))
cbind(CIlower =  -1.9033 - 1.96 *  0.7880 / sqrt(nrow(Air_Covid)), CIupper = -1.9033 + 1.96 * 0.7880  / sqrt(nrow(Air_Covid)))
cbind(CIlower =3.6044 - 1.96 *  20.7004 / sqrt(nrow(Air_Covid)), CIupper = 3.6044 + 1.96 *20.7004   / sqrt(nrow(Air_Covid)))

