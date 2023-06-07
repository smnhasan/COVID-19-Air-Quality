library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

# https://jroy042.github.io/nonlinear/week4.html
#https://sci-hub.hkvisa.net/10.1016/j.atmosenv.2007.02.032
#https://sci-hub.hkvisa.net/10.1016/j.envsoft.2019.03.027
#https://sci-hub.hkvisa.net/10.1111/rssc.12239
#https://bmcmedresmethodol.biomedcentral.com/articles/10.1186/1471-2288-12-165
#https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
#https://content.naic.org/sites/default/files/call_materials/GAM%20Smooth%20Examples.pdf
#https://environmentalcomputing.net/statistics/gams/
#https://multithreaded.stitchfix.com/blog/2015/07/30/gam/
#https://www.maths.ed.ac.uk/~swood34/talks/gam-mgcv.pdf
#https://petolau.github.io/Analyzing-double-seasonal-time-series-with-GAM-in-R/

setwd('E:\\ResearchProject\\Aminul\\COVID-19 Air Quality\\COVID-19 Air Quality')
Air_Covid <- read.csv("Air_Covid.csv")
Air_Covid2 <- Air_Covid[8:17]

Air_Covid3 <- subset(Air_Covid2, select = - c(COVID_BKK, PM10.BKK))         # Apply subset function
Air_Covid3 

summary(Air_Covid3)

sd(Air_Covid3$COVID_BKK_WM)
sd(Air_Covid3$PM2.5_BKK)
sd(Air_Covid3$WS.BKK)
sd(Air_Covid3$RH.BKK)
sd(Air_Covid3$AH.BKK)
sd(Air_Covid3$P_BKK)
sd(Air_Covid3$R_BKK)
sd(Air_Covid3$T.BKK)


t.test(Air_Covid[Air_Covid$Y == '2020', 'COVID_BKK_WM'], Air_Covid[Air_Covid$Y == '2021', 'COVID_BKK_WM'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'PM2.5_BKK'], Air_Covid[Air_Covid$Y == '2021', 'PM2.5_BKK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'WS.BKK'], Air_Covid[Air_Covid$Y == '2021', 'WS.BKK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'RH.BKK'], Air_Covid[Air_Covid$Y == '2021', 'RH.BKK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'AH.BKK'], Air_Covid[Air_Covid$Y == '2021', 'AH.BKK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'P_BKK'], Air_Covid[Air_Covid$Y == '2021', 'P_BKK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'R_BKK'], Air_Covid[Air_Covid$Y == '2021', 'R_BKK'])
t.test(Air_Covid[Air_Covid$Y == '2020', 'T.BKK'], Air_Covid[Air_Covid$Y == '2021', 'T.BKK'])
                     
ggpairs(Air_Covid3)



Air_Covid$time <- as.Date(Air_Covid$date, format = "%d/%m/%Y")

Air_Covid$COVID_BKK_WM = as.numeric(Air_Covid$COVID_BKK_WM)
Air_Covid$PM2.5_BKK = as.numeric(Air_Covid$PM2.5_BKK)
Air_Covid$WS.BKK = as.numeric(Air_Covid$WS.BKK)
Air_Covid$RH.BKK = as.numeric(Air_Covid$RH.BKK)
Air_Covid$AH.BKK = as.numeric(Air_Covid$AH.BKK)
Air_Covid$P_BKK = as.numeric(Air_Covid$P_BKK)
Air_Covid$R_BKK = as.numeric(Air_Covid$R_BKK)
Air_Covid$T.BKK = as.numeric(Air_Covid$T.BKK)
Air_Covid$Season = as.factor(Air_Covid$Season)

a <- ggplot(Air_Covid, aes(x=time, y=COVID_BKK_WM,color=Season)) + stat_smooth()+ theme(legend.title = element_text(color = "Black", size = 28),
                                                                                        legend.text = element_text(color = "Black", size = 28),
                                                                                        text = element_text(size = 28))
b <- ggplot(Air_Covid, aes(x=time, y=PM2.5_BKK,color=Season)) + stat_smooth()+ theme(legend.title = element_text(color = "Black", size = 28),
                                                                                     legend.text = element_text(color = "Black", size = 28),
                                                                                     text = element_text(size = 28))
c <- ggplot(Air_Covid, aes(x=time, y=WS.BKK,color=Season)) + stat_smooth()+ theme(legend.title = element_text(color = "Black", size = 28),
                                                                                  legend.text = element_text(color = "Black", size = 28),
                                                                                  text = element_text(size = 28))
d <- ggplot(Air_Covid, aes(x=time, y=RH.BKK,color=Season)) + stat_smooth()+ theme(legend.title = element_text(color = "Black", size = 28),
                                                                                  legend.text = element_text(color = "Black", size = 28),
                                                                                  text = element_text(size = 28))
e <- ggplot(Air_Covid, aes(x=time, y=AH.BKK,color=Season)) + stat_smooth()+ theme(legend.title = element_text(color = "Black", size = 28),
                                                                                  legend.text = element_text(color = "Black", size = 28),
                                                                                  text = element_text(size = 28))
f <- ggplot(Air_Covid, aes(x=time, y=P_BKK,color=Season)) + stat_smooth()+ theme(legend.title = element_text(color = "Black", size = 28),
                                                                                 legend.text = element_text(color = "Black", size = 28),
                                                                                 text = element_text(size = 28))
g <- ggplot(Air_Covid, aes(x=time, y=R_BKK,color=Season)) + stat_smooth()+ theme(legend.title = element_text(color = "Black", size = 28),
                                                                                 legend.text = element_text(color = "Black", size = 28),
                                                                                 text = element_text(size = 28))
h <- ggplot(Air_Covid, aes(x=time, y=T.BKK,color=Season)) + stat_smooth()+ theme(legend.title = element_text(color = "Black", size = 28),
                                                                                 legend.text = element_text(color = "Black", size = 28),
                                                                                 text = element_text(size = 28))



tiff("Graph.tiff", units="in", width=60, height=30, res=300)
gridExtra::grid.arrange(a,b,c,d,e,f,g,h)
dev.off()




Air_Covid$time = as.numeric(Air_Covid$time)
gamint3 = gam(COVID_BBK_WM ~ s(time) ,data=Air_Covid)
summary(gamint3)
visreg(gamint3 ,"time")

# gamint3 = gam(COVID_BBK_WM ~ s(time) + Season ,data=Air_Covid)
# summary(gamint3)
# 
# cbind(CIlower =  650.03 - 1.96 *  87.62 / sqrt(nrow(Air_Covid)), CIupper = 650.03 + 1.96 * 87.62 / sqrt(nrow(Air_Covid)))
# cbind(CIlower =  117.87 - 1.96 *  89.05 / sqrt(nrow(Air_Covid)), CIupper = 117.87 + 1.96 * 89.05 / sqrt(nrow(Air_Covid)))
# 
# 
# gamint3 = gam(COVID_BBK_WM ~ s(time) + PM2.5_BBK ,data=Air_Covid)
# summary(gamint3)
# 
# cbind(CIlower =  -2.027 - 1.96 *  1.941 / sqrt(nrow(Air_Covid)), CIupper = -2.027 + 1.96 * 1.941 / sqrt(nrow(Air_Covid)))
# 
# 
# gamint3 = gam(COVID_BBK_WM ~ s(time) + WS.BBK ,data=Air_Covid)
# summary(gamint3)
# 
# cbind(CIlower =  -163.30 - 1.96 *  64.53 / sqrt(nrow(Air_Covid)), CIupper = -163.30 + 1.96 * 64.53 / sqrt(nrow(Air_Covid)))
# 
# 
# gamint3 = gam(COVID_BBK_WM ~ s(time) + RH.BBK ,data=Air_Covid)
# summary(gamint3)
# 
# cbind(CIlower =  4.088 - 1.96 *  1.951 / sqrt(nrow(Air_Covid)), CIupper = 4.088 + 1.96 * 1.951 / sqrt(nrow(Air_Covid)))
# 
# 
# gamint3 = gam(COVID_BBK_WM ~ s(time) + AH.BBK ,data=Air_Covid)
# summary(gamint3)
# 
# cbind(CIlower =  12.464 - 1.96 *  7.555 / sqrt(nrow(Air_Covid)), CIupper = 12.464 + 1.96 * 7.555 / sqrt(nrow(Air_Covid)))
# 
# 
# gamint3 = gam(COVID_BBK_WM ~ s(time) + R_BBK ,data=Air_Covid)
# summary(gamint3)
# 
# cbind(CIlower =  11.20 - 1.96 *  12.01 / sqrt(nrow(Air_Covid)), CIupper = 11.20 + 1.96 * 12.01 / sqrt(nrow(Air_Covid)))
# 
# 
# gamint3 = gam(COVID_BBK_WM ~ s(time) + P_BBK ,data=Air_Covid)
# summary(gamint3)
# 
# cbind(CIlower =  6.314 - 1.96 *  2.283 / sqrt(nrow(Air_Covid)), CIupper = 6.314 + 1.96 * 2.283  / sqrt(nrow(Air_Covid)))
# 
# 
# gamint3 = gam(COVID_BBK_WM ~ s(time) + T.BBK ,data=Air_Covid)
# summary(gamint3)
# 
# cbind(CIlower =  -9.202 - 1.96 *  12.318 / sqrt(nrow(Air_Covid)), CIupper = -9.202 + 1.96 * 12.318  / sqrt(nrow(Air_Covid)))


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


