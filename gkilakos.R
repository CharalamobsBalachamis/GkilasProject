library(quantmod)
library(ggplot2)
library(ggfortify)
library(dplyr)
library(lubridate)
library(tseries)

y <- getSymbols.yahoo("AMD", from = "2000-01-01", to = "2015-01-01",periodicity= "daily", auto.assign=F)
x1 <- getSymbols.yahoo("DJI", from = "2000-01-01", to = "2015-01-01",periodicity= "daily", auto.assign=F)

stats_amd <- summary(
  fortify.zoo(y) %>%
  select(AMD.Close)
)


stats_DJI <- summary(
  fortify.zoo(x1) %>%
    select(DJI.Close)
)


JB_test_amd <- jarque.bera.test(y$AMD.Close)

JB_test_DJI <-jarque.bera.test(x1$DJI.Close)




line_amd <- fortify.zoo(y) %>%
  select(Index , AMD.Close) %>%
  ggplot(aes(x=Index , y=AMD.Close)) +
  geom_line(colour = "red") +
  labs(
    x = "TIME",
    y = "AMD PRICE",
    title = "LINE PLOT")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "black") ) 
  




line_DJWI <- fortify.zoo(x1) %>%
  select(Index , DJI.Close) %>%
  ggplot(aes(x=Index , y=DJI.Close)) +
  geom_line(colour = "blue") +
  labs(
    x = "TIME",
    y = "INDEX PRICE",
    title = "LINE PLOT")+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "black") ) 


qq_amd <- fortify.zoo(y) %>%
  select(Index , AMD.Close) %>%
  ggplot(aes(sample=AMD.Close))+
  stat_qq(colour = "red") +
  stat_qq_line(size = 1) +
  labs(
    x = "THEORETICAL QUANTILES",
    y = "SAMPLE QUANTILES ",
    title = "QQ PLOT")



qq_DJWI <- fortify.zoo(x1) %>%
  select(Index , DJI.Close) %>%
  ggplot(aes(sample=DJI.Close))+
  stat_qq(colour = "blue") +
  stat_qq_line(size = 1) +
  labs(
    x = "THEORETICAL QUANTILES",
    y = "SAMPLE QUANTILES ",
    title = "QQ PLOT")



boxplot_amd <- fortify.zoo(y) %>%
  select(Index , AMD.Close) %>%
  ggplot(aes(x= Index, y= AMD.Close)) +
  geom_boxplot()

  
boxplot_DJWI <- fortify.zoo(x1) %>%
  select(Index , DJI.Close) %>%
  ggplot(aes(x= Index, y= DJI.Close)) +
  geom_boxplot()



lm_data <-fortify.zoo((cbind.zoo(x1,y)))

ggplot(lm_data, aes(x=DJI.Close, y=AMD.Close))+
  geom_point()+
  geom_smooth(method = "lm", se= F)


lm_model <- lm(AMD.Close ~ DJI.Close, data = lm_data)
coef(lm_model)
residuals(lm_model)
fitted.values(lm_model)
df.residual(lm_model)
summary(lm_model)
mean(residuals(lm_model)) == mean(fitted.values(lm_model))
confint(lm_model, level = 0.95)


yf <- getSymbols.yahoo("AMD", from = "2000-01-01", to = "2014-12-01",periodicity= "daily", auto.assign=F)

model_neo_data <-fortify.zoo((cbind.zoo(x1,yf)))

model_neo <- lm(AMD.Close ~ DJI.Close, data = model_neo_data)
predict(model_neo, data.frame(DJI.Close = 17823.1))
