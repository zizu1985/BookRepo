require(fpp2)
require(gridExtra)

# Simple linear regression 

# Wykres 2 szeregow czasowych
autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

View(uschange)

# Wykres dwoch zmiennych w formie scatterplot
# razem z regresja liniowa (Consumption ~ Income)
uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

# Simple linear regression
# Consumption = B0 + B1*Income + error
tslm(Consumption ~ Income, data=uschange)
#Call:
#  tslm(formula = Consumption ~ Income, data = uschange)

#Coefficients:
#  (Intercept)       Income  
#0.5451       0.2806 



# Multiple linear regression - wiele zmiennych 
# Badanie korelacji miedzy parami zmiennych 
uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()

# 
fit.consMR <- tslm(
  Consumption ~ Income + Production + Unemployment + Savings,
  data=uschange)
summary(fit.consMR)
#Call:
#  tslm(formula = Consumption ~ Income + Production + Unemployment + 
#         Savings, data = uschange)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-0.88296 -0.17638 -0.03679  0.15251  1.20553 
#
#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.26729    0.03721   7.184 1.68e-11 ***
#  Income        0.71449    0.04219  16.934  < 2e-16 ***
#  Production    0.04589    0.02588   1.773   0.0778 .  
#Unemployment -0.20477    0.10550  -1.941   0.0538 .  
#Savings      -0.04527    0.00278 -16.287  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.3286 on 182 degrees of freedom
#Multiple R-squared:  0.754,	Adjusted R-squared:  0.7486 
#F-statistic: 139.5 on 4 and 182 DF,  p-value: < 2.2e-16


#             Estimate S
# (Intercept)     0.26729  
# Income          0.71449
# Production      0.04589
# Unemployment   -0.20477  
# Savings        -0.04527  


# Narysowanie sfitowanych wartosci i rzeczywistych danych
autoplot(uschange[,'Consumption'], series="Data") +
  autolayer(fitted(fit.consMR), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("Percent change in US consumption expenditure") +
  guides(colour=guide_legend(title=" "))

# Rzeczywiste dane vs dopasowane
# Scattered plot draw
cbind(Data = uschange[,'Consumption'],
      Fitted = fitted(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  xlab("Fitted (predicted values)") +
  ylab("Data (actual values)") +
  ggtitle("Percent change in US consumption expanditure") +
  geom_abline(intercept=0,slope=1)

#### Oceniamy model (Model evaluation)
checkresiduals(fit.consMR)

#Breusch-Godfrey test for serial correlation of order up to 8
#
#data:  Residuals from Linear regression model
#LM test = 14.874, df = 8, p-value = 0.06163
#

df <- as.data.frame(uschange)
df[,"Residuals"] <- as.numeric(residuals(fit.consMR))
p1 <- ggplot(df, aes(x=Income, y=Residuals)) + geom_point()
p2 <- ggplot(df, aes(x=Production, y=Residuals)) + geom_point()
p3 <- ggplot(df, aes(x=Savings, y=Residuals)) + geom_point()
p4 <- ggplot(df, aes(x=Unemployment, y=Residuals)) + geom_point()
gridExtra::grid.arrange(p1,p2,p3,p4, nrow=2)

cbind(Fitted = fitted(fit.consMR),
      Residuals=residuals(fit.consMR)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Fitted, y=Residuals)) + geom_point()

# Falszywa regresja (dla szeregow niestacjonarnych)
aussies <- window(ausair, end=2011)
fit <- tslm(aussies ~ guinearice)
summary(fit)

checkresiduals(fit)
# Breusch-Godfrey test for serial correlation of order up to 8
#
# data:  Residuals from Linear regression model
# LM test = 28.813, df = 8, p-value = 0.000342

################################ 
# Usefull predictors
################################

beer2 <- window(ausbeer, start=1992)
autoplot(beer2) + xlab("Year") + ylab("Megalitres")

fit.beer <- tslm(beer2 ~ trend + season)
summary(fit.beer)


autoplot(beer2, series="Data") +
  autolayer(fitted(fit.beer), series="Fitted") +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Quarterly Beer Production")


cbind(Data=beer2, Fitted=fitted(fit.beer)) %>%
  as.data.frame() %>%
  ggplot(aes(x = Data, y = Fitted,
             colour = as.factor(cycle(beer2)))) +
  geom_point() +
  ylab("Fitted") + xlab("Actual values") +
  ggtitle("Quarterly beer production") +
  scale_colour_brewer(palette="Dark2", name="Quarter") +
  geom_abline(intercept=0, slope=1)

# Regresja z wykorzystaniem Fouriera
fourier.beer <- tslm(beer2 ~ trend + fourier(beer2, K=2))
summary(fourier.beer)

# Metryki dopasowania dla modelu regresji liniowej 
CV(fit.consMR)
# CV          AIC         AICc          BIC           AdjR2 
# 0.1163477 -409.2980298 -408.8313631 -389.9113781    0.7485856 

########## Cross-validation


### Regresja w przewidywaniu wartosci szeregow
beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres")


# Scenario based forecasting
# Zakladamy co sie stanie z wartosciami predyktorow i jaka bedzie zmiana konsumpcji
fit.consBest <- tslm(
  Consumption ~ Income + Savings + Unemployment,
  data = uschange
)
h <- 4
newdata <- data.frame(
  Income = c(1, 1, 1, 1),
  Savings = c(0.5, 0.5, 0.5, 0.5),
  Unemployment = c(0, 0, 0, 0))
fcast.up <- forecast(fit.consBest, newdata=newdata)
newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(-0.5,h),
  Unemployment = rep(0,h))
fcast.down <- forecast(fit.consBest, newdata = newdata)

autoplot(uschange[,1]) +
  ylab("% change in US consumption") +
  autolayer(fcast.up, PI=TRUE, series="increase") +
  autolayer(fcast.down, PI=TRUE, series="decrease") +
  guides(colour = guide_legend(title="Scenario"))

# Jest to rodzaj przewidywania ex-post forecasts
# Tzn znamy wartosci predykatorow w przyszlosci przed przewidywanie wartosci


# Prediction intervals
fit.cons <- tslm(Consumption ~ Income, data=uschange)
h <- 4
fcast.ave <- forecast(fit.cons,
                      newdata = data.frame(
                        Income = rep(mean(uschange[,"Income"]),h)))

# Nonlinear regression
h <- 10
fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h = h)
fit.exp <- tslm(marathon ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h = h)
t <- time(marathon)
t.break1 <- 1940
t.break2 <- 1980
tb1 <- ts(pmax(0, t - t.break1), start = 1897)
tb2 <- ts(pmax(0, t - t.break2), start = 1897)
fit.pw <- tslm(marathon ~ t + tb1 + tb2)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)
newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>%
  as.data.frame()

fcasts.pw <- forecast(fit.pw, newdata = newdata)
fit.spline <- tslm(marathon ~ t + I(t^2) + I(t^3) +
                     I(tb1^3) + I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata = newdata)
autoplot(marathon) +
  autolayer(fitted(fit.lin), series = "Linear") +
  autolayer(fitted(fit.exp), series = "Exponential") +
  autolayer(fitted(fit.pw), series = "Piecewise") +
  autolayer(fitted(fit.spline), series = "Cubic Spline") +
  autolayer(fcasts.pw, series="Piecewise") +
  autolayer(fcasts.lin, series="Linear", PI=FALSE) +
  autolayer(fcasts.exp, series="Exponential", PI=FALSE) +
  autolayer(fcasts.spl, series="Cubic Spline", PI=FALSE) +
  xlab("Year") + ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour = guide_legend(title = " "))


########### Exercises #######################

###########################################################
###########################################################
###########################################################
# Ex.1 Daily electricity demand for Victoria
daily20 <- head(elecdaily,20)
View(daily20)

# a. Regression Model Demand ~ Temperature
fit.demand = tslm(
  Demand ~ Temperature,
  data = daily20
)
summary(fit.demand)

#Call:
#tslm(formula = Demand ~ Temperature, data = daily20)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-46.060  -7.117  -1.437  17.484  27.102 
#
#Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  39.2117    17.9915   2.179   0.0428 *  
#  Temperature   6.7572     0.6114  11.052 1.88e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 22 on 18 degrees of freedom
#Multiple R-squared:  0.8716,	Adjusted R-squared:  0.8644 
#F-statistic: 122.1 on 1 and 18 DF,  p-value: 1.876e-0
#

# Demand = 39.2117 + 6.7572 * Temperature

daily20 %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  ylab("Demand") +
  xlab("Temperature") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

# Pozytywne, poniewa¿ wiecej chlodzenia jest zu¿ywane

# Wykres residuum - oceniamy model (Model evaluation)
checkresiduals(fit.demand)

#
# Breusch-Godfrey test for serial correlation of order up to 5
#
# data:  Residuals from Linear regression model
# LM test = 3.8079, df = 5, p-value = 0.5774
#

# 0.5774 to jest za duzo. Powinno byc ponizej 0.05
# 


############################################################
# Przewidywania demandu
h <- 4
newdata <- data.frame(Temperature = 15)
fcast.deamand15 <- forecast(fit.demand, newdata = newdata)
summary(fcast.deamand15)
# Forecast 140.57 

newdata <- data.frame(Temperature = 35)
fcast.deamand35 <- forecast(fit.demand, newdata = newdata)
summary(fcast.deamand35)

autoplot(daily20[,1]) +
  ylab("% change in US consumption") +
  autolayer(fcast.deamand15, PI=TRUE, series="Demand Temp=15") +
  autolayer(fcast.deamand35, PI=TRUE, series="Demand Temp=35") +
  guides(colour = guide_legend(title="Scenario"))

# Bardzo duze wartosci prediction intervals
# Pierwszy w przedziale (80,175) a potem
# (225,325)

View(daily20[,1])


elecdaily %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  ylab("Demand") +
  xlab("Temperature") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

  
# Calosc danych pokazuje raczej dosc pozioma linie regresji,
# co znaczy ze sa duze rozbieznosci miedzy 

fit.demand.all <- tslm(Demand ~ Temperature, data=elecdaily)
summary(fit.demand.all)

# B1 = 0.4182 vs 6.75 dla ostatnich 20 dni
# Wniosek: Badac na wiekszym zbiorze danym 

############################################################
###########################################################
###########################################################

###########################################################
###########################################################
###########################################################
# Ex.2 mens400 set

?mens400
View(mens400)
autoplot(mens400)
# Trend mocno spadkowy z okresowym s


# Jak wyswietlic czas ?
# Rozwiazanie -> najpierw konwersja
mens400_df <- data.frame(velocity = mens400, as.numeric(time(mens400)))
names(mens400_df) <- c("Velocity","Time")

mens400_df %>%
  ggplot(aes(x=Time, y=Velocity)) +
  ylab("Velocity") +
  xlab("Time") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

fit.mens400 <- tslm(Velocity ~ Time,data=mens400_df)
summary(fit.mens400)

#Call:
#  tslm(formula = Velocity ~ Time, data = mens400_df)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.6002 -0.5747 -0.2858  0.5751  4.1505 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 172.481477  11.487522   15.02 2.52e-14 ***
#  Time         -0.064574   0.005865  -11.01 2.75e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 1.136 on 26 degrees of freedom
#(3 observations deleted due to missingness)
#Multiple R-squared:  0.8234,	Adjusted R-squared:  0.8166 
#F-statistic: 121.2 on 1 and 26 DF,  p-value: 2.752e-11

# B1 jest -0.06 co znaczy ze jest trend spadkowy z kazdym rokiem
# Average rate per year - downsize is 0.06 m/s

# Residuals plot
require(fpp2)
checkresiduals(fit.mens400)

# Duzo informacji marnuje sie w redisuach, co mocno utrudni przewidywan
# 

newdata <- data.frame(Time = 2020)
fcast.mens400 <- forecast(fit.mens400, newdata = newdata)
summary(fcast.mens400)

# W 2020 wynika na 400m mezczyzn powinien byc 42.04
# prediction interval 80% <40.45 ; 43.63>
# prediction interval 95% <39.56 ; 44.53>

autoplot(mens400_df[,1]) +
  ylab("% change in US consumption") +
  autolayer(fcast.mens400, PI=TRUE, series="Demand Temp=15") +
  guides(colour = guide_legend(title="Scenario"))

###########################################################
###########################################################
###########################################################
# Ex.3 

easter(ausbeer)
# W ktorym rozdziale jest najwieksza sprzedaz ???

#     Qtr1 Qtr2 Qtr3 Qtr4
#1956 0.67 0.33 0.00 0.00
#1957 0.00 1.00 0.00 0.00
#1958 0.00 1.00 0.00 0.00
#1959 1.00 0.00 0.00 0.00
#1960 0.00 1.00 0.00 0.00
#1961 0.33 0.67 0.00 0.00
#1962 0.00 1.00 0.00 0.00
#1963 0.00 1.00 0.00 0.00
#1964 1.00 0.00 0.00 0.00
#1965 0.00 1.00 0.00 0.00
#1966 0.00 1.00 0.00 0.00
#1967 1.00 0.00 0.00 0.00
#1968 0.00 1.00 0.00 0.00
#1969 0.00 1.00 0.00 0.00
#1970 1.00 0.00 0.00 0.00
#1971 0.00 1.00 0.00 0.00
#1972 0.33 0.67 0.00 0.00
#1973 0.00 1.00 0.00 0.00
#1974 0.00 1.00 0.00 0.00
#1975 1.00 0.00 0.00 0.00
#1976 0.00 1.00 0.00 0.00
#1977 0.00 1.00 0.00 0.00
#1978 1.00 0.00 0.00 0.00
#1979 0.00 1.00 0.00 0.00
#1980 0.00 1.00 0.00 0.00
#1981 0.00 1.00 0.00 0.00
#1982 0.00 1.00 0.00 0.00
#1983 0.00 1.00 0.00 0.00
#1984 0.00 1.00 0.00 0.00
#1985 0.00 1.00 0.00 0.00
#1986 1.00 0.00 0.00 0.00
#1987 0.00 1.00 0.00 0.00
#1988 0.00 1.00 0.00 0.00
#1989 1.00 0.00 0.00 0.00
#1990 0.00 1.00 0.00 0.00
#1991 1.00 0.00 0.00 0.00
#1992 0.00 1.00 0.00 0.00
#1993 0.00 1.00 0.00 0.00
#1994 0.00 1.00 0.00 0.00
#1995 0.00 1.00 0.00 0.00
#1996 0.00 1.00 0.00 0.00
#1997 1.00 0.00 0.00 0.00
#1998 0.00 1.00 0.00 0.00
#1999 0.00 1.00 0.00 0.00
#2000 0.00 1.00 0.00 0.00
#2001 0.00 1.00 0.00 0.00
#2002 1.00 0.00 0.00 0.00
#2003 0.00 1.00 0.00 0.00
#2004 0.00 1.00 0.00 0.00
#2005 1.00 0.00 0.00 0.00
#2006 0.00 1.00 0.00 0.00
#2007 0.00 1.00 0.00 0.00
#2008 1.00 0.00 0.00 0.00
#2009 0.00 1.00 0.00 0.00

autoplot(ausbeer)
View(ausbeer)

# Odpowiedz - informacje w ktorym miesiacu przypadaja swieta wielkanocne


###########################################################
###########################################################
###########################################################

###########################################################
###########################################################
###########################################################
# Ex.4 

# wyrazilem y, ale nie pokazalem ze B1 jest elestycznym wspolczynnikiem
# czyli procentowo wyraza zmiane y w zaleznosc od x

###########################################################
###########################################################
###########################################################


###########################################################
###########################################################
###########################################################
# Ex. 5
?fancy

autoplot(fancy)
# W 1991 efekt festiwalu byl mniejszy, ale tez nie ma 
# skokowego spadku po nim

# b. dlaczego trzeba robic logarytm
# odp.: bo daje lepszy model

View(fancy)


# Konwersja na data frame
# Dodanie kolumny z "festival". Dla marca ma byc 1 dla pozostalych 0.

#install.packages("lubridate")
#install.packages("zoo")
require(lubridate)
require(zoo)


fancy_df <- data.frame(sales = fancy, as.yearmon(time(fancy),"%b %Y"), format(as.yearmon(time(fancy),"%m"), "%m") )
names(fancy_df) <- c("Sales","Time","Month")
View(fancy_df)

# Chce dodac kolumne do dataframe w zaleznosc od miesiaca 
library(dplyr)

fancy_df <- fancy_df %>%
  mutate(Festival = if_else(Month == "03", 1, 0))

View(fancy_df)


fit.fancy = tslm(
  Sales ~ trend + season + Festival,
  data = fancy_df
)
summary(fit.fancy)

#season12    40123.26    4479.22   8.958 2.81e-13 ***
#Festival          NA         NA      NA       NA    

###########################################################
###########################################################
###########################################################











