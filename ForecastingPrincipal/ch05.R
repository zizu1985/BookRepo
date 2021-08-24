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
# Ex. 5 - data modeling with fancy
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
  log(Sales) ~ trend + season,
  data = fancy_df
)
summary(fit.fancy)

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -6449.04    3451.71  -1.868  0.06584 .  
#trend         319.04      37.93   8.411 2.89e-12 ***
#season2       996.98    4459.91   0.224  0.82376    
#season3      5216.36    4460.39   1.169  0.24612    
#season11    13989.73    4475.85   3.126  0.00257 ** 
#season12    40123.26    4479.22   8.958 2.81e-13 ***
#Festival          NA         NA      NA       NA    

# Festival w szczegolnosci pozytywny efekt
# Trend wzrostowy
# Najlepsza sprzedaz w miesiacu 12
# Zmienna dummy - nic nie dala. Dlaczego ?

checkresiduals(fit.fancy)

# Tak, jest troche informacji w nich

boxplot(residuals(fit.fancy))
# Srednia nie jest najgorsza, ale sa miesiace gdzie sa duzo odchylenia

newdata1994 <- data.frame(Time = c("sty 1994","lut 1994","mar 1994","kwi 1994","maj 1994","cze 1994","lip 1994","sie 1994","wrz 1994","paŸ 1994","lis 1994","gru 1994"),
                             Festival = c(0,0,0,0,0,0,0,0,0,0,0,0))
fcast.fancy1994 <- forecast(fit.fancy, newdata = newdata1994)
summary(fcast.fancy1994)


#  Forecasts:
#  Point Forecast     Lo 80    Hi 80     Lo 95    Hi 95
#Jan 1994       20669.22  8893.132 32445.31  2517.513 38820.93
#Feb 1994       21985.24 10209.150 33761.33  3833.531 40136.95
#Mar 1994       26523.66 14747.566 38299.75  8371.947 44675.36
#Apr 1994       23570.09 11793.996 35346.18  5418.377 41721.79
#May 1994       23572.14 11796.055 35348.23  5420.436 41723.85
#Jun 1994       24515.95 12739.858 36292.04  6364.238 42667.66
#Jul 1994       26765.85 14989.760 38541.94  8614.141 44917.56
#Aug 1994       27528.37 15752.283 39304.46  9376.664 45680.08
#Sep 1994       28636.79 16860.705 40412.88 10485.086 46788.50
#Oct 1994       29634.61 17858.518 41410.70 11482.898 47786.32
#Nov 1994       37849.34 26073.249 49625.43 19697.630 56001.05
#Dec 1994       64301.90 52525.813 76077.99 46150.194 82453.61

newdata1995 <- data.frame(Time = c("sty 1995","lut 1995","mar 1995","kwi 1995","maj 1995","cze 1995","lip 1995","sie 1995","wrz 1995","paŸ 1995","lis 1995","gru 1995"),
                      Festival = c(0,0,0,0,0,0,0,0,0,0,0,0))
fcast.fancy1995 <- forecast(fit.fancy, newdata = newdata1995)
summary(fcast.fancy1995)

# Problem - podaje dla 1994, zamiast 1995
# Solution - retrain model ? -> No
# Predict in one move ? -> Tak

fit.fancy = tslm(
  log(Sales) ~ trend + season,
  data = fancy_df
)
summary(fit.fancy)

newdata <- data.frame(Time = c("sty 1994","lut 1994","mar 1994","kwi 1994","maj 1994","cze 1994","lip 1994","sie 1994","wrz 1994","paŸ 1994","lis 1994","gru 1994",
                                   "sty 1995","lut 1995","mar 1995","kwi 1995","maj 1995","cze 1995","lip 1995","sie 1995","wrz 1995","paŸ 1995","lis 1995","gru 1995",
                                   "sty 1996","lut 1996","mar 1996","kwi 1996","maj 1996","cze 1996","lip 1996","sie 1996","wrz 1996","paŸ 1996","lis 1996","gru 1996"),
                          Festival = c(0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0,
                                       0,0,0,0,0,0,0,0,0,0,0,0))
fcast.fancy <- forecast(fit.fancy, newdata = newdata)
summary(fcast.fancy)

#Forecasts:
#  Point Forecast     Lo 80    Hi 80     Lo 95    Hi 95

#Jan 1994       20669.22  8893.132 32445.31  2517.513 38820.93
#Feb 1994       21985.24 10209.150 33761.33  3833.531 40136.95
#Mar 1994       26523.66 14747.566 38299.75  8371.947 44675.36
#Apr 1994       23570.09 11793.996 35346.18  5418.377 41721.79
#May 1994       23572.14 11796.055 35348.23  5420.436 41723.85
#Jun 1994       24515.95 12739.858 36292.04  6364.238 42667.66
#Jul 1994       26765.85 14989.760 38541.94  8614.141 44917.56
#Aug 1994       27528.37 15752.283 39304.46  9376.664 45680.08
#Sep 1994       28636.79 16860.705 40412.88 10485.086 46788.50
#Oct 1994       29634.61 17858.518 41410.70 11482.898 47786.32
#Nov 1994       37849.34 26073.249 49625.43 19697.630 56001.05
#Dec 1994       64301.90 52525.813 76077.99 46150.194 82453.61

#Jan 1995       24497.68 12589.849 36405.52  6142.903 42852.46
#Feb 1995       25813.70 13905.867 37721.53  7458.921 44168.48
#Mar 1995       30352.12 18444.283 42259.95 11997.337 48706.90
#Apr 1995       27398.55 15490.713 39306.38  9043.767 45753.33
#May 1995       27400.61 15492.771 39308.44  9045.825 45755.38
#Jun 1995       28344.41 16436.574 40252.24  9989.628 46699.19
#Jul 1995       30594.31 18686.477 42502.14 12239.531 48949.09
#Aug 1995       31356.83 19449.000 43264.67 13002.054 49711.61
#Sep 1995       32465.25 20557.421 44373.09 14110.475 50820.03
#Oct 1995       33463.07 21555.234 45370.90 15108.288 51817.85
#Nov 1995       41677.80 29769.966 53585.63 23323.020 60032.58
#Dec 1995       68130.36 56222.530 80038.20 49775.584 86485.14

#Jan 1996       28326.14 16259.242 40393.04  9726.176 46926.11
#Feb 1996       29642.16 17575.260 41709.06 11042.195 48242.13
#Mar 1996       34180.58 22113.676 46247.48 15580.611 52780.54
#Apr 1996       31227.01 19160.106 43293.91 12627.041 49826.97
#May 1996       31229.07 19162.165 43295.97 12629.099 49829.03
#Jun 1996       32172.87 20105.968 44239.77 13572.902 50772.84
#Jul 1996       34422.77 22355.870 46489.67 15822.805 53022.74
#Aug 1996       35185.29 23118.393 47252.20 16585.328 53785.26
#Sep 1996       36293.72 24226.815 48360.62 17693.749 54893.68
#Oct 1996       37291.53 25224.628 49358.43 18691.562 55891.50
#Nov 1996       45506.26 33439.359 57573.16 26906.293 64106.23
#Dec 1996       71958.82 59891.923 84025.73 53358.858 90558.79

# How to improve -> a) add constant to prediction b) use sqrt or log-log model

######################################################################################################################
######################################################################################################################
######################################################################################################################

###########################################################
# Ex. 6  - gasoline series
?gasoline
autoplot(gasoline, xlab="Year")

gasoline_data = window(gasoline, end=2004)
autoplot(gasoline_data, xlab="Year")
View(gasoline_data)

gasoline_df <- data.frame(Barrels = gasoline_data, as.numeric(time(gasoline_data)))
View(gasoline_df)
# do konca 2003 jest, wiec

gasoline_data = window(gasoline, end=2005)
autoplot(gasoline_data, xlab="Year")
gasoline_df <- data.frame(Barrels = gasoline_data, as.numeric(time(gasoline_data)))
names(gasoline_df) <- c("Barrels","Time")


# Fit a harmonic regression with trend to the data
fit.exp <- tslm(gasoline_data ~ trend, lambda = 0)
fit.fourier <- tslm(gasoline_data ~ trend + fourier(gasoline_data, K=2))
fit.fourier5 <- tslm(gasoline_data ~ trend + fourier(gasoline_data, K=5))

# autoplot
autoplot(gasoline_data) +
  autolayer(fitted(fit.exp), series = "Exponential") +
  autolayer(fitted(fit.fourier), series = "Fourier") +
  autolayer(fitted(fit.fourier5), series = "Fourier K=5") +
  xlab("Year") + ylab("Production in mln barrels") +
  ggtitle("Mln barrels") +
  guides(colour = guide_legend(title = " "))

# K=2 ju¿ wystarczajaco dobrze opisuje przebieg krzywej

# Dobranie parametru K w celu minimalizacji AICc / CV
CV(fit.fourier)
#CV           AIC          AICc           BIC         AdjR2 
#8.136854e-02 -1.819113e+03 -1.818957e+03 -1.787001e+03  8.269569e-01 

CV(fit.fourier5)

fit.fourier10 <- tslm(gasoline_data ~ trend + fourier(gasoline_data, K=10))
CV(fit.fourier10)
#CV           AIC          AICc           BIC         AdjR2 
#7.135754e-02 -1.915014e+03 -1.913441e+03 -1.809500e+03  8.516102e-01 

fit.fourier20 <- tslm(gasoline_data ~ trend + fourier(gasoline_data, K=20))
CV(fit.fourier20)
#CV           AIC          AICc           BIC         AdjR2 
#7.135754e-02 -1.915014e+03 -1.913441e+03 -1.809500e+03  8.516102e-01 

fit.fourier15 <- tslm(gasoline_data ~ trend + fourier(gasoline_data, K=15))
CV(fit.fourier15)

# Optymalna wartosc dla K=10

# Check residuals - minimalnie sa skorelowane, ale nie znaczaco
# nie powinno miec wpluwy na przewidywania
checkresiduals(fit.fourier10)

# Przewidywanie wartosci dla roku 2005 za pomoca modelu z szeregami fouriera
h <- 53
fc <- forecast(fit.fourier10, newdata=data.frame(fourier(gasoline_data,10,h)))

# B³¹d w poleceniu '...fourier(x, K, NROW(x) + (1:h))':
# K must be not be greater than period/2
# https://stackoverflow.com/questions/28841142/forecast-throws-error-k-must-be-not-be-greater-than-period-2
# Solution: nie wstawiles w miejsce x naszych danych

autoplot(gasoline) +
  autolayer(fitted(fit.fourier10), series = "Fourier 10") +
  autolayer(fc, series = "Fourier 10 - forecasting", PI=FALSE) +
  xlab("Year") + ylab("Production in mln barrels") +
  ggtitle("Mln barrels") +
  guides(colour = guide_legend(title = " "))

# Zjawisko - overfitting. W modelu dane sfitowane wygladaly dobrze
# Przewidywania roznie sie od wartosci rzeczywistych. Np zle przewidzial sezonowosc w jednym z lat.

##################################################
##################################################
##################################################
# Ex. 7 - huron 

?huron
autoplot(huron)
View(huron)

huron_df <- data.frame(Height = huron, as.character(time(huron)))
names(huron_df) <- c("Height","Time")
View(huron_df)


fit.huron = tslm(
  Height ~ trend,
  data = huron_df
)
summary(fit.huron)

#################################################

t <- time(huron)
t.break1 <- 1915
tb1 <- ts(pmax(0, t - t.break1), start = 1875)
fit.pw <- tslm(huron ~ t + tb1)
summary(fit.huron)

fit.huron = tslm(
  huron ~ t
)
summary(fit.huron)


autoplot(huron) +
  autolayer(fitted(fit.huron), series = "Linear regression") +
  autolayer(fitted(fit.pw), series = "Picewise regression") +
  xlab("Year") + ylab("Production in mln barrels") +
  ggtitle("Mln barrels") +
  guides(colour = guide_legend(title = " "))

View(huron)
h <- 20
fc <- forecast(fit.huron, newdata=data.frame(t=c(1973,1974,1975,1976,1977,1978,1979,1980)))
fc1 <- forecast(fit.pw, newdata=data.frame(t=c(1973,1974,1975,1976,1977,1978,1979,1980),tb1=c(1,1,1,1,1,1,1,1)))

autoplot(huron) +
  autolayer(fc, series = "Linear regression - forecasting", PI=FALSE) +
  autolayer(fc1, series = "Picewise regression - forecasting", PI=FALSE) +
  xlab("Year") + ylab("Production in mln barrels") +
  ggtitle("Mln barrels") +
  guides(colour = guide_legend(title = " "))

# Picewise zachowuje sie bardzo zle
