# Summary
# Benchmarkowe metody - naiwna, srednia, naiwna sezonowo, drift
# Poprawki - kalendarzowa, populacyjna, inflacyjna
# Transformacja Box-Cox
# Poprawka bias dla transofrmacji box-cox
# Residua - analiza
# Resiuda - sprawdzenie czy nie sa skorelowane. Testy statystyczne - Box-Pierce, Ljung-Box
# Metryki oceny przewidywania (RMSE, MAE, MAPE, MASE)
# Crosswalidacja



require(fpp2)

autoplot(beer2)

# Benchmarkowe przewidywania na jednym wykresie
# przewidywanie na liczbe pomiarow = h 
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(beer2, h=11),
            series="Naive", PI=FALSE) +
  autolayer(snaive(beer2, h=11),
            series="Seasonal naive", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") + 
  guides(colour=guide_legend(title="Forecast"))

# Benchmarki dla ceny akcji Google
autoplot(goog200) + 
  autolayer(meanf(goog200, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naive", PI=FALSE) + 
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

# Calendar adjustment
dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet=TRUE) + 
  xlab("Years") + ylab("Pounds") + 
  ggtitle("Milk production per cow")

# Transformacja Box-Cox
(lambda <- BoxCox.lambda(elec))
# 0.265

# Ten wykres fajnie pokazuje dlaczego wartoci 0.265 i 0.450 sa lepsze niz 0.7
autoplot(elec) +
  autolayer(BoxCox(elec,0.265),series = "BoxCox 0.265") + 
  autolayer(BoxCox(elec,0.450),series = "BoxCox 0.450") + 
  autolayer(BoxCox(elec,0.7),series = "BoxCox 0.7")

# Problem: Domyslnie odwrotna transformacja Boxa-Coxa wraca do mediany, a nie do sredniej
# Rozwiazanie: Uzywac wersja transformacji w wlaczona wartoscia bias-adjusted


# Predykcja z metoda drift, transformacja Box-Coxa z lambda = 0
# wersja z uwzglednieniem bias i wersja bez bias. Domyslnie jest bez bias

fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)
autoplot(eggs) + 
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))


## A jakie sa bledy ?
eggstrain <- window(eggs,start=1900,end=1943)
fc <- rwf(eggstrain, drift=TRUE, lambda=0, h=50, level=80)
eggstest <- window(eggs,start=1943,end=1993)
accuracy(fc,eggstest)
# RMSE = 191.9907

eggstrain <- window(eggs,start=1900,end=1943)
fc1 <- rwf(eggstrain, drift=TRUE, lambda=0, h=50, level=80,biasadj=TRUE)
eggstest <- window(eggs,start=1943,end=1993)
accuracy(fc1,eggstest)
# RMSE = 330.9258


autoplot(eggs) + 
  autolayer(fc,
            series="Without bias", PI=FALSE) +
  autolayer(fc1,
            series="With bias", PI=FALSE) + 
  ggtitle("Forecast eggs price") +
  xlab("Year") + ylab("Price (US$)") +
  guides(colour=guide_legend(title="Prediction model"))


# Wersja z biasem zachowuje sie duzo gorzej
# TODO - Kiedy uzywaæ transformacji Box-Cox z biasem a kiedy nie 


##### Residual diagnostics ############
# Residual = reszta z dopasowania.
# Cel: Residuly nie powinny miec w sobie informacji. Jezeli ja posiadaja to ta informacja
#      nie jest uzyta w modelu, a to zle
#
# Jezeli to zachodzi to znaczy, ze dany model (w tej konretnej sytuacji mozna) jeszcze tuningowac

# Example using Google stock data
autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naive method")

# TODO Jaka jest srednia residuow

gghistogram(res) + ggtitle("Histogram of residuals")

# ACFs 
ggAcf(res) + ggtitle("ACF of residuals")


# Na podsawie redisum
# a) dokladnosc 

# lag=h and fitdf=K
Box.test(res, lag=10, fitdf=0)
#
#	Box-Pierce test

#data:  res
#X-squared = 10.611, df = 10, p-value = 0.3886

Box.test(res,lag=10, fitdf=0, type="Lj")
#Box-Ljung test

#data:  res
#X-squared = 11.031, df = 10, p-value = 0.3551

checkresiduals(naive(goog200))


# Evaluating forecast accuracy

# Function to subset a time series
window(ausbeer, start=1995)
frequency(ausbeer) # 4, czyli 4 jednostki na rok

subset(ausbeer, start=length(ausbeer) - 4*5)
subset(ausbeer, quarter = 1)

tail(ausbeer, 4*5)

# Metryki skutecznosci predykcji - dane z sezonowoscia
beer2 <- window(ausbeer, start=1992,end=c(2007,4))
beerfit1 <- meanf(beer2,h=10)
beerfit2 <- rwf(beer2,h=10)
beerfit3 <- snaive(beer2,h=10)
autoplot(window(ausbeer, start=1992)) +
  autolayer(beerfit1, series="Mean", PI=FALSE) +
  autolayer(beerfit2, series="Naive", PI=FALSE) +
  autolayer(beerfit3, series="Seasonal naive", PI=FALSE) +
  xlab("Year") + ylab("Megaliters") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))


# wyliczenie metrk
beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

# Metryki - dane bez sezonowosci

googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naive") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))


googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)



# Time series cross-validation
e <- tsCV(goog200, rwf, drift=TRUE, h=1)
sqrt(mean(e^2, na.rm=TRUE))
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE))


# Pipe operator
goog200 %>% tsCV(forecastfunction=rwf, drift=TRUE, h=1) -> e
e^2 %>% mean(na.rm=TRUE) %>% sqrt()
# 6.233245

goog200 %>% rwf(drift=TRUE) %>% residuals() -> res
res^2 %>% mean(na.rm=TRUE) %>% sqrt()
# 6.168

# using tsCV()

e <- tsCV(goog200, forecastfunction = naive, h=8)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm=T)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE=mse) %>%
  ggplot(aes(x=h,y=MSE)) + geom_point()


# Prediction intervals
naive(goog200)
autoplot(naive(goog200))

#### forecast package in R
#forecast() - funkcja do ropienia prognoz

forecast(ausbeer, h=4)

###################
### Exercises
###################
# 1, Wyliczanie optymalnej wartosci lambda dla transformacji Box-Cox

# Transformacja Box-Cox
(lambda <- BoxCox.lambda(usnetelec))
# 0.5167

# Ten wykres fajnie pokazuje dlaczego wartoci 0.265 i 0.450 sa lepsze niz 0.7
autoplot(usnetelec) +
  autolayer(BoxCox(usnetelec,0.2),series = "BoxCox 0.2") + 
  autolayer(BoxCox(usnetelec,0.5167),series = "BoxCox 0.5167") + 
  autolayer(BoxCox(usnetelec,0.9),series = "BoxCox 0.9")

(lambda <- BoxCox.lambda(usgdp))
# 0.366

(lambda <- BoxCox.lambda(mcopper))
# 0.192

(lambda <- BoxCox.lambda(enplanements))
# -0.2269


# 2. Dlaczego transofrmacja Box-Cox nie pomaga dla dataseta cangas ?
gghistogram(cangas)
shapiro.test(cangas)
#  p-value = 5.161e-13

# Nie pomaga poniewa¿ ma wartosci <= 0

# 3, Ktora transformacje Box-Cox wybierzesz
# w przykladzie 3 w cwiczeniu 2.10 ?


# 4. Ocen zbior i tranformacja Box-Coxa

autoplot(dole)
gghistogram(dole)
shapiro.test(dole)
# p-value = 2.2e-16 < 0.05, wiec nie jest Gaussowski
# drugi problem to ze zawiera wartosci >= 0
# chcialbym dodac stala wartosc 

dole_test = dole + 2
gghistogram(dole_test)

















