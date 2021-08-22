#install.packages("fpp2")
require(fpp2)

# Podstawowy szereg czasowy
y <- ts(c(123,39,78,52,110), start=2012)

# Time plots
print(melsyd)
autoplot(melsyd[,"Economy.Class"]) + 
    ggtitle("Economy class passengers: Melbourne-Sydney") +
    xlab("Year") + 
    ylab("Thousands")

?a10
autoplot(a10) + 
  ggtitle("Antidiabetic drug sales") +
  ylab("$ milion") +
  xlab("Year")

# Seasonal plots
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("$ milion") +
  ggtitle("Seasonal plot: antidiabetic drug sales")


# Seasonal plots - polar version
ggseasonplot(a10, polar=TRUE) +
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")


# Analiza ekspolaracyjna pomiedzy dwoma szeregami
autoplot(elecdemand[,c("Demand","Temperature")], facets=TRUE) +
  xlab("Year: 2014") + ylab("") +
  ggtitle("Half-hourly electricity demand: Victoria, Australia")

# Czy zapotrzebowanie i temperatura sa skorelowane ?
qplot(Temperature, Demand, data=as.data.frame(elecdemand)) +
  ylab("Demand (GW)") + xlab("Temperature (Celsius)")


# Korelacja miedzy dwoma szeragami/zmiennymi
# korelacja = r
# -1 < r < 1
# korelacja ta mierzy korelacje liniowa
# Uwaga !!! Oprócz tego istnieje korelacje nieliniowe (logarytm, kwadrat)
# 


# Jezeli jest kilka zmiennych (moga byc to szeregi czasowe)
# to warto wyliczyc macierz koleracji

autoplot(visnights[,1:5], facets=TRUE) +
  ylab("Number of visitor nights each quarter (millions)")

#install.packages("GGally")
require("GGally")
GGally::ggpairs(as.data.frame(visnights[,1:5]))


# Lag plots
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2)

#autoplot(beer2)

# Autocorrelation
# Liniowa zaleznosc miedzy zmienna a wartosciami z przeszlosci

# Autocorrelation function, correlogram
ggAcf(beer2)

# Trend and seasonality in ACF plots
aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")

ggAcf(aelec, lag=48)


# White noise 
set.seed(30)
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")

ggAcf(y)

#################################
# Exercises 
#################################
# 2.10.1 

?gold
?frequency
tsdisplay(gold)
frequency(gold) # 1 - 1 dzien
autoplot(gold)
which.max(gold) # dzien 770

?woolyrnq
# kwartalna produkcja welny w Australii
autoplot(woolyrnq)
frequency(woolyrnq) # 4 jednostki = rok
which.max(woolyrnq) # 77 kwartal

?gas
# miesieczna produkcja gazu w Australii
autoplot(gas)
frequency(gas) # 12 jednostek = 1 rok
which.max(gas)

# 2.10.2
tute1 <- read.csv("tute1.csv", header=TRUE)
View(tute1)

mytimeseries <- ts(tute1[,-1],start=1981,frequency=4)
autoplot(mytimeseries,facets=TRUE)

autoplot(mytimeseries)
# Rozne kolory, ktore sa podpisane w legendzie

# 2.10.3
retaildata <- readxl::read_excel("retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349627V"],frequency=12, start=c(1982,4))

autoplot(myts)
# Widac trend i sezonowosc

ggseasonplot(myts)
# spadek sprzedazy w lutym, wzrost w grudniu i wzrost w marcu
# w pozostalym okresie w miare jednakowe

ggsubseriesplot(myts)
# nie wnosi nic nowego

gglagplot(myts)
# Silna korelacja dla kazdego lagu. Najsilniejsza w grudniu dla kazdego roku

ggAcf(myts)
# Jest trend, bo sumarycznie wartosc spadaja
# Jest sezonowosc z pikiem 12 i 24 miesiace czyli grudzien


# 2.10.4
?bicoal
autoplot(bicoal)
?chicken
autoplot(chicken)
?dole
autoplot(dole)
?usdeaths
autoplot(usdeaths)  
?lynx  
autoplot(lynx)
?goog
autoplot(goog) +
  ggtitle("Google stock price") +
  xlab("Day") +
  ylab("USD")

# 2.10.5
?writing
ggseasonplot(writing)
# sezonowosc z spadkiem w sezonie wakacyjnym i minimum w siernpniu
# potem pewnie powrot do szkol i biur
# wzrost w marcu i czerwcu
# Nie ma jakiegos specjalnego roku

?fancy
ggseasonplot(fancy)
# wyrazny wzrost w sezonie letnium 
# W ostatnich 2 latach w zbiorze 1988 + 1987 zaczyna sie trend wzrostowy od czerwca
# Moze zaczyna wiece ludzi odpoczywac nad morzem takze w zimie ?

?a10
ggseasonplot(a10)
# Wzrost pod koniec roku
# 2008 - nie regularny

# 2.10.6
?gasoline
# produkcja benzyny w USA
autoplot(gasoline)
# wyglada na trend wzrostowy do okolo 2007-8 roku
# potem spadkowy i potem znowu wzrostowy
# wyglada ze jest jakas sezonowosc (roczna ?)

frequency(gasoline) # 52.1 - rok

ggseasonplot(gasoline)
#

ggsubseriesplot(gasoline)
# Frequency nie jest liczba calkowita i ta funkcja odmawia posluszenstwa

ggAcf(gasoline)
# Tredny ale zmienne - wyglada na cykl. Spadek / wzrost w okresie 52 tygodni
# Sezonowosci brak


# 2.10.7
?arrivals
View(arrivals)
autoplot(arrivals[,0:4], facets=TRUE) +
  ylab("Number of visitor nights each quarter (thousands)")

# Arrivals[,1] to znaczniki czasu

ggseasonplot(arrivals[,1]) +
  ggtitle("Japan")
# W latach 90 duzo wiecej Japonskich turystow niz obecnie
# Wraz z koncem lata spada liczba turystow, potem wzrost jesieni do
# zimy. Na wiosne zmiana -> wczesniej spadek, teraz wzrost

ggseasonplot(arrivals[,2]) +
  ggtitle("NZ")
# Co roku wiecej turystow z NZ 
# Wyrazna sezonowosc -> duzy wzrost w lecie, potem wolniejszy wzrost w zimie.
# Spadek na wiosne

ggseasonplot(arrivals[,3]) +
  ggtitle("UK")
# Wyrazne spadek w lato. Za drogo / nie lubia upalow ?
# Stabilnosc na jesien. Wzost w zimie.
# UK - uzaleznione mocno od temperatury


ggseasonplot(arrivals[,4]) +
  ggtitle("US")
# W lato spadek. W Na jesieni lekki wzrost. W zimie mocniejszy wzrost.
# Wystapuje nieregularne lata


# 2.10.8
3-D
1-B
2-A
4-C

# 2.10.9
mypigs <- window(pigs,start=1990)
autoplot(mypigs)

ggAcf(mypigs)
# blisko bialego szumu.
# 
# Do 3 ostatnich probek oddzialywuja, ale nie sa to duzo odzialywania


# 2.10.10
?dj
ddj <- diff(dj)
autoplot(ddj)
ggAcf(ddj)
# Tak, to bialy szum, ale 
?diff

