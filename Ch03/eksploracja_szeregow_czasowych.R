# zmienna liniowa wyliczana w oknie czasowym
x <- rnorm(n = 100,mean = 0, sd = 10) + 1:100
mn <- function(n) rep(1/n, n)
plot(x, type='l', lwd=1)
lines(filter(x, mn(5)), col = 2, lwd = 3, lty =2)
lines(filter(x, mn(50)), col = 3, lwd = 3, lty =3)

# zmienna nieliniowa wyliczana w oknie czasowym
require(zoo)
f1 <- rollapply(zoo(x), 20, function(w) min(w),align="left",partial=TRUE)
f2 <- rollapply(zoo(x), 20, function(w) min(w),align="right",partial=TRUE)
plot(x, lwd=1, type='l')
lines(f1, col = 2, lwd=3, lty=2)
lines(f2, col = 3, lwd=3, lty=3)

#
# Czy wyrownanie do lewej pozwala oszacowac dane na podstawie danych z przyszlosci
# Jezeli zle odwzorowuja dane, to znaczy ze w tym szeregu na podstawie przeszlosci
# nie da sie przywidziec przyszlosci => nie ma sensu zajmowac sie tym szeregiem
#

