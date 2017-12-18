#VAJA4
library(dplyr)
library(Quandl)


#1.naloga

podatki <- Quandl("BITSTAMP/USD.3",collapse = "weekly") #uvozi podatke o zaključnih tečajih bitcoina iz Quandl-a

podatki <- podatki[c(191:1),] #obrnem podatke za pravi vrstni red datumov

casovna_vrsta <- ts(data = podatki[2], start = 2014, frequency = 48)  #uporabim le drugi stolpec


graf_bitcoin<- ts.plot(podatki,
                       xlab='Čas',
                       ylab ="Cena BitCoina v USD", 
                       main = "BitCoin",
                       col = c("red"), 
                       lwd = 2)


#2.naloga

#a.)
G <- function(vrsta,k){
  
}