#VAJA4
library(dplyr)
library(Quandl)


#1.naloga
#a.)
podatki <- Quandl("LBMA/GOLD", collapse="monthly", start_date="2012-12-07")[c(1,6)]#uvozi podatke o zaključnih tečajih bitcoina iz Quandl-a

podatki <- podatki[-1,] #zbrišem eno vrstico

podatki <- podatki[c(60:1),] #obrnem podatke za pravi vrstni red datumov

#b.)
t.series1 <- ts(podatki[2],start=c(2012,12), frequency=12)

graf.zlato <- ts.plot(t.series1, 
                      xlab='Leto', 
                      ylab ='Vrednost v evrih', 
                      main = 'Vrednost zlata', 
                      col ="cornflowerblue",
                      lwd = 3)


#2.naloga

#a.)
G <- function(vrsta,k){
  glajene.vrednosti <- c()
  for (i in 1:(length(vrsta)-k)){
    glajene.vrednosti[i] <- sum(vrsta[i:(k+i-1)])/k
    
  }
  zac_leto <- ceiling(2012 + k/12)
  zac_mesec <- (k/12 - floor(k/12)) * 12
  zglajena_vrsta <- ts(glajene.vrednosti, start = c(zac_leto, zac_mesec), frequency = 12)
  return(zglajena_vrsta)
}
#b.)
glajena.ts7 <- G(t.series1,7)

#napoved
napoved <- function(vrsta, k){
  return(sum(vrsta[(length(vrsta)-k+1):length(vrsta)])/k)
}

napoved(t.series1,7) #1095.52

#c.)
graf.zlato2 <- ts.plot(t.series1,glajena.ts7,
                       main ="Drseče povprečje k=7",
                       xlab='Leto',
                       ylab ='Vrednost v evrih', 
                      
                       col =c("cornflowerblue","red"),
                       lwd = 3)

legend('bottomright', 
       legend = c('vrsta', 'glajena vrsta'),
       col =c("cornflowerblue","red"),
       lwd = 1:1,
       bty = 'n')

#d.)
SKN <- function(vrsta, glajena.vrsta, k){
  l <- length(vrsta)
  napaka <- 0
  for (i in (k+1):l){
    napaka <- napaka + (vrsta[i] - glajena.vrsta[i-k])^2
  }
  return (napaka/(l-k))
}

#Srednja kvadratična napaka
SKN(t.series1,glajena.ts7,7) #4261,148

#e)
#red glajenja k=14
glajena.ts14 <-G(t.series1,14)
napoved(t.series1,14)
SKN(t.series1,glajena.ts14,14)

graf.zlato14 <- ts.plot(t.series1,glajena.ts14,
                       main ="Drseče povprečje k=14",
                       xlab='Leto',
                       ylab ='Vrednost v evrih', 
 
                       col =c("cornflowerblue","red"),
                       lwd = 3)

legend('bottomright', 
       legend = c('vrsta', 'glajena vrsta'),
       col =c("cornflowerblue","red"),
       lwd = 1:1,
       bty = 'n')

#red glajenja k=30
glajena.ts30 <-G(t.series1,30)
napoved(t.series1,30)
SKN(t.series1,glajena.ts30,30)

graf.zlato30 <- ts.plot(t.series1,glajena.ts30,
                        main ="Drseče povprečje k=30",
                        xlab='Leto',
                        ylab ='Vrednost v evrih', 
                        
                        col =c("cornflowerblue","red"),
                        lwd = 3)

legend('bottomright', 
       legend = c('vrsta', 'glajena vrsta'),
       col =c("cornflowerblue","red"),
       lwd = 1:1,
       bty = 'n')

#3.naloga

#a.)
EG <- function(vrsta, alpha){
  dolzina <- length(vrsta)
  zglajene_vrednosti <- vrsta[1]
  for (i in 2:dolzina){
    zglajene_vrednosti[i] <- alpha*vrsta[i] + (1-alpha)*zglajene_vrednosti[i-1]
  }
  zglajena_vrsta <- ts(zglajene_vrednosti, start = c(2013,1), frequency = 12)
  return(zglajena_vrsta)
}

#b.)
#alpha <- 0,75
eksponentno.glajena <- EG(t.series1,0.75)
last(eksponentno.glajena) #napoved

graf.zlatoE <- ts.plot(t.series1,eksponentno.glajena,
                       main ="Eksponentno glajenje alpha=0.75",
                       xlab='Leto',
                       ylab ='Vrednost v evrih', 
                       
                       col =c("cornflowerblue","red"),
                       lwd = 3)

legend('bottomright', 
       legend = c('vrsta', 'glajena vrsta'),
       col =c("cornflowerblue","red"),
       lwd = 1:1,
       bty = 'n')

#c.)
SKN.E <-function(vrsta, alpha){
  dolzina <- length(vrsta)
  napaka <- 0
  glajena <- EG(vrsta, alpha)
  for (i in 1:(dolzina-1)){
    napaka <- napaka + (vrsta[i+1] - glajena[i+1])^2
  }
  return(napaka/(dolzina-1))
}

optimal.alpha <- optimize(SKN.E, c(0,1), vrsta = t.series1)

#d.)alpha optimalna
eksponentno.glajena2 <- EG(t.series1,optimal.alpha$minimum)
last(eksponentno.glajena2) #napoved

graf.zlatoE2 <- ts.plot(t.series1,eksponentno.glajena2,
                       main ="Eksponentno glajenje alpha=0.75",
                       xlab='Leto',
                       ylab ='Vrednost v evrih', 
                       
                       col =c("cornflowerblue","red"),
                       lwd = 3)

legend('bottomright', 
       legend = c('vrsta', 'glajena vrsta'),
       col =c("cornflowerblue","red"),
       lwd = 1:1,
       bty = 'n')

