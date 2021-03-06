#2.vaja
library(readr)
library(actuar)
library(readtext)

##=========================================================
#1.naloga

#a.)
#uvozimo podatke
vzorec <- read.table("vzorec3.txt")

#narišemo histogram škodnih zahtevkov
histogram1 <-hist(vzorec[[1]],breaks = 50,
                  main = "Histogram škodnih zahtevkov",
                  xlab = "Škodni zahtevki",
                  ylab ="Višina škodnih zahtevkov",
                  col ="cornflowerblue")

#b.)#izračun neznanih parametrov
parametri <- mde(vzorec[[1]], ppareto1, start  = list(shape=1,min =1), measure = "CvM")
alfa <- parametri$estimate[1] # = 1.926127
min <- parametri$estimate[2] # = 4.065348 

#c.)
#krivuljo gostote slučajne spremenljivke Y dodamo histogramu
histogram1 <-hist(vzorec[[1]],breaks = 50,
                  main = "Histogram škodnih zahtevkov",
                  xlab = "Škodni zahtevki",
                  ylab ="Višina škodnih zahtevkov",
                  col ="cornflowerblue",
                  probability = TRUE)

curve( dpareto1(x, shape = alfa,min = min ),from = 0, to = max(vzorec[[1]]), add = TRUE)


#vzorčna in porazdelitvena funkcija odškodnin
plot(ecdf(vzorec[[1]]),main = "Porazdelitvena funkcija odškodnin",
     ylab = "Porazdelitvena funkcija",
     xlab="Višina odškodnine")

curve(ppareto1(x, shape = alfa,min = min ),
      from=0, to =40,
      add = TRUE, 
      col= "red",
      lwd = 2)
legend("bottomright",c("Vzorčna funkcija","Porazdeltivena funkcija"),col = c("black","red"),lty = 1)

#d.)Izračun upanja slučajne spremenljivke Y
upanje.Y <- as.numeric( alfa*min/(alfa-1)) # = 8.454971
upanje.N <- 15
upanje.S <- upanje.N * upanje.Y #126.8246

var.S <- Inf #ker je var.Y = Inf

#==================================================================================
#2.naloga

#a.)Diskretizacija porazdelitve Y

m <- 1 #začetek intervala
M <-40 #konec intervala
h <- 0.5 #dolžina koraka
diskretnaY <- discretize(ppareto1(x,shape = alfa, min = min), 
                          from = m, 
                          to = (M + 0.5),
                          step = h,
                          method = "rounding"
                          )

diskretnaY2 <- discretize(ppareto1(x,shape = alfa, min = min), #za kasnejšo uporabo pri panjerjevemu algoritmu
                         from = 0, 
                         to = 100000,
                         step = h)
                        

#b.) graf porazdelitvene funkcije Y ter diskretna porazdelitvena funkcija

poraz.Y <- stepfun(seq(m,M,h), diffinv(diskretnaY)) #diffinv nam da porazdelitveno funkcijo


plot(poraz.Y, 
     main = "Paretova porazdelitev",
     do.points = FALSE,
     xlab = "x",
     ylab = "Porazdelitvena funkcija",
     col = "red")

curve(ppareto1(x, shape = alfa,min = min ),
      from=m, to =M,
      add = TRUE, 
      col= "cornflowerblue",
      lwd = 2)
legend("bottomright",c("Diskretna funkcija","Porazdeltivena funkcija"),col = c("red","cornflowerblue"),lty = 1)

#c.)Panjerjev algoritem

F.S <- aggregateDist(method = "recursive",
                     model.freq = "poisson",
                     model.sev = diskretnaY2,
                     lambda = 15,
                     p0 = exp(-15),
                     x.scale = h,
                     convolve = 0,
                     maxit=1000000,
                     tol = 0.002)
plot(F.S)

#d.)S je diskretna spremenljivka

#izračun upanja in disperzije
E.S <- sum( knots(F.S) * diff(F.S)) #upanje slučanje spremenljivke S = (vrednosti S)* (skoki v teh točkah) # = 121.1313

E.S2 <- sum( knots(F.S)^2 * diff(F.S)) # drugi moment slučajne spremenljivke S # = 17260.9
D.S <- E.S2 - E.S^2 #disperzija od S = E(S^2)-E(S)^2 # = 2588.114

TV <- VaR(F.S,0.995) #tvegana vrednost = 392.5
PI <- CTE(F.S, 0.005) #pričakovani izpad = 121.8433 

#===============================================================
#3.naloga

#a.) simulacija 10000 vrednosti slučajne spremenljivke S

#simulacija spremenljivke N
simN <- rpois(10000, 15)

#simulacija spremenljivke S
simS<-c()

for (n in simN){
  simS <- c(simS, sum( rpareto1(n, alfa, min) ))
}

#b.)ocena za upanje in disperzijo spremenljivke S
E.simS <- mean(simS) #=126.2367
Var.simS <- var(simS) #= 3926.283

#s simulacijo dobimo boljše ocene za pričakovano vrednost in varianco, kot pa
#s Panjerjevim algoritmom, saj so vrednosti bližje tistim iz naloge 1.

#c.)Ocena tvegane vrednosti 
TV_2 <- sort(simS)[9950] #=398.2419, oceni za tvegano vrednost sta si precej blizu

#d.) 
plot(ecdf(simS),
     col = 'green',
     add = TRUE,
     lwd = 2)

legend('bottomright', 
       legend = c('Panjerjev algoritem', 'Monte Carlo simulacija'),
       col = c('black', 'green'),
       lty=1:1, cex=0.8)
