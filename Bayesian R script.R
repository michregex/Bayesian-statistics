#dati <- read.table("malignantmelanoma.txt", header = TRUE)
dati <- dati[,-6] 

head(dati)

datiAGG <- aggregate(cbind(dati$nMaleDeaths,dati$expectedDeaths), by=list(Category=dati$nation), FUN=sum)
colnames(datiAGG) <- c("nations","nMaleDeaths","expectedDeaths") 
datiAGG

#varianza elevatissima e ci sta perchè anni diversi
mu <- mean(datiAGG$nMaleDeaths)
sigma2 <- var(datiAGG$nMaleDeaths)
alpha <- mu^2 / sigma2 
beta <- mu / sigma2 
rgamma(alpha, beta)

#meglio usare una media
#9 anni per ogni nazione tranne United Kingdom, Ireland, Germany, Italy and The Netherlands
datiAGG$years <- c(10,6,10,10,6,6,10,10,6)
datiAGG$meanDeaths <- datiAGG$nMaleDeaths / datiAGG$years
datiAGG$nations <- c("Belgium","Germany","Denmark","France","UK","Italy","Ireland","Luxemburg","Netherlands")
datiAGG$deathsExpectedRatio <- datiAGG$nMaleDeaths / datiAGG$expectedDeaths
var(datiAGG$meanDeaths)

####
dati$years <- ifelse(dati$nation %in% c(1,3,4,7,8),10,6)
datiAGGreg <- aggregate(cbind(dati$nMaleDeaths,dati$expectedDeaths), by=list(nation=dati$nation,region=dati$regionID), FUN=sum)
colnames(datiAGGreg) <- c("nations","region" ,"nMaleDeaths","expectedDeaths") 
datiAGGreg$years <- ifelse(datiAGGreg$nation %in% c(1,3,4,7,8),10,6)
datiAGGreg$meanDeaths <- datiAGGreg$nMaleDeaths / datiAGGreg$years
datiAGGreg
var(datiAGGreg$meanDeaths)

library(dplyr)
medie <- dati %>% group_by(nation) %>% summarise(media = mean(nMaleDeaths))
plot(density(dati$nMaleDeaths[dati$nation==6]))
plot(density(stanTot$y_tilde.6, bw = 0.6))
mean(dati$nMaleDeaths[dati$nation==6])
mean(stanTot$y_tilde.6)

hist(stanTot$y_tilde.6)

mean(dati$nMaleDeaths)
var(dati$nMaleDeaths)


dati %>% group_by(nation) %>% summarise(media = mean(nMaleDeaths))
dati %>% group_by(nation) %>% summarise(media = mean(expectedDeaths))
#
sqrt(var(medie$media))

lambdaPrio = 39
mu0Prior = log(39)
sigma02 = 1 / 39
sigma0 = sqrt(sigma02)

log(mean(medie$media))
mu0Prior

lambdaPrio
exp(3.2) #nuovo mu0


log(lambdaPrio)
3.2 #nuovo mu0

dati %>% group_by(nation) %>% summarise(somma = sum(nMaleDeaths))


###################
datiMenoUna <- dati[!dati$nation==6,]
datiNazione6 <- dati[dati$nation==6,]

#nazioni dati a priori
a <- sum(datiMenoUna$nMaleDeaths)-1
b <- length(datiMenoUna$nMaleDeaths)

medie <- datiMenoUna %>% group_by(nation) %>% summarise(mediaTheta = mean(nMaleDeaths))

mean(datiMenoUna$nMaleDeaths)
var(datiMenoUna$nMaleDeaths) / nrow(datiMenoUna)
log(mean(datiMenoUna$nMaleDeaths)) #
1/var(medie$mediaTheta)

var(datiMenoUna$nMaleDeaths)
1/var(datiMenoUna$nMaleDeaths) #

var <- datiMenoUna %>% group_by(nation) %>% summarise(varTheta = var(nMaleDeaths))

mean(var$varTheta)
var(var$varTheta)

log(mean(var$varTheta))
round(1/var(var$varTheta),8)
sqrt(round(1/var(var$varTheta),8))
