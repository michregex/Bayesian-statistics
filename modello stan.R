dati <- read.table("Homework_3_b_mmmec.txt", header = TRUE)
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
1/var(datiMenoUna$nMaleDeaths) / nrow(datiMenoUna)
log(mean(datiMenoUna$nMaleDeaths)) #
1/var(medie$mediaTheta)

var(datiMenoUna$nMaleDeaths)
1/var(datiMenoUna$nMaleDeaths) #
log()


var <- datiMenoUna %>% group_by(nation) %>% summarise(varTheta = var(nMaleDeaths))

mean(var$varTheta)
var(var$varTheta)

log(mean(var$varTheta))
round(1/var(var$varTheta),8)
sqrt(round(1/var(var$varTheta),8))
##########stan#######
##########stan#######
library(rstan)
N <-  354
C <-  9
a <-  4.039
b <- 10
c <-  -0.036
d <-  2
e <- 0.001
f <- 0.001
IDnation <-  c(1,2,3,4,5,6,7,8,9)
meanuvb <-  c(-2.89,-2.79,-5.63, 1.59,-4.68, 5.86,-5.05,-2.33, -4.19)


dat <- list(N = N, C=C , nMaleDeaths = dati$nMaleDeaths, nation = dati$nation,
            a = a, 
            b = a,
            c=c,
            d=d,
            uvb= dati$uvbDOSE,
            e =e,
            f=f,
            IDnation =IDnation,
            meanuvb=meanuvb)
stan <- "
data {
  int<lower=0> N;
  int<lower=0> C;
  vector[N] uvb;
  int<lower=1,upper=C> nation[N];
  int<lower=0> nMaleDeaths[N];
  int<lower=1,upper=C> IDnation[C];
  vector[C] meanuvb;
  real a;
  real <lower=0> b;
  real c;
  real <lower=0> d;
  real <lower=0> e;
  real <lower=0> f;
}
parameters {
  vector[C] alpha;
  vector[C] beta;
  real mu_alpha;
  real mu_beta;
  real <lower=0> sigma21;
  real <lower=0> sigma22;
}
model {
  // hyperpriors
  mu_alpha ~ normal(a, sqrt(b));
  mu_beta ~ normal(c, sqrt(d));
  sigma21 ~ inv_gamma(e, f);
  sigma22 ~ inv_gamma(e, f);
  // coefficients
  alpha ~ normal(mu_alpha, sqrt(sigma21));
  beta ~ normal(mu_beta, sqrt(sigma22));
  for (i in 1:N) {
    nMaleDeaths[i] ~ poisson_log(alpha[nation[i]] + beta[nation[i]] * uvb[i]);
  }
}
generated quantities {
  real theta[C];
  real<lower=0> lambda[C];
  for (i in 1:C) {
    theta[i] = alpha[IDnation[i]] + beta[IDnation[i]] * meanuvb[IDnation[i]];
    lambda[i] = exp(theta[i]);
  }
}
"

fit <- stan(model_code = stan, 
            data = dat,
            iter = 10000, chains = 5, 
            cores = 8, 
            thin = 4 )


incredibile <- summary(fit, pars=c("beta", "alpha", "lambda", "mu_alpha", "mu_beta", "sigma21", "sigma22"), 
                       probs= c(0.05, 0.5, 0.95))

str(incredibile$summary)

datafit <- as.data.frame(fit)

summarystan <- as.data.frame(incredibile$summary)

modelprioralpha <- "
parameters{
real mu_alpha;
}
model {
mu_alpha~ normal(4.039, sqrt(10));
}"

priormualpha <- stan(model_code = modelprioralpha,
            iter = 10000, chains = 1, 
            cores = 8, 
            thin = 4 )


priormual <- as.data.frame(priormualpha)


plot(density(datafit$mu_alpha), ylim = c(0,0.9))
lines(density(priormual$mu_alpha))



modelpriorbeta <- "
parameters{
  real mu_beta;
}
model {
mu_beta ~ normal(-0.036, sqrt(2));
}"

priorbeta <- stan(model_code = modelpriorbeta, 
             iter = 10000, chains = 1, 
             cores = 8, 
             thin = 4 )


priorbetadata <- as.data.frame(priorbeta)



plot(density(datafit$mu_alpha), ylim = c(0,0.9))
lines(density(priorbetadata$mu_beta))


print(fit)
plot(fit)

plot(fit, pars=c("lambda"))

lam <- pairs(fit, pars=c("lambda"))
lam <- print(fit, pars=c("lambda"), probs=c(.1,.5,.9))


# Rappresenta prior e posterior distributions
stan_plot(fit, pars = c("mu0", "tau20", "theta"))

# Rappresenta le distribuzioni posterior dei parametri lambda per le nazioni
stan_plot(fit, pars = "lambda")

# Visualizza il resoconto del modello
print(fit)

# Rappresenta prior e posterior distributions come densità
stan_hist(fit, pars = c("mu0", "tau20", "theta"), bins = 50)

# Rappresenta le distribuzioni posterior dei parametri lambda per le nazioni come densità
stan_dens(fit, pars = "lambda")

# Visualizza il resoconto del modello
print(fit)
# Carica il pacchetto rstan
library(rstan)
library(ggplot2)

# Estrai la prior e la posterior di mu0
prior_mu0 <- stan_prior(fit, "mu0")
posterior_mu0 <- stan_posterior(fit, "mu0")

# Crea un grafico di densità con ggplot2
ggplot() +
  geom_density(data = prior_mu0, aes(x = mu0, color = "Prior"), fill = "lightblue", alpha = 0.7) +
  geom_density(data = posterior_mu0, aes(x = mu0, color = "Posterior"), fill = "lightgreen", alpha = 0.7) +
  labs(title = "Prior and Posterior Distributions of mu0 (Lambda General)") +
  theme_minimal()


library(rstan)
plot(fit)
plot(fit, show_density = TRUE, ci_level = 0.5, fill_color = "purple")
plot(fit, plotfun = "hist", pars = "theta", include = FALSE)
plot(fit, plotfun = "trace", pars = c("mu0", "tau20"), inc_warmup = TRUE)
plot(fit, plotfun = "rhat") + ggtitle("Example of adding title to plot")

setwd("C:\\Users\\andre\\Desktop\\esameBayesian\\")
load("modellostan.RData")


plot(density(datafit$mu_alpha, bw = 0.2), ylim = c(0,0.68), 
     xlim = c(-7.529456,16.345292), col = 2, lwd = 2, main = "Posterior and Prior distribution of mu_alpha")
lines(density(priormual$mu_alpha, bw = 1), col = 4, lwd = 2)
legend("topright", legend = c("Posterior","Prior"), col = c(2,4), lty = 1)


plot(density(datafit$mu_beta, bw = 0.2), col = 2, lwd = 2, xlim = c(-5.105708,  5.440793),
     main = "Posterior and Prior distribution of mu_beta")
lines(density(priorbetadata$mu_beta, bw = 1), col = 4, lwd = 2)
legend("topright", legend = c("Posterior","Prior"), col = c(2,4), lty = 1)

#######TASSO DI MORTALITA' PER UN MILIONE DI ABITANTI###########
Pop <- read.table("Popolazione1.txt", sep = ";", header = TRUE)
tassi <-  matrix(datiAGG$nMaleDeaths/(Pop$Pop_Media*datiAGG$years)*1000000)
colnames(tassi) <- "Tasso_Mortalità"

format(tassi, scientific = FALSE)
Pop$Tassi <- tassi
Pop[,-2]

mu_alpha_posterior_prior <- data.frame(post = datafit$mu_alpha, prior = priormual$mu_alpha)
mu_beta_posterior_prior <- data.frame(post = datafit$mu_beta, prior = priorbetadata$mu_beta)

# Data for mu_beta
mu_beta_posterior <- density(datafit$mu_beta, bw = 0.2)
mu_beta_prior <- density(priorbetadata$mu_beta, bw = 1)

library(ggplot2)
# Posterior distribution of mu_alpha
ggplot(mu_alpha_posterior_prior, aes(x = post,fill = "blue")) +
  geom_density( alpha = 0.5, bw = 0.2) +
  geom_density(aes(x = prior,fill = "red"), alpha = 0.5, bw = 1) +
  labs(title = "Posterior and prior distribution of mu_alpha", x = "mu_alpha", fill = "") +
  scale_fill_manual(values = c("blue","red"), labels = c("Posterior", "Prior")) +
  xlim(-7.529456, 16.345292) +
  ylim(0, 0.68) +
  theme_minimal()

# Posterior distribution of mu_beta
ggplot(mu_beta_posterior_prior, aes(x = post,fill = "blue")) +
  geom_density( alpha = 0.5, bw = 0.2) +
  geom_density(aes(x = prior,fill = "red"), alpha = 0.5, bw = 1) +
  labs(title = "Posterior and prior distribution of mu_alpha", x = "mu_beta", fill = "") +
  scale_fill_manual(values = c("blue","red"), labels = c("Posterior", "Prior")) +
  xlim(-5.105708, 5.440793)+
  theme_minimal()
