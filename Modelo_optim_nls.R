# https://en.wikipedia.org/wiki/Logistic_function
# https://www.youtube.com/watch?v=ykO8QVu-v8g



#Bibliotecas
library(tidyverse)

# Leitura dos dados
dados <- read.table("youtube.txt", header = TRUE)

# Constroe a tabela com as somas acumuladas 
dados <- dados %>%
    group_by(CANAL) %>%
    arrange(DIAS) %>%
    mutate(Y = cumsum(INSCRITOS)) %>%
    ungroup()


#Filtra para o canal vocesabia
canal_a <- dados %>%
    filter(CANAL == "vocesabia")



####### MODELO LOGÍSTICO ------------------------------------------------------- 

f_log <- function(DIAS, L, beta, beta0) {
  out <- L/(1+ exp(-beta*(DIAS - beta0)))  #pq esse -beta
  return(out)
}

DIAS <- 1:800

plot(f_log(DIAS = DIAS, L = 90, beta = 0.01, beta0 = 400) ~ DIAS, 
     ylab = "Número de inscritos", xlab = "Dias da abertura",
     type = "l", ylim = c(0,95))
abline(h = 90) 
text(x = 800, y = 93, label = "L")
text(x = 425, y = f_log(DIAS = 400, L = 90, beta = 0.01, beta0 = 400), 
     label = expression(beta))
points(x = 400, pch = 19, col = "red",
       y = f_log(DIAS = 400, L = 90, beta = 0.01, beta0 = 400))

###### MODELO DE OTIMIZAÇÃO ---------------------------------------------------- 

#y = número de assinantes      
f_ols <- function( par, dias, y) {
        mu <- par[1]/(1 + exp(par[2] * (dias - par[3])))
        SQ <- sum( (y - mu)^2 )
        return(SQ)
  }

xpar= c(3052496,0.01,1526248)

xpar= c(0,0,0)

fit_logit_ols <- optim( par= xpar,
                        fn = f_ols, 
                        y = canal_a$Y, 
                        dias = canal_a$DIAS)

fit_logit_ols$par

lst_day <- tail(canal_a,1)[1]%>%pull() + 365

DIAS <- seq(1:lst_day)
v <- f_log(DIAS = DIAS, L = fit_logit_ols$par[1], beta = fit_logit_ols$par[2], beta0 = fit_logit_ols$par[3])

plot(f_log(DIAS = DIAS, L = fit_logit_ols$par[1], beta = fit_logit_ols$par[2], beta0 = fit_logit_ols$par[3]) ~ DIAS, 
     ylab = "Número de inscritos", xlab = "Dias da abertura",
     type = "l")

############# MODELO UTILIZANDO O NLS ----------------------------------

start <- list(L = 3052496,  beta = -0.02, beta0 = 1526248)

start <- list(L = 0,  beta = 0, beta0 = 0)

n0 <- nls(Y ~ L/(1 + exp(-beta * (DIAS - beta0))),
          data = canal_a,
          start = start,
          trace = TRUE)

n0
