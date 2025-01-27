# https://en.wikipedia.org/wiki/Logistic_function
# https://www.youtube.com/watch?v=ykO8QVu-v8g

#Bibliotecas
library(tidyverse)

rm(list=ls())

# Leitura dos dados
dados <- read.table(  file = "http://leg.ufpr.br/~wagner/data/youtube.txt", 
                      header = TRUE)

# Constroe a tabela com as somas acumuladas 
acumulado <- dados %>%
          group_by(CANAL) %>%
          arrange(DIAS) %>%
          mutate(Y = cumsum(INSCRITOS)/100000) %>%
          ungroup()


#Filtra para os canais
canal_a <- acumulado %>%
            filter(CANAL == "vocesabia")


canal_b <- acumulado %>%
           filter(CANAL == "inventonahora")


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


############################# NLS #############################################
### CANAL vocesabia

plot(Y ~ DIAS, data = canal_a )

start <- list(L = 2000, M = 800, B = -0.01)

n0 <- nls(Y ~ L/(1 + exp(B * (DIAS - M))),
          data = canal_a,
          start = start,
          trace = TRUE)

fit <- as.list(coef(n0))


last_day <- tail(canal_a$DIAS, n=1) + 365

nviews <- predict(n0, newdata = list(DIAS = last_day))

subtitle <- sprintf("Número de inscritors %s milhões %s dias após a abertura (L=%s, Beta=%s, Beta0=%s) ", 
                    round(nviews,2),
                    last_day,
                    round(coef(n0)[1],2),
                    round(coef(n0)[3],2),
                    round(coef(n0)[2],2))

plot( formula = Y ~ DIAS, 
      data = canal_a,
      xlim = c(0, last_day + 50),
      ylim = c(0, nviews + 5),
      ylab = "Número de inscritos",
      xlab = "Dias da abertura",
      main ="Projeção de número de inscritos para os próximos 365 dias canal A",
      sub = subtitle)

with(fit,
     curve(L/(1 + exp(B * (x - M))),
           add = TRUE,
           lwd = 2,
           col = "#095e06"))

points(x = last_day, 
       pch = 35, 
       col = "red",
       y = nviews)


###### CANAL inventonahora
plot(Y ~ DIAS, data = canal_b )

start <- list(L = 2000, M = 800, B = -0.01)

n0 <- nls(Y ~ L/(1 + exp(B * (DIAS - M))),
          data = canal_b,
          start = start,
          trace = TRUE)

fit <- as.list(coef(n0))

last_day <- tail(canal_b$DIAS, n=1) + 365

nviews <- predict(n0, newdata = list(DIAS = last_day))


subtitle <- sprintf("Número de inscritors %s milhões %s dias após a abertura (L=%s, Beta=%s, Beta0=%s) ", 
                    round(nviews,2),
                    last_day,
                    round(coef(n0)[1],2),
                    round(coef(n0)[3],2),
                    round(coef(n0)[2],2))


plot( formula = Y ~ DIAS, 
      data = canal_b,
      xlim = c(0, last_day + 50),
      ylim = c(0, nviews + 5),
      ylab = "Número de inscritos",
      xlab = "Dias da abertura",
      main ="Projeção de número de inscritos para os próximos 365 dias canal B",
      sub = subtitle)


with(fit,
     curve(L/(1 + exp(B * (x - M))),
           add = TRUE,
           lwd = 2,
           col = "#095e06"))

points(x = last_day, 
       pch = 35, 
       col = "red",
       y = nviews)

