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

canal <- 'Canal B'

###### CANAL inventonahora
fn_mk_projection <- function( canal = c('Canal A', 'Canal B'),
                              dias_projecao = 365){
  
  
if( canal == 'Canal A'){
  canal_b <- acumulado %>%
             filter(CANAL == "vocesabia")
  
  title <- sprintf("Projeção de número de inscritos para os próximos %s dias (%s Você Sabia)",
                   dias_projecao,
                   canal)
  
}else{
  canal_b <- acumulado %>%
    filter(CANAL == "inventonahora")
  
  title <- sprintf("Projeção de número de inscritos para os próximos %s dias (%s Invento na hora)",
                   dias_projecao,
                   canal)
}
  
  plot(Y ~ DIAS, data = canal_b )
  
  start <- list(L = 2000, M = 800, B = -0.01)
  
  n0 <- nls(Y ~ L/(1 + exp(B * (DIAS - M))),
            data = canal_b,
            start = start,
            trace = TRUE)
  
  fit <- as.list(coef(n0))
  
  last_day <- tail(canal_b$DIAS, n=1) + dias_projecao
  
  nviews <- predict(n0, newdata = list(DIAS = last_day))
  
  subtitle <- sprintf("Número de inscritors %s milhões %s dias após a abertura ", 
                      round(nviews,2),
                      last_day)
  
  plot( formula = Y ~ DIAS, 
        data = canal_b,
        xlim = c(0, last_day + 50),
        ylim = c(0, nviews + 5),
        ylab = "Número de inscritos",
        xlab = "Dias da abertura",
        main = title,
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
}
fn_mk_projection(canal = 'Canal B', dias_projecao = 200 )
