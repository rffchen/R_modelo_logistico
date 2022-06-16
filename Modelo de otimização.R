#Bibliotecas
library(tidyverse)

# Leitura dos dados
dados <- read.table("youtube.txt", header = TRUE)

# Constroe a tabela com as somas acumuladas 
dados <- dados %>%
    group_by(CANAL) %>%
    arrange(DIAS) %>%
    mutate(Y = cumsum(VIEWS)/100000) %>%
    ungroup()


#Filtra para o canal vocesabia
canal_a <- dados %>%
    filter(CANAL == "vocesabia")

###### MODELO DE OTIMIZAÇÃO -------------------------------------------- 

#y = número de assinantes      
f_ols <- function( par, dias, y) {
        mu <- par[1]/(1 + exp(par[2] * (dias - par[3])))
        SQ <- sum( (y - mu)^2 )
        return(SQ)
    }


fit_logit_ols <- optim( par= c(1029,-0.01,607),
                        fn = f_ols, 
                        y = canal_a$Y, 
                        dias = canal_a$DIAS)

fit_logit_ols$par

############# MODELO UTILIZANDO O NLS ----------------------------------

start <- list(L = 1029,  beta = -0.01, beta0 = 607)

n0 <- nls(Y ~ L/(1 + exp(beta * (DIAS - beta0))),
          data = canal_a,
          start = start,
          trace = TRUE)

n0
