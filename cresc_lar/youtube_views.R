#-----------------------------------------------------------------------
# Pacotes Utilizados
library(tidyverse)


#-----------------------------------------------------------------------
#Leitura e tratamento.

dados <- read.table("youtube.txt", header = TRUE)

dados <- dados %>%
         group_by(CANAL) %>%
         arrange(DIAS) %>%
         mutate(Y = cumsum(VIEWS)/100000) %>%
         ungroup()

#-----------------------------------------------------------------------
#Visualização dos dados

ggplot(data = dados,
       mapping = aes(x = DIAS, y = Y, color = CANAL)) +
  geom_point() +
  stat_summary(geom = "line", fun = "mean")

#-----------------------------------------------------------------------


canal_a <- dados %>%
            filter(CANAL == "vocesabia")


plot(Y ~ DIAS, data = canal_a )

start <- list(L = 2000, M = 600, B = -0.01)

with(start,
     curve(L/(1 + exp(B * (x - M))), add = TRUE, col = "red"))

n0 <- nls(Y ~ L/(1 + exp(B * (DIAS - M))),
          data = canal_a,
          start = start,
          trace = TRUE)

fit <- as.list(coef(n0))

last_day <- tail(canal_a$DIAS, n=1) + 365

nviews <- predict(n0, newdata = list(DIAS = last_day))

subtitle <- sprintf("Número de inscritors %s * 100000", round(nviews,0))

plot(Y ~ DIAS,
     data = canal_a,
     xlim = c(0, 1100),
     ylim = c(0, 2500),
     ylab = "Número de inscritos",
     xlab = "Dias da abertura",
     main ="Projeção de número de inscritos para os próximos 365 dias",
     sub = subtitle)

with(fit,
     curve(L/(1 + exp(B * (x - M))),
           add = TRUE,
           lwd = 2,
           col = "pink"))

points(x = last_day, pch = 20, col = "red",
       y = nviews)


#-----------------------------------------------------------------------


canal_b <- dados %>%
  filter(CANAL == "inventonahora")


plot(Y ~ DIAS, data = canal_b )

start <- list(L = 2000, M = 600, B = -0.01)

with(start,
     curve(L/(1 + exp(B * (x - M))), add = TRUE, col = "red"))

n0 <- nls(Y ~ L/(1 + exp(B * (DIAS - M))),
          data = canal_b,
          start = start,
          trace = TRUE)

fit <- as.list(coef(n0))

last_day <- tail(canal_b$DIAS, n=1) + 365

nviews <- predict(n0, newdata = list(DIAS = last_day))

subtitle <- sprintf("Número de inscritors %s * 100000", round(nviews,2))

plot(Y ~ DIAS,
     data = canal_b,
     xlim = c(0, 1300),
     ylim = c(0, 1500),
     ylab = "Número de inscritos",
     xlab = "Dias da abertura",
     main ="Projeção de número de inscritos para os próximos 365 dias",
     sub = subtitle)
with(fit,
     curve(L/(1 + exp(B * (x - M))),
           add = TRUE,
           lwd = 2,
           col = "pink"))
points(x = last_day, pch = 20, col = "red",
       y = nviews)








