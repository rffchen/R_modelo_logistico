library(readr)


dados <- read_csv("emergencia_soja.csv")

str(dados)
names(dados)

plot(value ~ nweek, data = dados )

start <- list(L = 100 , M = 20 , B = -1.1)

with(start,
     curve(L/(1 + exp(B * (x - M))), add = TRUE, col = "red"))


n0 <- nls(value  ~ L/(1 + exp(B * (nweek - M))),
          data = dados,
          start = start,
          trace = TRUE)

fit <- as.list(coef(n0))


plot(value ~ nweek,
     data = dados,
     xlim = c(10, 45),
     ylim = c(0, 110),
     ylab = "% da área",
     xlab = "N semanas",
     main ="Projeção da emergencia da soja americana")

with(fit,
     curve(L/(1 + exp(B * (x - M))),
           add = TRUE,
           lwd = 2,
           col = "pink"))
fit
