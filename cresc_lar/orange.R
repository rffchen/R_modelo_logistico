
#-----------------------------------------------------------------------
library(datasets)

str(Orange)

ggplot(data = Orange,
       mapping = aes(x = age, y = circumference)) +
  facet_wrap(facets = ~Tree) +
  geom_point()


all_tree <- Orange %>%
            rename(circ = "circumference")


plot(circ ~ age, data = all_tree )

start <- list(L = 200 , M = 500 , B = -0.002758071)

with(start,
     curve(L/(1 + exp(B * (x - M))), add = TRUE, col = "red"))


n0 <- nls(circ ~ L/(1 + exp(B * (age - M))),
          data = all_tree,
          start = start,
          trace = TRUE)

fit <- as.list(coef(n0))


plot(circ ~ age,
     data = all_tree,
     xlim = c(0, 1800),
     ylim = c(0, 200),
     ylab = "Circuferencia",
     xlab = "Idade em dias",
     main ="Projeção do crescimento de todas")

with(fit,
     curve(L/(1 + exp(B * (x - M))),
           add = TRUE,
           lwd = 2,
           col = "pink"))



#-----------------------------------------------------------------------