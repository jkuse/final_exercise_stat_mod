## Exploratory Data Analysis de RIKZ 
library(tidyr)
library(ggplot2)
library(car)
library(MASS)
library(ggpubr)
library(beeswarm)

rikz <- read.table(file.choose(), header = T)
head(rikz)
str(rikz)
summary(rikz)

### com os dados RIKZ, a nossa pergunta é: quais fatores abióticos influenciam a riqueza de fauna bentonica em uma praia?
### varáivel resposta é Richness -> dados discretos/de contagem
### duas distribuições possíveis são poisson e binomial negativa
### sendo que binomial negativa suporta maior dispersão dos dados
### na dist possion média = variância
### a natureza da variável resposta (Richness) pode ser uma boa forme de saber a distribuição dos resíduos
### portanto, vamos explorar os dados

### QQ-plot poisson
qqPlot(rikz$Richness, dist = "pois", lambda = mean(rikz$Richness))

### parece que possion é um distribuição razoável para a variável resposta, com apenas dois pontos que poderiam ser outliers
### seria adequado criar modelos para as distribuições poisson e distribuição binomial negativa e depois validar os modelos

### QQ-plot Biomial Negativa
qqPlot(rikz$Richness, distribution = "nbinom", size=5, prob=0.5)
### parece que a biomial negativa acomoda melhor a sobredispersão dos dados


### Histogramas
ggplot(rikz, aes(Richness)) +
  geom_histogram(bins = 10) +
  xlab("Richness")

### identificando possíveis outliers 
dotchart(rikz$Richness) ### richness no eixo x
plot(rikz$Richness)
boxplot(rikz$Richness)
boxplot(rikz$Richness)$stat
beeswarm::beeswarm(rikz$Richness, col="red", pch=16, method="swarm")


plot(density(rikz$Richness))
### plotando a linha de densidade sobre o histograma
hist(rikz$Richness, probability = T)
lines(density(rikz$Richness))

### Diagrama de dispersão Riqueza ~ NAP -> ver a relação dee NAP com Richness
plot(rikz$Richness ~ rikz$NAP)
### quando a riqueza é maior, se tem uma maior variância dos dados

### transformando algumas variáveis explanatórias em fatores
rikz$fexposure <- as.factor(rikz$exposure)
rikz$fbeach <- as.factor(rikz$Beach)
rikz$fweek <- as.factor(rikz$Week)

write.csv(rikz, file="data/processed/frikz", sep=";")
### existe correlação entre variáveis explanatórias?
exp <- as.data.frame(rikz[,4:14])
pairs(exp, panel=panel.smooth)

# Função encontrada no help de 'pairs' para plotar histogramas
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

pairs(exp, diag.panel=panel.hist)

# Função para fornecer os coeficientes de correlacao com a fonte proporcional ao indice
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(exp, panel = panel.smooth, diag.panel=panel.hist, lower.panel = panel.cor)

### Parece que exposure e temperatura estão correlacionadas, bem como praia e salinidade, agle2 e grainsize
### e week e praia

### ajustando o modelo linear para ver os resíduos
mlm <- lm(Richness ~ NAP, data = rikz)
resid <- resid(mlm)
resid
plot(resid)
hist(resid)

par(mfrow = c(2,2))
plot(mlm)
par(mfrow  = c(1,1))
