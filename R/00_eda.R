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

### varáivel resposta é Richness -> dados discretos/de contagem
### duas distribuições possíveis são poisson e binomial negativa
### sendo que binomial negativa suporta maior dispersão dos dados
### na dist possion média = variância

### QQ-plot poisson
qqPlot(rikz$Richness, dist = "pois", lambda = mean(rikz$Richness))

### parece que possion é um distribuição razoável para a variável resposta, com apenas dois pontos
### que poderiam ser outliers
### seria adequado criar modelos para as distribuições poisson e com distribuição binomial negativa
### e depois validar os modelos


### Histogramas
ggplot(rikz, aes(Richness)) +
  geom_histogram(bins = 10) +
  xlab("Richness")
?geom_histogram

### Cleveland dotplot
dotchart(rikz$Richness)

### identificando outliers
boxplot(rikz$Richness)
beeswarm::beeswarm(rikz$Richness, col="red", pch=16, method="swarm")

ggplot(rikz, aes(y = Richness)) +
  geom_boxplot(alpha=0.5)

plot(density(rikz$Richness))

# par(mfrow = c(1,3))
# ggplot(rikz, aes(x = NAP, y = Richness)) +
#   geom_boxplot(alpha = 0.5) + xlab("NAP") + ylab("Richness")
# ggplot(rikz, aes(x = Beach, y = Richness)) + 
#   geom_boxplot(alpha = 0.5) + xlab("Beach") + ylab("Richness")
# ggplot(rikz, aes(x = exposure, y = Richness)) + 
#   geom_boxplot(alpha = 0.5) + xlab("Exposure") + ylab("Richness")
# par(mfrow = c(1,1))


### Diagrama de dispersão Riqueza ~ NAP
plot(rikz$Richness ~ rikz$NAP)

boxplot(rikz$Richness ~ rikz$Beach)
boxplot(rikz$Richness ~ rikz$exposure)


### transformando variáveis explanatórias em fatores
rikz$fexposure <- as.factor(rikz$exposure)
rikz$fbeach <- as.factor(rikz$Beach)
rikz$fNAP <- as.factor(rikz$NAP)
rikz$fangle1 <- as.factor(rikz$angle1)
rikz$fangle2 <- as.factor(rikz$angle2)
rikz$fsalinity <- as.factor(rikz$salinity)
rikz$ftemperature <- as.factor(rikz$temperature)
rikz$fpenetrability <- as.factor(rikz$penetrability)
rikz$fgrainsize <- as.factor(rikz$grainsize)
rikz$fhumus <- as.factor(rikz$humus)

write.csv(rikz, file="data/processed/frikz", sep=";")
### existe correlação entre variáveis explanatórias?
exp <- as.data.frame(rikz[,14:23])
pairs(exp, panel=panel.smooth)

# Funcao encontrada no help da funcao 'pairs' para plotar histogramas
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

# Outra funcao para fornecer os coeficientes de correlacao com a fonte proporcional ao indice
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

### Parece que exposure e temperatura estão correlacionadas, bem como praia e salinidade

