### Construindo um modelo ----
#### o processo de construção de um modelo e de selelção estão extremamente relacionados
#### portanto, a criação do modelo e a seleção estarão no mesmo script

### glm ---
library(car)
library(MuMIn)
library(DHARMa)
library(MASS)

rikz <- read.csv("data/processed/frikz")
str(rikz)

M1 <- glm(Richness ~ Week + angle1 + angle2 +
            exposure + salinity + temperature +
            NAP + penetrability + grainsize +
            humus + Beach, family = poisson, data = rikz)
summary(M1)
vif(M1)

### sobredispersão do modeol: residual deviance / degrees of freedom
37.852/33
### não é um valor muito alto, então tudo ok

### valores de vif devem ser menores que 3 para as variáveis explanatórias
### então week, angle2, exposure, salinity, temperature, penetrability, grainsize
### e beach devem sair do modelo
### mas vamos tentar ir excluindo as variáveis com ML (Drop1)

drop1(M1, test = "Chi")

M2 <- glm(Richness ~ Week + angle1 + angle2 +
            exposure + salinity + temperature +
            NAP + penetrability + grainsize +
            humus, family = poisson, data = rikz)
summary(M2)
drop1(M2, test="Chi")

M3 <- glm(Richness ~ Week + angle1 + angle2 +
            exposure + salinity +
            NAP + penetrability + grainsize +
            humus, family = poisson, data = rikz)
summary(M3)
drop1(M3, test="Chi")

M4 <- glm(Richness ~ Week + angle1 + angle2 +
            exposure + salinity +
            NAP + grainsize +
            humus, family = poisson, data = rikz)
summary(M4)
drop1(M4, test="Chi")


M5 <- glm(Richness ~ Week + angle1 + angle2 +
            exposure + salinity +
            NAP + grainsize, family = poisson, data = rikz)
summary(M5)
drop1(M5, test="Chi")

M6 <-  glm(Richness ~ Week + angle2 +
             exposure + salinity +
             NAP + grainsize, family = poisson, data = rikz)
summary(M6)
drop1(M6, test="Chi")

### usando drop1, chegamos em M6, no qual todas as variáveis são significativas
### no entanto, vamos usar vif para ver se não há colinearidade entre as variáveis que restaram
vif(M6)

### vif de week, angle2, salinity e grainsize estão muito altos

M7 <- glm(Richness ~ Week +
            exposure + salinity +
            NAP + grainsize, family = poisson, data = rikz)
summary(M7)
drop1(M7, test = "Chi")

### retirando a vairável com maior valor de vif (angle2) e usando novamente drop1, temos variáveis que perdem significância

M8 <- glm(Richness ~ Week +
            exposure + salinity +
            NAP, family = poisson, data = rikz)
summary(M8)
drop1(M8, test = "Chi")


M9 <- glm(Richness ~ exposure + salinity +
            NAP, family = poisson, data = rikz)
summary(M9)
drop1(M9, test = "Chi")

### parece que atingimos um modelo no qual todas as variáveis explanatórias são significativas
### mas vamos ver como que ficam modelos mais simples, com menos variáveis, para poder comparar via AIC depois

M10 <- glm(Richness ~ NAP + exposure,
           family = poisson,
           data = rikz)
summary(M10)

M11 <- glm(Richness ~ NAP * exposure, 
           family = poisson,
           data = rikz)
summary(M11)

M12 <- glm(Richness ~ NAP,
           family = poisson, 
           data = rikz)
summary(M12)

M13 <- glm(Richness ~ exposure,
           family = poisson,
           data = rikz)
summary(M13)

### após todos os modelos feitos, percebemos que talvez a variável Week não tenha um sentido biológico
### para explicar a riqueza, portanto, vamos testar modelos sem essa variável
M14 <-  glm(Richness ~ angle2 +
              exposure + salinity +
              NAP + grainsize, family = poisson, data = rikz)
summary(M14)
drop1(M14, test="Chi")

M15 <-  glm(Richness ~ exposure + salinity +
              NAP + grainsize, family = poisson, data = rikz)
summary(M15)
drop1(M15, test="Chi")

M16 <-  glm(Richness ~ exposure + salinity +
              NAP, family = poisson, data = rikz)
summary(M16)
drop1(M16, test="Chi")

model.sel(M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16)

par(mfrow = c(2,2))
plot(M5)
plot(M6)
plot(M11)
par(mfrow = c(1,1))

### o gráfico de residuals vs fitted não parece um céu estrelado
### e o de residuals vs leverage apresenta alguns pontos que podem ser outliers

### utilizando o DHARMa para a validação de modelo

### Calcular os residuos do M5
simulationOutput <- simulateResiduals(fittedModel = M5, n = 1000)
residuals(simulationOutput)

# Testando dispersao do M5
testDispersion(simulationOutput, type = "PearsonChisq")

# Plot principal M5
plot(simulationOutput)

### Calcular os residuos M6
simulationOutput <- simulateResiduals(fittedModel = M6, n = 1000)
residuals(simulationOutput)

# Testando dispersao M6
testDispersion(simulationOutput, type = "PearsonChisq")

# Plot principal M6
plot(simulationOutput)

### Calcular os residuos M11
simulationOutput <- simulateResiduals(fittedModel = M5, n = 1000)
residuals(simulationOutput)

# Testando dispersao M11
testDispersion(simulationOutput, type = "PearsonChisq")

# Plot principal M11
plot(simulationOutput)


### Testando modelos com a distribuição binomial negativa

m.nb1 <- glm.nb (Richness ~ Week + angle1 + angle2 +
                   exposure + salinity + temperature +
                   NAP + penetrability + grainsize +
                   humus + Beach, link = "log", data = rikz)
summary(m.nb1)
drop1(m.nb1, test = "Chi")

m.nb2 <- glm.nb (Richness ~ Week + angle1 + angle2 +
                   exposure + salinity + temperature +
                   NAP + penetrability + grainsize +
                   humus, link = "log", data = rikz)
summary(m.nb2)
drop1(m.nb2, test = "Chi")

m.nb3 <- glm.nb (Richness ~ Week + angle1 + angle2 +
                   exposure + salinity +
                   NAP + penetrability + grainsize +
                   humus, link = "log", data = rikz)
summary(m.nb3)
drop1(m.nb3, test = "Chi")

m.nb4 <- glm.nb (Richness ~ Week + angle1 + angle2 +
                   exposure + salinity +
                   NAP + grainsize +
                   humus, link = "log", data = rikz)
summary(m.nb4)
drop1(m.nb4, test = "Chi")

m.nb5 <- glm.nb (Richness ~ Week + angle1 + angle2 +
                   exposure + salinity +
                   NAP + grainsize, link = "log", data = rikz)
summary(m.nb5)
drop1(m.nb5, test = "Chi")


m.nb6 <- glm.nb (Richness ~ Week + angle2 +
                   exposure + salinity +
                   NAP + grainsize, link = "log", data = rikz)
summary(m.nb6)
drop1(m.nb6, test = "Chi")

model.sel(m.nb1, m.nb2, m.nb3, m.nb4, m.nb5, m.nb6)

### Validando modelos com DHARMa
### Calcular os residuos do m.nb5
simulationOutput <- simulateResiduals(fittedModel = m.nb5, n = 1000)
residuals(simulationOutput)

# Testando dispersao do m.nb5
testDispersion(simulationOutput, type = "PearsonChisq")

# Plot principal m.nb5
plot(simulationOutput)

### Calcular os residuos do m.nb6
simulationOutput <- simulateResiduals(fittedModel = m.nb6, n = 1000)
residuals(simulationOutput)

# Testando dispersao do m.nb6
testDispersion(simulationOutput, type = "PearsonChisq")

# Plot principal m.nb6
plot(simulationOutput)

ang <- as.data.frame(rikz[,5:6])
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

pairs(ang, panel = panel.smooth, lower.panel = panel.cor)

