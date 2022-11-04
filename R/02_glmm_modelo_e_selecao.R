### agora, vamos utilizar glmms
### não vamos usar lmm pois a distribuição da variável resposta não é normal, portanto, precisamos
### de uma generalização do modelo

### alguns passos para a contrução de modelos mistos são:
#### Primeiro passo  ----
#### começar com um modelo perto do ótimo com os componentes fixos (todas as variáveis explanatórias possíveis)
#### Segundo passo ----
#### testar variáveis aleatórias, procurando pela estrtura aleatória "ótima" via REML
#### Terceiro passo ----
#### tendo uma estrtura aleatória perto do ótimo, reduzir componentes fixos por máxima verossimilhança (drop1)
### Quarto passo ----
#### quando se alcançar a estrutura "perto de ótima" nos dois componentes (fixos e aleatórios),
#### validar modelo (pode ser por DHARMa)

library(glmmTMB)
library(car)
library(MuMIn)
library(DHARMa)
rikz <- read.csv("data/processed/frikz")

### montando um modelo misto com intercepto aletório
### NAP de variável explanatória e Beach como variável aleatória
(m1 <- glmmTMB(Richness ~ NAP + (1|fbeach),
               family=poisson, data=rikz, REML = T))
summary(m1)

### agora um modelo com intercepto e inclinação aleatórios
(m2 <- glmmTMB(Richness ~ NAP + (1 + NAP | fbeach),
               family = poisson, data = rikz, REML = T))
summary(m2)

### testando os componentes fixos por máxima verossimilhança

fm1 <- glm(Richness ~ NAP + fexposure + humus + angle2 + fbeach,
           family = poisson,
           data = rikz)
summary(fm1)

drop1(fm1, test = "Chi")

fm2 <- glm(Richness ~ NAP + fexposure + humus + fbeach,
           family = poisson,
           data = rikz)
summary(fm2)

drop1(fm2, test = "Chi")

fm3 <- glm(Richness ~ NAP + fexposure + humus, 
           family = poisson,
           data = rikz)
summary(fm3)

drop1(fm3, test = "Chi")

fm4 <- glm(Richness ~ NAP + fexposure,
           family = poisson,
           data = rikz)
summary(fm4)

model.sel(fm1, fm2, fm3, fm4)

### fm3 parece ser o modelo com componentes fixos mais perto do ótimo
### validar com DHARMA

# Calcular os residuos
simulationOutput <- simulateResiduals(fittedModel = fm3, n = 1000)
residuals(simulationOutput)

# Testando dispersao
testDispersion(simulationOutput, type = "PearsonChisq")

plot(simulationOutput)

### Não tá muito bom o modelo
### Vamos tentar validar adicionando o componente aleatório


