### Agora, vamos utilizar glmms
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
### utilizando NAP como variável explatória e Beach como variável aleatória
(m1 <- glmmTMB(Richness ~ NAP + (1|fbeach),
               family=poisson, data=rikz, REML = T))
summary(m1)

### agora um modelo com intercepto e inclinação aleatórios
(m2 <- glmmTMB(Richness ~ NAP + (1 + NAP | fbeach),
               family = poisson, data = rikz, REML = T))
summary(m2)

### agora um modelo apenas com inclinação aleatória
(m3 <- glmmTMB(Richness ~ NAP + (NAP | fbeach),
               family = poisson, data = rikz, REML = T))
summary(m3)

anova(m1,m2,m3)

### testando com distribuição binomial negativa
(m4 <- glmmTMB(Richness ~ NAP + (1|fbeach),
               family=, data=rikz, REML = T))
summary(m4)

### agora um modelo com intercepto e inclinação aleatórios
(m5 <- glmmTMB(Richness ~ NAP + (1 + NAP | fbeach),
               family = poisson, data = rikz, REML = T))
summary(m5)

### agora um modelo apenas com inclinação aleatória
(m6 <- glmmTMB(Richness ~ NAP + (NAP | fbeach),
               family = poisson, data = rikz, REML = T))
summary(m6)

### testando os componentes fixos por máxima verossimilhança

fm1 <- glmmTMB(Richness ~ NAP + fexposure + Week + angle1 + angle2 + salinity + grainsize + (1 | fbeach),
           family = poisson,
           data = rikz,
           REML = F)
summary(fm1)

fm2 <- glmmTMB(Richness ~ NAP + fexposure + Week + angle2 + salinity + grainsize + (1 | fbeach),
               family = poisson,
               data = rikz,
               REML = F)
summary(fm2)

model.sel(fm1, fm2)

### os dois modelos apresentam um delta AIC menor que 2, portando, ambos podem ser válidos
### validar com DHARMA

# Calcular os residuos de fm1
simulationOutput1 <- simulateResiduals(fittedModel = fm1, n = 1000)
residuals(simulationOutput1)

# Testando dispersao de fm1
testDispersion(simulationOutput1, type = "PearsonChisq")
plot(simulationOutput1)

### Calcular resíduos de fm2
simulationOutput2 <- simulateResiduals(fittedModel = fm2, n = 1000)
residuals(simulationOutput2)

# Testando dispersao fm2
testDispersion(simulationOutput2, type = "PearsonChisq")
plot(simulationOutput2)

