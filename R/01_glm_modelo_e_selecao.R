### Construindo um modelo ----
#### o processo de construção de um modelo e de selelção estão extremamente relacionados
#### portanto, a criação do modelo e a seleção estarão no mesmo script

### glm ---
library(car)
library(MuMIn)

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

M2 <- glm(Richness ~ angle1 + NAP + humus, 
          family = poisson, data = rikz)
summary(M2)
vif(M2)

drop1(M2, test="Chi")

M3 <- glm(Richness ~ NAP + humus,
          family = poisson, data=rikz)
summary(M3)
drop1(M3, test="Chi")

model.sel(M1, M2, M3)
