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