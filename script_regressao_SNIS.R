library(tidyverse)
library(abjData)
library(broom)
library(car)
library(stargazer)
library(report)
library(knitr)

#desabilitando a notação cientifica
options(scipen = 999) 

#grafico de dispersão
dadosUnidos |>
  ggplot() +
  aes(x = RendaPerCapita, y = IN022_AE) +
  geom_point(alpha = 0.5) +
  theme_light()

#criando modelo
modelo_linear1 <- lm(consumoMedioPerCapita ~ IN022_AE, data = dadosUnidos)

#visualização dos resultados do modelo
broom::tidy(modelo_linear1) |> 
  kable()

summary(modelo_linear1)

#Interpretação dos dados
# consumoMedioPerCapita = -16 + 0,71*RendaPerCapita
# cerca de 71% dos consumo médio de água é explicado pela renda per capita
# O p-valor muito pequeno (p < 0.001) indica que a renda per capita tem relação estatisticamente significativa com o consumo médio de agua

valores_ajustados1 = predict(modelo_linear1)

#ajustando os dados para a regressao
dadosUnidos_adjusted1 <- dadosUnidos |>
  mutate(
    residuos1 = IN022_AE - valores_ajustados1
  ) 

#visualizando a linha de regressao
dadosUnidos_adjusted |> 
  ggplot() + 
  aes(x = RendaPerCapita, y = IN022_AE) + 
  geom_point(alpha = 0.5) + 
  geom_line(aes(y = valores_ajustados1), color = "blue", linewidth = 1) +
  theme_light()

dadosUnidos |> 
  ggplot() + 
  aes(x = RendaPerCapita, y = IN022_AE) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  theme_light()

#Diagnostico dos modelos

#histograma dos residuos
dadosUnidos_adjusted1 |>
  ggplot(aes(x = residuos1)) +
  geom_histogram(
    aes(y = after_stat(density)),
    fill = "lightblue"
  ) +
  geom_density() +
  theme_light()

#Gráfico de resíduos vs valores ajustados
car::residualPlot(modelo_linear1)

#teste de homocedasticidade
car::ncvTest(modelo_linear1)

#Q-Qplot para a normalidade dos residuos
car::qqPlot(modelo_linear1)

#tabela para a regressao
stargazer::stargazer(modelo_linear1, type = "text")

#Conclusão
# 