#baixando os pacoites
pacotes <- c("tidyverse", "abjData", "broom", "car", "stargazer", "report")
install.packages(pacotes)

#carregando os pacotes
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
  aes(x = RendaPerCapita, y = consumoMedioPerCapita) +
  geom_point(alpha = 0.5) +
  theme_light()

#criando modelo
modelo_linear <- lm(consumoMedioPerCapita ~ RendaPerCapita, data = dadosUnidos)

#visualização dos resultados do modelo
broom::tidy(modelo_linear) |> 
  kable()

summary(modelo_linear)

#Interpretação dos dados
# consumoMedioPerCapita = 89 + 0,013*RendaPerCapita
# cerca de 1,3% dos consumo médio de água é explicado pela renda per capita
# O p-valor muito pequeno (p < 0.001) indica que a renda per capita tem relação estatisticamente significativa com o consumo médio de agua

#ajustando os dados para a regressao
dadosUnidos_adjusted <- dadosUnidos |>
  mutate(
    valores_ajustados = predict(modelo_linear),
    residuos = consumoMedioPerCapita - valores_ajustados
  ) 

#visualizando a linha de regressao
dadosUnidos_adjusted |> 
  ggplot() + 
  aes(x = RendaPerCapita, y = consumoMedioPerCapita) + 
  geom_point(alpha = 0.5) + 
  geom_line(aes(y = valores_ajustados), color = "blue", linewidth = 1) +
  theme_light()

dadosUnidos |> 
  ggplot() + 
  aes(x = RendaPerCapita, y = consumoMedioPerCapita) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  theme_light()

#Diagnostico dos modelos

#histograma dos residuos
dadosUnidos_adjusted |>
  ggplot(aes(x = residuos)) +
  geom_histogram(
    aes(y = after_stat(density)),
    fill = "lightblue"
  ) +
  geom_density() +
  theme_light()

#Gráfico de resíduos vs valores ajustados
car::residualPlot(modelo_linear)

#teste de homocedasticidade
car::ncvTest(modelo_linear)

#Q-Qplot para a normalidade dos residuos
car::qqPlot(modelo_linear)

#tabela para a regressao
stargazer::stargazer(modelo_linear, type = "text")

#Conclusão
# 