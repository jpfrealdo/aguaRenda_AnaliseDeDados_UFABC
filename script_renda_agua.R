install.packages(c(
  "tidyverse",
  "readxl",
  "sf",
  "corrplot",
  "GGally"
))

library(tidyverse)
library("readxl")
library(corrplot)
library(GGally)
library(sf)

#input dos dados

#dados_agua <- read_excel("municipioConsumoAgua.xlsx")
#dados_agua

#dados_indicador <- dados_agua %>% 
#  mutate(consumoMedioPerCapita = ((AG010 - AG019)/populacao)*(1000000/365))
#dados_indicador

dados_renda_consumo<-read_excel("rendaEconsumo.xlsx")

dados_renda_consumo #consumo de agua e renda tem que estar no mesmo dataframe para fazer garfico de dispersão

dados_renda_consumo <- dados_renda_consumo %>%
  mutate(consumoMedioPerCapita = as.numeric(consumoMedioPerCapita))
dados_renda_consumo <- dados_renda_consumo %>%
  mutate(consumoMedioPerCapita = as.numeric(str_replace(consumoMedioPerCapita, ",", ".")))


#grafico de dispersao Renda por Consumo de agua
dados_renda_consumo |>
  ggplot(aes(x = RendaPerCapita, y = consumoMedioPerCapita)) +
  geom_point(alpha = 0.2) +
  labs(
    tite = "Relação entre Renda per Capita e Consumo de Água per Capita",
    subtitle = "Dados de 2022 para Municípios Brasileiros",
    caption = "Fonte: SNIS e IBGE",
    x = "Renda per capita (R$)",
    y = "Consumo de água per capita (m³/ano)"
  ) +
  theme_minimal()+
  scale_x_continuous(limits = c(0, 8000)) +
  scale_y_continuous(limits = c(0, 700))

dados_renda_consumo <- dados_renda_consumo %>% 
  mutate(
    CODIBGE = as.character(CODIBGE), 
    consumoMedioPerCapita = as.numeric(consumoMedioPerCapita)
  )

br_municipios<-read_sf("dadosMapas/BR_Municipios_2024/BR_Municipios_2024.shp")
glimpse(br_municipios)


dados_unidos <- full_join(
  # tabela da esquerda
   br_municipios,
  # tabela da direita
  dados_renda_consumo, 
  # o argumento `by` especifica as colunas que serão utilizadas como chave para o join
  by = c("CD_MUN" = "CODIBGE")
)

dados_unidos |> 
  filter(SIGLA_UF== "MS")|>
  ggplot()+
  geom_sf(
    aes(fill=consumoMedioPerCapita)
  )+
  scale_fill_gradient(low = "white",high = "#0c4885", name="Consumo de Agua")

dados_unidos |> 
  filter(SIGLA_UF== "MS")|>
  ggplot()+
  geom_sf(
    aes(fill=RendaPerCapita)
    )+
  scale_fill_gradient(low = "#8bc18e",high = "#047d0c", name="Renda")


# calcuo de correlacao


# Calcular a correlação entre renda e consumo de água
correlacao <- cor(
  x = dados_renda_consumo$RendaPerCapita,
  y = dados_renda_consumo$consumoMedioPerCapita,
  method = "pearson",
  use = "complete.obs"
)

correlacao

#teste de significancia
resultado_cor_test <- cor.test(
  x = dados_renda_consumo$RendaPerCapita,         # variável explicativa
  y = dados_renda_consumo$consumoMedioPerCapita,          # variável resposta
  method = "pearson",          # correlação linear de Pearson
  alternative = "two.sided",   # hipótese bilateral (H0: r = 0)
  conf.level = 0.95            # intervalo de confiança de 95%
)
resultado_cor_test
