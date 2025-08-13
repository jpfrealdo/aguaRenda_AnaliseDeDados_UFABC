library(tidyverse)
library("readxl")
library(corrplot)
library(GGally)
library(sf)

#variaveis: renda, taxa de domicilios atendidos, atendidos(snis), não atendidos(ibge)

#lendo atendidos(snis), não atendidos(ibge)
dadosAtendidosBruto<-read_excel("dados/ConsolidadoMunicipio.xlsx")  #dados do SNIS
dadosNaoAtendidos<-read_excel("dados/rendaEconsumo.xlsx") #dados do snis + IBGE

#lendo a malha municipal
brMunicipios<-read_sf("BR_Municipios/BR_Municipios_2024.shp")

#Limpando os nomes repetidos
dadosAtendidos <- dadosAtendidosBruto[,-c(2,3,4,7)]
brMunicipios<- brMunicipios[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14)]

#Join para unir os dados do SNIS com o do IBGE
dadosAgua<- full_join(
  dadosNaoAtendidos,
  dadosAtendidos,
  by = c("CODIBGE" = "CODIBGE")
)

dadosAgua <- dadosAgua [-5571,]


# mudando a classe das variaveis
dadosAgua <- dadosAgua %>% 
  mutate(
    CODIBGE = as.character(CODIBGE), 
    consumoMedioPerCapita = as.numeric(consumoMedioPerCapita)
  )

# juntando a malha municipal com as variaveis
dadosVetorizados <- full_join(
  # tabela da esquerda
  brMunicipios,
  # tabela da direita
  dadosAgua, 
  # o argumento `by` especifica as colunas que serão utilizadas como chave para o join
  by = c("CD_MUN" = "CODIBGE")
)

#Adicionando a variavel de taxa atendida e mudando a classe de AG001
dadosUnidos <- dadosVetorizados %>% 
  mutate (
    AG001 = as.numeric(AG001),
    TaxaAtendida = (AG001/populacao)
  )
