library(tidyverse)

dados_agua <- municipioConsumoAgua

dados_indicador <- dados_agua %>% 
    mutate(consumoMedioPerCapita = ((AG010 - AG019)/populacao)*(1000000/365))


