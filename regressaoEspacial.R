install.packages("tidyverse")
install.packages("sf")
install.packages("spdep")
install.packages("spatialreg")
install.packages("spgwr")
install.packages("Rcpp")

library(tidyverse)
library(sf)
library(spdep)
library(spatialreg)
library(spgwr)
library(Rccp)

dadosUnidos_sem_na <- dadosUnidos |>
  drop_na(CODMUN, IN022_AE, RendaPerCapita)

geometrias_validas <- st_is_valid(dadosUnidos)
sum(geometrias_validas == FALSE)

dadosUnidos_cent <- dadosUnidos_sem_na %>% 
  mutate(LON = st_coordinates(st_centroid(dadosUnidos_sem_na))[,1],
         LAT = st_coordinates(st_centroid(dadosUnidos_sem_na))[,2])


gwr_kernel <- gwr.sel(formula = IN022_AE ~ RendaPerCapita, 
                      data = dadosUnidos_cent,
                      coords = cbind(dadosUnidos_cent$LON,
                                     dadosUnidos_cent$LAT),
                      adapt = TRUE)

gwr_modelo <- gwr(formula = IN022_AE ~ RendaPerCapita, 
                  data = dadosUnidos_cent,
                  coords = cbind(dadosUnidos_cent$LON,
                                 dadosUnidos_cent$LAT),
                  adapt = gwr_kernel, 
                  hatmatrix = TRUE,
                  se.fit = TRUE)

