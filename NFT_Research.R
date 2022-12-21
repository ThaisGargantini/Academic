Bases:https://drive.google.com/drive/folders/1zSrFoxrTrYERqk9fNeJp1_rqbvOP0QCx

rm(list = ls())
library(dplyr)
library(stringr)
library(stringi)
library(tidyr)
library(readr)
library(readxl)
library(xlsx)


setwd("C:/Users/anakn/OneDrive/Área de Trabalho/Kayma_InProgress/mohamed")

# When repeat, use the last one (excluding NA which means no actualization)

last_non_NA <- function(x) last(x[!is.na(x)])
first_non_NA <- function(x) first(x[!is.na(x)])

CensoFamilia <- read_xlsx("Censo_Dall_padronizado_tiago_08_11_2022.xlsx", sheet = "DataCensoChefeCasa")
CensoMembro <- read_xlsx ("Censo_Dall_padronizado_tiago_08_11_2022.xlsx", sheet = "DataCensoMembro")
Censo_FamiliaMembro<-left_join(CensoFamilia,CensoMembro,by="ID_member")
Censo_FamiliaMembro <- Censo_FamiliaMembro %>% rename(id_datalake = id_datalake.x)

Censo_SemDuplicata <- Censo_FamiliaMembro |> 
  arrange(end) |> 
  group_by(id_datalake) |> 
  summarise_all(first_non_NA) 