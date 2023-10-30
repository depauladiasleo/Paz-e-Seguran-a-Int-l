# Título: Genêro em Operações de Paz -- gênero
# Script por: @depauladiasleo
# Última atualização: outubro de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(janitor)


# Importação de dados ----------------------------------------------------------


df_gênero <- read_csv("df/full_gender.csv") |> 
             janitor::clean_names()


df_full <- read_csv("df/full_data.csv") |> 
           janitor::clean_names()


# Faxina de dados --------------------------------------------------------------


df_países_glossario <-
  df_full |> 
  select(!c(date, mission:total)) |> 
  unique()


df_missões_glossário <- 
  df_full |> 
  select(!c(date:contributor_sadc, experts_on_mission:total)) |> 
  unique()



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------



## Fontes ----------------------------------------------------------------------



## Gráfico ---------------------------------------------------------------------


write_csv(df_missões_glossário, file = "df/missões_glossário.csv")


write_csv(df_países_glossario, file = "df/contribuintes_glossário.csv")



# Exportação -------------------------------------------------------------------