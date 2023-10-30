# Título: Gênero em Operações de Paz -- Continentes
# Script por: @depauladiasleo
# Última atualização: outubro de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(ggtext)
library(janitor)


# Importação de dados ----------------------------------------------------------


df_full <- read_csv("df/full_data.csv") |> 
           janitor::clean_names()


# Faxina de dados --------------------------------------------------------------


df_continent_mission <- 
  df_full |> 
  group_by(date, contributor_continent, mission, mission_country, mission_continent, mission_region,
           mission_hq_longitude, mission_hq_latitude) |> 
  summarize(experts_on_mission = sum(experts_on_mission, na.rm = TRUE),
            observers = sum(observers, na.rm = TRUE),
            civilian_police = sum(civilian_police, na.rm = TRUE),
            troops = sum(troops, na.rm = TRUE),
            total = sum(total, na.rm = TRUE)) |> 
  mutate(experts = experts_on_mission + observers)
 

df_continent <- 
  df_full |> 
  group_by(date, contributor_continent) |> 
  summarize(experts_on_mission = sum(experts_on_mission, na.rm = TRUE),
            observers = sum(observers, na.rm = TRUE),
            civilian_police = sum(civilian_police, na.rm = TRUE),
            troops = sum(troops, na.rm = TRUE),
            total = sum(total, na.rm = TRUE)) |> 
  mutate(experts = experts_on_mission + observers) |> 
  select(date, contributor_continent, experts, civilian_police, troops)  |> 
  pivot_longer(experts:troops,
               names_to = "contingent",
               values_to = "sum") |> 
  mutate(contingent = case_match(contingent,
                                 "experts" ~ "Especialistas e observadores",
                                 "civilian_police" ~ "Policiais",
                                 "troops" ~ "Tropas"),
         contributor_continent = case_match(contributor_continent,
                                            "Africa" ~ "África",
                                            "Asia" ~ "Ásia",
                                            "Europe" ~ "Europa",
                                            "North America" ~ "América do Norte",
                                            "South America" ~ "América do Sul",
                                            "Oceania" ~ "Oceania"
                                            ))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos1 <- list(título = "Contribuições nacionais por continente para as Operações de Paz da ONU",
                 subtítulo = "Em destaque, <span style = color:#A41623><b>o crescimento das contribuições de África</span></b> para as missões")


## Paleta de cores -------------------------------------------------------------



## Fontes ----------------------------------------------------------------------



## Gráficos --------------------------------------------------------------------


### Gráfico I - Contribuição mensal por continentes e contingentes --------------


gráfico1 <- 
df_continent |> 
  ggplot() +
  geom_line(aes(x = date,
                y = sum,
                group = contributor_continent,
                color = contributor_continent)) +
  scale_color_manual(values = paleta_continente) +
  scale_y_continuous(name = "n.",
                     labels = scales::label_number(decimal.mark = ",")) +
  scale_x_date(name = "ano") +
  tema_gênero +
  theme(legend.position = "none") +
  facet_wrap("contingent",
             nrow = 3, scales = "free_y") +
  labs(title = títulos1$título,
       subtitle = títulos1$subtítulo)
  

# Exportação -------------------------------------------------------------------


ggview::ggview(plot = gráfico1,
               width = 1024*1.7779,
               height = 1024,
               dpi = 200,
               unit = "px"
)


ggsave(filename = "viz/Contribuições continentais (1990-2020).png",
       plot = gráfico1,
       width = 1024*1.7779,
       height = 1024,
       dpi = 200,
       unit = "px",
       bg = "gray99")
