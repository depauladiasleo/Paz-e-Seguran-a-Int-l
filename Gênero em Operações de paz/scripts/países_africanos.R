# Título: Gênero em operações de paz -- África
# Script por: @depauladiasleo
# Última atualização: outubro de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(ggtext)
library(geomtextpath)
library(janitor)


# Importação de dados ----------------------------------------------------------


df_full <- read_csv("df/full_data.csv") |> 
           janitor::clean_names()


# Faxina de dados --------------------------------------------------------------


df_áfrica <- 
  df_full |> 
  filter(contributor_continent == "Africa")


df_países_africanos <- 
  df_áfrica |> 
  group_by(date, contributor) |> 
  summarize(experts_on_mission = sum(experts_on_mission),
            observers = sum(observers),
            civilian_police = sum(civilian_police),
            troops = sum(troops),
            total = sum(total)) |> 
  mutate(experts = experts_on_mission + observers) |> 
  select(!c(experts_on_mission, observers))


df_rank <- 
  df_países_africanos |> 
  mutate(year = year(date)) |> 
  group_by(contributor, year) |> 
  summarize(mean_troops = mean(troops, na.rm = TRUE),
            mean_police = mean(civilian_police, na.rm = TRUE),
            mean_experts = mean(experts, na.rm = TRUE),
            mean_total = mean(total, na.rm = TRUE)) |> 
  select(year, contributor, mean_total) |> 
  group_by(year) |> 
  arrange(year, desc(mean_total)) |> 
  mutate(rank = row_number(),
         contributor = case_when(
           contributor == "Ethiopia" ~ "Etiópia",
           contributor == "Egypt" ~ "Egito",
           contributor == "Ghana" ~ "Gana",
           contributor == "Nigeria" ~ "Nigéria",
           contributor == "Rwanda" ~ "Ruanda",
           contributor == "Tanzania" ~ "Tanzânia",
           TRUE ~ contributor
         ))
  

five_high <- 
  df_rank |> 
  filter(year >= 2010) |> 
  group_by(contributor) |> 
  summarize(mean_rank = mean(rank),
            median_rank = median(rank)) |> 
  arrange(median_rank) |> 
  head(5)



# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos1 <- list(título = "Ranking de países africanos que contribuíram para Operações de Paz da ONU (2010-2020)",
                 subtítulo = "Os cinco principais contribuíntes de África no período foram: Etiópia, Ruanda, Egito, Gana e Nigéria")


## Paleta de cores -------------------------------------------------------------



## Fontes ----------------------------------------------------------------------



## Gráficos --------------------------------------------------------------------


## Gráfico 1 -- ranking de contribuições gerais --------------------------------


gráfico1 <- 
df_rank |> 
  filter(year >= 2010,
         rank <= 5) |> 
  ggplot(aes(x = year,
             y = rank,
             color = contributor)) +
  geom_text(data = df_rank |> 
                   filter(year == 2010,
                          rank <= 5),
            aes(x = year - 0.5,
                y = rank,
                label = contributor)) +
  geom_text(data = df_rank |> 
              filter(year == 2020,
                     rank <= 5),
            aes(x = year + 0.5,
                y = rank,
                label = contributor)) +
  geom_text(data = df_rank |> 
              filter(year == 2016,
                     contributor %in% c("Senegal", 
                                        "Burkina Faso"),
                     rank <= 5),
            aes(x = year,
                y = rank - 0.15,
                label = contributor)) +
  geom_point(size = 2.5) +
  geom_line() +
  scale_y_reverse(name = "posição") +
  scale_x_continuous(name = "ano",
                     breaks = c(2010, 2011,
                                2012, 2013,
                                2014, 2015,
                                2016, 2017,
                                2018, 2019,
                                2020)) +
  scale_color_manual(name = "País",
                     values = países_paleta) +
  tema_gênero +
  labs(title = títulos1$título,
       subtitle = títulos1$subtítulo
       )



# Exportação -------------------------------------------------------------------


ggview::ggview(plot = gráfico1,
               width = 1024*1.7779,
               height = 1024,
               dpi = 200,
               unit = "px"
               )


ggsave(filename = "viz/Ranking de países contribuintes em África (2010-2020).png",
       plot = gráfico1,
       width = 1024*1.7779,
       height = 1024,
       dpi = 200,
       unit = "px",
       bg = "gray99")
