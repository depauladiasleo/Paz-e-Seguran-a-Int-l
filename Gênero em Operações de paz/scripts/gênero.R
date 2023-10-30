# Título: Genêro em Operações de Paz -- contribuições por gênero
# Script por:
# Última atualização:


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)


# Importação de dados ----------------------------------------------------------


df_full_gender <- read_csv("df/full_gender.csv") |> 
                  janitor::clean_names()


# Faxina de dados --------------------------------------------------------------


países_analisados <- c("Etiópia", "Ruanda", "Egito", "Gana", "Nigéria")


countries <- c("Ethiopia", "Rwanda", "Egypt", "Ghana", "Nigeria")



df_gênero_tidy <- 
  df_full_gender |> 
  filter(contributing_country %in% countries,
         last_reporting_date <= as_date("2020-12-31")) |> 
  pivot_longer(cols = female_personnel:male_personnel, names_to = "gender",
               values_to = "n_contribution") |> 
  mutate(last_reporting_date = as_date(last_reporting_date),
         países_contribuintes = case_match(contributing_country,
                                           "Ethiopia" ~ "Etiópia",
                                           "Rwanda" ~ "Ruanda",
                                           "Egypt" ~ "Egito",
                                           "Ghana" ~ "Gana",
                                           "Nigeria" ~ "Nigéria"),
         tipos_de_pessoal = case_match(personnel_type,
                                       "Troops" ~ "Tropas",
                                       "Formed Police Units" ~ "Unidades Policiais",
                                       "Individual Police" ~ "Policiais Individuais",
                                       "Experts on Mission" ~ "Especialistas",
                                       "Staff Officer" ~ "Oficiais Militares"),
         gender = case_match(gender,
                             "female_personnel" ~ "Mulheres",
                             "male_personnel" ~ "Homens"))


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------


títulos1 <- list(título = "Contribuições totais por gênero",
                 subtítulo = "As contribuições nacionais de mulheres para operações de paz da ONU\nforam marginais para todo o período analisado",
                 legenda = "",
                 y = "n.",
                 x = "data")






## Paleta de cores -------------------------------------------------------------



## Fontes ----------------------------------------------------------------------



## Gráficos --------------------------------------------------------------------


## Gráfico 1 -- Contribuições totais por país e gênero -------------------------


gráfico1 <- 
df_gênero_tidy |> 
  group_by(países_contribuintes, last_reporting_date, gender) |> 
  summarize(sum_contribution = sum(n_contribution)) |> 
  mutate(area_alpha = case_match(gender,
                                    "Homens" ~ 0.6,
                                    "Mulheres" ~ 1)) |> 
  ggplot() +
  geom_area(aes(x = last_reporting_date,
            y = sum_contribution,
            fill = gender,
            color = gender,
            alpha = area_alpha)) +
  scale_fill_manual(values = gênero_paleta) +
  scale_color_manual(values = gênero_paleta) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_date(breaks = as_date(c("2010-01-01", "2012-01-01",
                                  "2014-01-01", "2016-01-01",
                                  "2018-01-01", "2020-01-01")),
               labels = scales::label_date_short()) +
  scale_alpha_identity() +
  tema_gênero + 
  theme(legend.title = element_blank()) +
  facet_wrap("países_contribuintes", ncol = 1) +
  labs(title = títulos1$título,
       subtitle = títulos1$subtítulo,
       y = "",
       x = "")


## Gráfico 2 -- Contribuições por país, gênero e contingente -------------------


plot_contingente_país <- function (df, país) {
  
  títulos2 <- list(título = glue::glue("Contribuições por gênero — {país}"),
                   subtítulo = "Dados segmentados por contingente",
                   legenda = "",
                   y = "n.",
                   x = "data")

df |> 
  filter(países_contribuintes == país) |> 
  group_by(países_contribuintes, last_reporting_date,
           personnel_type, tipos_de_pessoal, gender) |> 
  summarize(sum_contribution = sum(n_contribution)) |> 
  mutate(area_alpha = case_match(gender,
                                 "Homens" ~ 0.6,
                                 "Mulheres" ~ 1)) |> 
  ggplot() +
  geom_area(aes(x = last_reporting_date,
                y = sum_contribution,
                fill = gender,
                color = gender,
                alpha = area_alpha)) +
  scale_fill_manual(values = gênero_paleta) +
  scale_color_manual(values = gênero_paleta) +
  scale_y_continuous(labels = scales::label_number()) +
  scale_x_date(breaks = as_date(c("2010-01-01", "2012-01-01",
                                  "2014-01-01", "2016-01-01",
                                  "2018-01-01", "2020-01-01")),
               labels = scales::label_date_short()) +
  scale_alpha_identity() +
  tema_gênero +
  theme(legend.title = element_blank(),
        panel.background = element_rect(color = bg_cor,
                                        fill = bg_cor)) +
  labs(title = títulos2$título,
       subtitle = títulos2$subtítulo,
       y = "",
       x = "") +
  facet_wrap(fct_relevel(tipos_de_pessoal, c("Tropas", "Unidades Policiais",
                                           "Policiais Individuais", "Especialistas",
                                           "Oficiais Militares")) ~.,
             scales = "free_y", nrow = 5)


}


### Gráfico 2 -- Contribuições por país, gênero e contingente -- gráficos ------


gráfico2.1 <- plot_contingente_país(df_gênero_tidy, "Etiópia")

gráfico2.2 <- plot_contingente_país(df_gênero_tidy, "Ruanda")

gráfico2.3 <- plot_contingente_país(df_gênero_tidy, "Egito")

gráfico2.4 <- plot_contingente_país(df_gênero_tidy, "Gana")

gráfico2.5 <- plot_contingente_país(df_gênero_tidy, "Nigéria")


## Gráfico 3 -- Contribuições por contingente, gênero e país -------------------



plot_contingente <- function (df, contingente) {
  
  títulos3 <- list(título = glue::glue("Contribuições por país — {contingente}"),
                   subtítulo = "Dados segmentados por país",
                   legenda = "",
                   y = "n.",
                   x = "data")
  
  
  
  df |> 
    filter(tipos_de_pessoal == contingente) |> 
    group_by(países_contribuintes, last_reporting_date,
             personnel_type, gender) |> 
    summarize(sum_contribution = sum(n_contribution)) |> 
    mutate(area_alpha = case_match(gender,
                                   "Homens" ~ 0.6,
                                   "Mulheres" ~ 1)) |> 
    ggplot() +
    geom_area(aes(x = last_reporting_date,
                  y = sum_contribution,
                  fill = gender,
                  color = gender,
                  alpha = area_alpha)) +
    scale_fill_manual(values = gênero_paleta) +
    scale_color_manual(values = gênero_paleta) +
    scale_y_continuous(labels = scales::label_number()) +
    scale_x_date(breaks = as_date(c("2010-01-01", "2012-01-01",
                                    "2014-01-01", "2016-01-01",
                                    "2018-01-01", "2020-01-01")),
                 labels = scales::label_date_short()) +
    scale_alpha_identity() +
    tema_gênero +
    theme(legend.title = element_blank(),
          panel.background = element_rect(color = bg_cor,
                                          fill = bg_cor),
          plot.title = element_text(face = "bold")) +
    labs(title = títulos3$título,
         subtitle = títulos3$subtítulo,
         y = "",
         x = "") +
    facet_wrap("países_contribuintes", nrow = 5)
  
  
}


### Gráfico 3 -- Contribuições por contingente, gênero e país -- gráficos ------


gráfico3.1 <- plot_contingente(df_gênero_tidy, "Tropas")

gráfico3.2 <- plot_contingente(df_gênero_tidy, "Unidades Policiais")

gráfico3.3 <- plot_contingente(df_gênero_tidy, "Policiais Individuais")

gráfico3.4 <- plot_contingente(df_gênero_tidy, "Especialistas")

gráfico3.5 <- plot_contingente(df_gênero_tidy, "Oficiais Militares")


# Exportação -------------------------------------------------------------------



ggview::ggview(plot = gráfico3.1,
               width = 1024*1.7779,
               height = 1024*1.7779,
               dpi = 200,
               unit = "px"
)


ggsave(filename = "viz/Contribuições totais por gênero.png",
       plot = gráfico1,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")


ggsave(filename = "viz/Gráfico 2.1 -- Contribuições por país, gênero e contingente -- Etiópia.png",
       plot = gráfico2.1,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")


ggsave(filename = "viz/Gráfico 2.2 -- Contribuições por país, gênero e contingente -- Ruanda.png",
       plot = gráfico2.2,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")


ggsave(filename = "viz/Gráfico 2.3 -- Contribuições por país, gênero e contingente -- Egito.png",
       plot = gráfico2.3,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")


ggsave(filename = "viz/Gráfico 2.4 -- Contribuições por país, gênero e contingente -- Gana.png",
       plot = gráfico2.4,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")


ggsave(filename = "viz/Gráfico 2.5 -- Contribuições por país, gênero e contingente -- Nigéria.png",
       plot = gráfico2.5,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")


ggsave(filename = "viz/Gráfico 3.1 -- Contribuições por contingente, gênero e país -- Tropas.png",
       plot = gráfico3.1,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")


ggsave(filename = "viz/Gráfico 3.2 -- Contribuições por contingente, gênero e país -- Unidades Policiais.png",
       plot = gráfico3.2,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")

ggsave(filename = "viz/Gráfico 3.3 -- Contribuições por contingente, gênero e país -- Policiais Individuais.png",
       plot = gráfico3.3,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")


ggsave(filename = "viz/Gráfico 3.4 -- Contribuições por contingente, gênero e país -- Especialistas.png",
       plot = gráfico3.4,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")


ggsave(filename = "viz/Gráfico 3.5 -- Contribuições por contingente, gênero e país -- Oficiais Militares.png",
       plot = gráfico3.5,
       width = 1024*1.7779,
       height = 1024*1.779,
       dpi = 200,
       unit = "px",
       bg = "gray99")
