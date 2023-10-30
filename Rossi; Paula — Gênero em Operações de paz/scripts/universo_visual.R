# Título: Genêro em Operações de Paz -- universo visual
# Script por: @depauladiasleo
# Última atualização: outubro de 2023


# Descrição:


# Preparação -------------------------------------------------------------------



## Bibliotecas -----------------------------------------------------------------


library(tidyverse)
library(showtext)
library(ggtext)


# Visualização -----------------------------------------------------------------


## Texto -----------------------------------------------------------------------



## Paleta de cores -------------------------------------------------------------


### Cor para texto -------------------------------------------------------------


txt_cor <- "gray15"

ttl_cor <- "gray5"


### Cor para linhas ------------------------------------------------------------


lin_cor <- "gray50"

bg_cor <- "gray90"


### Paletas personalizadas -----------------------------------------------------


paleta_continente <- 
                     c("África" = "#a6162b",
                       "América do Norte" = lin_cor,
                       "América do Sul" = lin_cor,
                       "Ásia" = lin_cor,
                       "Europa" = lin_cor,
                       "Oceania" = lin_cor)


países_paleta <- 
  c("#12451B", "#384d3f", "#1A543B", "#61411D",
    "#e09743", "#d25a3e", "#d42538", "#a6162b")


gênero_paleta <- c("#0f203a", "#bf6329")


## Fontes ----------------------------------------------------------------------


roboto <- "roboto"
font_add_google(roboto)


showtext_opts(dpi = 200)
showtext_auto()


## Tema ------------------------------------------------------------------------

theme_set(theme_void(base_family = roboto))

tema_gênero <- 
  theme_void() +
  theme(axis.title.y = element_text(color = ttl_cor,
                                    face = "bold",
                                    size = 10,
                                    lineheight = 1.2,
                                    angle = 0,
                                    hjust = 1,
                                    vjust = 1),
        axis.title.x = element_text(color = ttl_cor,
                                    face = "bold",
                                    size = 10,
                                    lineheight = 1.2,
                                    hjust = 1),
        axis.text.y = element_text(color = txt_cor,
                                   size = 10,
                                   lineheight = 1.2,
                                   hjust = 1),
        axis.text.x = element_text(color = txt_cor,
                                   size = 10,
                                   lineheight = 1.2,
                                   hjust = 0.5),
        axis.line = element_blank(),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
        plot.title = element_text(color = ttl_cor,
                                  face = "bold",
                                  size = 12,
                                  lineheight = 1.2),
        plot.subtitle = element_markdown(color = txt_cor,
                                     size = 10,
                                     lineheight = 1.2,
                                     margin = margin(2, 0, 22, 0, "pt")),
        plot.title.position = "plot",
        strip.text = element_text(color = ttl_cor,
                                  face = "bold",
                                  size = 10,
                                  lineheight = 1.2,
                                  hjust = 0,
                                  margin = margin(0 ,0, 12, 0, "pt")),
        panel.spacing = unit(12, "pt"),
        legend.position = "top",
        legend.title = element_text(color = ttl_cor,
                                    face = "bold",
                                    lineheight = 1.2,
                                    hjust = 0))



# Exportação -------------------------------------------------------------------