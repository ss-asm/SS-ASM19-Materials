################################################################
################################ THEMES
################################################################ 

library(tidyverse)
library(ggplot2)

################################ WHITE
## Temporal (horizontal)

theme_hor <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_line(size = 0.5, colour = 'black'),
          axis.line.y = element_blank(),
          axis.ticks.x = element_line(size = 0.5, colour = 'black'),
          axis.ticks.y = element_line(size = 0.1, colour = 'grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(5, 5, 5, 5)
    )
}

## Nontemporal (horizontal)

theme_rot <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_line(size = 0.5, colour = 'black'),
          axis.line.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5, colour = 'black'),
          axis.ticks.x = element_line(size = 0.1, colour = 'grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black'),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(5, 5, 5, 5)
    )
}

## Scatter and point

theme_ver <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.y = element_line(size = 0.5, colour = 'black'),
          axis.line.x = element_blank(),
          axis.ticks.y = element_line(size = 0.5, colour = 'black'),
          axis.ticks.x = element_line(size = 0.1, colour = 'grey50'),
          axis.text.x = element_text(face = 'bold'),
          axis.text.y = element_text(face = 'bold'),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black'),
          strip.text = element_text(face = 'bold', colour = 'black'),
          plot.margin = margin(5, 5, 5, 5)
    )
}

##

theme_map <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'black', size = 15),
          plot.caption = element_text(face = 'bold', colour = 'black'),
          strip.text = element_text(face = 'bold', colour = 'black'),
          legend.title = element_text(face = 'bold', colour = 'grey50', angle = 270),
          legend.text = element_text(colour = 'black', angle = 270),
          plot.margin = margin(5, 5, 5, 5)
    )
  
}

################################ BLACK

theme_bm_legend <- function () {
  theme_void() + 
    theme(plot.background = element_rect(fill = 'black', colour = 'black'),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_text(colour = 'grey50', angle = 270),
          legend.text = element_text(colour = 'white', angle = 270),
          plot.title = element_text(face = 'bold', colour = 'grey50'),
          plot.subtitle =  element_text(face = 'plain', colour = 'white', size = 15),
          panel.grid.major = element_line(size = NA), 
          panel.grid.minor = element_line(size = NA),
          legend.position = c(0.8, 0.25),
          plot.margin = margin(20, 20, 20, 20)
    )
  
}

##

################################ Guides
## Vertical
# Continuous

guide_continuous <- 
  guide_colorbar(direction = "vertical",
                 barheight = unit(50, units = "mm"),
                 barwidth = unit(2, units = "mm"),
                 draw.ulim = FALSE,
                 title.position = 'right',
                 label.position = 'left',
                 title.hjust = 0.5,
                 label.hjust = 0.5)

# Discrete

guide_discrete <-
  guide_legend(direction = "vertical",
               keyheight = unit(10, units = "mm"),
               keywidth = unit(2, units = "mm"),
               title.position = 'right',
               label.position = 'left',
               title.hjust = 0.5,
               label.hjust = 1,
               ncol = 1,
               bycol = TRUE)

################################ PALETTES
## Space Syntax

pal <- c('#00007f', '#0000fe', '#0160ff', '#01d0ff', '#49ffad', 
         '#a4ff53', '#fbec00', '#ff8500', '#ff1e00', '#7f0000')


fun <- 
  colorRampPalette(colors = 
                     c('#00007f', '#0000fe', '#0160ff', '#01d0ff', '#49ffad', 
                       '#a4ff53', '#fbec00', '#ff8500', '#ff1e00', '#7f0000'))

## Read in from climate viz (https://www.metoffice.gov.uk/hadobs/climviz/climpal/)

pal <- read_csv("~/Desktop/R/Palettes/terrain.txt", col_names = FALSE) %>% pull(X1)
pal <- read_csv("~/Desktop/R/Palettes/seismic.txt", col_names = FALSE) %>% pull(X1)
pal <- read_csv("~/Desktop/R/Palettes/cool.txt", col_names = FALSE) %>% pull(X1)
pal <- read_csv("~/Desktop/R/Palettes/hot.txt", col_names = FALSE) %>% pull(X1)
pal <- read_csv("~/Desktop/R/Palettes/muted.txt", col_names = FALSE) %>% pull(X1)
pal <- read_csv("~/Desktop/R/Palettes/orred.txt", col_names = FALSE) %>% pull(X1)

## Topo

pal <- topo.colors(10)

## Heat

pal <- heat.colors(10)

## Politico
# with white

fun <- colorRampPalette(c('#F05154', '#FAFBFB', '#62ACC9'))
fun(10)

# with purple

fun <- colorRampPalette(c('#F05154', '#62ACC9', '#7B6576'))
fun(5)
