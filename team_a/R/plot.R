library(tidyverse)
library(janitor)

##

library(readxl)

##

national_ages <-
  bind_rows(read_xls("data/aging.xls", skip = 5, sheet = 2) %>%
              clean_names() %>%
              filter(area == "England" & age_group != "All ages") %>%
              separate(age_group, sep = "-", into = c("lower", "upper")) %>%
              replace_na(list("upper" = 94)) %>%
              mutate(upper = as.numeric(upper)) %>%
              select(upper, x2016:x2041) %>%
              gather(year, population, x2016:x2036) %>%
              mutate(population = population * -1, group = "male"),
            read_xls("data/aging.xls", skip = 5, sheet = 3) %>%
              clean_names() %>%
              filter(area == "England" & age_group != "All ages") %>%
              separate(age_group, sep = "-", into = c("lower", "upper")) %>%
              replace_na(list("upper" = 94)) %>%
              mutate(upper = as.numeric(upper)) %>%
              select(upper, x2016:x2041) %>%
              gather(year, population, x2016:x2036) %>%
              mutate(group = "female"))

##

theme_pop <- function () {
  theme_minimal() +
    theme(plot.background = element_rect(fill = 'transparent', colour = 'transparent'),
          panel.grid.major.y = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.major.x = element_line(size = 0.1, colour = 'grey50'),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
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

##

library(gganimate)

##

groups <- 
  read_xls("data/aging.xls", skip = 5, sheet = 2) %>%
  clean_names() %>%
  filter(area == "England" & age_group != "All ages") %>%
  separate(age_group, sep = "-", into = c("lower", "upper"), remove = FALSE) %>%
  replace_na(list("upper" = 94)) %>%
  mutate(upper = as.numeric(upper)) %>%
  select(age_group, lower, upper) %>%
  group_by(age_group) %>%
  slice(1)

##

national_aging <- 
  ggplot(national_ages %>%
           left_join(groups) %>%
           mutate(year = str_replace_all(year, pattern = "x", replacement = ""))) +
  geom_bar(aes(y = population, x = upper, fill = group), stat = 'identity',
           show.legend = FALSE) +
  geom_text(aes(x = upper, y = 0, label = age_group), colour = '#ffffff') +
  scale_fill_manual(values = c(scico(palette = "grayC", 10)[3], scico(palette = "grayC", 10)[10])) +
  scale_y_continuous(labels = abs, limits = max(national_ages$population, na.rm = TRUE) * c(-1, 1)) +
  scale_x_continuous(labels = c("", "", ""), breaks = c(25, 50, 75), limits = c(-1, 100)) +
  transition_states(year) +
  ease_aes('linear') +
  labs(title = "national projections {closest_state}",
       subtitle = "PROJECTED POPULATION BY AGE",
       x = "age bracket", y = "population") +
  coord_flip() +
  theme_pop()

##

anim_save(national_aging, filename = "aging.gif")

##

local_ages <- 
  read_xls("data/aging.xls", skip = 5, sheet = 4) %>%
  clean_names() %>%
  filter(area != "England" & age_group != "All ages") %>%
  separate(age_group, sep = "-", into = c("lower", "upper")) %>%
  replace_na(list("upper" = 94)) %>%
  mutate(age = as.numeric(upper)) %>%
  select(code, age, x2016:x2041) %>%
  gather(year, population, x2016:x2041)

##

library(sf)

##

authorities <-
  st_read("data/authorities_WGS84.geojson") %>%
  clean_names() %>%
  rename(code = lad17cd) %>%
  select(code, st_areashape, st_lengthshape)

##

local_ages %>%
  group_by(code) %>%
  summarise(age = weighted.mean(age, population)) %>%
  left_join(authorities) %>%
  drop_na() %>%
  st_as_sf() %>%
  select(age) %>%
  plot()

##

expectancy <- 
  read_xls("data/healthy.xls", skip = 10, sheet = 1) %>%
  clean_names()

names(expectancy)

##

expectancy_spatially <- 
  expectancy %>%
  left_join(authorities) %>%
  drop_na() %>%
  st_as_sf()

##

background <- 
  authorities %>%
  mutate(dissolve = 1) %>%
  group_by(dissolve) %>%
  summarise()

##

library(scico)

##

ggplot(data =
         expectancy_spatially %>%
         select(healthy_life_expectancy_for_females_2009_2013_years,
                healthy_life_expectancy_for_males_2009_2013) %>%
         gather(variable, value, healthy_life_expectancy_for_females_2009_2013_years:healthy_life_expectancy_for_males_2009_2013) %>%
         mutate(name = case_when(variable == "healthy_life_expectancy_for_males_2009_2013" ~ "male",
                                 variable == "healthy_life_expectancy_for_females_2009_2013_years" ~ "female")) %>%
         select(variable, name, value, geometry)) +
  geom_sf(data = background,
          aes(), 
          fill = 'grey70', colour = NA, size = 0) +
  geom_sf(aes(fill = value), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'lajolla', direction = -1,
                   guide = guide_continuous) +
  facet_wrap(~ name) +
  theme_map()

##

map_differences <- 
  ggplot(data =
         expectancy_spatially %>%
         mutate(difference_male = life_expectancy_at_birth_for_males_2009_2013 - healthy_life_expectancy_for_males_2009_2013,
                difference_female = life_expectancy_at_birth_for_females_2009_2013 - healthy_life_expectancy_for_females_2009_2013_years) %>%
         select(difference_female, difference_male) %>%
         gather(variable, value, difference_male:difference_female) %>%
         mutate(name = case_when(variable == "difference_male" ~ "male",
                                 variable == "difference_female" ~ "female")) %>%
         select(variable, name, value, geometry)) +
  geom_sf(data = background,
          aes(), 
          fill = 'grey70', colour = NA, size = 0) +
  geom_sf(aes(fill = value), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'lajolla', direction = 1,
                   guide = guide_continuous) +
  facet_wrap(~ name) +
  labs(title = "local authorities",
       subtitle = "DIFFERENCES WITH HEALTHSPAN AND LIFESPAN") +
  theme_map()

##

ggsave(map_differences, filename = "differences.png", height = 8, width = 8, dpi = 300)

##

names(expectancy)

## 

library(magrittr)

##

lm(healthy_life_expectancy_for_females_2009_2013_years ~ life_expectancy_at_birth_for_females_2009_2013, 
   data = expectancy) %>%
  summary() %>%
  use_series("r.squared")

lm(healthy_life_expectancy_for_males_2009_2013 ~ life_expectancy_at_birth_for_males_2009_2013, 
   data = expectancy) %>%
  summary() %>%
  use_series("r.squared")

##

plot_expectancies <- 
  ggplot(bind_rows(select(expectancy, 
                          healthy_life_expectancy_for_females_2009_2013_years,
                          life_expectancy_at_birth_for_females_2009_2013) %>%
                     set_names(c("in good health", "at birth")) %>%
                     mutate(class = "female"),
                   select(expectancy, healthy_life_expectancy_for_males_2009_2013,
                          life_expectancy_at_birth_for_males_2009_2013) %>%
                     set_names(c("in good health", "at birth")) %>%
                     mutate(class = "male")), 
         aes(x = `at birth`, y = `in good health`, colour = class)) +
  geom_point(alpha = 0.5, show.legend = FALSE) +
  geom_smooth(method = lm, se = FALSE, fullrange = TRUE, linetype = 2, show.legend = FALSE) +
  geom_text(aes(x = 75, y = 70, label = "r-squared = 0.88"), hjust = 0) +
  geom_text(aes(x = 80, y = 50, label = "r-squared = 0.73"), hjust = 0) +
  scale_colour_manual(values = c(scico(palette = "grayC", 10)[3], scico(palette = "grayC", 10)[10])) +
  labs(title = "local authorities",
       subtitle = "TWIN LIFE EXPECTANCIES") +
  theme_ver()

##

ggsave(plot_expectancies, filename = "expectancies.png", height = 6, width = 6, dpi = 300)

##

scico_palette_show()

##

scico(palette = "grayC", 10)
scico(palette = "lajolla", 10)

##

expectancy$code[1]

area <- read_csv("data/ahahinputs.csv")

glimpse(area)

##

crosswalk <- 
  st_read("data/lsoa.geojson") %>%
  st_drop_geometry() %>%
  transmute(lsoa11 = lsoa01cd,
            code = lad17cd) %>%
  left_join(area) %>%
  select(lsoa11:green900) %>%
  group_by(code) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 

##

area_spatially <- 
  crosswalk %>%
  left_join(authorities) %>%
  st_as_sf()

##

map_green <- 
  ggplot(data =
           area_spatially) +
  geom_sf(data = background,
          aes(), 
          fill = 'grey70', colour = NA, size = 0) +
  geom_sf(aes(fill = green900), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'lajolla', direction = -1,
                   guide = guide_continuous) +
  labs(title = "local authorities",
       subtitle = "DIFFERENCES IN NATURAL AREAS") +
  theme_map()

##

ggsave(map_green, filename = "green.png", height = 8, width = 8, dpi = 300)  

##

library(geogrid)
library(sf)

##

par(mfrow = c(2, 3), mar = c(0, 0, 2, 0))

for (i in 1:6) {
  new_cells <- calculate_grid(shape = as(authorities, 'Spatial'), grid_type = "hexagonal", seed = i)
  plot(new_cells, main = paste("Seed", i, sep = " "))
}

##

hexdata <- 
  authorities %>%
  filter(code %in% expectancy$code) %>%
  as('Spatial')

new_cells_hex <- calculate_grid(shape = hexdata, grid_type = "hexagonal", seed = 1)
resulthex <- assign_polygons(hexdata, new_cells_hex)

##

resulthex %>% 
  st_as_sf() %>% 
  select(code) %>%
  st_write("hexgrid_britain.geojson")

##

summary_variables <- 
  resulthex %>%
  st_as_sf() %>%
  select(code) %>%
  left_join(crosswalk) %>%
  st_as_sf()

##

hexground <-
  st_read("data/hexgrid.geojson") %>%
  st_as_sf() %>%
  mutate(dissolve = 1) %>%
  group_by(dissolve) %>%
  summarise() %>%
  st_buffer(-0.5)
  
?st_buffer

##

library(scales)

##

map_summary <-
  ggplot(data = 
           summary_variables %>%
           filter(code %in% expectancy$code) %>%
           transmute(`general practicioners` = gpp_d, 
                     `hospitals` = ed_d, 
                     `dentists` = dent_d, 
                     `pharmacies` = pharm_d) %>%
           gather(variable, value, `general practicioners`:`pharmacies`) %>%
           group_by(variable)) +
  geom_sf(data = hexground,
          aes(), 
          fill = NA, colour = '#000000', size = 0.25, alpha = 0.5) +
  geom_sf(aes(fill = value), 
          colour = NA, size = 0) +
  scale_fill_scico(palette = 'lajolla', direction = 1,
                   guide = guide_continuous,
                   name = "distance to...",
                   limits = c(0, 10), oob = squish) +
  facet_wrap(~ variable) +
  labs(title = "local authorities",
       subtitle = "PREDICTORS OF ILL HEALTH") +
  theme_map()

##

ggsave(map_summary, filename = "summary.png", height = 8, width = 8, dpi = 300)

##

data <- 
  expectancy %>%
  mutate(difference_male = life_expectancy_at_birth_for_males_2009_2013 - healthy_life_expectancy_for_males_2009_2013,
         difference_female = life_expectancy_at_birth_for_females_2009_2013 - healthy_life_expectancy_for_females_2009_2013_years) %>%
  left_join(area_spatially)

rururb <- 
  read_csv("data/rururb.csv") %>%
  clean_names() %>%
  rename(code = lad11cd) %>%
  select(code, total_rural_population_2011:total_population_2011, ruc11, broad_ruc11) %>%
  select()
  rename(class = ruc11,
         class_broad = broad_ruc11)

data <- 
  data %>%
  left_join(rururb)

deaths <- read_csv("data/deaths.csv") %>%
  clean_names() %>%
  rename(code = lad11cd) %>%
  select(code, deaths_num:agestand_mortality)
  
data <-
  data %>%
  left_join(deaths)

income <- 
  read_xlsx("data/income.xlsx", sheet = 2, skip = 2) %>%
  clean_names() %>%
  rename(code = lau1_code) %>%
  select(code, region, x1998:x2016)

names(income) <- str_replace_all(names(income), pattern = "x", replacement = "income_")

data <-
  data %>%
  left_join(income) %>%
  select(code, region, everything())

##

data %>%
  st_as_sf() %>%
  st_write("data.shp")

data %>%
  select(-geometry) %>%
  write_csv("data.csv")

##

correlations <-
  data %>%
  transmute(`male (diff)` = difference_male,
            `female (diff)` = difference_female,
            `mortality` = agestand_mortality,
            `fast food (d)` = ffood_d,
            `pubs (d)` = pubs2_d,
            `GPs (d)` = gpp_d,
            `population` = total_population_2011,
            `density` = total_population_2011 / (st_areashape / (1000 * 1000)),
            `income` = income_2011,
            `rur-urban` = urban_city_and_town_population_2011 / total_population_2011,
            `living alone` = older_people_living_alone,
            `unemployment` = unemployment,
            `NO2` = no2,
            `PM10` = pm10, 
            `parks` = green900) %>%
  drop_na()

## 

mat <- round(cor(correlations), 2)

##

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

##

upper_tri <- get_upper_tri(mat)
melted_mat <- reshape2::melt(upper_tri, na.rm = TRUE)

##

theme_cor <- function() {
  theme_minimal() +
    theme(legend.text = element_text(angle = 90),
          legend.title = element_text(angle = 90),
          axis.text.x = element_text(angle = 90, vjust = 1, 
                                     size = 8, hjust = 1),
          axis.text.y = element_text(angle = 0, vjust = 1,
                                     size = 8, hjust = 1),
          axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          panel.grid.major = element_line(size = 0.25), 
          panel.grid.minor = element_line(size = 0.25), 
          legend.position = c(0.25, 0.75),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank())
}

##

ggheatmap <- 
  ggplot(data = na.omit(melted_mat), aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_colour_scico(palette = 'cork', direction = -1,
                     guide = 'none') +
  scale_fill_scico(palette = 'cork',
                   limit = c(-1,1), 
                   name = "pearson\ncorrelation",
                   guide = guide_colorbar(direction = "vertical",
                                          barheight = unit(50, units = "mm"),
                                          barwidth = unit(2, units = "mm"),
                                          draw.ulim = FALSE,
                                          title.position = 'left',
                                          label.position = 'right',
                                          title.hjust = 0.5,
                                          label.hjust = 0.5)) +
  theme_cor() +
  coord_fixed()

ggheatmap +
  geom_text(aes(Var2, Var1, label = value, colour = value), size = 3) 

##

ggsave(filename = "corplot.png", height = 10, width = 10, dpi = 300)

##

data_spatially <-
  data %>%
  st_as_sf()

##

co2 <- 
  read_csv("data/mapcoamean2010.csv", skip = 5) %>%
  set_names("gridcode", "x", "y", "value") %>%
  mutate(variable = "co2") %>%
  filter(value != "MISSING")


no2 <-
  read_csv("data/mapno22011.csv", skip = 5) %>%
  set_names("gridcode", "x", "y", "value") %>%
  mutate(variable = "no2") %>%
  filter(value != "MISSING")

so2 <-
  read_csv("data/mapso211ann.csv", skip = 5) %>%
  set_names("gridcode", "x", "y", "value") %>%
  mutate(variable = "so2") %>%
  filter(value != "MISSING")

pm10 <-
  read_csv("data/mappm102011g.csv", skip = 5) %>%
  set_names("gridcode", "x", "y", "value") %>%
  mutate(variable = "pm10") %>%
  filter(value != "MISSING")

pm25 <-
  read_csv("data/mappm252011g.csv", skip = 5) %>%
  set_names("gridcode", "x", "y", "value") %>%
  mutate(variable = "pm25") %>%
  filter(value != "MISSING")

ozone <-
  read_csv("data/mapdgt120_11.csv", skip = 5) %>%
  set_names("gridcode", "x", "y", "value") %>%
  mutate(variable = "ozone") %>%
  filter(value != "MISSING")

pollution <-
  bind_rows(no2, so2, co2, pm10, pm25, ozone) %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE) %>%
  st_set_crs(4326) %>%
  st_transform(4326)

st_read("data/authorities_WGS84.geojson") %>%
  as_tibble()

plot(data_spatially[, 4])
plot(pollution[2000:3000, 3], add = TRUE)

test <- 
  ggplot() +
  geom_sf(data = data_spatially,
          aes(fill = agestand_mortality), colour = NA, size = 0, show.legend = FALSE) +
  geom_sf(data= filter(pollution, variable == "no2"),
          aes(colour = value), fill = NA, size = 0.01, alpha = 0.5, show.legend = FALSE)

pollution_joined <- 
  pollution %>%
  st_join(data_spatially) %>%
  st_drop_geometry() %>%
  drop_na() %>%
  group_by(code, variable) %>%
  summarise(difference = ((difference_male + difference_female) / 2),
            pollution = mean(value, na.rm = TRUE))


##

library(maptools)
library(rgdal)
library(spdep)
library(tidyverse)
library(gridExtra)

#coordintates

left <- 
  data_spatially %>%
  mutate(difference = ((difference_male + difference_female) / 2))



coords <- 
  left %>%
  st_centroid() %>%
  st_coordinates()

nearest <- knn2nb(knearneigh(coords, 5))
weights <- nb2listw(nearest, style = "W")
moransi <- as_tibble(localmoran(left$difference, weights))

left <- 
  bind_cols(left, moransi) %>%
  rename(locali = Ii,
         expectation = E.Ii,
         variance = Var.Ii,
         deviation = Z.Ii,
         p_value = `Pr(z > 0)`)

scico_palette_show()

##

theme_map_legend <- function () {
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
          legend.position = 'bottom',
          legend.title = element_text(face = 'bold', colour = 'grey50'),
          legend.text = element_text(colour = 'black'),
          plot.margin = margin(5, 5, 5, 5)
    )
  
}

##

guide_discrete <-
  guide_legend(direction = "horizontal",
               keyheight = unit(2, units = "mm"),
               keywidth = unit(10, units = "mm"),
               title.position = 'top',
               label.position = 'bottom',
               title.hjust = 0.5,
               label.hjust = 1,
               nrow = 1,
               byrow = TRUE)

##

map_moran_difference <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = left,
          aes(fill = factor(ntile(difference, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 5),
                    labels = as.character(quantile(left$difference,
                                                   c(.1,.2,.4,.6,.8),na.rm = TRUE)),
                    name = "difference",
                    guide = guide_discrete) +
  labs(title = "difference") +
  theme_map_legend()

##

map_moran_i <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = left,
          aes(fill = factor(ntile(locali, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 5),
                    labels = str_sub(as.character(quantile(left$locali,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "i value",
                    guide = guide_discrete) +
  labs(title = "local moran's i") +
  theme_map_legend()

##

map_moran_p <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = left,
          aes(fill = factor(ntile(p_value, 5))), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 5),
                    labels = str_sub(as.character(quantile(left$p_value,
                                                   c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "p value",
                    guide = guide_discrete) +
  labs(title = "p value") +
  theme_map_legend()  

##

moran <- grid.arrange(map_moran_difference, map_moran_i, map_moran_p, ncol = 3)

ggsave(moran, filename = "matrix.png", height = 6, width = 12, dpi = 300)

##

left_moran <-
  left %>%
  mutate(scaled_difference = scale(difference)) %>%
  select(difference, scaled_difference, locali, expectation, variance, deviation, p_value)

left_moran$lagged_differenced<- lag.listw(weights, left_moran$scaled_difference)

left_moran$quad_sig <- NA

##

left_moran <-
  left_moran %>%
  mutate(quad_sig = case_when(scaled_difference >= 0 & lagged_differenced >= 0 & p_value <= 0.05 ~ 1,
                              scaled_difference <= 0 & lagged_differenced <= 0 & p_value <= 0.05 ~ 2,
                              scaled_difference >= 0 & lagged_differenced <= 0 & p_value <= 0.05 ~ 3,
                              scaled_difference >= 0 & lagged_differenced <= 0 & p_value <= 0.05 ~ 4,
                              scaled_difference <= 0 & lagged_differenced >= 0 & p_value <= 0.05 ~ 5)) %>%
  st_as_sf()

##

labels <- c("high-high", "low-low")

##

map_quads <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = left_moran,
          aes(fill = factor(quad_sig)), size = 0, colour = NA) +
  scale_fill_manual(values = scico(palette = 'cork', 2),
                    name = "quadrants",
                    labels = labels,
                    guide = guide_discrete,
                    na.translate = FALSE) +
  labs(title = "local moran's i") +
  theme_map_legend()  

##

ggsave(map_quads, filename = "quadrants.png", height = 8, width = 8, dpi = 300)

##

montecarlo <- moran.mc(left$difference, weights, nsim = 999)
montecarlo 

##

ggplot(as.data.frame(montecarlo$res), aes(montecarlo$res)) + 
  geom_histogram(binwidth = 0.01) +
  geom_vline(aes(xintercept = 0.466), colour = "grey70",size = 1) +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(title = "observed and permuted Moran's I",
       subtitle = "I = 0.466 | P < 0.01",
       x = "results",
       y = "count") +
  theme_ver()

ggsave(filename = "montecarlo.png", height = 4, width = 6, dpi = 300)

##

results <- read_csv("data/gwrresults.csv")

dim(results)
dim(data_spatially)

shape <- 
  data_spatially %>%
  filter(code != "E06000053") %>%
  select(code, geometry)

results_shape <-
  results %>%
  bind_cols(shape) %>%
  st_as_sf() %>%
  clean_names()

##

map_deprivation <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(depriv_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$depriv_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "deprivation") +
  theme_map_legend()  

map_air <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(airpol_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$airpol_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "air pollution") +
  theme_map_legend() 

map_lifestyle <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(lifest_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5, direction = -1),
                    labels = str_sub(as.character(quantile(results_shape$lifest_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "lifestyle distances") +
  theme_map_legend() 

map_unemployment <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(unempl_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$unempl_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "unemployment") +
  theme_map_legend()

map_health <- 
  ggplot() + 
  geom_sf(data = background,
          aes(), fill = 'grey70', colour = NA, size = 0) +
  geom_sf(data = results_shape,
          aes(fill = factor(ntile(hlthserv_coef, 5))), size = 0.01, colour = 'gray70') +
  scale_fill_manual(values = scico(palette = 'oslo', 5),
                    labels = str_sub(as.character(quantile(results_shape$hlthserv_coef,
                                                           c(.1,.2,.4,.6,.8),na.rm = TRUE)), 1, 4),
                    name = "coefficent",
                    guide = guide_discrete) +
  labs(title = "health services") +
  theme_map_legend()

##

results <- grid.arrange(map_lifestyle, map_health, map_unemployment, map_deprivation, ncol = 2)

ggsave(results, filename = "results.png", height = 12, width = 12, dpi = 300)

ggsave(map_air, filename = "pollution.png", height = 8, width = 8, dpi = 300)

##

regression <- 
  left %>%
  transmute(difference = difference,
            agestand_mortality = agestand_mortality,
            ffood_d = ffood_d,
            pubs2_d = pubs2_d,
            gpp_d = gpp_d,
            total_population_2011 = total_population_2011,
            density = total_population_2011 / (st_areashape / (1000 * 1000)),
            income = income_2011,
            rururban = urban_city_and_town_population_2011 / total_population_2011,
            lives_alone = older_people_living_alone,
            unemployment = unemployment,
            no2 = no2,
            pm10 = pm10, 
            parks = green900) %>%
  mutate_if(is.numeric, scale)
