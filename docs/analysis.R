library("ggplot2")
library("dplyr")
library("tidyverse")
library("openintro")
library("plotly")

# Read csv files
jurisdiction <- read_csv("../source/incarceration_trends_jail_jurisdiction.csv")
incarceration_trends <- read_csv("../source/incarceration_trends.csv")



# Trend over time chart
minority_in_jail <- jurisdiction %>%
  select(black_jail_pop, aapi_jail_pop, native_jail_pop,
         other_race_jail_pop, year, total_jail_pop, white_jail_pop) %>%
  group_by(year) %>%
  summarise(
    black_pop = sum(black_jail_pop, na.rm = T),
    white_pop = sum(white_jail_pop, na.rm = T),
    other_minor_race = sum(aapi_jail_pop, native_jail_pop, other_race_jail_pop, na.rm = T),
    total_pop = sum(total_jail_pop, na.rm = T)) %>%
  arrange(-year)
minority_in_jail_10 <- minority_in_jail[1:10,]
trend_map <- ggplot(data = minority_in_jail_10) +
  geom_line(mapping = aes(x = year, y = black_pop, color = "Black"), size = 3) +
  geom_line(mapping = aes(x = year, y = other_minor_race, color = "Other Race"), size = 3) + 
  geom_line(mapping = aes(x = year, y = white_pop, color = "White"), size = 3) +
  labs(x = "Year", y = "Population", title = "Population in Different Race in Prison",
       colour = "Race")



# Comparison chart
comparison <- jurisdiction %>%
  select(black_jail_pop, aapi_jail_pop, native_jail_pop,
         other_race_jail_pop, year, total_jail_pop, white_jail_pop) %>%
  group_by(year) %>%
  summarise(
    minor_race_pop = sum(aapi_jail_pop, native_jail_pop, 
                         other_race_jail_pop, na.rm = T),
    black_pop = sum(black_jail_pop, na.rm = T),
    white_pop = sum(white_jail_pop, na.rm = T))
white_pop <- comparison %>%
  filter(year == 2018) %>%
  pull(white_pop)
black_pop <- comparison %>%
  filter(year == 2018) %>%
  pull(black_pop)

difference <- toString(round(white_pop - black_pop, 0))

white_temp <- select(comparison, year, white_pop) %>%
  rename(pop = white_pop) %>%
  mutate(Race = "White")
black_temp <- select(comparison, year, black_pop) %>%
  rename(pop = black_pop) %>%
  mutate(Race = "Black")
graph_df <- rbind(white_temp, black_temp) %>%
  filter(year >= 2009) %>%
  arrange(-year)

comparison_graph <- ggplot(data = graph_df) +
  geom_col(mapping = aes(x = year, y = pop, fill = Race), position = "dodge") + 
  labs(colour = "Race", x = "Year", y = "Population", title = "Prison Population Distribution in Race from 2009 to 2018")



# Map chart
over_state <- incarceration_trends %>%
  select(year, state, total_jail_pop, black_jail_pop, aapi_jail_pop, native_jail_pop, other_race_jail_pop) %>%
  group_by(state) %>%
  filter(year >= 2009) %>%
  summarise(
    total_pop = sum(total_jail_pop, na.rm = T),
    minor_pop = sum(black_jail_pop, na.rm = T) +
                sum(aapi_jail_pop, na.rm = T) +
                sum(native_jail_pop, na.rm = T) +
                sum(other_race_jail_pop, na.rm = T)
  ) %>%
  mutate(proportion = round(minor_pop / total_pop * 100, 2))
over_state$proportion[is.na(over_state$proportion)] <- 0

state_shape <- map_data("state") %>%
  mutate(state = state2abbr(region)) %>%
  left_join(over_state, by = "state")
state_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = proportion),
    color = "white",
    size = 0.1 ) +
  coord_map() +
  scale_fill_continuous(low = "#132B43", high = "White") +
  labs(fill = "Proportion", title = "Minor Race Proportion in Jail from 2009 to 2018")



# Summary data
overall_black_prop <- round(sum(minority_in_jail$black_pop, na.rm = T) / sum(minority_in_jail$total_pop, na.rm = T) * 100, 2)
overall_black_prop <- paste(overall_black_prop, "%", sep = "")

overall_white_prop <- round(sum(minority_in_jail$white_pop, na.rm = T) / sum(minority_in_jail$total_pop, na.rm = T) * 100, 2)
overall_white_prop <- paste(overall_white_prop, "%", sep = "")

overall_other_prop <- round(sum(minority_in_jail$other_minor_race, na.rm = T) / sum(minority_in_jail$total_pop, na.rm = T) * 100, 2)
overall_other_prop <- paste(overall_other_prop, "%", sep = "")

black_in_jail_2018 <- filter(minority_in_jail, year == 2018) %>%
  pull(black_pop)
white_in_jail_2018 <- filter(minority_in_jail, year == 2018) %>%
  pull(white_pop)
total_in_jail_2018 <- filter(minority_in_jail, year == 2018) %>%
  pull(total_pop)

black_2018 <- paste(round(black_in_jail_2018 / total_in_jail_2018 * 100, 2), "%", sep = "")
white_2018 <- paste(round(white_in_jail_2018 / total_in_jail_2018 * 100, 2), "%", sep = "")

summary_data <- list(
  overall_black_prop,
  overall_white_prop,
  black_2018,
  white_2018,
  overall_other_prop
)
