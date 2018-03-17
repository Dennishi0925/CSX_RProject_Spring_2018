library(tidyverse)
library(nycflights13)
rm(list = ls())

#7.5.1.1 Exercises of R4DS
#Use what you¡¦ve learned to improve the visualisation of the
#departure times of cancelled vs. non-cancelled flights.

data(flights)
str(flights)
summary(flights$arr_time)

# arr_time = NA means that the flight was cancelled
# add cancelled column to identify
flights_cancellation <- flights %>%
  mutate(cancelled = is.na(arr_time))

# draw the plot00
flights_cancellation %>%
  ggplot(aes(x = cancelled)) +
  geom_bar()

# draw the plot01
flights_cancellation %>%
  ggplot(aes(x = dep_time)) +
  geom_freqpoly(aes(col = cancelled), binwidth = 50)

# draw the plot02
flights_cancellation %>%
  ggplot(aes(x = dep_time)) +
  geom_density(aes(col = cancelled), bw = 40) 

# draw the plot03
flights_cancellation %>%
  ggplot(aes(x = dep_time)) +
  geom_freqpoly(binwidth = 50) +
  facet_grid( . ~ cancelled)

# draw the plot04
flights_cancellation %>%
  ggplot(aes(x = dep_time)) +
  geom_density(bw = 40) +
  facet_grid( . ~ cancelled)

# draw the plot05
flights_cancellation %>%
  ggplot(aes(x = cancelled, y = dep_time)) +
  geom_boxplot() 
