# R-Primer 2 - Tidyverse

## ---------------------------------------------------------------------------------------------------
library(tidyverse)
library(palmerpenguins)


## ---------------------------------------------------------------------------------------------------
tidyverse_packages()

summarise(group_by(filter(mpg, manufacturer=="audi"), model), hwy_mean = mean(hwy))


mpg %>% filter(manufacturer=="audi") %>% group_by(model) %>% summarise(hwy_mean = mean(hwy))


mpg %>%
   filter(manufacturer=="audi") %>%
   group_by(model) %>%
   summarise(hwy_mean = mean(hwy))


## ---------------------------------------------------------------------------------------------------
penguins <- penguins
names(penguins)

## EX2 ---------------------------------------------------------------------------------------------------
dplyr::select(penguins, species, island, year, body_mass_g, sex)

penguins %>%
  select(species : sex)


## EX3 ----------------------------------------------------------------------------------------

p_new <- penguins%>%
  dplyr::filter(year == 2007 | year == 2009)


## EX4 ----------------------------------------------------------------------------
p_new <- penguins %>%
  dplyr::filter(year == 2009 & species == "Chinstrap") %>%
  dplyr::select(species, sex, year)


## ---------------------------------------------------------------------------------------------------
p_new %>%
  dplyr::mutate(body_mass_g = body_mass_g/453.6)

## ---------------------------------------------------------------------------------------------------
# compare this output with the one below
penguins %>%
  dplyr::summarize(heaviest_penguin = max(body_mass_g, na.rm = T)) 

## ---------------------------------------------------------------------------------------------------
penguins %>%
  dplyr::group_by(species) %>%
  dplyr::summarize(heaviest_penguin = max(body_mass_g, na.rm = T))


## EX5 --------------------------------------------------------------------------------------
penguins %>%
  dplyr::group_by(species, year) %>%
  dplyr::summarize(lightest_penguin = min(body_mass_g, na.rm = T))


## EX6-------------------------------------------------------------------------------------------
penguins %>%
  dplyr::filter(island == "Dream") %>%
  dplyr::arrange(desc(body_mass_g))


## Quiz
penguins %>%
  dplyr::filter(species == "Gentoo" & year == 2008)%>%
  dplyr::arrange(desc(bill_length_mm))

penguins %>%
  dplyr::filter(species == "Gentoo" & year == 2008)%>%
  dplyr::arrange(bill_length_mm)


############################################################################################

## -----------------------------------------------------------------------------------------
library(nycflights13)
names(flights)
names(planes)


## ---------------------------------------------------------------------------------------------------
left_join(flights, planes) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum, type, model)%>%
  head(3) ## Just to save vertical space in output


## ---------------------------------------------------------------------------------------------------
left_join(
  flights,
  planes %>% rename(year_built = year), ## Not necessary w/ below line, but helpful
  by = "tailnum" ## Be specific about the joining column
  ) %>%
  select(year, month, day, dep_time, arr_time, carrier, flight, tailnum, year_built, type, model) %>%
  head(3) 


## ----join3------------------------------------------------------------------------------------------
left_join(
  flights,
  planes, ## Not renaming "year" to "year_built" this time
  by = "tailnum"
  ) %>%
  select(contains("year"), month, day, dep_time, arr_time, carrier, flight, tailnum, type, model) %>%
  head(3)


## ---------------------------------------------------------------------------------------------------
stocks = data.frame( ## Could use "tibble" instead of "data.frame" if you prefer
  time = as.Date('2009-01-01') + 0:1,
  X = rnorm(2, 0, 1),
  Y = rnorm(2, 0, 2),
  Z = rnorm(2, 0, 4)
  )
stocks

stocks %>% pivot_longer(-time, names_to="stock", values_to="price")


## ---------------------------------------------------------------------------------------------------
## Write out the argument names this time: i.e. "names_to=" and "values_to="
tidy_stocks <- stocks %>% 
  pivot_longer(-time, names_to="stock", values_to="price")


## ----------------------------------------------------------------
tidy_stocks %>% pivot_wider(names_from=stock, values_from=price)
tidy_stocks %>% pivot_wider(names_from=time, values_from=price)


## ----sep1-------------------------------------------------------------------------------------------
economists = data.frame(name = c("Adam.Smith", "Paul.Samuelson", "Milton.Friedman"))
economists
economists %>% separate(name, c("first_name", "last_name"), sep = ".") 


## ----sep2-------------------------------------------------------------------------------------------
jobs = data.frame(
  name = c("Jack", "Jill"),
  occupation = c("Homemaker", "Philosopher, Philanthropist, Troublemaker") 
  ) 
jobs
## Now split out Jill's various occupations into different rows

jobs %>% separate_rows(occupation)


## ----unite1-----------------------------------------------------------------------------------------
gdp = data.frame(
  yr = rep(2016, times = 4),
  mnth = rep(1, times = 4),
  dy = 1:4,
  gdp = rnorm(4, mean = 100, sd = 2)
  )
gdp 
## Combine "yr", "mnth", and "dy" into one "date" column
gdp %>% unite(date, c("yr", "mnth", "dy"), sep = "-")


## ----unite2-----------------------------------------------------------------------------------------
gdp_u = gdp %>% unite(date, c("yr", "mnth", "dy"), sep = "-") %>% as_tibble()
gdp_u


## ----unite3-----------------------------------------------------------------------------
library(lubridate)
gdp_u %>% mutate(date = ymd(date))


#########################################################################################
library(ggplot2)
## ---------------------------------------------------------------------------------------------------
ggplot(penguins, aes(x = flipper_length_mm, y=body_mass_g)) +
  geom_point()


## ------------------------------------------------------------------------------------------------------
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density(bins = 60, colour = "red", fill = "blue")


## EX ------------------------------------------------------------------------------------------------------
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density(bins = 15, fill = "#FF6666")

ggplot(penguins, aes(x = body_mass_g)) +
  geom_bar(bins = 15, fill = "#FF6666")


## ---------------------------------------------------------------------------------------------------------------------
ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3, colour = "blue") +
  theme_minimal() +
  ggtitle("Plot")+
  labs(x = "Species",
       y = "Body mass (g)")


## ------------------------------------------------------------------------------------------------
ggplot(penguins, aes(x = flipper_length_mm, y=body_mass_g)) +
  geom_point()


ggplot(penguins, aes(x= flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point() + 
  geom_smooth(method = "lm", se = T) +
  theme_minimal() +
  labs(x = "Length of the flipper",
       y = "Body mass (g)",
       color = "Species")


####################################################################################################

## ---- results=FALSE---------------------------------------------------------------------------------
df <- data.frame(
  a = rnorm(100, 5, 2),
  b = rnorm(100, 100, 15),
  c = rnorm(100, 2, 1),
  d = rnorm(100, 36, 7)
)

df$a <- (df$a - mean(df$a, na.rm = TRUE)) / sd(df$a, na.rm = TRUE)
df$b <- (df$b - mean(df$b, na.rm = TRUE)) / sd(df$a, na.rm = TRUE) # spot the mistake?
df$c <- (df$c - mean(df$c, na.rm = TRUE)) / sd(df$c, na.rm = TRUE)
df$d <- (df$d - mean(df$d, na.rm = TRUE)) / sd(df$d, na.rm = TRUE)


## ---------------------------------------------------------------------------------------------------
zscale <- function(x){
  (x - mean(x, na.rm = T) / sd(x, na.rm = T))
}


## ---------------------------------------------------------------------------------------------------
zscale <- function(x){
  if (is.numeric(x)) {
  (x - mean(x, na.rm = T) / sd(x, na.rm = T))
  }
}


## ---- results=FALSE---------------------------------------------------------------------------------
df$a <- zscale(df$a)
df$b <- zscale(df$b)
df$c <- zscale(df$c)
df$d <- zscale(df$d)

# you can also use your function with a pipe!
df$d %>% zscale()


## ---- results=FALSE---------------------------------------------------------------------------------
# repetitive code
df$a <- zscale(df$a)
df$b <- zscale(df$b)
df$c <- zscale(df$c)
df$d <- zscale(df$d)

# equivalent iteration
for (i in seq_along(df)) {       # seq_along() similar to length()
  df[[i]] <- zscale(df[[i]])     # [[]] because we are working on single elements
}


## ---- results=FALSE---------------------------------------------------------------------------------
# repetitive code
mean(df$a)
mean(df$b)
mean(df$c)
mean(df$d)

# equivalent map function
map_dbl(df,mean)

# map function in tidyverse style
df %>% map_dbl(mean)

# map with zscale
map(df,zscale)

