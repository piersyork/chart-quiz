library(owidR)
library(data.table)
library(ggplot2)
library(dplyr)

theme_set(theme_owid(import_fonts = FALSE) + theme(axis.title = element_blank()))

## famous charts:
# March on moscow
# Cholera cases
# gapminder, lifeExp vs income
# first tube map
# languages of the world: http://www.puffpuffproject.com/languages.html


# what are the four variables in the graph
life_exp_gdp <- owid("life-expectancy-vs-gdp-per-capita", rename = c("population", "continent", "life_exp", "gdp")) |>
  na.omit()

life_exp_gdp$entity

life_exp_gdp |>
  ggplot(aes(gdp, life_exp, size = population, colour = continent, label = entity)) +
  geom_point(show.legend = FALSE, shape = 1, stroke = 0.8) +
  scale_x_log10(labels = scales::label_dollar()) +
  ggsci::scale_colour_lancet() +
  scale_size_area(max_size = 40)

plotly::ggplotly()

owid_search("life expectancy")


battle <- owid("state-based-battle-related-deaths-per-100000-since-1946",
               rename = c("colonial", "interstate", "civil", "civil_intervention")) |>
  as.data.table()
battle[entity == "World"]


## UK inflation
owid_search("inflation")


## Political support


owid_search("trust")


## Royal instagram followers | source:https://www-statista-com.libproxy.ucl.ac.uk/study/25128/the-british-royal-family-uk-statista-dossier/
royal_followers <- data.table(family = c("The Royal Family (United Kingdom)",
                                         "Koninklijk Huis (the Netherlands)",
                                         "Kungahuset (Sweden)",
                                         "Det Norske Kongehuset (Norway)",
                                         "Belgian Royal Palace (Belgium)"),
                              followers = c(8700000,
                                            746000,
                                            482000,
                                            215000,
                                            80900))

royal_followers |>
  ggplot(aes(followers, forcats::fct_reorder(family, followers))) +
  geom_col(fill = "#631879") +
  scale_x_continuous(labels = scales::label_comma())



## UK covid cases
covid <- owid_covid() |>
  as.data.table()

covid[location == "United Kingdom", .(date, new_cases_smoothed)] |>
  readr::write_csv("data/uk_covid.csv")

## Physical activity: https://www-statista-com.libproxy.ucl.ac.uk/statistics/326672/physical-activity-levels-by-gender-in-england/
# create four graphs: which one is correct
# aerobic guidelines: At least 150 minutes moderately intensive activity (MPA) or 75 minutes vigorous activity (VPA) per week (pw) or an equivalent combination of these
activity_levels <- factor(c("Meets aerobic guidelines", "Some activity", "Low activity", "Inactive"),
                          levels = c("Meets aerobic guidelines", "Some activity", "Low activity", "Inactive"))

plot1 <- data.table(level = activity_levels, pct = c(0.23, 0.3, 0.04, 0.43)) |>
  ggplot(aes(level, pct)) +
  geom_col(fill = "#00468B") +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.7))

#actual
plot2 <- data.table(level = activity_levels, pct = c(0.62, 0.11, 0.04, 0.23)) |>
  ggplot(aes(level, pct)) +
  geom_col(fill = "#00468B")  +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.7))

plot3 <- data.table(level = activity_levels, pct = c(0.05, 0.3, 0.5, 0.15)) |>
  ggplot(aes(level, pct)) +
  geom_col(fill = "#00468B") +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.7))

plot4 <- data.table(level = activity_levels, pct = c(0.21, 0.6, 0.13, 0.06)) |>
  ggplot(aes(level, pct)) +
  geom_col(fill = "#00468B") +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 0.7))

cowplot::plot_grid(plot1, plot2, plot3, plot4, align = TRUE)

## Volunteers
# Source https://register-of-charities.charitycommission.gov.uk/sector-data/top-10-charities
charities <- stringr::str_to_title(c("PARKRUN GLOBAL LIMITED",
                                     "KEEP BRITAIN TIDY",
                                     "THE SCOUT ASSOCIATION",
                                     "THE GUIDE ASSOCIATION SOUTH-WEST ENGLAND",
                                     "NEIGHBOURHOOD WATCH NETWORK",
                                     "THE GUIDE ASSOCIATION",
                                     "EDUCATION & EMPLOYERS TASKFORCE",
                                     "THE CHURCH OF JESUS CHRIST OF LATTER-DAY SAINTS",
                                     "THE ROYAL BRITISH LEGION",
                                     "THE NATIONAL TRUST"))



data.table(charity = charities,
           volunteers = c(279710, 196260, 141659, 100000, 100000, 80000, 63608, 61000, 55000, 50000)) |>
  ggplot(aes(volunteers, forcats::fct_reorder(charity, volunteers))) +
  geom_col(fill = "#008280") +
  scale_x_continuous(labels = scales::label_comma(), limits = c(0, 300000))


## Public holidays
countries <- c("Serbia", "Argentina","Australia", "Austria", "Bangladesh", "Belgium", "Barba", "Brazil", "Bulgaria", "Cambodia", "Canada",
               "Chile", "Colombia", "Croatia", "Cyprus", "Czechia", "Dominican Republic", "Egypt", "Estonia", "Fiji", "Finland", "France",
               "Germany", "Greece", "Hong Kong", "Hungary", "India", "Indonesia", "Iran", "Ireland", "Italy", "Japan", "Kazakhstan", "Kosovo",
               "Latvia", "Lebanon", "Liechtenstein", "Lithuania", "Malaysia", "Mexico", "Mongolia", "Myan", "Nepal", "New Zealand", "Norway",
               "Pakistan", "Philippines", "Poland", "Portugal", "Romania", "Singapore", "Slovakia", "Slovenia", "South Africa", "South Korea",
               "Spain", "Sri Lanka", "Sweden", "Switzerland", "Taiwan", "Thailand", "Tanzania", "Trinidad and Tobago", "Turkey ",
               "the Netherlands", "United Kingdom", "United States", "Uruguay")

holidays <- c(13, 19, 10, 14, 22, 10, 12, 9, 12, 21, 8, 14, 18, 14, 13, 14, 12, 22, 10, 10, 13, 11, 10,
               12, 17, 12, 21, 16, 26, 10, 12, 16, 16, 11, 13, 19, 20, 15, 23, 8, 14, 32, 30, 11, 14,
               16, 18, 13, 13, 15, 11, 14, 14, 12, 14, 12, 25, 15, 7, 9, 16, 16, 18, 16, 11, 8, 11, 12)

lapply(list(countries, holidays), length)

world_map_data() |>
  left_join( tibble(country = countries,
                    holidays = holidays,
                    ISO_A3 = countrycode::countrycode(countries, "country.name", "iso3c"))) |>
  ggplot(aes(fill = holidays, id = country)) +
  geom_sf() +
  scale_fill_distiller(palette = "Greens", direction = 1) +
  ggplot2::theme(axis.line.x = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
                 panel.grid.major = ggplot2::element_blank(), axis.text = ggplot2::element_blank(),
                 legend.position = "bottom", legend.title = ggplot2::element_blank(),
                 legend.key.width = ggplot2::unit(2, units = "cm"),
                 legend.key.height = ggplot2::unit(0.3, units = "cm"),
                 plot.title = element_text(vjust = 1))


## 10 most popular newspapers
# popularity = percent of people with a positive opinion
# source: https://yougov.co.uk/ratings/media/popularity/newspaper/all

tibble(paper = c("The Guardian", "Metro", "The Times", "Daily Mail", "The Independent", "Financial Times",
                 "The Sun", "The Daily Telegraph", "The Daily Mirror", "The Mail on Sunday"),
       popularity = c(38, 37, 34, 33, 30, 30, 29, 29, 28, 28)) |>
  ggplot(aes(popularity, forcats::fct_reorder(paper, popularity))) +
  geom_col(fill = "#925E9F") +
  labs(title = "Most popular UK newspapers")

papers <- readxl::read_xlsx("data/popular_newspapers.xlsx", sheet = 2, range = "B6:C15", col_names = c("paper", "popularity"))

papers |>
  mutate(paper = if_else(paper == "The Times", "", paper)) |>
  ggplot(aes(popularity, forcats::fct_reorder(paper, popularity))) +
  geom_col(fill = "#925E9F") +
  labs(x = "Share of respondants having a positive opinion (Q4 2020)") +
  theme(axis.title.x = element_text())


## Music chart

## Film chart

imdb <- readr::read_csv("https://raw.githubusercontent.com/thechaudharysab/imdb-data-pandas-visualization/master/data/imdb_1000.csv") |>
  as.data.table()

imdb[order(-star_rating), head(.SD[, .(title, rating = star_rating)], 10)] |>
  ggplot(aes(rating, forcats::fct_reorder(title, rating))) +
  geom_col()

films_gross <- "
Rank 	Title 	Worldwide gross (2020 $)  Year
1 	Gone with the Wind 	$3,724,000,000 	1939
2 	Avatar 	A$3,273,000,000 	2009
3 	Titanic 	T$3,096,000,000 	1997
4 	Star Wars 	$3,059,000,000 	1977
5 	Avengers: Endgame 	AE$2,811,000,000 	2019
6 	The Sound of Music 	$2,562,000,000 	1965
7 	E.T. the Extra-Terrestrial 	$2,501,000,000 	1982
8 	The Ten Commandments 	$2,368,000,000 	1956
9 	Doctor Zhivago 	$2,244,000,000 	1965
10 	Star Wars: The Force Awakens 	$2,213,000,000 	2015
"

film_gross <- data.frame(
  stringsAsFactors = FALSE,
             title = c("","Avatar", # top = gone with the wind
                       "Titanic","Star Wars","Avengers: Endgame",
                       "The Sound of Music","E.T. the Extra-Terrestrial",
                       "The Ten Commandments","Doctor Zhivago","Star Wars: The Force Awakens"),
             gross = c(3724000000, 3273000000, 3096000000, 3059000000, 2811000000, 2562000000,
                       2501000000, 2368000000, 2244000000, 2213000000),
              Year = c(1939L,2009L,1997L,1977L,
                       2019L,1965L,1982L,1956L,1965L,2015L)
)

film_gross |>
  ggplot(aes(gross/1e6, forcats::fct_reorder(title, gross))) +
  geom_col(fill = "#DC5E78") +
  theme(panel.grid.major.x = element_blank(),
        axis.title.x = element_text()) +
  labs(x = "Highest grossing films adjusted for inflation")





