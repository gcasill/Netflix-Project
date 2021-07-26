install.packages("stringr")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("htmlwidgets")
library(stringr)
library(tidyverse)
library(readr)
library(corrplot)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(htmlwidgets)
library(viridisLite)


netflix <- read.csv('netflix.csv')
net <- read.csv('net.csv')

# 1. Is there a correlation between runtime and country of origin of movies?

movies <- filter(net, type == "Movie")
movies$runtime <- gsub("min","", movies$duration)
as.numeric(movies$runtime)
movies_cleaned <- movies[order(movies$runtime),]


# bar graph
ggplot(movies_cleaned, aes(fill=runtime, y=runtime, x=country)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title="Netflix Movie Countries",
       x="Country", y="Runtime")

# scatter
ggplot(movies_cleaned,aes(x=country, y=runtime, color=country, size=duration)) +
  geom_point() +
  labs(title="Netflix Movie Origins",
       x="Country", y="Runtime") +
  theme(legend.position="none",
        axis.text.x = element_text( 
          size=6, angle=45),
        axis.text.y = element_text( 
          size=4.5))

# 2a. Which countries are the most (the top 10 countries) represented on Netflix? 

country_count <- count(net, country, sort = TRUE)
country_count_sort <- country_count [1:10,]
country_count_sort

ggplot(country_count_sort, aes(fill=country, y=n, x=country))+
  geom_bar(postion="dodge", stat="identity")+
  labs(title="Top Ten Most-Represented Countries on Netflix", x="Country", y="Amount of Media")+
  scale_fill_manual("country", values = c("Canada" = "red", "France" = "blue", "Germany" = "green", "India" = "orange1", "Japan" = "purple", "Mexico" = "darkgreen", "South Korea" = "brown4", "Spain" = "yellow", "United Kingdom" = "grey50", "United States" = "navy"))


# 2b. Of these countries, what is the split between serialized programming and movies?

country_count_type <- count(net, country, type, sort = TRUE)
country_count_split <- merge(country_count_sort, country_count_type, by = c("country"))
country_count_split

ggplot(country_count_split, aes(fill=country, y=n.y, x=type))+
  geom_bar(postion="dodge", stat="identity")+
  labs(title="Movies and TV Shows of the Top Ten", x="Type", y="Amount of Media")+ 
  scale_fill_manual("country", values = c("Canada" = "red", "France" = "blue", "Germany" = "green", "India" = "orange1", "Japan" = "purple", "Mexico" = "darkgreen", "South Korea" = "brown4", "Spain" = "yellow", "United Kingdom" = "grey50", "United States" = "navy"))

# 3. How many listings are international co-productions?

counts <- count(many, " , ")
counts


class_count <- dplyr::count(netflix, type, country)
class_count

ggplot(class_count, aes(x=type, y=n)) +
  geom_bar(stat = "identity") +
  labs(title="Netflix Movies and TV Shows",
       x="Type", y="Count")

# 3a
ggplot(net, aes(fill=type, y=country, x=type)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title="Netflix Movies and TV Shows",
       x="Type of Production", y="Country of Origin")

# 3. Do movies or tv shows get co-produced the most?

class_count <- dplyr::count(netflix, type, country)
class_count

tr <- class_count %>%
  filter_all(any_vars(grepl(',', .)))

ggplot(tr, aes(x=type, y=n)) +
  geom_bar(stat = "identity") +
  labs(title="Netflix Co-Produced Movies and TV Shows",
       x="Type", y="Count")


# 4a. How long do most shows run on Netflix?

runn <- filter(netflix, type == "TV Show")

Seasons <- ggplot(runn, aes(x=duration)) +
  geom_bar(fill="#69b3a2", color="#69b3a2") +
  labs(title="Netflix TV Show Seasons",
       x="Number of Seasons", y="Number of Shows")

# 4b. Is there a correlation between show seasons and genre? 

genre <- filter(net, type == "TV Show")

Genres <- ggplot(genre, aes(fill=duration, y=listed_in, x=duration)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title="Netflix TV Show Genres",
       x="Number of Seasons", y="Genres")

# 4c. Is there a correlation between show seasons and country of origin?

origin <- filter(net, type == "TV Show")

# bar graph
Countries <- ggplot(origin, aes(fill=duration, y=country, x=duration)) + 
  geom_bar(position="dodge", stat="identity") +
  labs(title="Netflix TV Show Countries",
       x="Number of Seasons", y="Country")

# scatter
Origins <- ggplot(origin,
       aes(x=duration,
           y=country,
           color=country,
           size=duration)) +
  geom_point() +
  labs(title="Netflix TV Show Origins",
       x="Number of Seasons", y="Country")


# 5. Whether or not Netflix titles showed up in top rated films/shows.

title <- c("Planet Earth II", "Planet Earth", "Band of Brothers", "Breaking Bad", "Chernobyl", "The Wire", "Blue Planet II", "Our Planet", "Cosmos: A Spacetime Odyssey", "Cosmos")
type <- c("TV Show", "TV Show", "TV Show", "TV Show", "TV Show", "TV Show", "TV Show", "TV Show", "TV Show", "TV Show")
release_year <- c(2016, 2006, 2001, 2008, 2019, 2002, 2017, 2019, 2014, 1980)
rated <- c(9.5, 9.4, 9.4, 9.4, 9.3, 9.3, 9.3, 9.3, 9.2, 9.2)
from <- c("IMDB", "IMDB", "IMDB", "IMDB", "IMDB", "IMDB", "IMDB", "IMDB", "IMDB", "IMDB")
imdb_show <- data.frame(title, type, release_year, rated, from)
view(imdb_show)

title <- c("The Shawshank Redemption", "The Godfather", "The Godfather: Part II", "The Dark Knight", "12 Angry Men", "Schindler's List", "The Lord of the Rings: The Return of the King", "Pulp Fiction", "The Good, the Bad and the Ugly", "The Lord of the Rings: The Fellowship of the Ring")
type <- c("Movie", "Movie", "Movie", "Movie", "Movie", "Movie", "Movie", "Movie", "Movie", "Movie")
release_year <- c(1994, 1972, 1974, 2008, 1957, 1993, 2003, 1994, 1966, 2001)
rated <- c(9.2, 9.1, 9.0, 9.0, 8.9, 8.9, 8.9, 8.8, 8.8, 8.8)
from <- c("IMDB", "IMDB", "IMDB", "IMDB", "IMDB", "IMDB", "IMDB", "IMDB", "IMDB", "IMDB")
imdb_movie <- data.frame(title, type, release_year, rated, from)
view(imdb_movie)

finaldt <- rbind(imdb_show, imdb_movie)

new <- merge(finaldt, netflix, by = c("title", "type", "release_year"))


