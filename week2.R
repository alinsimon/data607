#Install package
install.packages("RMySQL")
#json config file
install.packages("jsonlite")
#Load Package
library(RMySQL)
library(jsonlite)
library(ggplot2)
# Read the JSON file
config <- fromJSON("C:/Users/msall/Documents/CUNY/Master of Science in Data Science/classes/data-607/RProgramming/data607/assignments/config.json")
#Connector
con <- dbConnect(RMySQL::MySQL(),
                 user = config$user,
                 password = config$password,
                 host = config$host,
                 port = config$port)
#Execute
dbSendQuery(con, "DROP DATABASE IF EXISTS movies")

dbSendQuery(con, "CREATE DATABASE movies")

dbSendQuery(con, "USE movies")

dbSendQuery(con, "CREATE TABLE `Popular_Movie` (
	`id` INT(10) UNSIGNED NOT NULL AUTO_INCREMENT,
	`movie_title` VARCHAR(255) NOT NULL COLLATE 'utf8mb4_general_ci',
	PRIMARY KEY (`id`)
)
COLLATE='utf8mb4_general_ci'
ENGINE=InnoDB")

dbSendQuery(con, "INSERT INTO `movies`.`Popular_Movie` (`movie_title`)  VALUES
('Dune: Part Two'),('The Marvels'), ('Killers of the Flower Moon'),('Mission: Impossible – Dead Reckoning Part Two'),
('The Last Voyage of the Demeter'), ('Poor Things');")

dbSendQuery(con, "CREATE TABLE `Rate_Scale` (
	`id` INT(10) UNSIGNED NOT NULL AUTO_INCREMENT,
  description VARCHAR(255),
	PRIMARY KEY (`id`) 
)
COLLATE='utf8mb4_general_ci'
ENGINE=InnoDB")

dbSendQuery(con, "INSERT INTO Rate_Scale (id, description) VALUES
(1, 'Very Poor'),(2, 'Poor'),(3, 'Average'),(4, 'Good'),(5, 'Excellent');")

dbSendQuery(con, "CREATE TABLE `film_critic` (
	`id` INT(10) NOT NULL AUTO_INCREMENT,
	`name` VARCHAR(50) NOT NULL COLLATE 'utf8mb4_general_ci',
	`comment` ENUM('friends','family','classmates','imaginary friend') NOT NULL DEFAULT 'imaginary friend' COLLATE 'utf8mb4_general_ci',
	PRIMARY KEY (`id`) USING BTREE
)
COLLATE='utf8mb4_general_ci'
ENGINE=InnoDB;")

dbSendQuery(con, "INSERT INTO `film_critic` (`name`) VALUES ('Juan'),('David'),('Michael'),('Chris'),('Sam');")

dbSendQuery(con, "DROP TABLE if EXISTS movie_rate;")

# FLOOR: round down to nearest integer
# RAND() * 5 : a range between 0 and 5 (not including 5), that is why +1 is added
dbSendQuery(con, "CREATE TABLE movie_rate
SELECT b.id AS movie_id,b.movie_title,c.id AS film_critic_id,c.name,c.comment,
 CASE WHEN RAND() < 0.15 THEN NULL ELSE FLOOR(1 + (RAND() * 5)) END AS rating FROM popular_movie AS b
JOIN film_critic AS c
ORDER BY b.movie_title,c.name;")

df_movies_rate <- dbGetQuery(con, "SELECT b.movie_id,b.movie_title,b.film_critic_id,b.name,b.`comment`,
IFNULL(b.rating,0) AS rating, w.description as rating_desc FROM movie_rate AS b
LEFT JOIN rate_scale AS w ON (b.rating=w.id)")

str(df_movies_rate)

#Overall Average Movie Rating#
means_df_movies_rate <- aggregate(rating ~  movie_title, df_movies_rate, mean)
#geom_boxplot
ggplot(df_movies_rate, aes(x=movie_title,y=rating)) +
  geom_boxplot(fill = "white", colour = "#ED7014",
               outlier.colour = "#F6BE00", outlier.shape = 1)+
  coord_flip()+
  labs(
    x = "Movies",
    y = "Rating",
    title = "Overall Average Movie Rating"
  )+
  theme_minimal()+
  stat_summary(fun=mean, colour="#F6BE00", geom="point", 
               shape=18, size=3, show.legend=FALSE) +
  geom_text(data = means_df_movies_rate,
            aes(label = round(rating,2), y = rating ,vjust=-0.7),
            colour="#ED7014")

#geom_histogram
ggplot(df_movies_rate, aes(x=rating)) + 
  geom_histogram(fill = "#ED7014")+
  labs(
    x = "Rates",
    y = "Number of Votes",
    title = paste("Number of Votes per Rates - Histogram")
  )+
  theme_minimal()


summarydf_movies_rate <- df_movies_rate |>
  group_by(rating,rating_desc) |>
  summarise( count_rates = n() )
summarydf_movies_rate


ggplot(data = summarydf_movies_rate, aes(x = rating, y = count_rates)) +
  geom_line(color="darkgreen")+
  geom_point(size =2)+
  geom_text(aes(label =count_rates ),fontface ="bold",nudge_x = 0.07, nudge_y = 0.1)+
  geom_label(aes(label = rating_desc))+
  xlab('Rates')+
  ylab('Number Of Movies')+
  ggtitle("Number of Movies By Rates") +
  theme_minimal()




#Disconnect
dbDisconnect(con)