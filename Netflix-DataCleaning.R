# Web App to Big Data: How do GDP and Social Inequality Influence Netflix’s Growth?

setwd("C:/Users/Leo/Desktop/Portfolio/Netflix-R")
getwd()

# Packages used
library(dplyr)
library(tidyr)
library(readxl)
library(readr)


# LOADING THE DATASETS

# Loading Netflix Data
netflix_data <- read.csv("originals_datasets/netflix_data_dec_2021.csv")
View(netflix_data)

# Loading world bank data for GDP
gdp_data <- read.csv("originals_datasets/world_bank_data.csv", header = FALSE)
View(gdp_data)

# Loading pay inequality data offered by harvard
salary_data <- read.csv("originals_datasets/pay_inequality_data_harvard.csv")
View(salary_data)

# Loading IMDB data - user review for movies
IMDB_data <- read_tsv("originals_datasets/data_IMDB.tsv")
View(IMDB_data)

# Loading top 10 data netflix shows by country
top10_data <- read_excel("originals_datasets/top_10_shows_netflix.xlsx")
View(top10_data)

# Loading netflix subscriber data July/2021 - March/2022
sub_data <- read.csv("originals_datasets/netflix_subscribers_jul_2021.csv")
View(sub_data)

# Loading country ISO code data
countrycode <- read.csv("originals_datasets/wikipedia_iso_country_codes.csv")
View(countrycode)


# CLEANING AND PREPARING DATASETS

# CLEANING AND PREPARING FIRST DATASET

# Creates a column with the data difference for the bar chart (standard plan - basic plan)
netflix_data$basic_standard_diff = (netflix_data$Cost.Per.Month...Standard.... - netflix_data$Cost.Per.Month...Basic....)

# Creates a column with the data difference for the bar chart (premium plan - standard plan)
netflix_data$standard_premium_diff = (netflix_data$Cost.Per.Month...Premium.... - netflix_data$Cost.Per.Month...Standard....)

# Combines previous data with GDP data
names(gdp_data)[names(gdp_data) == 'V1'] <- 'Country'
netflix_gdp_data <- merge(netflix_data, gdp_data, by = "Country")

# Extracts 2020 GDP
netflix_2020gdp_data <- netflix_gdp_data[-c(11:72, 74, 75)]
names(netflix_2020gdp_data)[names(netflix_2020gdp_data) == 'V64'] <- "2020 GDP (World Bank)"

# Cleanup of salary inequality dataframe
# Removing NA values
salary_data <- salary_data[, c(1:3)]
year_salary_data <- salary_data %>% group_by(country) %>% summarise(max = max(year, na.rm = TRUE))

# Combine the dataframes
salary_data <- merge(salary_data, year_salary_data, by.x = c("country", "year"), by.y = c("country", "max"))
netflix_gdp_2020salary_data <- merge(netflix_2020gdp_data, salary_data, by.x = c("Country"), by.y = c("country"))

# Cleans the billing and subscription dataset and combines it with the previous dataframe
sub_data <- sub_data[,c(1, 23, 24)]
complete <- merge(netflix_gdp_2020salary_data, sub_data, by = c("Country"))

# faz o merge do countrycode para o choropleth map
countrycode <- countrycode[,c(1,3)]
complete <- merge(complete, countrycode, by.x = c("Country"), by.y = c("English.short.name.lower.case"))
View(complete)

# Saves the dataframe
write.csv(complete, "clean_datasets/dataset1.csv", row.names = FALSE)


# CLEANING AND PREPARING SECOND DATASET

# Cleans and filters IMDB dataframe
genre <- IMDB_data[,-c(1, 4:8)]
View(genre)
names(genre)[names(genre) == 'primaryTitle'] <- 'show_title'

# Associates genre with the top 10 shows
topgenre <- merge(top10_data, genre, by = "show_title")
View(topgenre)

# Cleans the previous dataframe to keep only 1 entry for each top 10
topgenre <- topgenre[(topgenre$category == "Films" & topgenre$titleType == "movie") | (topgenre$category == "TV" & topgenre$titleType == "tvSeries"), ] 
topgenre <- distinct(topgenre, show_title, week, country_name, category, titleType,cumulative_weeks_in_top_10, .keep_all= TRUE)
View(topgenre)

# Keep only film genre information by country
topgenrecountrys <- topgenre[, -c(1, 3:9)]
View(topgenrecountrys)

# Dataframe Pivot
topgenrecountrys <- separate(topgenrecountrys, c("genres") , c("genre1", "genre2", "genre3"), sep = ",")
topgenrecountrys <- pivot_longer(topgenrecountrys, c("genre1", "genre2", "genre3"), names_to = "genre123", values_to = "genres")
View(topgenrecountrys)

# Counts the number of genres
genrecount <- count(topgenrecountrys, country_name, genres)
genrecount <- na.omit(genrecount)
genrecount <- subset(genrecount, genres!= "\\N")
genrecount$n <- as.numeric(genrecount$n)
View(genrecount)

# Saves the dataframe
write.csv(genrecount, "clean_datasets/dataset2.csv", row.names = FALSE)


# CLEANING AND PREPARING THIRD DATASET

# Rename previous dataframe
sunburst <- rename(genrecount, label = country_name)

# Remove dashes
sunburst$genres = sub("-", " ", sunburst$genres)

# Adjust name
sunburst$parent = c("total - ")
sunburst$parent <- paste(sunburst$parent, sunburst$genres)
sunburst$id = c(" - ")
sunburst$id <- paste(sunburst$parent, sunburst$id)
sunburst$id <- paste(sunburst$id, sunburst$label)
sunburst$n <- as.numeric(sunburst$n)
View(sunburst)

# Agregation
added <- aggregate(sunburst$n, list(sunburst$genres), FUN = sum)
added <- rename(added, label = Group.1)
added <- rename(added, n = x)
added$n <- as.numeric(added$n)
added$genres <- c(NA)
added$parent <- c("total")
added$id <- c(" - ")
added$id <- paste(added$parent, added$id)
added$id <- paste(added$id, added$label)
View(added)

# Calculate sum
total = sum(added$n)
total

# Combine everything for final dataframe
sunburst <- rbind(added, sunburst)
sunburst <- rbind(c("total", total, NA, NA, "total"), sunburst)
sunburst <- sunburst[,-c(3)]
sunburst$n <- as.numeric(sunburst$n)
View(sunburst)

# Saves the dataframe
write.csv(sunburst, "clean_datasets/dataset3.csv", row.names = FALSE)


# CLEANING AND PREPARING FOURTH DATASET

# Work with top 10 to avoid performance problems
top10sunburst <- sunburst[-c(1:28),]
top10sunburst$n <- as.numeric(top10sunburst$n)
View(top10sunburst)

# top 10 genres by country
top10sunburst <- top10sunburst %>%
  group_by(label) %>%
  top_n(10,n)
View(top10sunburst)

# Recalculate totals, adjust and combine the dataframe
top10add <- aggregate(top10sunburst$n, list(top10sunburst$parent), FUN = sum)
top10add <- rename(top10add, id = Group.1)
top10add <- rename(top10add, n = x)
top10add$label = sub("total  -  ", "", top10add$id)
top10add$parent = c("total")
top10add$n <- as.numeric(top10add$n)
total = sum(top10add$n)
top10sunburst <- rbind(top10add, top10sunburst)
top10sunburst <- rbind(c("total", total, NA, NA, "total"), top10sunburst)
top10sunburst$n <- as.numeric(top10sunburst$n)
View(top10sunburst)

# Saves the dataframe
write.csv(top10sunburst, "clean_datasets/dataset4.csv", row.names = FALSE)


# CLEANING AND PREPARING FIFTH DATASET

# Filters the previous dataframe and creates a new one
nototal <- sunburst[-c(1),]
nototal$parent = sub("total - ", "", nototal$parent)
nototal$parent = sub("total", NA, nototal$parent)
nototal$id = sub("total - ", "", nototal$id)
View(nototal)

# Saves the dataframe
write.csv(top10sunburst, "clean_datasets/dataset5.csv", row.names = FALSE)


# CLEANING AND PREPARING SIXTH DATASET

# Filter previous dataframe and create a new one
countrytree <- nototal[-c(1:28),]
countrytree <- rename(countrytree, parents = label)
countrytree <- rename(countrytree, labels = parent)
countrytree$id = c(" - ")
countrytree$id <- paste(countrytree$parent, countrytree$id)
countrytree$id <- paste(countrytree$id, countrytree$label)
countries <- aggregate(countrytree$n, list(countrytree$parents), FUN = sum)
countries <- rename(countries, labels = Group.1)
countries <- rename(countries, n = x)
countries$n <- as.numeric(countries$n)
countries$id <- countries$label
countries$parents <- c(NA)
countrytree <- rbind(countrytree, countries)
View(countrytree)

# Saves the dataframe
write.csv(countrytree, "clean_datasets/dataset6.csv", row.names = FALSE)






