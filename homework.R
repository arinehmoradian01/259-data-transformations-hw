#PSYC 259 Homework 2 - Data Transformation- Arineh Moradian
#For full credit, provide answers for at least 7/10

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

# loading the package necessary

library(tidyverse)

# loading data 

ds <- read_csv("data_raw/rolling_stone_500.csv")




### Question 1 ---------- 

#Use glimpse to check the type of "Year". 
#Then, convert it to a numeric, saving it back to 'ds'
#Use typeof to check that your conversion succeeded

#ANSWER

# check the type of year
glimpse(ds)

# converting back to numeric

ds <- ds %>%
  mutate(Year = as.numeric(Year))

# checking!

typeof(ds$Year)






### Question 2 ---------- 

# Using a dplyr function,
# change ds so that all of the variables are lowercase

#ANSWER

# dyplr

library(tidyverse)

# changing so that it is lowercase

ds <- ds %>%
  rename_with(tolower)
glimpse(ds)







### Question 3 ----------

# Use mutate to create a new variable in ds that has the decade of the year as a number
# For example, 1971 would become 1970, 2001 would become 2000
# Hint: read the documentation for ?floor

#ANSWER
# creating a new variable- decade of the year as a number

ds <- ds %>%
  mutate(decade = floor(year / 10) * 10)
glimpse(ds)








### Question 4 ----------

# Sort the dataset by rank so that 1 is at the top

#ANSWER

# sorting/arranging data set

ds <- ds %>%
  arrange(rank)
glimpse(ds)






### Question 5 ----------

# Use filter and select to create a new tibble called 'top10'
# That just has the artists and songs for the top 10 songs

#ANSWER

# creating a new tibble called "top10"

top10 <- ds %>%
  filter(rank <= 10) %>%
  select(artist, song)

# checking

top10






### Question 6 ----------

# Use summarize to find the earliest, most recent, and average release year
# of all songs on the full list. Save it to a new tibble called "ds_sum"

#ANSWER

# saving summary to tibble

ds_sum <- ds %>%
  summarize(
    earliest_year = min(year, na.rm = TRUE),
    most_recent_year = max(year, na.rm = TRUE),
    average_year = mean(year, na.rm = TRUE)
  )

# viewing tibble

ds_sum





### Question 7 ----------

# Use filter to find out the artists/song titles for the earliest, most 
# recent, and average-ist years in the data set (the values obtained in Q6). 
# Use one filter command only, and sort the responses by year

#ANSWER

# values from question 6

ds_sum <- ds %>%
  summarize(
    earliest_year = min(year, na.rm = TRUE),
    most_recent_year = max(year, na.rm = TRUE),
    average_year = round(mean(year, na.rm = TRUE))
  )

# filtering data set!

ds_filtered <- ds %>%
  filter(year %in% c(ds_sum$earliest_year, ds_sum$most_recent_year, ds_sum$average_year)) %>%
  select(year, artist, song) %>%
  arrange(year)

# check result

ds_filtered





### Question 8 ---------- 

# There's and error here. The oldest song "Brass in Pocket"
# is from 1979! Use mutate and ifelse to fix the error, 
# recalculate decade, and then
# recalculate the responses from Questions 6-7 to
# find the correct oldest, averag-ist, and most recent songs



#ANSWER


# fixing the error

ds <- ds %>%
  mutate(year = ifelse(song == "Brass in Pocket" & artist == "The Pretenders", 1979, year))

# recalculating the decade

ds <- ds %>%
  mutate(decade = floor(year / 10) * 10)

# recalculating responses from questions 6-7

ds_sum <- ds %>%
  summarize(
    earliest_year = min(year, na.rm = TRUE),
    most_recent_year = max(year, na.rm = TRUE),
    average_year = round(mean(year, na.rm = TRUE))
  )

# filtering data set, again

ds_filtered <- ds %>%
  filter(year %in% c(ds_sum$earliest_year, ds_sum$most_recent_year, ds_sum$average_year)) %>%
  select(year, artist, song) %>%
  arrange(year)

# checking results

ds_filtered







### Question 9 ---------

# Use group_by and summarize to find the average rank and 
# number of songs on the list by decade. To make things easier
# filter out the NA values from decade before summarizing
# You don't need to save the results anywhere
# Use the pipe %>% to string the commands together

#ANSWER

# filtering out NA values

ds %>%
  filter(!is.na(decade)) %>%
  group_by(decade) %>%
  summarize(
    average_rank = mean(rank, na.rm = TRUE),
    song_count = n()
  )





### Question 10 --------

# Look up the dplyr "count" function
# Use it to count up the number of songs by decade
# Then use slice_max() to pull the row with the most songs
# Use the pipe %>% to string the commands together

#ANSWER

# count up the number of songs by decade, then pulling rows with the most songs

ds %>%
  filter(!is.na(decade)) %>%
  count(decade, name = "song_count") %>%
  slice_max(song_count, n = 1)

