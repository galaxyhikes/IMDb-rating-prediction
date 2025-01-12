# Downloading relevant packages not already present
library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("skimr")
library(skimr)
library(car)

######## Setting up data environment #########
imdb <- read.csv("~/Documents/McGill/Fall Term/MGSC 661/Midterm/IMDB_data_Fall_2024.csv")
datadict <- read.csv("~/Documents/McGill/Fall Term/MGSC 661/Midterm/data_dictionary_IMDB_Fall_2024.csv")
attach(imdb)
attach(datadict)

# Obtain detailed summary of dataset
skim(imdb) # no NA values found

# drop identifiers
imdb <- subset(imdb, select=-c(movie_title,movie_id, imdb_link))


#############################################################
######################### Dummifying ########################

##### Diving in to the genre: total number of genres for each film #####
imdb$total_genres <- rowSums(imdb[,c("action","adventure","scifi","thriller","musical","romance","western","sport","horror","drama","war","animation","crime")])
attach(imdb)
summary(total_genres)

# Extracting info on observations with 0 genre tabulated
issue <- subset(imdb, total_genres==0)
View(issue) 
# Inconsistency between dummified genre columns and genre column alone, drop dummified genre columns, create own dummified columns

# Split genre string into indiv genre type, but separated into rows
imdb_genresep <- imdb %>%
  separate_rows(genres, sep="\\|") # genres column now comprises of the indiv genre names

# Reorganise data such that same film with multiple genres are now combined into a single row, but genres are vectorized/dummified!
genre_vectorized <- imdb_genresep %>%
  mutate(value = 1) %>%  # Create a column with 1s to represent genre count x1
  pivot_wider(names_from = genres, values_from = value, values_fill = 0)
# pivot_wider reshapes data from long to wide format by forming individual columns for each unique value
# where genre is present, value = 1, otherwise 0
# remove the original dummified columns that came in the training data
genre_vectorized <- subset(genre_vectorized, select=-c(23:35))

# Updating total no of genres
genre_vectorized$total_genres <- rowSums(genre_vectorized[,c(27:47)])

# Dummify language
genre_vectorized$language <- as.factor(genre_vectorized$language)
table(language) 
# re-categorise into eng vs non-eng
genre_vectorized$eng_lang <- ifelse(genre_vectorized$language=='English',1,0)


# Dummify country
genre_vectorized$country <- as.factor(genre_vectorized$country)
table(country)
# noticed overlooked the presence of incorrect data in country - official site!
# reading org file again to obtain the movie name to amend org data
org_db <- read.csv("~/Documents/McGill/Fall Term/MGSC 661/Midterm/IMDB_data_Fall_2024.csv")
wrong_country <- org_db %>%
  filter(country == "Official site")
wrong_country # country = USA!
# updating the wrong field
genre_vectorized <- genre_vectorized %>%
  mutate(country = case_when(
    country == "Official site" ~ "USA",
    TRUE ~ country  # Keep other values unchanged
  ))

# changing US as the ref country
genre_vectorized$country = relevel(as.factor(genre_vectorized$country), ref = "USA")
# might want to re-categorise countries as US (80%) vs non-US (20%)
table(genre_vectorized$country) # confirm that official site has been amended!

# Dummify colour_film
genre_vectorized$colour_film <- as.factor(genre_vectorized$colour_film)
# 63 black & white, 1867 colour

# Dummify maturity rating
genre_vectorized$maturity_rating <- as.factor(genre_vectorized$maturity_rating)
table(maturity_rating) # need to reorganise them into umbrella terms

# Dummify aspect_ratio
genre_vectorized$aspect_ratio <- as.factor(genre_vectorized$aspect_ratio)
table(aspect_ratio) # need to reorganise them into broad categories as well


# Dummify months
genre_vectorized$release_month <- as.factor(genre_vectorized$release_month)


##########################################################################
############################# RELATIONSHIPS ##############################


################### Looking at relationship between scores and countries ###############
t <- lm(imdb_score~country, data=genre_vectorized)
summary(t)

# if we want to reclassify into just USA and non-USA
genre_vectorized$USA_country <- ifelse(genre_vectorized$country == 'USA',1,0) # but might need to review this
# relationship between score and USA_country
t_re <- lm(imdb_score~USA_country, data=genre_vectorized)
summary(t_re)

# if we want to reclassify into more countries based on the regression between scores and countries
# not sure if we want to consider other categories eg USA, Canada, Europe, Asia? might be complicated though. or consider grouping according to the coeff? Only Canada w -ve coef has significance score diff from USA tho
genre_vectorized <- genre_vectorized %>%
  mutate(country_grp = case_when(
         country == 'USA' ~ "USA",
         country == "UK" ~ "UK",
         country == "Canada" ~ "Canada",
         TRUE ~ "Others"))
genre_vectorized$country_grp <- relevel(as.factor(genre_vectorized$country_grp),ref="USA")
table(genre_vectorized$country_grp)
# relationship between score and re-classified countries
t_re2 <- lm(imdb_score~country_grp,data=genre_vectorized)
summary(t_re2)



############### Relationship between scores and language ###############
l <- lm(imdb_score~eng_lang, data=genre_vectorized)
summary(l)
# eng as the language is a significant predictor of imdb score (i suppose that makes sense because the majority of films are produced in USA, collinearity must definitely exist)
# notice eng films on the whole are lower in ratings compared to other lang
table(genre_vectorized$eng_lang) # considering that the num of non eng films is only 38 vs 1892, definitely worth including in predictor


############## Relationship between scores and aspect ratio #################
genre_vectorized$aspect_ratio <- relevel(genre_vectorized$aspect_ratio,ref="2.35") # 2.35 as the most common aspect_ratio
a <- lm(imdb_score~aspect_ratio, data=genre_vectorized)
summary(a)
table(aspect_ratio)
# ratio 2.2 (only 6 films) is still significant relative to 2.35. other aspect ratios above 2.35 behave similarly to 2.35, maybe wise to split aspect ratio into 2.35 and above vs < 2.35
# let's look at aspect ratio distribution relative to year before proceeding 
# Scatter plot of aspect ratio vs release year
ggplot(genre_vectorized, aes(x = release_year, y = as.numeric(as.character(aspect_ratio)))) +
  geom_point(color = "blue") +
  labs(title = "Aspect Ratio Distribution Over Time", x = "Release Year", y = "Aspect Ratio") +
  theme_minimal()
# does not seem to have any kind of relationship from scatterplot, shouldn't lose valuable info wrt year with categorising aspect ratio as above


ggplot(genre_vectorized, aes(x = aspect_ratio, y = imdb_score)) +
  geom_point(color = "blue") + geom_smooth(method = 'lm', formula = y~x) +
  labs(title = "IMDB Score against aspect ratio", x = "Aspect Ratio", y = "IMDB Score") +
  theme_minimal()


# Re-categorize aspect_ratio into two groups
genre_vectorized$aspect_ratio_cat <- ifelse(as.numeric(as.character(genre_vectorized$aspect_ratio)) >= 2.35, "2.35 and above", "Below 2.35")
a_re <- lm(imdb_score ~ aspect_ratio_cat, data = genre_vectorized)
summary(a_re)
# significance observed when leveled on 2.35 is drowned out. not sure if we want to keep the aspect ratio honestly



############# r/s between colour and score ##############
colorf <- lm(imdb_score~colour_film, data=genre_vectorized)
summary(colorf)
# do we want to consider? there is a significant difference in imdb score though between the 2 cats



############# Looking at relationship between duration and score ################
ggplot(genre_vectorized, aes(x=duration, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Duration", x = "Duration", y = "IMDB Score")
# Does not seem to have much value comparing duration
hist(log(genre_vectorized$duration), breaks=30)

ggplot(genre_vectorized, aes(x=duration, y=log(imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Duration", x = "Duration", y = "IMDB Score"))



############ Looking at r/s between score and budget #################
ggplot(genre_vectorized, aes(x=movie_budget, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Budget", x = "Budget", y = "IMDB Score")
# does not have any form of pattern as well?

# Budget vs year comparison
ggplot(genre_vectorized, aes(x=release_year, y=movie_budget)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "Budget against Year", x = "Release year", y = "Budget")
# main takeaway: budget of films are higher for more recent films but aside from that, nothing else really tangible



#################### r/s between score and news article ###################
news <- lm(imdb_score~nb_news_articles, data=genre_vectorized)
summary(news)
ggplot(genre_vectorized, aes(x=nb_news_articles, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "Score against number of articles", x = "Number of articles", y = "IMDB Score")
# some pretty obvious outliers

# outlier test
outlierTest(news)
# remove outlier round 1
genre_vectorized2 <- genre_vectorized[-c(492,1581,989),]
news2 <- lm(imdb_score~nb_news_articles, data=genre_vectorized2)
ggplot(genre_vectorized2, aes(x=nb_news_articles, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "Score against number of articles v2", x = "Number of articles", y = "IMDB Score")
outlierTest(news2)

# remove outlier round 2
genre_vectorized2 <- genre_vectorized2[-c(12),]
news3 <- lm(imdb_score~nb_news_articles, data=genre_vectorized2)
ggplot(genre_vectorized2, aes(x=nb_news_articles, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "Score against number of articles v3", x = "Number of articles", y = "IMDB Score")
outlierTest(news3)

# remove outlier round 3
genre_vectorized2 <- genre_vectorized2[-c(490,1579,987),]
ggplot(genre_vectorized2, aes(x=nb_news_articles, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "Score against number of articles v4", x = "Number of articles", y = "IMDB Score")
news4 <- lm(imdb_score~nb_news_articles, data=genre_vectorized2)
outlierTest(news4) # no more significant difference from rest of sets
summary(news4) # significant coef but judging from the scatter plot, doesnt not seem to bring much value
ncvTest(news4) # no hetereoskedasticity but looks like a possible spread visually




################# Understanding maturity rating ##################
table(maturity_rating)
levels(maturity_rating)


pg_pg13_films <- genre_vectorized %>%
  filter(maturity_rating %in% c("PG", "PG-13"))

# Create a boxplot to visualize IMDb scores
ggplot(pg_pg13_films, aes(x = maturity_rating, y = imdb_score, fill = maturity_rating)) + 
  geom_boxplot() + 
  labs(title = "IMDB Scores for PG and PG-13 Rated Films", 
       x = "Maturity Rating", 
       y = "IMDB Score") +
  theme_minimal()

pg_pg13_r_films <- genre_vectorized %>%
  filter(maturity_rating %in% c("PG", "PG-13" ,"R"))

ggplot(pg_pg13_r_films, aes(x = maturity_rating, y = imdb_score, fill = maturity_rating)) + 
  geom_boxplot() + 
  labs(title = "IMDB Scores for PG and PG-13 and R Rated Films", 
       x = "Maturity Rating", 
       y = "IMDB Score") +
  theme_minimal()


ggplot(genre_vectorized, aes(x = maturity_rating, y = imdb_score, fill = maturity_rating)) + 
  geom_boxplot() + 
  labs(title = "IMDB Scores across maturity ratings", 
       x = "Maturity Rating", 
       y = "IMDB Score") +
  theme_minimal()


movies_data_base_n_o = genre_vectorized[genre_vectorized$release_year>1968,]
ggplot(movies_data_base_n_o, aes(x = maturity_rating, y = imdb_score, fill = maturity_rating)) + 
  geom_boxplot() + 
  labs(title = "IMDB Scores across maturity ratings", 
       x = "Maturity Rating", 
       y = "IMDB Score") +
  theme_minimal()

table(movies_data_base_n_o$maturity_rating)


# R-rated films
r_rated_films <- genre_vectorized %>%
  filter(maturity_rating == "R")
ggplot(r_rated_films, aes(x = release_year)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Year Distribution of R-Rated Films", x = "Year", y = "Count") +
  theme_minimal() ### take note that r-rated films had very different meanings in the past. the reclassification might need some work

# M rated films
m_films <- genre_vectorized %>%
  filter(maturity_rating == "M")
ggplot(m_films, aes(x = release_year)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Year Distribution of M-Rated Films", x = "Year", y = "Count") +
  theme_minimal() 
summary(m_films$release_year) # only 2 films from 1969 rated M, which was the start of rating system. to classify the same as for GP/PG


############ Understand distribution of year for rating classification ###########
summary(release_year) # note that the latest data were from 2018, and the films are dated as old as 1936
# filter out films before 1968
old_films <- genre_vectorized %>%
  filter(release_year<1968)
ggplot(old_films, aes(x = maturity_rating)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Maturity ratings of old films before 1968", x = "Year", y = "Count") +
  theme_minimal() 
# all approved and passed films were recorded before 1968, when the official rating system came in. to take that into consideration when classifying films. should we drop? is there any value?

# films recorded before 1968
ggplot(old_films, aes(x = release_year)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Year distribution of old films before 1968", x = "Year", y = "Count") +
  theme_minimal() 


############ Understanding year vs score##########
# year distribution of all films in histogram
ggplot(genre_vectorized, aes(x = release_year)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Year distribution of all films", x = "Year", y = "Count") +
  theme_minimal() 

# scatter plot of year vs imdb score
ggplot(genre_vectorized, aes(x=release_year, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Year", x = "Year", y = "IMDB Score") +
  xlim(min(genre_vectorized$release_year, na.rm = TRUE),NA) # adjust the scale of x axis such that it starts only from min value for year. nil max value, the model adjusts accordingly

y <- lm(imdb_score~release_year, data=genre_vectorized)
summary(y) 
# does not make sense for coef to be -ve. prob not valuable to keep as a predictor, but definitely necessary to use it for rating classification
ncvTest(y) # no heteroskedasticity which is weird but it's fine year should not be kept as predictor


###################### Score vs month ##################
# Distribution of films by months
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
genre_vectorized$release_month <- factor(genre_vectorized$release_month, levels=month_names)
# do the above to arrange the month axis by chronological order of calendar month
ggplot(genre_vectorized, aes(x = release_month)) +
  geom_bar(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of films by months", x = "Month", y = "Count") +
  theme_minimal() 
mth <- lm(imdb_score~release_month,data = genre_vectorized)
summary(mth)
## hmm not sure how we want to split the months but it seems like months is valuable predictor 


# Let's try and see the distribution of score vs months when the months are continuous instead
# let me change the months into numeric form so scatter plot can be plotted
genre_vectorized$release_month_numeric <- match(genre_vectorized$release_month, month_names)

# let's now see how the plot looks
ggplot(genre_vectorized, aes(x=release_month_numeric, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Month", x = "Month", y = "IMDB Score")
m <- lm(imdb_score~release_month_numeric,data=genre_vectorized)
summary(m)  # coef is significant but looking at the scatterplot, i am not sure. possibly a high degree polynomial though
# same remark as above


####################### Score vs release day #########################
ggplot(genre_vectorized, aes(x=release_day, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Day", x = "Day", y = "IMDB Score")
d <- lm(imdb_score~release_day,data=genre_vectorized)
summary(d) # coef insignificant. besides, scatterplot doesnt seem to make sense having such a high degree polynomial to fit the data



#################### Score vs actor star meter ##################
star1 <- lm(imdb_score~actor1_star_meter,data=genre_vectorized)
star2 <- lm(imdb_score~actor2_star_meter,data=genre_vectorized)
star3 <- lm(imdb_score~actor3_star_meter,data=genre_vectorized)
summary(star1)
summary(star2)
summary(star3)

################ star 1 r/s
ggplot(genre_vectorized, aes(x=actor1_star_meter, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Actor 1 star meter", x = "Actor 1 star meter", y = "IMDB Score")
summary(actor1_star_meter)
# A lot of outliers in star 1 model

# remove outliers
outlierTest(star1) # round 1 outlier removal
genre_vectorized_star <- genre_vectorized[-c(989),] # remove outlier
star1 <- lm(imdb_score~actor1_star_meter,data=genre_vectorized_star)
outlierTest(star1) # round 2 outlier removal
genre_vectorized_star <- genre_vectorized_star[-c(1580),]
star1 <- lm(imdb_score~actor1_star_meter,data=genre_vectorized_star)
outlierTest(star1) # no more outliers
ggplot(genre_vectorized_star, aes(x=actor1_star_meter, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Actor 1 star meter", x = "Actor 1 star meter", y = "IMDB Score")

summary(star1)
summary(genre_vectorized_star$actor1_star_meter)


############### star 2 r/s
ggplot(genre_vectorized, aes(x=actor2_star_meter, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Actor 2 star meter", x = "Actor 2 star meter", y = "IMDB Score")
# very similar kind of output honestly might need to narrow down the range otherwise i dont think it will be feasible to make any analysis
summary(actor2_star_meter)


############## star 3 r/s 
ggplot(genre_vectorized, aes(x=actor3_star_meter, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Actor 3 star meter", x = "Actor 3 star meter", y = "IMDB Score")
# slightly different in terms of the slope

# lets try and see how it changes with 2 and 3 stars in a single model
star1n2 <- lm(imdb_score~actor1_star_meter+actor2_star_meter)
summary(star1n2)
# adjusted r2 increased when actor 2 was added compared to adjusted r2 of actor 1 meter alone
# but adjusted r2 for model w actor 2 meter was higher than the combined model though just a very slight drop


all_stars <- lm(imdb_score~actor1_star_meter+actor2_star_meter+actor3_star_meter)
summary(all_stars) 
# 3rd actor meter does not add value (adjusted r square dropped), can be omitted 



######################## Movie meter ######################
mmtr <- lm(imdb_score~movie_meter_IMDBpro,data=genre_vectorized)
summary(mmtr)
ggplot(genre_vectorized, aes(x=movie_meter_IMDBpro, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Movie meter", x = "Movie meter", y = "IMDB Score")
summary(genre_vectorized$movie_meter_IMDBpro)
outlierTest(mmtr) # remove outliers
genre_vectorized_mmtr <- genre_vectorized[-c(1581,989),]
mmtr <- lm(imdb_score~movie_meter_IMDBpro,data=genre_vectorized_mmtr) # trying out the r/s again and checking for outlier
outlierTest(mmtr) # no more outliers
# view plot again
ggplot(genre_vectorized_mmtr, aes(x=movie_meter_IMDBpro, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Movie meter v2", x = "Movie meter", y = "IMDB Score")
# not much difference honestly, esp when the movie meter has such a huge range. might be worth considering classifying into blocks/ranges



####################### Number of total genres ############################

tot_g <- lm(imdb_score~total_genres,data=genre_vectorized)
ggplot(genre_vectorized, aes(x=total_genres, y=imdb_score)) + 
  geom_point() + geom_smooth(method = 'lm', formula = y~x) + 
  labs(title = "IMDB Score against Total num of genres", x = "Number of genres", y = "IMDB Score")
summary(tot_g) # not significant in coef, might need to consider dummified genres


