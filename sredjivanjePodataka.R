#Učitavanje potrebnih paketa

#install.packages("stringr")
library(stringr)
library(tidyverse)

#Učitavanje dataset-ova

movies <- read.csv("data/imdb_movies.csv")
meta <- read.csv("data/meta.csv")

colnames(meta)[2] <- "metascore"

## JJ: nisam sigurna da je metascore toliko bitna varijabla da se pri redukovanju 
## dataset-a krene od nje; mislim da je bolje krenuti od varijable koju hocete da 
## koristite kao izlaznu

sum(is.na(meta$metascore))
# 0

sum(is.na(movies$metascore))
# 468254
sum(is.na(movies$metascore))/nrow(movies)
# 0.9723

##JJ: check if metascore from the meta df can be used for filling up missing values
## in the movies df
?intersect
length(intersect(meta$name, 
                 movies[is.na(movies$metascore),]$name)) # names of movies without metascore
## by merging meta and movies, we can add metascores for 4982 movies that currently
## do not have metascore

## JJ: check if name can be used for merging the two DFs
?n_distinct
n_distinct(meta$name)
n_distinct(movies$name)
# names are not unique in either data frame => DFs cannot be merged and metascore cannot be
# added from the meta df in a credible way
# => no need for meta DF 
remove(meta)

# izlazna varijabla usa_gross

sort(unique(movies$usa_gross))[1:50]
sum(movies$usa_gross == "")/nrow(movies)
# 0.962

sort(unique(movies$u_gross_money))[1:50]
sum(is.na(movies$u_gross_money))/nrow(movies)
# 0.962

## JJ: usa_gross and u_gross_money seem to keep the same data but stored differently
## => better use u_gross_money for the outcome as it has already processed into
## numerical values


with_usa_gross <- movies[!is.na(movies$u_gross_money),]
dim(with_usa_gross)
# 18528    33

remove(movies)

# izbacivanje string atributa

n_distinct(with_usa_gross$production_companies)
# 15636
table(with_usa_gross$production_companies) |> sort(decreasing = T) |> head(10)
# too granular

vars_to_remove <- c('name', 'org_name', 'date', 'director', 'writer', 
                    'story_line', 'cast','world_gross', 'usa_gross', 'budget',
                    'casts_id', 'keywords', 'production_companies')
## JJ: world_gross, usa_gross, budget i date have their numerical 'versions'
## which can be directly used, making these irrelevant

vars_to_keep <- setdiff(names(with_usa_gross), vars_to_remove)
with_usa_gross <- with_usa_gross[, vars_to_keep]

colnames(with_usa_gross)[2] <- 'release_year'


table(with_usa_gross$BlogPage) |> prop.table() |> round(3)
# 0.997  0.003  => useless
table(with_usa_gross$CompPage) |> prop.table() |> round(3)
# 0.954  0.046 => remove 
table(with_usa_gross$HomePage) |> prop.table() |> round(3)
# 0.65 0.35 => keep

with_usa_gross <- with_usa_gross[, -c(16,17)]

##JJ: check for missing in numerical variables

with_usa_gross |> select_if(is.numeric) -> num_vars_df
apply(num_vars_df, 2, function(x) sum(is.na(x)))
apply(num_vars_df, 2, function(x) round(sum(is.na(x))/nrow(num_vars_df), 4))
## very large proportion of rows miss metascore and dollar_budget
## for w_gross_money, it is questionable if it should be used at all
## considering that u_gross_money is used as the outcome


## JJ: Check language
n_distinct(with_usa_gross$language)
# 1537
table(with_usa_gross$language) |> sort(decreasing = TRUE) |> head(20)
# too many different combinations 
# one option is to have 3 groups for language - for example: 
# English_only, English_option, Non_English

lang_group <- function(movie_language){
  langs <- str_split(movie_language, ",")[[1]]
  if((length(langs) == 1) & (langs[1] == "English"))
    return("English")
  if((length(langs) > 1) & ("English" %in% langs))
    return("English_option")
  return("Non_English")
}

with_usa_gross$language_group <- sapply(with_usa_gross$language, lang_group)
with_usa_gross$language_group <- as.factor(with_usa_gross$language_group)
table(with_usa_gross$language_group)

with_usa_gross$language <- NULL

# Check country
n_distinct(with_usa_gross$country)
# 2107

unique(with_usa_gross$country) |> sort() |> head(20)

table(with_usa_gross$country) |> sort(decreasing = TRUE) |> head(20)

# Have 3 groups for country: USA_only; USA_co_production; Non_USA

?str_split
cont_group <- function(movie_country){
  countries <- str_split(movie_country, c(",", ", ", " ,"))[[1]]
  if((length(countries) == 1) & (countries[1] == "USA"))
    return("USA_only")
  if((length(countries) > 1) & ("USA" %in% countries))
    return("USA_co_production")
  return("Non_USA")
}

with_usa_gross$country_group <- sapply(with_usa_gross$country, cont_group)
with_usa_gross$country_group <- as.factor(with_usa_gross$country_group)
table(with_usa_gross$country_group)
with_usa_gross$country <- NULL

# Check Genres
n_distinct(with_usa_gross$genres)
# 1909

table(with_usa_gross$genres) |> sort(decreasing = TRUE) |> head(20)

# create a list of all unique genres
all_genres <- c("Drama")
for(genres in with_usa_gross$genres) {
  g_list <- str_split(genres, ',', simplify = TRUE)
  for(g_item in g_list) {
    if(!(g_item %in% all_genres))
      all_genres = append(all_genres, g_item)
  }
}

all_genres

# compute the frequency of each genre
all_genres_df <- data.frame(genre = all_genres,
                            count = 0)
for(genres in with_usa_gross$genres) {
  g_list <- str_split(genres, ',', simplify = TRUE)
  for(g_item in g_list) {
    cnt <- all_genres_df$count[all_genres_df$genre == g_item]
    all_genres_df$count[all_genres_df$genre == g_item] <- cnt + 1
  }
}

all_genres_df |> arrange(desc(count))

# Take, for example, the first 8 and merge all others into the Other genre
# Have one column for each of these 8+1 genres

# Define the unique genres
unique_genres <- all_genres_df |> arrange(desc(count)) |> pull(genre) |> head(8)
unique_genres

library(tidyr)
# Separate the Genre column into multiple rows
genres_df <- with_usa_gross |>
  select(movie_id, genres) |>
  separate_rows(genres, sep = ",") |>
  mutate(genres = ifelse(genres %in% unique_genres, genres, "Other"))
?pivot_wider
# Create a binary column for each genre
genres_df_wide <- genres_df %>%
  mutate(value = 1) %>%
  pivot_wider(names_from = genres, values_from = value, values_fn = mean, values_fill = 0) #????

with_usa_gross |>
  select(-c(genres)) |> 
  inner_join(genres_df_wide) -> movies_final

glimpse(movies_final)


#provera korelacije w_gross i u_gross, provera korelacije dollar_budget i metascore sa U_gross
#install.packages("corrplot")
library(corrplot)
movies_cor <- cor(num_vars_df[!is.na(num_vars_df$w_gross_money + num_vars_df$metascore + num_vars_df$dollar_budget),])
corrplot.mixed(movies_cor) #korelacija je 0.94 izmedju w_gross i u_gross, korelacije izmedju dollar_budget i u_gross je 0.69, a izmedju 
#metascore i u_gross je 0.14 sto nam govori da postoji visoka korelacija izmedju w i u gross, pa cemo w gross izbaciti
#a posto metascore ima jako malu korelaciju sa u gross, redukovacemo dataset u donosu na dollar_budget
movies_final$w_gross_money <- NULL

final_data <- movies_final[!is.na(movies_final$dollar_budget),]
str(final_data)
summary(final_data)



#provera nedostajucih vrednosti nakon sredjivanja podataka i redukcije 

final_data |> select_if(is.numeric) -> num_vars_df_2
apply(num_vars_df_2, 2, function(x) sum(is.na(x)))
apply(num_vars_df_2, 2, function(x) round(sum(is.na(x))/nrow(num_vars_df_2), 4))

final_data$metascore <- NULL #izbacujemo varijablu metascore jer ima dosta nedostajucih vrednosti i kroz korelacionu matricu smo videli
#da nije od velikog znacaja za model

#dopuna nedostajucih vrednosti za pont i runtime

library(nortest)
#koristimo Anderson-Darling test za proveru normalnosti jer shapiro.test prihvata samo do 5000 opservacija

ad.test(final_data$point) #nema normalnu raspodelu p-value < 2.2e-16
mdn_point <- median(final_data$point, na.rm = TRUE)
final_data$point[is.na(final_data$point)] <- mdn_point

ad.test(final_data$runtime) #nema normalnu raspodelu p-value < 2.2e-16
mdn_runtime <- median(final_data$runtime, na.rm = TRUE)
final_data$runtime[is.na(final_data$runtime)] <- mdn_runtime

#mnozenje dollar_budzeta i u_gross sa inflation_coef

final_data$dollar_budget <- final_data$dollar_budget * final_data$inflation_coeff
final_data$u_gross_money <- final_data$u_gross_money * final_data$inflation_coeff
final_data$inflation_coeff <- NULL
summary(final_data)

####### PRAVLJENJE IZLAZNE PROMENLJIVE #######

revenue_75perc <- quantile(final_data$u_gross_money, probs = 0.75)
final_data$high_revenue <- ifelse(test = final_data$u_gross_money > revenue_75perc,
                                  yes = "Yes",
                                  no = "No")

final_data$u_gross_money <- NULL
final_data$high_revenue <- as.factor(final_data$high_revenue)
prop.table(table(final_data$high_revenue))
