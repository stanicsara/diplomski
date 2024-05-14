movies <- read.csv("data/imdb_movies.csv")
meta <- read.csv("data/meta.csv")

colnames(meta)[2] <- "metascore"

missing_meta <- is.na(movies$metascore)
missing_meta
with_meta <- subset(movies, subset = !missing_meta)

install.packages("data.table")
library(data.table)
setorder(with_meta, metascore)
setorder(meta, metascore)

install.packages("arsenal")
library(arsenal)
comparedf(with_meta, meta)


# izlazna varijabla usa_gross

missing_usa <- is.na(with_meta$u_gross_money)

with_usa_gross <- subset(with_meta, subset = !missing_usa)

sum(is.na(with_usa_gross$dollar_budget))  #fali 35.57% od svih vrednosti - 3630

#### sa usa_gross kao izlaznom varijablom ####

# izbacivanje string atributa

#with_usa_gross <- with_usa_gross[, -c(1:4, 11:14, 18:20, 22, 24, 26:27, 33)]

#missing_budget_for_usa <- is.na(with_usa_gross$dollar_budget)
#data_usa <- subset(with_usa_gross, subset = !missing_budget_for_usa)

#summary(data_usa)

## nedostajuce vrednosti

#all(sum(is.na(data_usa$runtime)))
#summary(data_usa$runtime)

library(nortest)
#ad.test(data_usa$runtime) #nema normalnu raspodelu
#mdn_runtime_usa <- median(data_usa$runtime, na.rm = TRUE) #??
#data_usa$runtime[is.na(data_usa$runtime)] <- mdn_runtime_usa
#all(sum(is.na(data_usa$runtime)))
#summary(data_usa$runtime)


#izlazna varijabla world_gross

missing_world <- is.na(with_meta$w_gross_money)

with_world_gross <- subset(with_meta, subset = !missing_world)

(sum(is.na(with_world_gross$dollar_budget)))  #fali 38.10% - 4302


#### sa world_gross kao izlaznom varijablom ####

# izbacivanje string atributa

with_world_gross <- with_world_gross[, -c(1:4, 11:14, 18:20, 22, 25:27, 33)]

# redukovanje zbog na's kod budzeta

missing_budget_world <- is.na(with_world_gross$dollar_budget)
data_world <- subset(with_world_gross, subset = !missing_budget_world)

summary(data_world)

#@@@@@@@ koristicemo world_gross kao izlaznu varijablu jer nam nakon redukcije ostaje vise vrednosti

# nedostajuce vrednosti za world

sum(is.na(data_world))

ad.test(data_world$runtime) #nema normalnu raspodelu
mdn_runtime_world <- median(data_world$runtime, na.rm = TRUE) #??
data_world$runtime[is.na(data_world$runtime)] <- mdn_runtime_world
all(sum(is.na(data_world$runtime)))
summary(data_world$runtime)

sum(data_world$genres == "" | data_world$genres == " " | data_world$genres == "-", data_world$genres == "genre_not_provided")
sum(data_world$country == "" | data_world$country == " " | data_world$country == "-", data_world$country == "country_not_provided")
sum(data_world$language == "" | data_world$language == " " | data_world$language == "-", data_world$language == "language_not_provided")









