######################## PROCENA ZNACAJNOSTI PROMENLJIVIH ###############################

source('sredjivanjePodataka.R')

library(ggplot2)

library(caret)


# provera faktorskih promenljivih language_group i country_group

gp_l <- ggplot(final_data, aes(x = language_group, fill = high_revenue)) +
  geom_bar(position = "dodge", width = 0.45) +
  ylab("Number of movies") +
  xlab("Language group") +
  theme_bw()

gp_l

gp_l_2 <- ggplot(final_data, aes(x = language_group, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Language group") +
  theme_bw() #napravili smo grafikon sa proporcijama

gp_l_2 #znacajna

gp_c <- ggplot(final_data, aes(x = country_group, fill = high_revenue)) +
  geom_bar(position = "dodge", width = 0.45) +
  ylab("Number of movies") +
  xlab("Country group") +
  theme_bw()

gp_c

gp_c_2 <- ggplot(final_data, aes(x = country_group, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Country group") +
  theme_bw()

gp_c_2 #znacajna


# provera numerickih varijabli


gp_rel_year <- ggplot(final_data, aes(x = release_year, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
gp_rel_year 
kruskal.test(release_year ~ high_revenue, data = final_data) #znacajna p-value < 2.2e-16

gp_point <- ggplot(final_data, aes(x = point, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
gp_point
kruskal.test(point ~ high_revenue, data = final_data) #znacajna

gp_point_vol <- ggplot(final_data, aes(x = point_volume, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
gp_point_vol 
kruskal.test(point_volume ~ high_revenue, data = final_data) #znacajna

gp_user_rew <- ggplot(final_data, aes(x = user_reviews, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
gp_user_rew
kruskal.test(user_reviews ~ high_revenue, data = final_data) #znacajna

gp_critic_rew <- ggplot(final_data, aes(x = critic_reviews, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
gp_critic_rew 
kruskal.test(critic_reviews ~ high_revenue, data = final_data) #znacajna

gp_runtime <- ggplot(final_data, aes(x = runtime, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
gp_runtime 
kruskal.test(runtime ~ high_revenue, data = final_data) #znacajna

gp_budget <- ggplot(final_data, aes(x = dollar_budget, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
gp_budget
kruskal.test(dollar_budget ~ high_revenue, data = final_data) #znacajna

gp_rel_month <- ggplot(final_data, aes(x = release_month, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
gp_rel_month
kruskal.test(release_month ~ high_revenue, data = final_data) #znacajna

gp_rel_day <- ggplot(final_data, aes(x = release_day, fill = high_revenue)) +
  geom_density(alpha = 0.5) + 
  theme_minimal()
gp_rel_day
kruskal.test(release_day ~ high_revenue, data = final_data) #nije znacajna


#pretvaramo numericke u faktorske

final_data$Comedy <- as.factor(final_data$Comedy)
gp_comedy <- ggplot(final_data, aes(x = Comedy, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Comedy") +
  theme_bw()
gp_comedy #nije znacajna


final_data$HomePage <- as.factor(final_data$HomePage)
gp_homepage <- ggplot(final_data, aes(x = HomePage, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("HomePage") +
  theme_bw()
gp_homepage #nije znacajna


final_data$Action <- as.factor(final_data$Action)
gp_action <- ggplot(final_data, aes(x = Action, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Action") +
  theme_bw()
gp_action #znacajna

final_data$Adventure <- as.factor(final_data$Adventure)
gp_adventure <- ggplot(final_data, aes(x = Adventure, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Adventure") +
  theme_bw()
gp_adventure #znacajna

final_data$Thriller <- as.factor(final_data$Thriller)
gp_thriller <- ggplot(final_data, aes(x = Thriller, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Thriller") +
  theme_bw()
gp_thriller #nije znacajna


final_data$Documentary <- as.factor(final_data$Documentary)
gp_doc <- ggplot(final_data, aes(x = Documentary, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Documentary") +
  theme_bw()
gp_doc #znacajna

final_data$Drama <- as.factor(final_data$Drama)
gp_drama <- ggplot(final_data, aes(x = Drama, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Drama") +
  theme_bw()
gp_drama #znacajna


final_data$Romance <- as.factor(final_data$Romance)
gp_rom <- ggplot(final_data, aes(x = Romance, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Romance") +
  theme_bw()
gp_rom #nije znacajna


final_data$Crime <- as.factor(final_data$Crime)
gp_crime <- ggplot(final_data, aes(x = Crime, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Crime") +
  theme_bw()
gp_crime #nije znacajna


final_data$Other <- as.factor(final_data$Other)
gp_other <- ggplot(final_data, aes(x = Other, fill = high_revenue)) +
  geom_bar(position = "fill", width = 0.45) +
  ylab("Number of movies") +
  xlab("Other genre") +
  theme_bw()
gp_other #znacajna

vars_to_keep_2 <- c(2:8, 10, 12:13, 15:17, 19:20, 23)
final_data <- final_data[, vars_to_keep_2]
saveRDS(final_data, "movies_final.RDS")

