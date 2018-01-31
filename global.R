#Keep these install notes. They could be useful in the future.

#install.packages("devtools")
#library(devtools)
#install_github("shinyGlobe", "trestletech")

library(data.table)
library(dplyr)
library(magrittr)
library(tm)
library(SnowballC)
library(MASS)
library(Matrix)
library(lsa)

wine <- fread(file = "wine4.csv")
wine2 = wine


#Getting the varieties of wine for the selectize options
varietyDT = as.data.table(wine[,variety]) %>% distinct(V1)
varietyDT = setorder(varietyDT)
varietyStr = c('Any',varietyDT[,V1])


#Initialize the globe with all wines:
wine_lat_long = wine %>% group_by(lat, long) %>%
  summarise(n_wines = n()/nrow(wine))
wine_max = wine_lat_long %>% arrange(desc(n_wines))
wine_max = wine_max[1,3]$n_wines[1]
wine_lat_long = wine_lat_long %<>% mutate(n_wines = (n_wines/wine_max))

#Initialize inputs
variety = "Riesling"


#Lexicon 
lexicon <- fread(file = "lexicon.csv")
listLex = as.character(lexicon$ST)
mm = get(load("smartWine.Rdata"))
wine3000 <- fread(file = "wine3000.csv") #Read the 3000 wines to choose from
ctrl <- list(
  removePunctuation = list(preserve_intra_word_dashes = TRUE),
  stopwords = TRUE,
  stemming = TRUE,
  wordLengths = c(4, Inf))



