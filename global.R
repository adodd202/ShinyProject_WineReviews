library(data.table)
library(dplyr)
library(magrittr)
library(tm)
library(SnowballC)
library(MASS)
library(Matrix)
library(lsa)

# Initilizing large wine dataframe, ~120,000 observations
wine <- fread(file = "./Data/wine4.csv")
wine2 = wine


# Getting the varieties of wine for the selectize options
varietyDT = as.data.table(wine[,variety]) %>% distinct(V1)
varietyDT = setorder(varietyDT)
varietyStr = c('Any',varietyDT[,V1])


# Initialize the globe with all wines:
# Latitude and longitude are google map queried with a script in HelperFunctions/geoCoder.R
wine_lat_long = wine %>% group_by(lat, long) %>%
  summarise(n_wines = n()/nrow(wine))
wine_max = wine_lat_long %>% arrange(desc(n_wines))
wine_max = wine_max[1,3]$n_wines[1]
wine_lat_long = wine_lat_long %<>% mutate(n_wines = (n_wines/wine_max))

# Initialize inputs
variety = "Riesling"

# Lexicon and search function data initialization
# Note: The lexicon is already created in HelperFunctions/lexicon_build.R
lexicon <- fread(file = "./Data/lexicon.csv") # Lexicon of 1000 common words in wine
listLex = as.character(lexicon$ST) 
mm = get(load("./Data/smartWine.Rdata")) #Matrix of 3000 wines word vectors
wine3000 <- fread(file = "./Data/wine3000.csv") #Read the 3000 wines to choose from

# ctrl is used in tm package calls
ctrl <- list(
  removePunctuation = list(preserve_intra_word_dashes = TRUE),
  stopwords = TRUE,
  stemming = TRUE,
  wordLengths = c(4, Inf))



