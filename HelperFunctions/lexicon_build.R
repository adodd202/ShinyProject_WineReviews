library(data.table)
library(dplyr)
library(magrittr)
library(tm)
library(SnowballC)
library(MASS)
library(Matrix)
library(lsa)

#####################################################################################
# HERE WE BUILD THE LEXICON. OUTPUT IS 300 MOST COMMONLY USED LEMMATIZED WORDS.
#####################################################################################

wine <- fread(file = "wine4.csv")
wine <- subset(wine, select = -c(1,2) )
write.csv(wine,"wine4.csv")


wine = wine %>% mutate(description2 = paste(description,variety,country,
                                             province,region_1,winery))
wineHead = wine$description2[1:40000]
wineHead = sample_n(wine, 40000)
myCorpus = Corpus(VectorSource(wineHead))
ctrl <- list(#tokenize = strsplit_space_tokenizer,
             removePunctuation = list(preserve_intra_word_dashes = TRUE),
             stopwords = TRUE,
             stemming = TRUE,
             wordLengths = c(4, Inf))
tdm = TermDocumentMatrix(myCorpus, control = ctrl)
myTdm <- as.matrix(tdm)
freqDF1 <- data.frame(ST = rownames(myTdm), 
                      Freq = rowSums(myTdm), 
                      row.names = NULL)
freqDF2 = freqDF1 %>% filter(!grepl("[0-9]{5,}",ST))
lexicon = freqDF2 %>% arrange(desc(Freq)) %>% head(300)

#####################################################################################
# CREATING [0 0 1 0 1 0] OUT OF SUBSETTED WINE LIBRARY (VECTORIZE WORDS)
#####################################################################################
# Input: Description, char
# Output: Normalized vector of wine library (3000 wines), length of vector = 1000, normalized
lexicon <- fread(file = "lexicon.csv")
n_wines = 3000
wineHead <- fread(file = "wine3000.csv")
#wineHead = wine %>% head(n_wines)
nrow1 = 1000
mm <- Matrix(0, nrow = nrow1, ncol = n_wines, sparse = TRUE)
object.size(mm)
listLex = as.character(lexicon$ST)
ctrl <- list(
  removePunctuation = list(preserve_intra_word_dashes = TRUE),
  stopwords = TRUE,
  stemming = TRUE,
  wordLengths = c(4, Inf))
for (k in 0:2){
  print(k)
  for (j in 1:1000){
    lineNum = j + (k*1000)
    tdm1 = TermDocumentMatrix(Corpus(VectorSource(wineHead$description2[lineNum])), control = ctrl)
    myTdm2 <- as.matrix(tdm1)
    freqDF3 <- data.frame(ST = rownames(myTdm2), 
                          Freq = rowSums(myTdm2), 
                          row.names = NULL)
    freqOneDesc = freqDF3 %>% filter(!grepl("[0-9]{5,}",ST))
    vec = seq(1,nrow1)
    descripOne1 = paste(freqOneDesc$ST, sep = " ", collapse = " ")
    vec1 = sapply(vec,function(x) {x = 1*grepl(listLex[x],descripOne1)})
    mm[,lineNum] = vec1
  }
}
#print(object.size(mm))

mm = sweep(mm,2,colSums(mm),`/`)

#mm3 = source("smartWine.Rdata")
#mm3 = get(load("smartWine.Rdata"))

wineHead <- subset(wineHead, select = -1 )
write.csv(wineHead,"wine3000.csv")


#####################################################################################
# CHECKING WITH USER INPUT
#####################################################################################

# Optionally filter by cost in dplyr. Pass the chosen indices to matrix.

# Perform cross correlation on whole matrix, then get top ten summed cross correlations indices:
nrow1 = 1000
wineHead <- fread(file = "wine3000.csv") #Read the 3000 wines to choose from

UserInput = "i would like a cabernet sauvignon from napa valley"
tdm_user = TermDocumentMatrix(Corpus(VectorSource(UserInput)), control = ctrl)
myTdm_user <- as.matrix(tdm_user)
freqDF_user <- data.frame(ST = rownames(myTdm_user), 
                      Freq = rowSums(myTdm_user), 
                      row.names = NULL)
freqOneDesc_user = freqDF_user %>% filter(!grepl("[0-9]{5,}",ST))
vec = seq(1,nrow1)
descripOne_user = paste(freqOneDesc_user$ST, sep = " ", collapse = " ")
userV1 = sapply(vec,function(x) {x = 1*grepl(listLex[x],descripOne_user)})

vecResults = seq(1:n_wines)
vecResults = sapply(vecResults,function(x) {x = cosine(userV1,mm[,x])})
print(vecResults)

whichpart <- function(x, n=5) {
  nx <- length(x)
  p <- nx-n
  xp <- sort(x, partial=p)[p]
  which(x > xp)
}

wineIndices = whichpart(vecResults,n = 5)
print(wineIndices)

#####################################################################################
# GOING INTO DPLYR AND DATAFRAME
#####################################################################################

# Pass indices to wine filter, filter by 
#wineOutput = subset(wineHead, V1 %in% wineIndices)
wineOutput = wineHead[wineIndices,]
maxPrice = 3000 #will be user input later
wineOutput = wineOutput %>%
  filter(price  <= maxPrice) %>% 
  arrange(desc(points))
  








