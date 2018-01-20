# importing and installing important libraries
install.packages('qdap', dependencies = TRUE)
require(tm) 
require(qdap)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(MASS)
require(biclust)
library(cluster)
library(igraph)
install.packages('fpc') 
library(fpc)
require(twitteR)
set.seed(123)
# importing files
airbnb = read.csv('reviews.csv', stringsAsFactors=F)
listings = read.csv('listings.csv', stringsAsFactors = F)
summary(airbnb)
str(airbnb)
summary(listings)
str(listings)
head(airbnb,n=3)
tail(airbnb,n=3)
head(listings,n=2)
tail(listings,n=3)
dim(airbnb)
dim(listings)
#create a corpus
is.vector(listings$description)
listingscor <- Corpus(VectorSource(listings$description))
# Print out listingscor
listingscor
# Print data on the 15th description in listingscor
listingscor[[15]][1]
# Print the content of the 15th description in listingscor
listingscor[[15]]

### Next step is to take the corpus and clean it up from its raw state.(Using Preprocessing text functions)
#listingscor <- tm_map(listingscor, removeWords, stopwords(“english”))

listingscor <- tm_map(listingscor, removePunctuation)
listingscor <- tm_map(listingscor, removeNumbers)
listingscor <- tm_map(listingscor, tolower)
#listingscor <- tm_map(listingscor, removeWords, stopwords('english'))
stopwords <- local({
  en <- english <- readLines(system.file("stopwords", "english.dat", package = "tm"))
  de <- german <- deutsch <- readLines(system.file("stopwords", "german.dat", package = "tm"))
  fr <- french <- francais <- readLines(system.file("stopwords", "french.dat", package = "tm"))
  function(language = "en") get(sub("_.*", "", language))
})

listingscor <- tm_map(listingscor, removeWords, c(stopwords("en"),"austin", "room",
                                                  "bed","beds","home","two" , 
                                                  "need", "bedroom",
                                                  "apartment", "like",
                                                  "street", "one", "also",
                                                  "kitchen", "bathroom",
                                                  "stay", "place",
                                                  "house", "will",
                                                  "felt", "time",
                                                  "around", "airbnb", "day",
                                                  "next", "time", "just","didnt","get","everything","can","youll","austins","well","great"))
listingscor <- tm_map(listingscor, stemDocument)
listingscor <- tm_map(listingscor, stripWhitespace)
listingscor <- tm_map(listingscor, PlainTextDocument)


# Print out post in cleaned form
listingscor[[227]][1]
#color scheme for the word clouds
pal2 <- brewer.pal(8,"Set2")

wordcloud(listingscor, min.freq=10,
          max.words=100, random.order=F, rot.per=.3, colors=pal2)
###=========================================================================================================================
#create a corpus
airbnbcorp <- Corpus(VectorSource(airbnb$comments))

# Print out airbnbcorp
airbnbcorp
# Print data on the 15th review/ comment in airbnbcorp
airbnbcorp[[15]][1]
# Print the content of the 15th review in airbnbcorp
airbnbcorp[[15]]

#start removing non essentials, lowering capital letters, and getting rid of stop words

airbnbcorp <- tm_map(airbnbcorp, removePunctuation)
airbnbcorp <- tm_map(airbnbcorp, removeNumbers)
airbnbcorp <- tm_map(airbnbcorp, tolower)
airbnbcorp <- tm_map(airbnbcorp, removeWords, stopwords('en'))
airbnbcorp <- tm_map(airbnbcorp, removeWords, c(stopwords("en"),"austin", "room",
                                                "bed","beds","home","two" , 
                                                "need", "bedroom",
                                                "apartment", "like",
                                                "street", "one", 
                                                "kitchen", "bathroom",
                                                "stay", "place",
                                                "house", "will",
                                                "felt", "time",
                                                "around", "airbnb", "day",
                                                "next", "time", "just", "also","didnt","get","everything","can","youll","austins","well","great"))
airbnbcorp <- tm_map(airbnbcorp, stemDocument)
airbnbcorp <- tm_map(airbnbcorp, PlainTextDocument)

# Print out a comment in a cleaned corpus form
airbnbcorp[[15]][1]

wordcloud(airbnbcorp, min.freq=6,
          max.words=100, random.order=F, rot.per=.3, colors=pal2)
#====================================================================================
library(ggplot2)
tdm1<- DocumentTermMatrix(airbnbcorp, control = list(weighting = weightTf, 
                                                     stopwords = FALSE))
tdm1 <- removeSparseTerms(tdm1, .99)
freq <- colSums(as.matrix(tdm1)) 

# Sort term_frequency in descending order
freq_sort <- sort(freq,decreasing=T)

# View the top 12 most common words
freq_sort[1:12]
###barplot for the most ten frequent words: las=2 makes the axis labled vertical.
barplot(freq_sort[1:10],col = "tan",las=2)
df <- data.frame(word=names(freq_sort), freq=freq_sort)

dffreq <- subset(df, freq_sort>=10000)
###plot most frequent words frequency >= 10000
ggplot(aes(x=word, y=freq), data = dffreq)+
  geom_bar(stat = "identity")+
  coord_flip()
###=======================================================================================
findAssocs(tdm1, "downtown", corlimit=0.15)
###®AlhamadaniA
