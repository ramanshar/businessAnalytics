# Loading the Required Packages
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(syuzhet)
library(ggplot2)

# For reading the text file from our local machine , choose file interactively
text <- readLines(file.choose())
# The text is loaded using Corpus() function from text mining package. Corpus is a list of a document (in our case, we only have one document).
Docs <- Corpus(VectorSource(text))
#inspect the content of the document & Replacing "/", "@" and "|" with space
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
Docs <- tm_map(Docs toSpace, "/")
Docs<- tm_map(Docs, toSpace, "@")
Docs <- tm_map(Dosc, toSpace, "\\|")
# the tm_map() function is used to remove unnecessary white space
# Convert the text to lower case
Docs <- tm_map(Docs, content_transformer(tolower))
# Remove numbers
Docs <- tm_map(Docs, removeNumbers)
# Remove english common stopwords
Docs <- tm_map(Docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
Docs  <- tm_map(Docs , removeWords, c("s", "company","team")) 
# Remove punctuations
Docs  <- tm_map(Docs , removePunctuation)
# Eliminate extra white spaces
Docs  <- tm_map(Docs , stripWhitespace)
# Text stemming - which reduces words to their root form
Docs  <- tm_map(Docs , stemDocument)

# Build a term-document matrix
Docs _dtm <- TermDocumentMatrix(Docs )
dtm_m <- as.matrix(Docs _dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 10 most frequent words
head(dtm_d, 10)

# Plot the most frequent words
barplot(dtm_d[1:10,]$freq, las = 2, names.arg = dtm_d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")
 

#generating word cloud
set.seed(1)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
 

# Sentiment Analysis
# Word Association :
# Find associations 
findAssocs(Docs _dtm, terms = c("good","work","health"), corlimit = 0.25)
# Find associations for words that occur at least 50 times
findAssocs(Docs _dtm, terms = findFreqTerms(Docs _dtm, lowfreq = 50), corlimit = 0.25)
# possibly creat a heat map ?

# run nrc sentiment analysis to return data frame with each row classified as one of the following
# emotions, rather than a score : 
# anger, anticipation, disgust, fear, joy, sadness, surprise, trust 
# and if the sentiment is positive or negative
d<-get_nrc_sentiment(text)

# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(text, method="syuzhet")

# see the first row of the vector
head(syuzhet_vector)

# see summary statisics of the vector
summary(syuzhet_vector)

# bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)

#affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)

#nrc
nrc_vector <- get_sentiment(text, method="nrc")
head(nrc_vector)
summary(nrc_vector)

#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector)),
  sign(head(nrc_vector))
)

# head(d,10) - just to see top 10 lines
head (d,10)

#transpose
td<-data.frame(t(d))

#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))

#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]

#Plot 1 - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar",fill=sentiment,ylab="count")+ggtitle("Survey sentiments")

 

##Topic Modeling
#latent dirichlet allocation (LDA) models are a widely used topic model
#Createt DTM
library(topicmodels)
articleDtm=DocumentTermMatrix(Docs,
                              control=list(minwordLength=3))
k=4   #If we need 4 topics to list out
SEED=1234
article.lda=LDA(articleDtm,k,method="Gibbs",control=list(seed=SEED))

lda.topics=as.matrix(topics(article.lda))

lda.topics
lda.terms=terms(article.lda)
lda.terms
