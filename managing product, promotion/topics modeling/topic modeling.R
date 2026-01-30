#############################
# Structural Topic Modeling #
#############################

## Install Packages (if needed)
install.packages("stm")
install.packages("tm")
install.packages("Rtsne")
install.packages("rsvd")
install.packages("geometry")
install.packages("SnowballC")
install.packages("wordcloud")

## Load Packages and Set Seed
library(stm)
library(tm)
library(Rtsne)
library(rsvd)
library(geometry)
library(SnowballC)
library(wordcloud)
set.seed(1)

## Read in the product reviews data
reviews <- read.csv(file.choose())  ## Choose the file reviews_data.csv
head(reviews)

## Run if Mac
reviews$comments <- iconv(reviews$comments, to = "utf-8-mac")

## Process Documents
customwords = c("watch", "iphone", "apple")
processed <- textProcessor(reviews$review_text, metadata = reviews, 
    customstopwords=customwords)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

## Determine Number of Topics (Takes Significant Time)
reviewsFit <- stm(documents = out$documents, vocab = out$vocab, K = 0, seed = 1,
  prevalence =~ review_star, data = out$meta, init.type = "Spectral")

## See how many topics
num_topics <- reviewsFit$settings$dim$K
num_topics

## See which topics relate to high vs. low ratings
out$meta$rating <- as.factor(out$meta$review_star)
prep <- estimateEffect(1:num_topics ~ rating, reviewsFit, meta=out$meta, 
     uncertainty="Global")
plot(prep, covariate="rating", topics=c(1:num_topics), model=reviewsFit, 
     method="difference", cov.value1=5, cov.value2=1,
     xlab="Lower Rating ... Higher Rating", main="Relationship between Topic and Rating",
     labeltype ="custom", custom.labels=c(1:num_topics))

## Visualize Topics

## Positive Topics - Top 3
cloud(reviewsFit, topic=26)
cloud(reviewsFit, topic=12)
cloud(reviewsFit, topic=29)

## Negative Topics - Bottom 3
cloud(reviewsFit, topic=18)
cloud(reviewsFit, topic=41)
cloud(reviewsFit, topic=40)