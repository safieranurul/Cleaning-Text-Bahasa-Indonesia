#====================================================================================
#OTW Cleaning
library(tm)

data.cleaning=read.csv("file:///F:/BLOG/5. Cleaning Text Data/Jalan Malioboro Reviews.csv",header=T)
#Mengubah Data Menjadi Vector
data.cleaning=data.cleaning$Review
data.cleaning.text=Corpus(VectorSource(data.cleaning))

##Cleaning data
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
clean <- tm_map(data.cleaning.text, removeURL)

removeNL <- function(y) gsub("\n", " ", y)
clean <- tm_map(clean, removeNL)

removepipe <- function(z) gsub("<[^>]+>", "", z)
clean <- tm_map(clean, removepipe)

remove.mention <- function(z) gsub("@\\S+", "", z)
clean <- tm_map(clean, remove.mention)

remove.hashtag <- function(z) gsub("#\\S+", "", z)
clean <- tm_map(clean, remove.hashtag)

removeamp <- function(y) gsub("&amp;", "", y)
clean <- tm_map(clean, removeamp)

removetitik3 <- function(y) gsub("[[:punct:]]", "", y)
clean <- tm_map(clean, removetitik3)

remove.all <- function(xy) gsub("[^[:alpha:][:space:]]*", "", xy)
clean <- tm_map(clean,remove.all)

clean <- tm_map(clean, tolower)

#remove extra whitespace (spasi)
clean <- tm_map(clean, stripWhitespace)

##load stopword-ID
stopwordID <- "F:/BLOG/5. Cleaning Text Data/ID-Stopwords.txt"
##membaca stopwordID perbaris
cStopwordID<-readLines(stopwordID);

#load slangword
slang <- read.csv("file:///F:/BLOG/5. Cleaning Text Data/Slangword.csv", header=T)
old_slang <- as.character(slang$old) 
new_slang <- as.character(slang$new)
#load stemming
stemm <- read.csv("file:///F:/BLOG/5. Cleaning Text Data/Stemming.csv", header=T)
old_stemm <- as.character(stemm$old)
new_stemm <- as.character(stemm$new)
#load lemmatization
lemma <- read.csv("file:///F:/BLOG/5. Cleaning Text Data/Lemmatization.csv", header=T)
old_lemma <- as.character(stemm$old)
new_lemma <- as.character(stemm$new)

stemmword <- function(x) Reduce(function(x,r) gsub(stemm$old[r],stemm$new[r],x,fixed=T),
                                seq_len(nrow(stemm)),x)
clean <- tm_map(clean,stemmword)
slangword <- function(x) Reduce(function(x,r) gsub(slang$old[r],slang$new[r],x,fixed=T),
                                seq_len(nrow(slang)),x)
clean <- tm_map(clean,slangword)
lemmatization <- function(x) Reduce(function(x,r) gsub(lemma$old[r],lemma$new[r],x,fixed=T),
                                seq_len(nrow(lemma)),x)
clean <- tm_map(clean,lemmatization)

clean <- tm_map(clean, removeWords, cStopwordID)
writeLines(strwrap(clean[[2]]$content, 100))

## save data
dataframe=data.frame(text=unlist(sapply(clean, `[`)), stringsAsFactors=F)
View(dataframe)
write.csv(dataframe,file = 'F:/BLOG/5. Cleaning Text Data/DataClean.csv')

###------------CARI KATA------------
library(tm)
clean.for.find.word=read.csv('F:/BLOG/5. Cleaning Text Data/DataClean.csv', header = T)
View(clean.for.find.word)
clean.for.find.word=clean.for.find.word$text
find.word<- Corpus(VectorSource(clean.for.find.word))

writeLines(strwrap(find.word[[1]]$content, 100))

myCorpus=find.word
tdm <- TermDocumentMatrix(myCorpus,
                          control = list(wordLengths = c(1, Inf)))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 10)
df <- data.frame(term = names(term.freq), freq = term.freq)
df <- df[with(df, order(-freq)), ]
n=dim(df)[1]
df <- data.frame(no=1:n, df)
View(df)

#wordcloud
library(wordcloud2)
words <- data.frame(word = names(term.freq), freq = term.freq)
wordcloud2(words, figPath = "jogja.png")
