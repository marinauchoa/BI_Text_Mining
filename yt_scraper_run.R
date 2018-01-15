# Author: Marina Ferreira Uchoa - h16mferr@du.se

### Packages for YouTube sraping ###
# install.packages("httr")
# install.packages("xlsx")

### Packages for textual analysis ###
# install.packages("devtools")
require(devtools)
install_url("http://www.omegahat.net/Rstem/Rstem_0.4-1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.1.tar.gz")
install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
# install.packages("ggplot2")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("tm")
p_list<-c("httr","xlsx","sentiment","ggplot2", "wordcloud","RColorBrewer","tm")
lapply(p_list,require, character.only=T)


##########################################################
################## GET YOUTUBE COMMENTS ##################
##########################################################
# main code source:
# http://stackoverflow.com/questions/29692972/youtube-comment-scraper-returns-limited-results

# create youtube scraper object
setwd("C:\\Users\\uchoa\\OneDrive\\Documents\\DU\\v17\\DT3018 - BI\\Natural_Language_Processing")
source("yt_scraper.R")
rObj <- yt_scraper()
# check it is still empty
rObj$data
rObj$unique_count
# scrape comments from video - Worth It - Salmon episode (ID V9eeg8d9XEg)
rObj$scrape_all()
# check it is not empty anymore
rObj$unique_count
length(rObj$data)
# get top 6 observations
head(rObj$core_df)
# save comments to file
write.xlsx(rObj$core_df, "yt_comments.xlsx")
# save comments to specific data frame
comments <- rObj$core_df

####################################################
################## CLEAN COMMENTS ##################
####################################################

# remove parts of comments with unicode that are not letters, spaces and dollar signs
# http://stackoverflow.com/questions/8697079/remove-all-punctuation-except-apostrophes-in-r
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
comments$Comment <- gsub("[^[:alpha:][:space:]$]", "",  comments$Comment)

# all comments into lowercase letters
comments$Comment <- tolower(comments$Comment)

# remove comments with 0 characters
nrow(comments)
for (i in comments$Comment){
  if (nchar(i) == 0) comments <- comments[!(comments$Comment==i),]
}
nrow(comments)
## it went from 11155 to 11137 comments

#########################################################
################## CLASSIFY SENTIMENTS ##################
#########################################################
# main code source:
# https://www.r-bloggers.com/intro-to-text-analysis-with-r/
# classify emotions and polarity
# functions classify_emotion and classify_polarity give the log likelihood for each 
# possible sentiment and then determines the most likely
class_emo = classify_emotion(comments$Comment, algorithm="bayes", prior=1.0) 
emotion = class_emo[,7]
# if you want unclassified comments to show up in the graph
# emotion[is.na(emotion)] = "unknown"
rm(class_emo)
class_pol = classify_polarity(comments$Comment, algorithm="bayes")
polarity = class_pol[,4]
rm(class_pol)

# create data set with all comments and their respective sentiments and polarity
sent_df = data.frame(text=comments$Comment, emotion=as.factor(emotion),
                     polarity=as.factor(polarity), stringsAsFactors=FALSE)

###########################################
################## PLOTS ##################
###########################################

# sentiment bar plot
ggplot(sent_df[!(is.na(sent_df$emotion)),], aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="Emotion Categories", y="# of Occurrences")
# polarity bar plot
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdBu") +
  labs(x="Polarity Categories", y="# of Occurrences")

#### WORDCLOUDS ####
# of all comments (no sentiment classes)
docs1<-removeWords(comments$Comment, stopwords("english"))
freq=termFreq(docs1)
words <- names(freq)
dev.new()
wordcloud(words, freq, scale = c(6,1.5), random.order = FALSE, 
          max.words=650, colors = rev(brewer.pal(8,"Paired")))

# sentiment
emos = levels(sent_df$emotion)
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo){
  tmp = na.omit(sent_df$text[emotion == emos[i]])
  emo.docs[i] = paste(tmp, collapse=" ")
}
rm(tmp)
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos
dev.new()
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(6,1), random.order = FALSE,
                 title.size = 1.5, max.words=650)
# sadness and joy (most common sentiments)
corpus = Corpus(VectorSource(emo.docs[4:5]))
tdm2 = TermDocumentMatrix(corpus)
tdm2 = as.matrix(tdm2)
colnames(tdm2) = emos[4:5]
dev.new()
comparison.cloud(tdm2, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(6,1.5), random.order = FALSE,
                 title.size = 1.5, max.words=650)
# polarity
emos = levels(sent_df$polarity)
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo){
  tmp = na.omit(sent_df$text[polarity == emos[i]])
  emo.docs[i] = paste(tmp, collapse=" ")
}
rm(tmp)
emo.docs = removeWords(emo.docs, stopwords("english"))
corpus = Corpus(VectorSource(emo.docs))
tdm3 = TermDocumentMatrix(corpus)
tdm3 = as.matrix(tdm3)
colnames(tdm3) = emos
dev.new()
comparison.cloud(tdm2, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(6,1.5), random.order = FALSE,
                 title.size = 2, max.words=650)

##########################################################
################## SUPPORT FOR ANALYSIS ##################
##########################################################
freq[words=="adam"]
freq[words=="camera"]
freq[words=="salmon"]
