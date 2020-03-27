library(rtweet)
library(tidyr)
library(textdata)
library(dplyr)
library(base64enc)
library(httr)
library(jsonlite)
library(textstem)

consumer_key = 'Wr7LoVupBasUq2RiUL7ATeokw'
consumer_secret = 'CBKaasoX5X6bKASadjXiJ7FpHgglsARpX15j8MXqnpROIj74BA'
access_token = '1218990448481579011-sR3724S17aHQksVBJnSQ1IqMGErJBO'
access_secret = 'lrTmRP41fF0he7MYYyI7VkBlf6xhPw6zKYcX3WE32R9M9'


twitter_token <- create_token(
  app = "SMA_ZXH",
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret,
  set_renv=FALSE)


rt <- search_tweets(
  "verizon", n = 100000, retryonratelimit = TRUE
)

# We already saved the tweets we collected, 
#so instead of collecting the tweets again, we loaded the tweets we collected 

load("C:/Users/xzong/Desktop/MBD/SocialMediaAnalytics/group_project/rt_final.Rdata")
View(rt)
rt_origin <- rt


for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}


# Remove unuseful patterns.
Vtweets <- mutate(rt, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))


# Tokenization (+ going to lowercase)
Vtweets <- Vtweets %>% unnest_tokens(output = "word", 
                                                  input = text, 
                                                  token = "words", 
                                                  drop=FALSE,to_lower=TRUE) 

# Remove some other elements such as # and @ signs if they might occur
Vtweets <- filter(Vtweets, substr(word, 1, 1) != '#', 
                         substr(word, 1, 1) != '@') 

# remove stopwords
Vtweets <- Vtweets %>% anti_join(get_stopwords()) 

# lemmatizing
Vtweets <- Vtweets %>% mutate(word = lemmatize_words(word)) 

#Create the document-term matrix

Vtweets <- Vtweets %>% count(status_id,word)

VtweetsDTM <- Vtweets %>% cast_dtm(status_id,word,n,weighting = tm::weightTfIdf)

VtweetsDTMDense <- removeSparseTerms(VtweetsDTM,0.999)

VtweetsFreq <- Vtweets %>% group_by(word) %>% 
  summarize(freq = n()) %>%
  arrange(-freq)                  

View(VtweetsFreq)

# Build the word cloud on the word basis
if (!require("wordcloud")) {
  install.packages("wordcloud",repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require("wordcloud")
}

#create word cloud

wordcloud(VtweetsFreq$word, VtweetsFreq$freq,
          max.words=50,
          scale=c(3,1))


# Preprocessing for later sentiment analysis and topic modeling.

rt_texts <- mutate(rt, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

rt_texts_1 <- rt_texts %>% unnest_tokens(output = "word", 
                                                  input = text, 
                                                  token = "words", 
                                                  drop=FALSE,to_lower=TRUE)

rt_texts_2 <- filter(rt_texts_1, substr(word, 1, 1) != '#', 
                         substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags


rt_texts_4 <- rt_texts_2 %>% anti_join(get_stopwords())

rt_texts_5 <- rt_texts_4 %>% mutate(word = lemmatize_words(word)) 


#Sentiment analysis on the word basis.

get_sentiments("bing") %>% 
  count(sentiment)


rt_texts_51 <- inner_join(rt_texts_4,get_sentiments("bing"))
View(rt_texts_51)


summarySentiment <- rt_texts_51 %>%  count(word,sentiment,sort=TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%  
  arrange(n) %>%
  as.data.frame(stringsAsFactors=FALSE)


# Sentiment Analysis on the word basis
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")

summarySentiment %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


#Add negations words and reinforcement words into analysis

rt_texts_52 <- rt_texts_4 %>% group_by(status_id) %>% 
  mutate(prevword = lag(x=word,n=1)) %>% 
  ungroup()

negationWords <- get_stopwords()[c(81:98,165:167),"word"]

reinforcement <- get_stopwords()[c(160:161,171:174),"word"]


# Document Matrix with negations words and reinforcement words
sentiments_DM <- rt_texts_52 %>% inner_join(get_sentiments("afinn")) %>%
  mutate(correct_sentiment = ifelse(prevword %in% negationWords$word,-value,ifelse(prevword %in% reinforcement$word,1.5*value,value))) %>%
  group_by(status_id) %>%                      
  summarize(Sentiment = sum(correct_sentiment))


#Topic Modeling

rt_texts_5_topic <- rt_texts_5%>%count(status_id,word , sort=TRUE)%>%cast_dtm(document = status_id, term = word,
         value = n, weighting = tm::weightTf)

if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")

tweets_lda <- LDA(rt_texts_5_topic, k = 3,method="gibbs",control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 2:6) )

tweet_topics <- tidy(tweets_lda, matrix = "beta")
View(tweet_topics)


# Document matrix for each tweet.

top_tweet_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_tweet_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

tweet_documents <- tidy(tweets_lda, matrix = "gamma")

tweet_doc_topic <- tweet_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) 


tweet_doc_topic %>%
  group_by(topic) %>% 
  summarise(nbr_documents = n())

View(tweet_doc_topic)


#sentiment analysis with machine learning

# This machine learning model is trained on the labeled data set online for academic purpuose.
# Link: http://help.sentiment140.com/for-students

train_ini <- read.csv(file = "C:/Users/xzong/Desktop/MBD/SocialMediaAnalytics/group_project/training/training.csv",stringsAsFactors = FALSE)

set.seed(2)


# Rename some of the columns of the training data set.

train1 <- train_ini %>% 
  rename(
    id = X1467810369,
    text = X.switchfoot.http...twitpic.com.2y1zl...Awww..that.s.a.bummer...You.shoulda.got.David.Carr.of.Third.Day.to.do.it...D,
    value = X0
  )

# Split the data set into training set and test set.

train1 [,"value"] <- as.factor(train1 [,"value"] )
y <- as.factor(train1 [,"value"] )
levels(y)

p = 0.5

#The data set is too large, so we only selected 160000 tweets of it for the training.

class1_train <- sample(which(y==as.integer(levels(y)[1])), floor(p*table(y)[1]),replace=FALSE)
class2_train <- sample(which(y==as.integer(levels(y)[2])), floor(p*table(y)[2]),replace=FALSE)

class1_train_sub <- class1_train[c(1:40000)]
class2_train_sub <- class2_train[c(1:40000)]

class1_test_sub <- class1_train[c(40001:80000)]
class2_test_sub <- class2_train[c(40001:80000)]

training_locations <- c(class1_train_sub,class2_train_sub) 
test_locations <- c(class1_test_sub,class2_test_sub) 

trainingSET <- train1[training_locations,]
testSET <- train1[test_locations,]

View(trainingSET)
View(testSET)

train_dtm <- trainingSET %>% unnest_tokens(output = "word",
  input = text,
  token = "words",
  drop=FALSE,to_lower=TRUE) %>%  
  anti_join(get_stopwords()) %>%
  count(id,word, sort=TRUE)%>%
  cast_dtm(document = id, term = word,
           value = n, weighting = tm::weightTf)

# Collect all of the distinct terms in the training set.
train_vocab <- tidy(train_dtm) %>%
  distinct(term) 

View(train_vocab)

test_table <- testSET %>% unnest_tokens(output = "word",
                                         input = text,
                                         token = "words",
                                         drop=FALSE,to_lower=TRUE) %>%  
  anti_join(stop_words,lexicon="snowball") %>%
  count(id,word , sort=TRUE)

# Remove the terms that appeared only in the test set.

test_table <- test_table %>%
  right_join(train_vocab,by=c("word"="term"))

test_dtm <- test_table %>% 
  arrange(desc(id)) %>% 
  mutate(id = ifelse(is.na(id), first(id), id),
         n = ifelse(is.na(n), 0, n)) %>% 
  cast_dtm(document=id, term=word, value=n)

in_dtm <- rownames(test_dtm) 

View(in_dtm)

# find the ones that are not in that set
lostids <- testSET[! testSET$id %in% in_dtm,"id"] 
View(lostids)

# create a new matrix of the same length as the number of missing ids, and the same number of columns 
test_dtm <- rbind(test_dtm,matrix(data=0,nrow=length(lostids),ncol=ncol(test_dtm)))

# add the rownames of the originally missing observations as well
rownames(test_dtm)[(nrow(test_dtm)-length(lostids)+1):nrow(test_dtm)] <- paste(lostids)

# Apply dimension reduction to the training set

if (!require("Matrix")) install.packages("Matrix", quiet=TRUE) ; require("Matrix")

dtm.to.sm <- function(dtm) {sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v,dims=c(dtm$nrow, dtm$ncol))}

train=dtm.to.sm(train_dtm)

View(train)

# Apply singular value decomposition

if (!require("irlba")) install.packages("irlba", quiet=TRUE) ; require("irlba")

k = 200
  
trainSVD <- irlba(t(train), nu=k, nv=k)

train <- as.data.frame(trainSVD$v)


# add our rownames again, as a columns, in order to be able to merge
train <- data.frame(cbind(id = rownames(train_dtm),train),stringsAsFactors=FALSE)

# Apply dimension reduction to the test set

test=dtm.to.sm(test_dtm)

# use the SVD made on the training set to use the same loadings on the terms in the test set

test <- as.data.frame(as.matrix(test %*% trainSVD$u %*%  solve(diag(trainSVD$d))))

# add our rownames again, as columns, in order to be able to merge
test <- cbind(id = rownames(test_dtm),test,stringsAsFactors=FALSE)


# merge with other columns we need

train <- merge(trainingSET[,c("id","value")],train)
View(train)
test <- merge(testSET[,c("id","value")],test)
View(test)

## Apply Random Forest

if (!require("randomForest")) install.packages("randomForest", quiet=TRUE) ; require("randomForest")

RF_model_train <- randomForest(x=train[,3:ncol(train)],y=train[,2],importance=TRUE,ntree=500)
RF_predict <- predict(RF_model_train,test[,3:ncol(test)],type = "prob")[,2]

RF_predict_resp <- predict(RF_model_train,test[,2:dim(test)[[2]]],type = "response")

View(RF_predict_resp)

# Calculate auc
if (!require("ROCR")) install.packages("ROCR", quiet=TRUE) ; require("ROCR")

predML <- prediction(RF_predict,test[,2])

# ROC curve
perfML <- performance(predML,"tpr","fpr")
plot(perfML)
abline(0,1)

## auc
auc.perfML = performance(predML, measure = "auc")
auc.perfML@y.values


#Apply the model to the tweets we colleceted

rt_table <- rt %>% 
  rename(
    id = status_id,
  )

rt <- rt %>% 
  rename(
    id = status_id,
  )

View(rt)
rt_table <- rt_table %>% unnest_tokens(output = "word",
                                        input = text,
                                        token = "words",
                                        drop=FALSE,to_lower=TRUE) %>%  
  anti_join(stop_words,lexicon="snowball") %>%
  count(id,word , sort=TRUE)

#Remove the words only appear in the training data set
rt_table <- rt_table %>%
  right_join(train_vocab,by=c("word"="term"))


rt_dtm <- rt_table %>% 
  arrange(desc(id)) %>% 
  mutate(id = ifelse(is.na(id), first(id), id),
         n = ifelse(is.na(n), 0, n)) %>% 
  cast_dtm(document=id, term=word, value=n)

in_dtm <- rownames(rt_dtm) 

lostids <- rt[! rt$id %in% in_dtm,"id"] 
View(lostids)

# create a new matrix of the same length as the number of missing ids, and the same number of columns 
rt_dtm <- rbind(rt_dtm,matrix(data=0,nrow=length(lostids),ncol=ncol(rt_dtm)))

# add the rownames of the originally missing observations as well
rownames(rt_dtm)[(nrow(rt_dtm)-length(lostids)+1):nrow(rt_dtm)] <- paste(lostids)

rt_ana=dtm.to.sm(rt_dtm)
# use the SVD made on the training set to use the same loadings on the terms in the test set

rt_ana <- as.data.frame(as.matrix(rt_ana %*% trainSVD$u %*%  solve(diag(trainSVD$d))))

# add our rownames again, as columns, in order to be able to merge
rt_ana <- cbind(id = rownames(rt_dtm),rt_ana,stringsAsFactors=FALSE)

rt_ana <- merge(rt[,c("id","user_id")],rt_ana)
View(rt_ana)

RF_predict <- predict(RF_model_train,rt_ana[,3:ncol(rt_ana)],type = "prob")[,2]

View(RF_predict)

RF_predict_resp <- predict(RF_model_train,rt_ana[,2:dim(rt_ana)[[2]]],type = "response")

View(RF_predict_resp)

#Merge the original tweets with predications

rt_ana_final <- bind_cols(id = rt_ana$id,result = RF_predict_resp)
View(rt_ana_final)

#Join the predications with the original tweets

tweets_result<- inner_join(rt_ana_final, rt, by = c("id" = "id")) 
View(tweets_result)
save(tweets_result, file = "tweets_result2.Rdata")

library(data.table)
dt <- as.data.table(tweets_result) 

#Get the date of the tweets
dt$date <- as.Date(dt$created_at)

#Change the predications into the numeric values
dt$result <- as.numeric(as.character(dt$result))

#Group the tweets by date, so we can caculate the average sentimens predications
dt_ana <- dt[order(date)][,.(Total_Result = mean(result)),by = date]
View(dt_ana)

# Sentiment Machine Learning Model for the tweets from Verizon offcial account

tmls <- get_timelines(c("verizon"), n = 5000)

# We already saved the tweets we collected, so instead of collecting the tweets again, we loaded the tweets we collected 
load("C:/Users/xzong/Desktop/MBD/SocialMediaAnalytics/group_project/tmls.Rdata")

# Preprocessing for the offcial tweets as we did on the previous data set 
rt <- tmls %>% 
  rename(
    id = status_id,
  )

rt_table <- tmls %>% 
  rename(
    id = status_id,
  )

rt_table <- rt_table %>% unnest_tokens(output = "word",
                                       input = text,
                                       token = "words",
                                       drop=FALSE,to_lower=TRUE) %>%  
  anti_join(stop_words,lexicon="snowball") %>%
  count(id,word , sort=TRUE)


rt_table <- rt_table %>%
  right_join(train_vocab,by=c("word"="term"))

rt_dtm <- rt_table %>% 
  arrange(desc(id)) %>% 
  mutate(id = ifelse(is.na(id), first(id), id),
         n = ifelse(is.na(n), 0, n)) %>% 
  cast_dtm(document=id, term=word, value=n)

in_dtm <- rownames(rt_dtm) 

View(in_dtm)

# find the ones that are not in that set
lostids <- rt[! rt$id %in% in_dtm,"id"] 
View(lostids)

# create a new matrix of the same length as the number of missing ids, and the same number of columns 
rt_dtm <- rbind(rt_dtm,matrix(data=0,nrow=length(lostids),ncol=ncol(rt_dtm)))

# add the rownames of the originally missing observations as well
rownames(rt_dtm)[(nrow(rt_dtm)-length(lostids)+1):nrow(rt_dtm)] <- paste(lostids)

# also convert to a sparse matrix

rt_ana=dtm.to.sm(rt_dtm)
# use the SVD made on the training set to use the same loadings on the terms in the test set

rt_ana <- as.data.frame(as.matrix(rt_ana %*% trainSVD$u %*%  solve(diag(trainSVD$d))))

# add our rownames again, as columns, in order to be able to merge
rt_ana <- cbind(id = rownames(rt_dtm),rt_ana,stringsAsFactors=FALSE)


rt_ana <- merge(rt[,c("id","user_id")],rt_ana)
View(rt_ana)

## Apply Random Forest

RF_predict <- predict(RF_model_train,rt_ana[,3:ncol(rt_ana)],type = "prob")[,2]
# This returns the probabilities, which is more useful for the evaluation measures

View(RF_predict)

RF_predict_resp <- predict(RF_model_train,rt_ana[,2:dim(rt_ana)[[2]]],type = "response")

View(RF_predict_resp)

#Merge the original tweets with results 

rt_ana_final <- bind_cols(id = rt_ana$id,result = RF_predict_resp)
View(rt_ana_final)

#Join the the predications with the origianl tweets
Verizon_Tweets_result<- inner_join(rt_ana_final, tmls, by = c("id" = "status_id")) 

View(Verizon_Tweets_result)
save(Verizon_Tweets_result, file = "Verizon_Tweets_result2.Rdata")

dt <- as.data.table(tweets_result) 

#Get the date of the tweets
dt$date <- as.Date(dt$created_at)

#Change the predication results to numeric values.
dt$result <- as.numeric(as.character(dt$result))

#Group the tweets so we can have a average sentiments result for each day
dt_ana <- dt[order(date)][,.(Total_Result = mean(result)),by = date]
View(dt_ana)

stock_ana_vz <- dt_ana


#Stock Price analysis with all of the relevant tweets we collected
library(quantmod)

# Collect the stock price from yahoo 
getSymbols("VZ",from="2020-01-22",to="2020-02-01")
vz <- as.data.table(VZ) 

# Merge the price with the sentiment analysis results.
stock_ana <- inner_join(dt_ana,vz,by = c("date" = "index"))
View(stock_ana)

#Stock Price analysis with all of the official tweets from Verizon
getSymbols("VZ",from="2019-03-11",to="2020-02-01")

vz1 <- as.data.table(VZ) 

# Join the stock price with the sentiments results.
stock_ana_1 <- inner_join(stock_ana_vz,vz1,by = c("date" = "index"))


#Here we multiply the sentiments result by 20, so it could be in the same scale
# as the stock price.

ggplot(stock_ana_1, aes(x=date)) + 
  geom_line(aes(y = VZ.Close), color = "darkred") + 
  geom_line(aes(y = Total_Result*20), color="steelblue")+
  labs(subtitle="", 
       y=" ", 
       x="Date", 
       title="Stock Price Vs Sentiments", 
       caption = "Redline: stock price; Blueline: sentiment result")



#Reply Analysis

library(data.table)

Verizon_T <- as.data.table(tmls)
Verizon_T$date <- as.Date(Verizon_T$created_at)

# here we inner join the tweets from the offcial Verizon acccount and all of the tweets
# we collected by the screen name, so we can have the replies from the offcial account to 
# the clients who mentioned the Verizon.

reply_ana1 <- inner_join(Verizon_T,rt_origin , by = c("reply_to_screen_name" = "screen_name")) 

#Remove the mentions tweets from Verizon offcial accounts
reply_ana1 <- reply_ana1[reply_ana1$user_id.x != reply_ana1$user_id.y,]


# Join the the offcail tweets from Verizon with the predication results.
merge <- tweets_result[,c("id","result")]
reply_analysis<- inner_join(reply_ana1, merge, by = c("status_id.y" = "id")) 
View(reply_analysis)


#Favorite analyis 
Favorite_ana <- tweets_result

#Get the date of the tweets
Favorite_ana$date <- as.Date(Favorite_ana$created_at)

#Change the predication results to numeric values.
Favorite_ana$result <- as.numeric(as.character(Favorite_ana$result))
View(Favorite_ana)

Favorite_ana <- as.data.table(Favorite_ana) 

#Group the tweets by date and caculate the average sentiments results.
Favorite_ana_date <- Favorite_ana[order(date)][,.(result = mean(result),favourites_count = log10(mean(favourites_count))),by = date]

#Plot the average sentiments results and the average favourites in a graph.
ggplot(Favorite_ana_date, aes(x=date)) + 
  geom_line(aes(y = result), color="darkred")+
  geom_line(aes(y = favourites_count), color="steelblue")+
  labs(subtitle="", 
       y=" ", 
       x="Date", 
       title="Favourites Vs Sentiments", 
       caption = "Redline: Sentiment Result; Blueline: Favourites Count")

View(Favorite_ana_date)

#Get the date of when those tweets accounts are created.
Favorite_ana$account_date <- as.Date(Favorite_ana$account_created_at)

#Group those accounts together by the registration year.
Favorite_ana_AccountDate <- Favorite_ana[order(account_date)][,.(result = mean(result),favourites_count = log10(mean(favourites_count)),Observations = .N),by = year(account_date)]

ggplot(Favorite_ana_AccountDate, aes(x= year,y= Observations)) +
  geom_bar(stat="identity",fill = "steelblue")+
  labs(title="Tweets Amount of Users of Different Registration Date", y ="Tweets Amount", x ="Registration Year" ) +
  geom_text(hjust=-0.55, size = 3, label = Favorite_ana_AccountDate$Observations)


# Amount comparison between tweets from verified account and unverified account
Favorite_ana_Verified <- Favorite_ana[order(date)][,.(result = mean(result),favourites_count = log10(mean(favourites_count)),Observations = .N),by = verified]
View(Favorite_ana_Verified)
Verified <- ggplot(data = Favorite_ana_Verified, mapping = aes(x = 'verified', y = Observations, fill = verified)) + geom_bar(stat = 'identity', position = 'stack', width = 1)
Verified + coord_polar(theta = 'y')


#Positive and nagative resluts comparison.

Favorite_ana_result_com <- Favorite_ana[order(date)][,.(favourites_count = log10(mean(favourites_count)),Observations = .N),by = result]
View(Favorite_ana_result_com)
Favorite_ana_result_com$result <- as.character(Favorite_ana_result_com$result)
result_com <- ggplot(data = Favorite_ana_result_com, mapping = aes(x = 'result', y = Observations, fill = result)) + geom_bar(stat = 'identity', position = 'stack', width = 1)
result_com + coord_polar(theta = 'y')



#Comparison betweem the directly mentioned tweets, undirectly mentioned tweets and the tweets from Verizon. 
Favourite_ana_mention <- Favorite_ana
Favourite_ana_mention[Favourite_ana_mention$screen_name == "verizon","Mention"] <- "From Verizon"
Favourite_ana_mention[Favourite_ana_mention$text %like% "@verizon" & Favourite_ana_mention$screen_name!= "verizon","Mention"] <- "Directly Mentioned"
Favourite_ana_mention[!Favourite_ana_mention$text %like% "@verizon" & Favourite_ana_mention$screen_name!= "verizon","Mention"] <- "Undirectly Mentioned"

#Caculate the amount based different types.
Favourite_ana_mention <- Favourite_ana_mention[order(date)][,.(favourites_count = log10(mean(favourites_count)),Observations = .N),by = Mention]

mention <- ggplot(data = Favourite_ana_mention, mapping = aes(x = 'Mention', y = Observations, fill = Mention)) + geom_bar(stat = 'identity', position = 'stack', width = 1)
mention + coord_polar(theta = 'y')


# Influence analysis
influence_ana <- Favorite_ana[Favorite_ana$screen_name != "verizon",]

View(influence_ana$followers_count)

#Divied the tweets by followers of the users into 10 gourps.
influence_ana$followers <- ifelse(influence_ana$followers_count >6000,">6000",
                          ifelse(influence_ana$followers_count >5000,">5000",
                                 ifelse(influence_ana$followers_count >4000,">4000",
                                        ifelse(influence_ana$followers_count >3000,">3000",
                                               ifelse(influence_ana$followers_count >2000,">2000",
                                                      ifelse(influence_ana$followers_count >1000,">1000",
                                                             ifelse(influence_ana$followers_count >800,">800",
                                                                    ifelse(influence_ana$followers_count >600,">600",
                                                                           ifelse(influence_ana$followers_count >400,">400",
                                                                                  ifelse(influence_ana$followers_count >200,">200","< 200")))))))))) 


#Group the tweets based on different followers range.
influence_ana_1 <- influence_ana[,.(Followers_Amount = .N),by = followers]
View(influence_ana_1)

#Divied the tweets by friends of the users into 10 gourps.
influence_ana$friends <- ifelse(influence_ana$friends_count >6000,">6000",
                                  ifelse(influence_ana$friends_count >5000,">5000",
                                         ifelse(influence_ana$friends_count >4000,">4000",
                                                ifelse(influence_ana$friends_count >3000,">3000",
                                                       ifelse(influence_ana$friends_count >2000,">2000",
                                                              ifelse(influence_ana$friends_count >1000,">1000",
                                                                     ifelse(influence_ana$friends_count >800,">800",
                                                                            ifelse(influence_ana$friends_count >600,">600",
                                                                                   ifelse(influence_ana$friends_count >400,">400",
                                                                                          ifelse(influence_ana$friends_count >200,">200","< 200")))))))))) 

ggplot(influence_ana_1, aes(x= followers,y= Followers_Amount)) +
  geom_bar(stat="identity",fill = "steelblue")+
  labs(title="Influence Analysis - Followers", y ="Tweets Amount", x ="Follower Range" ) +
  geom_text(hjust=-0.55, size = 3, label = influence_ana_1$Followers_Amount)+
  coord_flip()

influence_ana_2 <- influence_ana[,.(Friends_Amount = .N),by = friends]

save(influence_ana_2, file ="influence_ana_2.Rdata")

ggplot(influence_ana_2, aes(x= friends ,y= Friends_Amount)) +
  geom_bar(stat="identity",fill = "steelblue")+
  labs(title="Influence Analysis - Friends", y ="Tweets Amount", x ="Friends Range" ) +
  geom_text(hjust=-0.55, size = 3, label = influence_ana_2$Friends_Amount)+
  coord_flip()
