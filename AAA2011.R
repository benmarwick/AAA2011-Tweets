# code in DMAR chapter

# This code was last run sucessfully on Nov 2013 in the this environment:
# R version 3.0.2 (2013-09-25)
# Platform: x86_64-w64-mingw32/x64 (64-bit)

# and these packages:

# [1] topicmodels_0.1-12 slam_0.1-30        pvclust_1.2-2      plyr_1.8           Snowball_0.0-10   
# [6] rJava_0.9-4        tm_0.5-9.1         Matrix_1.1-0       sna_2.3-1          igraph_0.6.6      
# [11] ggplot2_0.9.3.1    stringr_0.6.2

# get package with functions for interacting with Twitter.com
require(twitteR) 
# get 1500 tweets with #aaa2011 tag, note that 1500 is the max, and it's subject to filtering and other restrictions by Twitter
aaa2011 <- searchTwitter('#AAA2011', n=1500) 
# convert to data frame
df <- do.call("rbind", lapply(aaa2011, as.data.frame)) 
# get column names to see structure of the data
names(df) 
# look at the first three rows to check content
head(df,3) 

# Note that the code accompanying this chapter allows for the reader to 
# collect their own data directly from Twitter. However, the Twitter API changes
# frequently, causing difficulties in reproducing the results of this code. To 
# improve the reproducbility of this study, the  data used in this chapter 
# are also available as a CSV file. The data are available at 
# http://dx.doi.org/10.6084/m9.figshare.855630

# The following three lines will read in the 
# CSV file to the workspace read for further analysis
 
dir <- "..." # full path to the folder on your computer containing the file
file <- "Marwick_DMAR-chapter-3.csv" # name of the CSV file
df <- read.csv(paste(dir, file, sep = "/"), stringsAsFactors = FALSE)

# see how many unique Twitter accounts in the sample
length(unique(df$screenName)) 


# Create a new column of random numbers in place of the usernames and redraw the plots
# find out how many random numbers we need
n <- length(unique(df$screenName))
# generate a vector of random number to replace the names, we'll get four digits just for convenience 
randuser <- round(runif(n, 1000, 9999),0)
# match up a random number to a username
screenName <- unique(df$screenName)
screenName <- sapply(screenName, as.character)
randuser <- cbind(randuser, screenName)
# Now merge the random numbers with the rest of the Twitter data, and match up the correct random numbers with multiple instances of the usernames...
rand.df  <-  merge(randuser, df, by="screenName")


# determine the frequency of tweets per account
counts <- table(rand.df$randuser)
# create an ordered data frame for further manipulation and plotting
countsSort <- data.frame(user = unlist(dimnames(counts)), count = sort(counts, decreasing = TRUE), row.names = NULL)
# create a subset of those who tweeted at least 5 times or more
countsSortSubset <- subset(countsSort,countsSort$count > 0)
# extract counts of how many tweets from each account were retweeted
# first clean the twitter messages by removing odd characters
rand.df$text <- sapply(rand.df$text,function(row) iconv(row,to = 'UTF-8')) 
# remove @ symbol from user names
trim <- function (x) sub('@','',x) 
# pull out who the message is to
require(stringr)
rand.df$to <- sapply(rand.df$text, function(name) trim(name)) 
# extract who has been retweeted
rand.df$rt <- sapply(rand.df$text, function(tweet) 
  trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2])) 
# replace names with corresponding anonymising number 
randuser <- data.frame(randuser)
rand.df$rt.rand <- as.character(randuser$randuser)[match(as.character(rand.df$rt), 
                                                         as.character(randuser$screenName))]
# make a table with anonymised IDs and number of RTs for each account
countRT <- table(rand.df$rt.rand)
countRTSort <- sort(countRT)
# subset those people RTd at least twice
countRTSortSubset <- subset(countRTSort,countRT>2)
# create a data frame for plotting
countRTSortSubset.df <-data.frame(user = as.factor(unlist(dimnames(countRTSortSubset))), RT_count = as.numeric(unlist(countRTSortSubset)))
# combine tweet and retweet counts into one data frame
countUser <- merge(randuser, countsSortSubset, by.x = "randuser", by.y = "user")
TweetRetweet <- merge(countUser, countRTSortSubset.df, by.x = "randuser", by.y = "user", all.x = TRUE)
# create a Cleveland dot plot of tweet counts and retweet counts per Twitter account
# solid data point = number of tweets, letter R = number of retweets
require(ggplot2)
require(grid)
ggplot() +  
  geom_point(data = TweetRetweet, mapping =  aes(reorder(randuser, count), count), size = 3) + 
  geom_point(data = TweetRetweet, mapping =  aes(randuser, RT_count), size = 4, shape = "R") +
  xlab("Author") + 
  ylab("Number of messages") + 
  coord_flip() + 
  theme_bw() + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) +
  theme(axis.title.y = element_text(size = 14, angle=90)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))


# calculate the number of followers of each Twitter account
# extract the usernames from the non-anonymised dataset
# Note that this may take some time! And that's it's dependant on the Twitter
# API, which changes frequently.
users <- unique(df$screenName)
users <- sapply(users, as.character)
# make a data frame for further manipulation 
users.df <- data.frame(users = users, followers = "", stringsAsFactors = FALSE)
# loop to populate users$followers with a follower count obtained from Twitter API
for (i in 1:nrow(users.df)) 
{
  # tell the loop to skip a user if their account is protected 
  # or some other error occurs  
  result <- try(getUser(users.df$users[i])$followersCount, silent = FALSE);
  if(class(result) == "try-error") next;
  # get the number of followers for each user
  users.df$followers[i] <- getUser(users.df$users[i])$followersCount
  # tell the loop to pause for 60 s between iterations to 
  # avoid exceeding the Twitter API request limit
  # this is going to take a long time if there are a lot
  # of users, good idea to let it run overnight
  print('Sleeping for 60 seconds...')
  Sys.sleep(60); 
}
# merge follower count with number of tweets per author
followerCounts <- merge(TweetRetweet, users.df, by.x = "screenName", by.y = "users")
# convert to value to numeric for further analysis
followerCounts$followers <- as.numeric(followerCounts$followers)
followerCounts$counts <-    as.numeric(followerCounts$counts)

# create a plot
ggplot(data = followerCounts, aes(count, followers)) +
  geom_text(aes(label = randuser, size = RT_count)) +
  scale_size(range=c(3,10)) +
  scale_x_log10(breaks = c(10,20,40,60,80,100)) + 
  scale_y_log10(breaks = c(10,100,seq(1000,7000,1000))) +
  xlab("Number of Messages") + 
  ylab("Number of Followers") + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) +
  theme(axis.title.y = element_text(size = 14, angle=90)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))



# Ratio of retweets to tweets for some more insights
#
# make table with counts of tweets per person
t <- as.data.frame(table(rand.df$randuser))  
# make table with counts of retweets per person
rt <- as.data.frame(table(rand.df$rt.rand))  
# combine tweet count and retweet count per person
t.rt <- merge(t,rt,by="Var1") 
# creates new col and adds ratio tweet/retweet
t.rt["ratio"] <- t.rt$Freq.y / t.rt$Freq.x 
# sort it to put names in order by ratio
sort.t.rt <- t.rt[order(t.rt$ratio),] 
# exclude those with 2 tweets or less
sort.t.rt.subset <- subset(sort.t.rt,sort.t.rt$Freq.x>2) 
#
# drop unused levels leftover from subsetting 
sort.t.rt.subset.drop <- droplevels(sort.t.rt.subset) 
# plot nicely ordered counts of tweets by person for 
# people > 5 tweets
ggplot(sort.t.rt.subset.drop, aes(reorder(Var1, ratio), ratio)) + 
  xlab("Author") + 
  ylab("Ratio of messages retweeted by others to original messages")+ 
  geom_point() + 
  coord_flip() + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + 
  theme(axis.title.y = element_text(size = 14, angle=90)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))


# extract tweeter-retweeted pairs
rt <- data.frame(user=rand.df$randuser, rt=rand.df$rt.rand)
# omit pairs with NA and get only unique pairs
rt.u <- na.omit(unique(rt)) #
# begin social network analysis plotting
require(igraph)
require (sna)
degree <- sna::degree
g <- graph.data.frame(rt.u, directed = F) 
# note that the igraph package appears to have changed its plot
# function between the time that the original code was used for the
# analysis and revision for publication. The following line will
# plot a basic network graph, ready for further customisation:
plot.igraph(g)
# and here is a rough approximation of the published plot
plot(g,				#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=3,			#the font of the name labels
     vertex.label.cex=0.5,			#specifies the size of the font of the labels. can also be made to vary
     vertex.size = 0.5
)

# get some basic network attributes
g.adj <- get.adjacency(g, sparse=FALSE)
grecip(g.adj) # reciprocity
gtrans(g.adj) # transitivity 
centralization(g.adj, degree) 
# calculate Univariate Conditional Uniform Graph Tests
# density
print(cug.gden   <- cug.test(g.adj, gden))
plot(cug.gden)
range(cug.gden$rep.stat)
# reciprocity
cug.recip <- cug.test(g.adj, grecip)
print(cug.recip)
plot(cug.recip)
range(cug.recip$rep.stat)
# transistivity
cug.gtrans <- cug.test(g.adj, gtrans)
print(cug.gtrans)
plot(cug.gtrans)
range(cug.gtrans$rep.stat)
# centralisation
print(cug.cent <- cug.test(g.adj, centralization, FUN.arg=list(FUN=degree)))

# find out how many communities exist in the network using the walktrap
g.wc <- walktrap.community(g, steps = 1000, modularity=TRUE)
plot(as.dendrogram(g.wc))
max(g.wc$membership)+1

# some basic and widely-used text mining techniques to identify the issues 
# that captured the attention of Twitter-using anthropologists during the meeting. 
require(tm) 
a <- Corpus(VectorSource(df$text)) # create corpus object
a <- tm_map(a, tolower) # convert all text to lower case
a <- tm_map(a, removePunctuation) 
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removeWords, stopwords("english")) # this list needs to be edited and this function repeated a few times to remove high frequency context specific words with no semantic value 
require(rJava) # needed for stemming function 
require(Snowball) # also needed for stemming function 
a <- tm_map(a, stemDocument, language = "english") # converts terms to tokens
a.tdm <- TermDocumentMatrix(a, control = list(minWordLength = 3)) # create a term document matrix, keepiing only tokens longer than three characters, since shorter tokens are very hard to interpret
inspect(a.tdm[1:10,1:10]) # have a quick look at the term document matrix
findFreqTerms(a.tdm, lowfreq=30) # have a look at common words, in this case, those that appear at least 30 times, good to get high freq words and add to stopword list and re-make the dtm, in this case add aaa, panel, session
findAssocs(a.tdm, 'scienc', 0.3) # find associated words and strength of the common words. I repeated this function for the ten most frequent words.

# investigate the URLs contained in the Twitter messages
require(stringr)
require(ggplot2)
require(grid)
df$link <- sapply(df$text,function(tweet) str_extract(tweet,("http[[:print:]]+"))) # creates new field and extracts the links contained in the tweet
df$link <- sapply(df$text,function(tweet) str_extract(tweet,"http[[:print:]]{16}")) # limits to just 16 characters after http so I just get the shortened link. 
countlink <- data.frame(URL = as.character(unlist(dimnames(sort(table(df$link))))), N = sort(table(df$link))) # get frequencies of each link and put in rank order
rownames(countlink) <- NULL # remove rownames
countlinkSub <- subset(countlink, N>2) # subset of just links appearing more than twice
# plot to see distribution of links
ggplot(countlinkSub, aes(reorder(URL, N), N)) + 
  xlab("URL") + 
  ylab("Number of messages containing the URL")+ 
  geom_point() + 
  coord_flip() + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + 
  theme(axis.title.y = element_text(size = 14, angle=90)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))


# plot links
countlink<-data.frame(na.omit((df$link)))
names(countlink)="link"
qplot(countlink$link, geom="bar")+coord_flip() # but not sorted, so let's keep going...
require(plyr)
links<-count(countlink, "link") # to get a data frame with two named vectors for sorting
qplot(reorder(link,freq),freq,data=links, geom="bar")+coord_flip() # and here it is in order
links2<-subset(links,links$freq>2) # subset of just links appearing more than twice
links2$link<-factor(links2$link,levels=links2$link) # create factor for sorting
qplot(reorder(links2$link,links2$freq),links2$freq,data=links2, geom="bar")+coord_flip() # plot nicely sorted


# This is based on Jeffrey Breen's excellent tutorial at http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/

# download sentiment word list from here: http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar un-rar and put somewhere logical on your computer
hu.liu.pos = scan('E:/My Documents/My Papers/conferences/SAA2010/opinion-lexicon-English/positive-words.txt', what = 'character',comment.char=';') #load +ve sentiment word list
hu.liu.neg = scan('E:/My Documents/My Papers/conferences/SAA2010/opinion-lexicon-English/negative-words.txt',what = 'character',comment.char= ';') #load -ve sentiment word list
pos.words = c(hu.liu.pos)
neg.words = c(hu.liu.neg)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
require(plyr)
aaa.text <- df$text # get text of tweets
length(aaa.text) #check how many tweets, make sure it agrees with the original sample size
head(aaa.text, 5) #check content sample, see that it looks as expected, no weird characters, etc. 
aaa.scores <- score.sentiment(aaa.text,pos.words,neg.words,.progress='text') # get scores for the tweet text 
# create a histogram of sentiment scores
ggplot(aaa.scores, aes(x=score)) + 
  geom_histogram(binwidth=1) + 
  xlab("Sentiment score") + 
  ylab("Frequency") + 
  theme_bw()  + 
  opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) + 
  opts(axis.title.y=theme_text(size = 14, angle=90, vjust = -0.25)) + 
  opts(plot.margin = unit(c(1,1,2,2), "lines")) 
aaa.pos <- subset(aaa.scores,aaa.scores$score >= 2) # get tweets with only very +ve scores
aaa.neg <- subset(aaa.scores,aaa.scores$score <= -2) # get tweets with only very -ve scores

# Now create subset based on tweets with certain words, such as the high frequency words identified in the text mining. eg. science
scien <- subset(aaa.scores, regexpr("scien", aaa.scores$text) > 0)   # extract tweets containing only 'scien'
# plot histogram for this token, 
ggplot(scien, aes(x = score)) + geom_histogram(binwidth = 1) + xlab("Sentiment score for the token 'scien'") + ylab("Frequency") + theme_bw()  + opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) + opts(axis.title.y = theme_text(size = 14, angle = 90, vjust = -0.25)) + opts(plot.margin = unit(c(1,1,2,2), "lines")) 
# repeat this block with different high frequency words

a.tdm.sp <- removeSparseTerms(a.tdm, sparse=0.989)  # I found I had to iterate over this to ensure the tdm doesn't get too small... for example: 0.990 nrow=88, 0.989, nrow=67, 0.985, nrow=37, 0.98 nrow=23, 0.95 nrow=6
a.tdm.sp.df <- as.data.frame(inspect(a.tdm.sp )) # convert document term matrix to data frame
nrow(a.tdm.sp.df) # check to see how many words we're left with after removing sparse terms
# this analysis is based on http://www.statmethods.net/advstats/cluster.html 
# scale and transpose data for cluster analysis
a.tdm.sp.df.sc.t <- t(scale(a.tdm.sp.df))
require(pvclust)
fit <- pvclust(a.tdm.sp.df.sc.t, method.hclust = "average", method.dist = "correlation", nboot = 10) # this method may take a few hours the bootstraping, you can reduce the nboot value for a quicker result
plot(fit, cex = 1.5, cex.pv = 1.2, col.pv = c(1,0,0), main="", xlab="", sub="")  # draw the dendrogram

require(slam)
a.tdm.sp.t <- t(a.tdm.sp) # transpose document term matrix, necessary for the next steps using mean term frequency-inverse document frequency (tf-idf) to select the vocabulary for topic modeling
summary(col_sums(a.tdm.sp.t)) # check median...
term_tfidf <- tapply(a.tdm.sp.t$v/row_sums(a.tdm.sp.t)[a.tdm.sp.t$i], a.tdm.sp.t$j,mean) * log2(nDocs(a.tdm.sp.t)/col_sums(a.tdm.sp.t>0)) # calculate tf-idf values
summary(term_tfidf) # check median... note value for next line... 
a.tdm.sp.t.tdif <- a.tdm.sp.t[,term_tfidf>=1.0] # keep only those terms that are slightly less frequent that the median
a.tdm.sp.t.tdif <- a.tdm.sp.t[row_sums(a.tdm.sp.t) > 0, ]
summary(col_sums(a.tdm.sp.t.tdif)) # have a look

# Before going right into generating the topic model and analysing the output, we need to decide on the number of topics that the model should use
# Here's a function to loop over different topic numbers, get the log liklihood of the model for each topic number and plot it so we can pick the best one
# The best number of topics is the one with the highest log liklihood value.

require(topicmodels)
best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(a.tdm.sp.t.tdif, d)}) # this will make a topic model for every number of topics between 2 and 50... it will take some time! 
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))  # this will produce a list of logLiks for each model... 

# plot the distribution of logliklihoods by topic
best.model.logLik.df <- data.frame(topics=c(2:50), LL = as.numeric(as.matrix(best.model.logLik)))
ggplot(best.model.logLik.df, aes(x = topics, y = LL)) + 
  xlab("Number of topics") + 
  ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw()  + 
  opts(axis.title.x = theme_text(vjust = -0.5, size = 14)) + 
  opts(axis.title.y=theme_text(size = 14, angle=90, vjust= -0.25)) + 
  opts(plot.margin = unit(c(1,1,2,2), "lines"))  

# ggsave(file = "model_LL_per_topic_number.pdf") # export the plot to a PDF file
# it's not easy to see exactly which topic number has the highest LL, so let's look at the data...
best.model.logLik.df.sort <- best.model.logLik.df[order(-best.model.logLik.df$LL), ] # sort to find out which number of topics has the highest loglik, in this case 23 topics. 
best.model.logLik.df.sort # have a look to see what's at the top of the list, the one with the highest score
ntop <- best.model.logLik.df.sort[1,]$topics


lda <- LDA(a.tdm.sp.t.tdif, ntop) # generate a LDA model the optimum number of topics
get_terms(lda, 5) # get keywords for each topic, just for a quick look
get_topics(lda, 5) # gets topic numbers per document
lda_topics<-get_topics(lda, 5) 
beta <- lda@beta # create object containing parameters of the word distribution for each topic
gamma <- lda@gamma # create object containing posterior topic distribution for each document
terms <- lda@terms # create object containing terms (words) that can be used to line up with beta and gamma
colnames(beta) <- terms # puts the terms (or words) as the column names for the topic weights.
id <- t(apply(beta, 1, order)) # order the beta values
beta_ranked <- lapply(1:nrow(id),function(i)beta[i,id[i,]])  # gives table of words per topic with words ranked in order of beta values. Useful for determining the most important words per topic





