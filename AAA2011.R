# 
# Some preliminary details:
# 
# I used R x32 2.14.0 on Windows 7
# Although I have a 64-bit machine, I found that the RJava and Snowball packages only work on the 32 bit version of R
# For the workflow, I used a text editor called Notepad++ (http://notepad-plus-plus.org/), which connects nicely to R using NppToR, 
# setup of R and Notepad++ is very easy: http://jekyll.math.byuh.edu/other/howto/notepadpp/
# 

# here are the packages used throughout, might be easier to load them up at the start to avoid any unpleasant suprises 

library(twitteR)
library(tm)
library(ggplot2)
library(rJava)
library(Snowball)
library(plyr)
library(stringr)
library(topicmodels)

# First step: acquire the raw data
# Due to the cryptic methods that Twitter use to make their data publically available, this is  
# not easy to replicate. That is, if you use this method to get tweets today then you wont get the
# same sample as me. If you were to use the date range arguments of searchTwitter you'd probably get nothing,
# since Twitter don't give public access to archival data beyond a certain date. They also don't allow public sharing of archived tweets.
# In any case, I'm happy to share my original corpus, just send me an email.

require(twitteR) # get package with functions for interacting with Twitter.com
s <- searchTwitter('#aaa2011', n=1500) # get 1500 tweets with #aaa2011 tag, note that 1500 is the max, and it's subject to mysterious filtering and other restrictions by Twitter
df <- do.call("rbind", lapply(s, as.data.frame)) # convert to data frame
names(df) # get column names
head(df,3) # look at some example content

# Second step: have a bit of a look at the raw data too see how many tweeters there were
# and the distribution of tweets per person

length(unique(df$screenName)) # see how many unique tweeter accounts in the sample
counts=table(df$screenName)
counts.sort<-sort(counts) 
barplot(counts.sort) #barplot of names of tweeters and number of tweets, sorted by count
counts.sort.subset=subset(counts.sort,counts.sort>5) # create a subset of those who tweeted at least 5 times or more
barplot(counts.sort.subset,las=2,cex.names =0.75) # plot to have a look at who these people are

# Third step: examine some basic retweet statistics to see who are the influential tweeters
# much of this comes from http://blog.ouseful.info/2011/11/09/getting-started-with-twitter-analysis-in-r/
# Another thing that could have been done here is to look at the number of followers each tweeter has, but 
# I kept getting �Error: Rate limit exceeded. Clients may not make more than 150 requests per hour.� when
# I tried to do this. Links may be removed from the text of the tweets at this stage, I didn't, but here's 
# a bit of code that might be helpful: http://heuristically.wordpress.com/2011/04/08/text-data-mining-twitter-r/#comment-199

df$text=sapply(df$text,function(row) iconv(row,to='UTF-8')) #remove odd characters
trim <- function (x) sub('@','',x) # remove @ symbol from user names
library(stringr)
df$to=sapply(df$to,function(name) trim(name)) # pull out who msg is to
df$rt=sapply(df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2])) #extract who has been RT�d
sum(!is.na(df$rt))  # see how many tweets are retweets
sum(!is.na(df$rt))/length(df$rt)  # the ratio of retweets to tweets
countRT<-table(df$rt)
countRT<-sort(countRT)
cd=subset(countRT,countRT>2) # subset those RT�d at least twice
barplot(cd,las=2,cex.names =0.75) # plot them

# Fourth step: raw retweet counts are all very well, but let's look at the ratio of retweets to tweets
# for some more insights

t<-as.data.frame(table(df$screenName)) # make table with counts of tweets per person
rt<-as.data.frame(table(df$rt))  # make table with counts of retweets per person
t.rt<-merge(t,rt,by="Var1") # combine tweet count and retweet count per person
t.rt["ratio"]<-t.rt$Freq.y / t.rt$Freq.x # creates new col and adds ratio tweet/retweet
sort.t.rt<-t.rt[order(t.rt$ratio),] # sort it to put names in order by ratio
sort.t.rt<-subset(sort.t.rt,sort.t.rt$Freq.x>2) # exclude those with 2 tweets or less
barplot(sort.t.rt$ratio,las=2,cex.names =0.75, names.arg=sort.t.rt$Var1) # plot ratio by name

# Fifth step: see what people are linking to
# I switched from base graphics to gglop2 here, just for fun

df$link=sapply(df$text,function(tweet) str_extract(tweet,("http[[:print:]]+"))) # creates new field and extracts the links contained in the tweet
df$link=sapply(df$text,function(tweet) str_extract(tweet,"http[[:print:]]{16}")) # limits to just 16 characters after http so I just get the shortened link. They are all shortened, so this is fine, but there might be a better way using regex.
countlink<-table(df$link) # get frequencies of each link
countlink<-sort(countlink) # sort them
barplot(countlink) # plot freqs
# or to use ggplot2, read on...
countlink<-data.frame(na.omit((df$link)))
names(countlink)="link"
qplot(countlink$link, geom="bar")+coord_flip() # but not sorted, so let's keep going...
links<-count(countlink, "link") # to get a data frame with two named vectors for sorting
qplot(reorder(link,freq),freq,data=links, geom="bar")+coord_flip() # and here it is in order
links2<-subset(links,links$freq>2) # subset of just links appearing more than twice
links2$link<-factor(links2$link,levels=links2$link) # create factor for sorting
qplot(reorder(links2$link,links2$freq),links2$freq,data=links2, geom="bar")+coord_flip() # plot nicely sorted

# Sixth step: do some basic text mining. Now that we've got a sense of what the corpus is made up of, let's
# drill into it a bit with some basic token frequency and association analysis

library(tm) # load text mining package
a <- Corpus(VectorSource(df$text)) # create corpus object
a <- tm_map(a, tolower) # convert all text to lower case
a <- tm_map(a, removePunctuation) 
a <- tm_map(a, removeNumbers)
a <- tm_map(a, removeWords, stopwords("english")) # this list needs to be edited and this function repeated a few times to remove high frequency context specific words with no semantic value
# on my computer this stopword file is at C:\Users\...myusername...\Documents\R\win-library\2.13\tm\stopwords I use notepad++ to edit it.
library(rJava) # needed for stemming function, remember this will only work on the 32-bit version of R, as far as my experience suggests 
library(Snowball) # also needed for stemming function 
a <- tm_map(a, stemDocument, language = "english") # converts terms to tokens, I found this to be a very slow and unstable process, with frequent crashes
a.dtm <- TermDocumentMatrix(a, control = list(minWordLength = 3)) # create a term document matrix, keepiing only tokens longer than three characters, since shorter tokens are very hard to interpret!
inspect(a.dtm[1:10,1:10]) # have a quick look at the term document matrix
findFreqTerms(a.dtm, lowfreq=30) # have a look at common words, in this case, those that appear at least 30 times, good to get high freq words and add to stopword list and re-make the dtm, in this case add aaa, panel, session, 
findAssocs(a.dtm, 'science', 0.30) # find associated words and strength of the common words. I repeated this funciton for the ten most frequent words.

# Seventh step: simple sentiment analysis to find out what the tweeters thought about the topics they were writing about
# This is entirely borrowed from Jeffrey Breen's excellent tutorial: http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/

#get sentiment word list from here: http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar un-rar and put somewhere logical on your computer
#other possible lists: http://www.cs.pitt.edu/mpqa/ I haven't tried these
hu.liu.pos=scan('C:/...somewhere on your computer.../opinion-lexicon-English/positive-words.txt',what='character',comment.char=';') #load +ve sentiment word list
hu.liu.neg=scan('C:/...somewhere on your computer.../opinion-lexicon-English/negative-words.txt',what='character',comment.char=';') #load -ve sentiment word list
pos.words=c(hu.liu.pos)
neg.words=c(hu.liu.neg)
# Thanks again to Jeffrey Breen for making this fuction available!
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
aaa.text<-laply(s,function(t)t$getText()) # draw on the original object of tweets that we first got to extract just the text of the tweets
length(aaa.text) #check how many tweets, make sure it agrees with the original sample size
head(aaa.text,5) #check content sample, see that it looks as expected, no weird characters, etc. 
aaa.scores<-score.sentiment(aaa.text,pos.words,neg.words,.progress='text') # get scores for the tweet text 
hist(aaa.scores$score) # quick chart of overall sentiment
aaa.pos<-subset(aaa.scores,aaa.scores$score>=2) # get tweets with only very +ve scores
aaa.neg<-subset(aaa.scores,aaa.scores$score<=-2) # get tweets with only very -ve scores
# have a quick scan through these very -/+ver tweets to see what they're about...

# Now create subset based on tweets with certain words, such as the high frequency words identified in the text mining. eg. science
# Then I can see the sentiment for more specific topics
# There's probably a better way to do this with regex, but this is what got me though...
scienc<-subset(aaa.scores, regexpr("scienc", aaa.scores$text) > 0)   # extract tweets containing only scienc
scient<-subset(aaa.scores, ignore.case = TRUE, regexpr("scient", aaa.scores$text) > 0) # extract tweets containing only scient
sci_all<-rbind(scienc,scient) #that seems to get most of them in one table... 
# now repeat this subsetting for the ten most frequently occuring tokens...

# Eigth step: cluster analysis to reveal less prominent topics that the token frequency and association analysis may have missed
# This comes from Andrew Ziem's excellent post at http://heuristically.wordpress.com/2011/04/08/text-data-mining-twitter-r/

a.dtm.sp<- removeSparseTerms(a.dtm, sparse=0.989)  # I found I had to iterate over this to ensure the dtm doesn�t get too small... for example: 0.990 nrow=88, 0.989, nrow=67, 0.985, nrow=37, 0.98 nrow=23, 0.95 nrow=6
a.dtm.sp.df<- as.data.frame(inspect(a.dtm.sp)) # convert document term matrix to data frame
nrow(a.dtm.sp.df) # check to see how many words we�re left with after removing sparse terms
# next bit is from http://www.statmethods.net/advstats/cluster.html 
a.dtm.sp.df.sc<-scale(a.dtm.sp.df) # scale data ready for distance matrix
d <- dist(a.dtm.sp.df.sc, method = "euclidean") # make distance matrix
fit <- hclust(d, method="ward") # perform hierarchical cluster analysis
plot(fit) # display dendogram
# This basic cluseter was fine for a quick look, but read on for a more fancy (and time consuming) analysis that I like better
# This next bit gives a cluster analysis p-values for hierarchical clustering based on multiscale bootstrap resampling, 
# also from http://www.statmethods.net/advstats/cluster.html
a.dtm.sp.df.sc.t<-t(a.dtm.sp.df.sc) # transpose scaled data
fit <- pvclust(a.dtm.sp.df.sc.t, method.hclust="average", method.dist="correlation", nboot=10000) # this method gives good clusters... It took about 3 h to do the bootstraping, you can reduce the nboot value for a quicker result
plot(fit, col.pv=c(1,0,0), main="Cluster dengrogram with AU values (%)")  # hide BP and edge values, since AU values are the most important
pvrect(fit, alpha=.95) # add rectangles around groups highly supported by the data, if you want to, useful for a quick look, but too cluttered for anything else.
#now do some diagnostics, cf. http://www.is.titech.ac.jp/~shimo/prog/pvclust/
seplot(fit) # show standard errors for p-values
seplot(fit,identify=TRUE) # click on the chart to brush points with very high values, then hit stop when done, returns point IDs
print(fit, which=c(61)) # gives diagnostics for eg point 61, can help to decide if need to increase bootstrap resamples to reduce standard errors.

# Nineth step: topic modeling to automatically identify topics in the corpus. There are two packages for this, topic models and lda. lda seems to be 
# popular, perhaps because the author is a data scientist at facebook. But I couldn't figure it out so I used topicmodels, which is better documented 
# than lda. I followed the example on page 12 of http://www.jstatsoft.org/v40/i13/paper very closely for this.

a.dtm.sp.t<-t(a.dtm.sp) # transpose document term matrix, seems to be necessary for the next steps using mean term frequency-inverse document frequency (tf-idf) to select the vocabulary for topic modeling
summary(col_sums(a.dtm.sp.t)) # check median...
term_tfidf<-tapply(a.dtm.sp.t$v/row_sums(a.dtm.sp.t)[a.dtm.sp.t$i],a.dtm.sp.t$j,mean)*log2(nDocs(a.dtm.sp.t)/col_sums(a.dtm.sp.t>0)) # calculate tf-idf values
summary(term_tfidf) # check median... note value for next line... 
a.dtm.sp.t.tdif<-a.dtm.sp.t[,term_tfidf>=1.0] # keep only those terms that are slightly less frequent that the median
a.dtm.sp.t.tdif<-a.dtm.sp.t[row_sums(a.dtm.sp.t)>0,]
summary(col_sums(a.dtm.sp.t.tdif)) # have a look

# Before going right into generating the topic model and analysing the output, we need to decide on the number of topics that the model should use
# Here�s a function to loop over different topic numbers, get the log liklihood of the model for each topic number and plot it so we can pick the best one
# The best number of topics is the one with the highest log liklihood value.

best.model<-lapply(seq(2,50,by=1),function(d){LDA(a.dtm.sp.t.tdif,d)}) # this will make a topic model for every number of topics between 2 and 50... it will take some time! 
best.model.logLik<-as.data.frame(as.matrix(lapply(best.model,logLik)))  # this will produce a list of logLiks for each model... 
plot(as.matrix(best.model.logLik$V1), type="l") #have a quick look at the distribution of log liklihood values for all the models generated
qplot(best.model.logLik$topics,best.model.logLik$loglik, geom="line") # make a more fancy chart to see the peak
best.model.logLik.sort<-best.model.logLik[order(best.model.logLik$loglik),] # sort to find out which number of topics has the highest loglik, in this case 23 topics. 
# So from this function I've got 23 topics as the best number to use for the topic model to investigate in more detail

# Now I'll look in more detail at the model with 23 topics to find out the keywords for each topic
# I could also find which topics are associated with each document, but didn't have space in this analysis

lda<-LDA(a.dtm.sp.t.tdif,23) # generate a LDA model with 23 topics, as found to be optimum
get_terms(lda, 5) # gets keywords for each topic, just for a quick look
lda_terms<-get_terms(lda, 5) # make object to store them 
write.csv(lda_terms,"lda_terms.csv") # Output to csv file, I found this a bit easier to browse
get_topics(lda, 5) # gets topic numbers per document
lda_topics<-get_topics(lda, 5) 
write.csv(lda_topics,"lda_topics.csv") # output to csv file. 
# If you need to find out where all these csv files are being saved, try thi
getwd() # tells you R's  working directory, which is where it saves files and looks for files, etc.
beta<-lda@beta # create object containing parameters of the word distribution for each topic
gamma<-lda@gamma # create object containing posterior topic distribution for each document
terms<-lda@terms # create object containing terms (words) that can be used to line up with beta and gamma
colnames(beta)<-terms # puts the terms (or words) as the column names for the topic weights...
id<-t(apply(beta,1,order)) # order them (can't recall what this does, to be honest)
beta_ranked<- lapply(1:nrow(id),function(i)beta[i,id[i,]])  # gives table of words per topic with words ranked in order of beta values. Very handy for determing the most important words per topic

# Tenth step: make a pseudo-time series plot of topics to show how topics rose and fell in popularity 
# during the meeting. This comes from an answer to a question a posted on SO: 
# http://stackoverflow.com/questions/8337980/r-yaletoolkit-how-to-change-the-font-size-of-tick-labels-on-the-sparklines
# and from a very helpful post by Jason Dieterle on the Edward Tufte forum: http://www.edwardtufte.com/bboard/q-and-a-fetch-msg?msg_id=00037p

gamma.df<-as.data.frame(gamma)) # prepare the topics per document output for plotting
# setup the plot area, see http://www.programmingr.com/content/controlling-margins-and-axes-oma-and-mgp for details
par(mfrow=c(ncol(gamma.df),1), # sets number of rows in space to number of cols in the gamma data frame (ie. 23 topics)
    mar=c(0,0,1,0), # sets margin size for the figures
    oma=c(4,5,4,2), # sets outer margin
    mgp=c(2,1,0))   # sets the way axes and labels are spatially arranged
for (i in 1:ncol(gamma.df)){ # setup for statement to loops over all topics (ie. columns) in gamma.df
        plot(gamma.df[,i], # use col data, not rows from data frame gamma.df
        col="#858585",lwd=0.5, #make the line grey and thin
        axes=F,ylab="",xlab="",main="",type="l"); # suppress all axes lines, set as line plot
        axis(2,yaxp=c(min(gamma.df[,i]),max(gamma.df[,i]),2), # define the y-axis: only show tickmarks for max and min values of col
        cex.axis=0.85,las=1, # shrink fontsize of tickmark labels slightly, make text horizontal for easy reading
        at=c(round(min(gamma.df[,i]),3),round(max(gamma.df[,i]),3))); #specify where tickmark numbers go and round them to keep it tidy
        axis(2,yaxp=c(min(gamma.df[,i]),max(gamma.df[,i]),2),col="white",tcl=0,labels=FALSE)  # further define the y-axis: put a 2nd white axis line over the 1st y-axis to make it invisible
        ymin<-min(gamma.df[,i]); tmin<-which.min(gamma.df[,i]);ymax<-max(gamma.df[,i]);tmax<-which.max(gamma.df[,i]); # determines min, max and indices of these for the next line...
        points(x=c(tmax),y=c(ymax),pch=19,col=c("black"),cex=0.8); # add coloured points at maxima to highlight them, if you like
        }
axis(1,pos=c(-4)) # puts an x-axis along the bottom of it all

# That's all for the data analysis, but here are a few scraps I found useful along the way

# Exporting high quality image files of the plots
capabilities()["cairo"] # checks I've got Cairo support for SVG export, should return "cairo TRUE" if you're using R 2.14.0. If not, use something from here http://en.wikibooks.org/wiki/R_Programming/Graphics#Exporting_graphs
svg("your_filename.svg") # creates object ready to receive my plot, there are many arguments this fuction can take, have a look here: http://stat.ethz.ch/R-manual/R-patched/library/grDevices/html/cairo.html
# ... now do the plotting fuction... you wont see anything because it's going into the file you've created
dev.off() # finish creating the svg object
# now go to R's working directory and find the svg file you made and have a look using Inkscape (http://inkscape.org/) and edit if required or 'export bitmap'/'save as' the format required.

# Saving and loading data
# Stopping in the middle of the analysis and saving the data so far
save(s,file="your_filename.RData")
# Coming backing to it and loading up the data to continue
load("your_filename.RData", .GlobalEnv)
