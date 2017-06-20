Text Mining Notes
================

Introduction
------------

The notes you are reading now are what I learned from courses, projects and other experts. I tried to give a structured overview of text mining programming in R. My goal is not to give a theoretical overview of text mining but to give practical examples with real code and real data.

As a dataset we will use tweets. I decided I could download some real tweets by myself. This could prove an interesting challenge and could give some interesting insights if we download the right tweets. To do this I followed the instructions on this websites

[Link on twitter download 1](https://www.credera.com/blog/business-intelligence/twitter-analytics-using-r-part-1-extract-tweets/) [Link on twitter download 2](http://thinktostart.com/twitter-authentification-with-r/)

Let's get the tweets
====================

Let's load the necessary libraries

``` r
library("devtools")
library("twitteR")
library("ROAuth")
```

Now to be able to download tweets you need to have a twitter account and authorize it (using special generated keys). To know how follow the instructions you can found on the page linked above. I saved my keys in a file that I access to read it. You don't have to do that, but I wanted to have a working piece of code that I can run and at the same time publish. Again, use google to find out how you can do it differently. You will understand if I don't want to put my keys here ;-)

``` r
secrets <- read.csv("/Users/umberto/Documents/Passwords and Secrets/twitter-keys.csv", stringsAsFactors = FALSE, header = TRUE, sep =",")

api_key <- secrets$api_key
api_secret <- secrets$api_secret
access_token <- secrets$access_token
access_token_secret <- secrets$access_token_secret
 

setup_twitter_oauth(api_key,api_secret)
```

    ## [1] "Using browser based authentication"

Coffee Tweets
-------------

Let's now see what we can find out on Coffee. Let's find tweets that have the hashtag Coffee (`#Coffee`) in them and let's exclude the retweets, since they will falsify our results since they contains almost always the same exact text with RT at the beginning.

``` r
search.string <- "#coffee exclude:retweets"
no.of.tweets <- 1000

c_tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en")
```

Now we need to access the text of the tweets. So we do it in this way (we also need to clean up the tweets from special characters that for now we don't need, like emoticons with the `sapply` function.). At the same time let's remove all web links from the tweets, since we are not interested in having them in our wordclouds or networks.

``` r
coffee_tweets = sapply(c_tweets, function(t) t$getText())

coffee_tweets <- sapply(coffee_tweets,function(row) iconv(row, "latin1", "ASCII", sub=""))
names(coffee_tweets) <- NULL

coffee_tweets <- gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", coffee_tweets)

head(coffee_tweets)
```

    ## [1] "Did you know we make #Coffee? @dilorenzocaffe \n\n\n\n#doughboxdiner #doughbox #coffeetime..."    
    ## [2] "The latest The luv-my-latte Daily! Thanks to @LuckyChandraA @nuxvomo @whizztips #coffee #food"    
    ## [3] "#Coffee Roasts Guide - via @nationalcoffee "                                                      
    ## [4] "Anyone for a cavacinno?  #cavalierkingcharlsespaniel #SundayMorning #coffee #dogsoftwitter"       
    ## [5] "Want to know more about the #CafeRacer? Watch this space! #video #coffee #wearesanremo"           
    ## [6] "...Lowkey but high-key wanting or needing a cup of coffee rn... #CoffeeLover #coffee #CoffeeorDie"

It is interested to see how many parameters we get from the search

``` r
str(c_tweets[[1]])
```

    ## Reference class 'status' [package "twitteR"] with 17 fields
    ##  $ text         : chr "Did you know we make #Coffee? @dilorenzocaffe \n\n<U+2615><U+FE0F><U+2615><U+FE0F><U+2615><U+FE0F><U+2615><U+FE"| __truncated__
    ##  $ favorited    : logi FALSE
    ##  $ favoriteCount: num 0
    ##  $ replyToSN    : chr(0) 
    ##  $ created      : POSIXct[1:1], format: "2017-06-18 07:18:06"
    ##  $ truncated    : logi FALSE
    ##  $ replyToSID   : chr(0) 
    ##  $ id           : chr "876338214482714624"
    ##  $ replyToUID   : chr(0) 
    ##  $ statusSource : chr "<a href=\"http://www.facebook.com/twitter\" rel=\"nofollow\">Facebook</a>"
    ##  $ screenName   : chr "DoughboxDiner"
    ##  $ retweetCount : num 0
    ##  $ isRetweet    : logi FALSE
    ##  $ retweeted    : logi FALSE
    ##  $ longitude    : chr(0) 
    ##  $ latitude     : chr(0) 
    ##  $ urls         :'data.frame':   1 obs. of  5 variables:
    ##   ..$ url         : chr "https://t.co/qmI8pfixzC"
    ##   ..$ expanded_url: chr "http://fb.me/EczCrm4n"
    ##   ..$ display_url : chr "fb.me/EczCrm4n"
    ##   ..$ start_index : num 112
    ##   ..$ stop_index  : num 135
    ##  and 53 methods, of which 39 are  possibly relevant:
    ##    getCreated, getFavoriteCount, getFavorited, getId, getIsRetweet,
    ##    getLatitude, getLongitude, getReplyToSID, getReplyToSN, getReplyToUID,
    ##    getRetweetCount, getRetweeted, getRetweeters, getRetweets,
    ##    getScreenName, getStatusSource, getText, getTruncated, getUrls,
    ##    initialize, setCreated, setFavoriteCount, setFavorited, setId,
    ##    setIsRetweet, setLatitude, setLongitude, setReplyToSID, setReplyToSN,
    ##    setReplyToUID, setRetweetCount, setRetweeted, setScreenName,
    ##    setStatusSource, setText, setTruncated, setUrls, toDataFrame,
    ##    toDataFrame#twitterObj

So there is quite some possibilities here. But we are not actually interested in twitters now, but just in the text `tweetsText`. (check for example as reference this [stackoverflow post](http://stackoverflow.com/questions/14549305/searchtwitter-timestamps)).

Tea tweets
----------

Since we are going to compare corpora of text later on, we need a second set of tweets. I decided to download the first 1000 tweets on Tea

Tea Tweets
----------

``` r
search.string <- "#tea exclude:retweets"
no.of.tweets <- 1000

t_tweets <- searchTwitter(search.string, n=no.of.tweets, lang="en")
```

Now we need to access the text of the tweets. So we do it in this way (we also need to clean up the tweets from special characters that for now we don't need, like emoticons with teh `sapply` function.)

``` r
tea_tweets = sapply(t_tweets, function(t) t$getText())

tea_tweets <- sapply(tea_tweets,function(row) iconv(row, "latin1", "ASCII", sub=""))
names(tea_tweets) <- NULL

tea_tweets <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", tea_tweets)

head(tea_tweets)
```

    ## [1] "Clear blue skies so another warm/hot day ahead \n#sunscreen \n#BBQ weather #breakfast #lunch #tea \n#HappyFathersDay "   
    ## [2] "The best way to spend a Sunday, is to indulge in a cup of #tea made with #rich, pure and wholesome Dairyland #Milk. "    
    ## [3] "A very Happy Father's Day to all you tea-drinking super-heroes out there!\n\n#FathersDay #Dad #Tea #Norfolk "            
    ## [4] "#Win the amazing #international #WithLoveforBooks #mug, #pillow, #Kindle Fire,#tea &amp; #sterling #necklace #giveaway! "
    ## [5] "It's all about the cream tea today! #jamfirst #creamteam #homemade #tea #prosecco #saltash "                             
    ## [6] "Happy Father's Day  -  "

Let's start with text mining
============================

To do text mining one of the most used library (and the one I will use here) is `tm`.

``` r
library("tm")
```

First we need to create a vector of texts

``` r
coffee_source <- VectorSource(coffee_tweets)
tea_source <- VectorSource(tea_tweets)
```

Then we need to make a `VCorpus` of the list of tweets

``` r
coffee_corpus <- VCorpus(coffee_source)
tea_corpus <- VCorpus(tea_source)
coffee_corpus
```

    ## <<VCorpus>>
    ## Metadata:  corpus specific: 0, document level (indexed): 0
    ## Content:  documents: 1000

So if we want to see the text of a tweet in the corpus we can use

``` r
coffee_corpus[[15]][1]
```

    ## $content
    ## [1] "Good morning, enjoy your Sunday  #coffee #morningcoffee #goodmorning #cafe #illy #pula"

``` r
tea_corpus[[15]][1]
```

    ## $content
    ## [1] "Good to see a lovely display of #fairtrade decaffeinated #tea at the KLCC Suria this past week. #FairtradeRamadan "

Cleaning text
-------------

Now that I how to make a corpus, I can focus on cleaning, or preprocessing, the text. In bag of words text mining, cleaning helps aggregate terms. For example, it may make sense that the words "miner", "mining" and "mine" should be considered one term. Specific preprocessing steps will vary based on the project. For example, the words used in tweets are vastly different than those used in legal documents, so the cleaning process can also be quite different. (Text Source: Datacamp)

Common preprocessing functions include:

-   tolower(): Make all characters lowercase
-   removePunctuation(): Remove all punctuation marks
-   removeNumbers(): Remove numbers
-   stripWhitespace(): Remove excess whitespace

Note that tolower() is part of base R, while the other three functions come from the tm package. Going forward, we'll load the tm and qdap for you when they are needed.

The **qdap** package offers other text cleaning functions. Each is useful in its own way and is particularly powerful when combined with the others.

-   bracketX(): Remove all text within brackets (e.g. "It's (so) cool" becomes "It's cool")
-   replace\_number(): Replace numbers with their word equivalents (e.g. "2" becomes "two")
-   replace\_abbreviation(): Replace abbreviations with their full text equivalents (e.g. "Sr" becomes "Senior")
-   replace\_contraction(): Convert contractions back to their base words (e.g. "shouldn't" becomes "should not")
-   replace\_symbol() Replace common symbols with their word equivalents (e.g. "$" becomes "dollar")

Stopwords
---------

Using the c() function allows you to add new words (separated by commas) to the stop words list. For example, the following would add "word1" and "word2" to the default list of English stop words:

    all_stops <- c("word1", "word2", stopwords("en"))

You can use the following command to remove stopwords

    removeWords(text, stopwords("en"))

Stemming
--------

I won't go into stemming here but I tought is worthwile mentioning it. Here is an example of stemming

``` r
stemDocument(c("computational", "computers", "computation"))
```

    ## [1] "comput" "comput" "comput"

Here is an example of using stemming

``` r
# Create complicate
complicate <- c("complicated", "complication", "complicatedly")
# Perform word stemming: stem_doc
stem_doc <- stemDocument(complicate)
# Create the completion dictionary: comp_dict
comp_dict <- "complicate"
# Perform stem completion: complete_text 
complete_text <- stemCompletion(stem_doc, comp_dict)
# Print complete_text
complete_text
```

    ##      complic      complic      complic 
    ## "complicate" "complicate" "complicate"

Clean the Corpus
================

Let's get back to our set of tweets. Let's start cleaning it. To clean the Corpus we can define a function that applies several functions on the corpus

``` r
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "mug", "coffee", "amp","now","cafe"))
  return(corpus)
}
```

Then we can try to apply it on our coffee corpus

``` r
clean_corp <- clean_corpus(coffee_corpus)
```

Then we can pring a cleaned-up tweet

``` r
clean_corp[[227]][1]
```

    ## $content
    ## [1] " corning stovetop  tea pot 6 cup new carafe weekend ebay"

and the original one

``` r
coffee_corpus[[227]][1]
```

    ## $content
    ## [1] "[ Corning Stovetop #coffee Tea Pot 6 cup NEW Carafe  #weekend #ebay"

So we have removed special characters, punctuation and so on. Not all the words make much sense really (for example twitter usernames) but it should not be a problem since we don't expect to see them very often in our corpus.

Make a document-term matrix
---------------------------

We can use the following code to make a DTM. Each document is represented as a row and each word as a column.

``` r
coffee_dtm <- DocumentTermMatrix(clean_corp)

# Print out coffee_dtm data
print(coffee_dtm)
```

    ## <<DocumentTermMatrix (documents: 1000, terms: 3512)>>
    ## Non-/sparse entries: 7564/3504436
    ## Sparsity           : 100%
    ## Maximal term length: 89
    ## Weighting          : term frequency (tf)

``` r
# Convert coffee_dtm to a matrix: coffee_m
coffee_m <- as.matrix(coffee_dtm)

# Print the dimensions of coffee_m
dim(coffee_m)
```

    ## [1] 1000 3512

``` r
# Review a portion of the matrix
coffee_m[1:10, 253: 259]
```

    ##     Terms
    ## Docs bake bakerbusiness2 baking bakuman balcony bali bam
    ##   1     0              0      0       0       0    0   0
    ##   2     0              0      0       0       0    0   0
    ##   3     0              0      0       0       0    0   0
    ##   4     0              0      0       0       0    0   0
    ##   5     0              0      0       0       0    0   0
    ##   6     0              0      0       0       0    0   0
    ##   7     0              0      0       0       0    0   0
    ##   8     0              0      0       0       0    0   0
    ##   9     0              0      0       0       0    0   0
    ##   10    0              0      0       0       0    0   0

Make a document-term matrix (DTM)
---------------------------------

You can also transpose a TDM, to have each word as a row and each column as a document.

``` r
# Create a TDM from clean_corp: coffee_tdm
coffee_tdm <- TermDocumentMatrix(clean_corp)

# Print coffee_tdm data
print(coffee_tdm)
```

    ## <<TermDocumentMatrix (terms: 3512, documents: 1000)>>
    ## Non-/sparse entries: 7564/3504436
    ## Sparsity           : 100%
    ## Maximal term length: 89
    ## Weighting          : term frequency (tf)

``` r
# Convert coffee_tdm to a matrix: coffee_m
coffee_m <- as.matrix(coffee_tdm)

# Print the dimensions of the matrix
dim(coffee_m)
```

    ## [1] 3512 1000

``` r
# Review a portion of the matrix
coffee_m[2587:2590, 148:150]
```

    ##                Docs
    ## Terms           148 149 150
    ##   relationships   0   0   0
    ##   relax           0   0   0
    ##   relaxing        0   0   0
    ##   relaxingtime    0   0   0

Frequent terms with tm
----------------------

Now that you know how to make a term-document matrix, as well as its transpose, the document-term matrix, we will use it as the basis for some analysis. In order to analyze it we need to change it to a simple matrix.

Calling `rowSums()` on your newly made matrix aggregates all the terms used in a passage. Once you have the `rowSums()`, you can `sort()` them with `decreasing = TRUE`, so you can focus on the most common terms.

Lastly, you can make a barplot() of the top 5 terms of term\_frequency with the following code (we will make something prettier later on with ggplot2).

    barplot(term_frequency[1:5], col = "#C0DE25")

So let's try with out coffee tweets

``` r
## coffee_tdm is still loaded in your workspace

# Create a matrix: coffee_m
coffee_m <- as.matrix(coffee_tdm)

# Calculate the rowSums: term_frequency
term_frequency <- rowSums(coffee_m)

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency, decreasing = TRUE)

# View the top 10 most common words
term_frequency[1:10]
```

    ##    morning     sunday        day       love  starbucks        cup 
    ##         74         64         62         57         54         51 
    ##        tea      great coffeetime       good 
    ##         48         45         42         42

``` r
# Plot a barchart of the 10 most common words
barplot(term_frequency[1:10], col = "tan", las = 2)
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-22-1.png) Now let's make it a bit prettier with **ggplot2**...

``` r
library(ggplot2)
```

``` r
library(dplyr)

tf <- as.data.frame(term_frequency)
tf$words <- row.names(tf)
tf10 <- as.data.frame(tf[1:10,])

# We need to make the words factors (ordered) otherwise ggplot2 will order the 
# x axis alphabetically
tf10 <- mutate(tf10, words = factor(words, words))

ggplot(tf10, aes(x = tf10$words , y = tf10$term_frequency   )) + geom_bar(stat = "identity", fill = "tan", col = "black")+ theme_grey()+theme(text = element_text(size=16),  axis.title.x=element_blank(),axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ylab("Words Frequency") 
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-24-1.png)

Note that the knitr button command don't work from Rstudio if you want to use `knitr`. So the solution is to do it from the console with the following commands

``` r
library(rmarkdown)
render("/Users/umberto/Documents/Education/Data Camp/Text Mining/Text-Mining-Notes/text-mining-notes.Rmd")
```

The command will render an html file in the directory where the `Rmd` file is.

``` r
library(rJava)
library(qdap)
```

Let's build a word frequency plot with `qdap` library (note that we are not working with our cleaned up corpus, and therefore we will see different words).

``` r
frequency <- freq_terms(coffee_tweets, top = 10, at.least = 3, stopwords = "Top200Words")

frequency <- mutate(frequency, WORD = factor(WORD, WORD))

ggplot(frequency, aes(x = frequency$WORD , y = frequency$FREQ   )) + geom_bar(stat = "identity", fill = "tan", col = "black")+ theme_grey()+theme(text = element_text(size=16),  axis.title.x=element_blank(),axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ylab("Words Frequency") 
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-27-1.png) Now let's remove more stopwords (now it looks similar to what we obtained at the beginning)

``` r
frequency2 <- freq_terms(coffee_tweets, top = 10, at.least = 3, stopwords = c(tm::stopwords("english"),"coffee","httpstco","amp","now","cafe"))

frequency2 <- mutate(frequency2, WORD = factor(WORD, WORD))

ggplot(frequency2, aes(x = frequency2$WORD , y = frequency2$FREQ   )) + geom_bar(stat = "identity", fill = "tan", col = "black")+ theme_grey()+theme(text = element_text(size=16),  axis.title.x=element_blank(),axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ylab("Words Frequency") 
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-28-1.png)

Wordclouds
----------

A very cool way of visualizing frequency of words, are the "wordcloud". You will always get a wow effect when showing it to people. To build beatiful clouds you will need the library `wordcloud`. Here is an example

``` r
library(wordcloud)

term_frequency[1:10]
```

    ##    morning     sunday        day       love  starbucks        cup 
    ##         74         64         62         57         54         51 
    ##        tea      great coffeetime       good 
    ##         48         45         42         42

``` r
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
wordcloud(word_freqs$term, word_freqs$num, max.words = 100, colors = "red")
```

    ## Warning in wordcloud(word_freqs$term, word_freqs$num, max.words = 100,
    ## colors = "red"): morning could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(word_freqs$term, word_freqs$num, max.words = 100,
    ## colors = "red"): saturday could not be fit on page. It will not be plotted.

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-29-1.png)

Now we need to remove some words that are clear are appearing while talking about coffee

``` r
# Add new stop words to clean_corpus()
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, 
                   c(stopwords("en"), "brew", "cafe", "coffeetime", "cup", "coffee"))
  return(corpus)
}

clean_coffee <- clean_corpus(coffee_corpus)
coffee_tdm <- TermDocumentMatrix(clean_coffee)
coffee_m <- as.matrix(coffee_tdm)
coffee_words <- rowSums(coffee_m)
```

Now we prepare the right order of words for the wordcloud

``` r
coffee_words <- sort(coffee_words, decreasing = TRUE)
coffee_words[1:6]
```

    ##   morning    sunday       day      love starbucks       amp 
    ##        74        64        62        57        54        51

``` r
coffee_freqs <- data.frame (term = names(coffee_words), num = coffee_words)

wordcloud(coffee_freqs$term, coffee_freqs$num, max.words = 50, colors = "red")
```

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words = 50, :
    ## starbucks could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words = 50, :
    ## sunday could not be fit on page. It will not be plotted.

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-31-1.png)

### Improve word colours

``` r
wordcloud(coffee_freqs$term, coffee_freqs$num, max.words = 100, colors = c("grey80", "darkgoldenrod1", "tomato"))
```

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words =
    ## 100, : thecupoffaith could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words =
    ## 100, : morning could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words =
    ## 100, : great could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words =
    ## 100, : thanks could not be fit on page. It will not be plotted.

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-32-1.png)

Now let's improve even more on the colors. To do that we will need to use RColorBrewer. RColorBrewer color schemes are organized into three categories:

-   Sequential: Colors ascend from light to dark in sequence
-   Qualitative: Colors are chosen for their pleasing qualities together
-   Diverging: Colors have two distinct color spectra with lighter colors in between

To change the colors parameter of the `wordcloud()` function you can use a select a palette from `RColorBrewer` such as "Greens". The function `display.brewer.all()` will list all predefined color palettes. More information on ColorBrewer (the framework behind RColorBrewer) is available on its [website](http://www.colorbrewer.org/). (Source: datacamp)

The function `brewer.pal()` allows you to select colors from a palette. Specify the number of distinct colors needed (e.g. 8) and the predefined palette to select from (e.g. "Greens"). Often in word clouds, very faint colors are washed out so it may make sense to remove the first couple from a brewer.pal() selection, leaving only the darkest.

Here's an example:

    green_pal <- brewer.pal(8, "Greens")
    green_pal <- green_pal[-(1:2)]

Then just add that object to the wordcloud() function.

    wordcloud(chardonnay_freqs$term, chardonnay_freqs$num, max.words = 100, colors = green_pal)

The command `display.brewer.all()` will display all palettes. Is a very cool command

``` r
display.brewer.all()
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-33-1.png)

Let's try to use the `PuOr` palette

``` r
# Create purple_orange
PuOr <- brewer.pal(10, "PuOr")
purple_orange <- PuOr[-(1:2)]
```

And now we can create the wordcloud woith this palette

``` r
wordcloud(coffee_freqs$term, coffee_freqs$num, max.words = 100, colors = purple_orange)
```

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words =
    ## 100, : shop could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words =
    ## 100, : great could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words =
    ## 100, : happy could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words =
    ## 100, : breakfast could not be fit on page. It will not be plotted.

    ## Warning in wordcloud(coffee_freqs$term, coffee_freqs$num, max.words =
    ## 100, : weekend could not be fit on page. It will not be plotted.

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-35-1.png)

Weel it seems that people talking about coffee are talking also about "morning". Make sense. When is Coffee important if not on the morning?

Sometimes not all the words can be plotted. In this case the only solutions are to reduce the number of words or to reduce the scale of the words themselves. For example

``` r
wordcloud(coffee_freqs$term, coffee_freqs$num, max.words = 100, colors = purple_orange, scale = c(2,0.3))
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-36-1.png)

Wordclouds with bigrams
-----------------------

Now sometimes single words don't tell the entire story and is interesting to do the same plot with bigrams (words that appear together in the corpus). The tokenizer from `RWeka` is very useful.

``` r
library(RWeka)
```

Then we need to get the couples of words (note that the definition give below will give you **only** bigrams, and not single words anymore).

``` r
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 2))
tdm.bigram <- TermDocumentMatrix(clean_corp, control = list(tokenize = BigramTokenizer))
```

Then we can get the frequencies of the bigrams

``` r
freq <- sort(rowSums(as.matrix(tdm.bigram)), decreasing = TRUE)
freq.df <- data.frame(word = names(freq), freq= freq)
head(freq.df)
```

    ##                word freq
    ## morning     morning   74
    ## sunday       sunday   64
    ## day             day   62
    ## love           love   57
    ## starbucks starbucks   54
    ## cup             cup   51

Now we can plot the wordcloud

``` r
wordcloud(freq.df$word, freq.df$freq, max.words = 50, random.order = F, colors = purple_orange, scale = c(4,0.7))
```

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 50,
    ## random.order = F, : coffeeshop could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 50,
    ## random.order = F, : thecupoffaith thecupoffaith could not be fit on page.
    ## It will not be plotted.

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-40-1.png)

Apparently not many bigrams appear so frequently. The only one is "good morning". It makes again sense...

We need of course first to do a different cleanup of the bigrams list. But that is something that goes beyond the notes I am writing. An important point is that if you remove all stop words like "not" you may loose important informations for bigrams (like negations).

Trigrams
--------

Just as a reference here is the code to do wordclouds with trigrams and bigrams

``` r
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 3))
tdm.trigram <- TermDocumentMatrix(clean_corp, control = list(tokenize= TrigramTokenizer))

freq <- sort(rowSums(as.matrix(tdm.trigram)), decreasing = TRUE)
freq.df <- data.frame(word = names(freq), freq= freq)
head(freq.df)
```

    ##                                                    word freq
    ## good morning                               good morning   17
    ## thecupoffaith thecupoffaith thecupoffaith thecupoffaith   16
    ## fathers day                                 fathers day   15
    ## weve got                                       weve got   14
    ## weekend ebay                               weekend ebay   13
    ## saturday morning                       saturday morning   12

``` r
wordcloud(freq.df$word, freq.df$freq, max.words = 40, random.order = F, colors = purple_orange, scale = c(3,0.7))
```

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : thecupoffaith thecupoffaith could not be fit on page.
    ## It will not be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : probably bit hungover could not be fit on page. It will
    ## not be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : saturday morning youre could not be fit on page. It
    ## will not be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : sunday morning could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : weve got cure could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : worry weve got could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : yeah worry weve could not be fit on page. It will not
    ## be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : youre probably could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : youre probably bit could not be fit on page. It will
    ## not be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : caffeine kill could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : can much caffeine could not be fit on page. It will not
    ## be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : coffeenews findthelittleman could not be fit on page.
    ## It will not be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : coffeenews findthelittleman quote could not be fit on
    ## page. It will not be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : findthelittleman quote could not be fit on page. It
    ## will not be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : findthelittleman quote quoteoftheday could not be fit
    ## on page. It will not be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : happy sunday could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : kill caffeine could not be fit on page. It will not be
    ## plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : quote quoteoftheday could not be fit on page. It will
    ## not be plotted.

    ## Warning in wordcloud(freq.df$word, freq.df$freq, max.words = 40,
    ## random.order = F, : starbucks love could not be fit on page. It will not be
    ## plotted.

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-42-1.png) Words are long so not all could be written in the plot. You have to choose between something that is not readable and something that has less words...

Common Words between Corpora
============================

To find common words we need to create two "big" documents of tweets. We need to collapse all tweets together separated by a space

``` r
all_coffee <- paste (coffee_tweets, collapse = " ")
all_tea <- paste (tea_tweets,collapse = " ")
all_tweets <- c(all_coffee, all_tea)
```

Now we convert to a Corpus

``` r
# Convert to a vector source
all_tweets <- VectorSource(all_tweets)

# Create all_corpus
all_corpus <- VCorpus(all_tweets)
```

Now that we have a corpus filled with words used in both the tea and coffee tweets files, we can clean the corpus, convert it into a TermDocumentMatrix, and then a matrix to prepare it for a commonality.cloud(). First we need to define a proper cleaning function that contains words like *coffee* and *tea*

``` r
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "mug", "coffee", "tea", "amp", "cup"))
  return(corpus)
}
```

Let's clean the corpus

``` r
# Clean the corpus
all_clean <- clean_corpus (all_corpus)

# Create all_tdm
all_tdm <- TermDocumentMatrix(all_clean) 

# Create all_m
all_m <- as.matrix(all_tdm)
```

Now the communality cloud

``` r
commonality.cloud(all_m, max.words = 100, colors = "steelblue1")
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-47-1.png)

Comparison Cloud
----------------

You can plot a comparison cloud in this way

``` r
comparison.cloud(all_m, max.words = 50, colors = c("orange", "blue"), scale = c(3,0.5))
```

    ## Warning in comparison.cloud(all_m, max.words = 50, colors = c("orange", :
    ## withloveforbooks could not be fit on page. It will not be plotted.

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-48-1.png)

(Source Datacamp) A commonality.cloud() may be misleading since words could be represented disproportionately in one corpus or the other, even if they are shared. In the commonality cloud, they would show up without telling you which one of the corpora has more term occurrences.

To solve this problem, we can create a `pyramid.plot()` from the `plotrix` package.

``` r
library(plotrix)
```

``` r
all_tdm_m <- all_m
# Create common_words
common_words <- subset(all_tdm_m, all_tdm_m[, 1] > 0 & all_tdm_m[, 2] > 0)

# Create difference
difference <- abs(common_words[, 1] - common_words[, 2])

# Combine common_words and difference
common_words <- cbind(common_words, difference)

# Order the data frame from most differences to least
common_words <- common_words[order(common_words[, 3], decreasing = TRUE), ]

# Create top25_df
top25_df <- data.frame(x = common_words[1:25, 1], 
                       y = common_words[1:25, 2], 
                       labels = rownames(common_words[1:25, ]))

# Create the pyramid plot
pyramid.plot(top25_df$x, top25_df$y,
             labels = top25_df$labels, gap = 60,
             top.labels = c("Coffee", "Words", "Tea"),
             main = "Words in Common", laxlab = NULL, 
             raxlab = NULL, unit = NULL)
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-50-1.png)

    ## [1] 5.1 4.1 4.1 2.1

Word Networks
-------------

In a network graph, the circles are called nodes and represent individual terms, while the lines connecting the circles are called edges and represent the connections between the terms.

For the over-caffeinated text miner, qdap provides a shorcut for making word networks. The word\_network\_plot() and word\_associate() functions both make word networks easy!

``` r
word_associate(coffee_tweets, match.string = c("books"), 
               stopwords = Top200Words, 
               network.plot = TRUE)
```

    ## Warning in text2color(words = V(g)$label, recode.words = target.words,
    ## colors = label.colors): length of colors should be 1 more than length of
    ## recode.words

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-51-1.png)

    ##   row group unit text                                                                                                           
    ## 1  23   all   23 Beauty may get him killed!#book #Amazon #Readers #UK #Romance #books #musiclover #greatreads #Historical       
    ## 2 329   all  329 8 New Books We Recommend This Week #coffee #books #booklover #amreading                                        
    ## 3 398   all  398 Follow the Ghosts in his Past...BLACKHEART!#Romance #books #novel #story #History #historicalromance #Amazon   
    ## 4 437   all  437 Your cup of #coffee and this post on my #blog. Sounds like a good day to me! =0) #booklove #amreading #books...
    ## 5 749   all  749 A coffee shop, a kiss, will this love last a lifetime? #kindlebooks #coffee #epub #kindle

    ## 
    ## Match Terms
    ## ===========

    ## 
    ## List 1:
    ## books, kindlebooks

    ## 

Distance Matrix and Dendograms
==============================

First let's limit the number of words in your TDM using removeSparseTerms() from tm. Why would you want to adjust the sparsity of the TDM/DTM?

TDMs and DTMs are sparse, meaning they contain mostly zeros. Remember that 1000 tweets can become a TDM with over 3000 terms! You won't be able to easily interpret a dendrogram that is so cluttered, especially if you are working on more text.

A good TDM has between 25 and 70 terms. The lower the sparse value, the more terms are kept. The closer it is to 1, the fewer are kept. This value is a percentage cutoff of zeros for each term in the TDM.

Let's see the dimensions of your coffee tdm

``` r
dim(coffee_tdm)
```

    ## [1] 3435 1000

Let's remove some terms

``` r
coffee_tdm1 <- removeSparseTerms(coffee_tdm, sparse = 0.97)
dim(coffee_tdm1)
```

    ## [1]   13 1000

Let's see a dendrogram now

``` r
coffee_tdm1_m <- as.matrix(coffee_tdm1)
coffee_tdm1_df <- as.data.frame(coffee_tdm1_m)  
coffee_dist <- dist(coffee_tdm1_df)

coffee_hc <- hclust(coffee_dist)
plot(coffee_hc)
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-54-1.png)

Now let's make the dendrogram more appealing

``` r
library(dendextend)
```

Now

``` r
hcd <- as.dendrogram(coffee_hc)
labels(hcd)
```

    ##  [1] "morning"   "sunday"    "day"       "great"     "love"     
    ##  [6] "starbucks" "amp"       "good"      "shop"      "latte"    
    ## [11] "via"       "mug"       "tea"

Now let's work on the appearance

``` r
hcd <- branches_attr_by_labels(hcd, c("mondaymorning", "work"), "red")
```

    ## Warning in branches_attr_by_labels(hcd, c("mondaymorning", "work"), "red"): Not all of the labels you provided are included in the dendrogram.
    ## The following labels were omitted:mondaymorningwork

``` r
plot(hcd, main = "Better Dendrogram")
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-57-1.png) Now let's add rectangular shapes around the clusters

``` r
# Add cluster rectangles 
plot(hcd, main = "Better Dendrogram")
rect.dendrogram(hcd, k = 2, border = "grey50")
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-58-1.png)

Word Associations
=================

Another way to think about word relationships is with the findAssocs() function in the tm package. For any given word, findAssocs() calculates its correlation with every other word in a TDM or DTM. Scores range from 0 to 1. A score of 1 means that two words always appear together, while a score of 0 means that they never appear together.

To use findAssocs() pass in a TDM or DTM, the search term, and a minimum correlation. The function will return a list of all other terms that meet or exceed the minimum threshold.

    findAssocs(tdm, "word", 0.25)

``` r
# Create associations
associations <- findAssocs(coffee_tdm, "starbucks", 0.2)

# View the venti associations
print(associations)
```

    ## $starbucks
    ##         frappuccino               yummy           argentina 
    ##                0.34                0.26                0.25 
    ##              faster           fireworks               means 
    ##                0.25                0.25                0.25 
    ##           oversized     recentforrecent        relaxingtime 
    ##                0.25                0.25                0.25 
    ##                sbux underthebuckeyetree          weekendoff 
    ##                0.25                0.25                0.25 
    ##               latte 
    ##                0.22

As you can see, some more serious cleaning of the text should be done to gain real interesting insights.

``` r
library(ggthemes)

# Create associations_df
associations_df <- list_vect2df(associations)[,2:3]

# Plot the associations_df values (don't change this)
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3) + 
  theme_gdocs()
```

![](text-mining-notes_files/figure-markdown_github/unnamed-chunk-60-1.png)

Similarity matrix
=================

Another very useful thing to calculate is the similarity matrix between tweets. I won't explain here what it is, but show how to calculate it. I will use the cosine distance here (note that the following piece of code may take sometime, since it must calculate one million of values)

``` r
require(proxy)

coffee_tdm_m <- as.matrix(coffee_tdm)

coffee_cosine_dist_mat <- as.matrix(dist(coffee_tdm_m, method = "cosine"))
```

what dimensions we have in this matrix?

``` r
dim(coffee_cosine_dist_mat)
```

    ## [1] 3435 3435

as expected. Let's check some rows

``` r
coffee_cosine_dist_mat[1:5,1:5]
```

    ##                 aandladventures abbeygroup absolute absolutely accents
    ## aandladventures               0          1        1          1       1
    ## abbeygroup                    1          0        1          1       1
    ## absolute                      1          1        0          1       1
    ## absolutely                    1          1        1          0       1
    ## accents                       1          1        1          1       0

We can do the same calculations using the fact we have sparse matrices

``` r
library(slam)
cosine_dist_mat <- crossprod_simple_triplet_matrix(coffee_tdm)/(sqrt(col_sums(coffee_tdm^2) %*% t(col_sums(coffee_tdm^2))))
```

``` r
cosine_dist_mat[1:15,1:15]
```

    ##     Docs
    ## Docs         1 2 3 4         5 6         7         8 9 10 11 12 13 14 15
    ##   1  1.0000000 0 0 0 0.1690309 0 0.0000000 0.0000000 0  0  0  0  0  0  0
    ##   2  0.0000000 1 0 0 0.0000000 0 0.0000000 0.0000000 0  0  0  0  0  0  0
    ##   3  0.0000000 0 1 0 0.0000000 0 0.0000000 0.0000000 0  0  0  0  0  0  0
    ##   4  0.0000000 0 0 1 0.0000000 0 0.0000000 0.0000000 0  0  0  0  0  0  0
    ##   5  0.1690309 0 0 0 1.0000000 0 0.0000000 0.0000000 0  0  0  0  0  0  0
    ##   6  0.0000000 0 0 0 0.0000000 1 0.0000000 0.0000000 0  0  0  0  0  0  0
    ##   7  0.0000000 0 0 0 0.0000000 0 1.0000000 0.1066004 0  0  0  0  0  0  0
    ##   8  0.0000000 0 0 0 0.0000000 0 0.1066004 1.0000000 0  0  0  0  0  0  0
    ##   9  0.0000000 0 0 0 0.0000000 0 0.0000000 0.0000000 1  0  0  0  0  0  0
    ##   10 0.0000000 0 0 0 0.0000000 0 0.0000000 0.0000000 0  1  1  1  1  0  0
    ##   11 0.0000000 0 0 0 0.0000000 0 0.0000000 0.0000000 0  1  1  1  1  0  0
    ##   12 0.0000000 0 0 0 0.0000000 0 0.0000000 0.0000000 0  1  1  1  1  0  0
    ##   13 0.0000000 0 0 0 0.0000000 0 0.0000000 0.0000000 0  1  1  1  1  0  0
    ##   14 0.0000000 0 0 0 0.0000000 0 0.0000000 0.0000000 0  0  0  0  0  1  0
    ##   15 0.0000000 0 0 0 0.0000000 0 0.0000000 0.0000000 0  0  0  0  0  0  1

Tweets 14 and 4 seems similar. Let's check them

``` r
print(coffee_tweets[[14]])
```

    ## [1] "When your keyboard changes \"Coffee\" to \"Covfefe\". \n#Trump\n#Coffee"

``` r
print(coffee_tweets[[5]])
```

    ## [1] "Want to know more about the #CafeRacer? Watch this space! #video #coffee #wearesanremo"

Bag of words
============

``` r
my.tdm <- TermDocumentMatrix(coffee_corpus, control = list(weighting = weightTfIdf))
my.dtm <- DocumentTermMatrix(coffee_corpus, control = list(weighting = weightTfIdf, stopwords = TRUE))
inspect(my.dtm)
```

    ## <<DocumentTermMatrix (documents: 1000, terms: 4306)>>
    ## Non-/sparse entries: 9213/4296787
    ## Sparsity           : 100%
    ## Maximal term length: 98
    ## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
    ## Sample             :
    ##      Terms
    ## Docs  #coffeetime #mug #starbucks &amp; coffee cup day good great morning
    ##   256           0    0          0     0      0   0   0    0     0       0
    ##   502           0    0          0     0      0   0   0    0     0       0
    ##   668           0    0          0     0      0   0   0    0     0       0
    ##   669           0    0          0     0      0   0   0    0     0       0
    ##   748           0    0          0     0      0   0   0    0     0       0
    ##   786           0    0          0     0      0   0   0    0     0       0
    ##   879           0    0          0     0      0   0   0    0     0       0
    ##   919           0    0          0     0      0   0   0    0     0       0
    ##   938           0    0          0     0      0   0   0    0     0       0
    ##   978           0    0          0     0      0   0   0    0     0       0

Let's find (for example) all words that appear twice in any document

``` r
findFreqTerms(my.tdm, 200)
```

    ## character(0)

``` r
cosine_dist_mat <- crossprod_simple_triplet_matrix(my.tdm)/(sqrt(col_sums(my.tdm^2) %*% t(col_sums(my.tdm^2))))
cosine_dist_mat[1:5,1:5]
```

    ##     Docs
    ## Docs          1            2            3            4            5
    ##    1 1.00000000 0.000000e+00 0.000000e+00 0.000000e+00 6.807778e-02
    ##    2 0.00000000 1.000000e+00 5.810629e-05 4.406399e-05 1.502096e-02
    ##    3 0.00000000 5.810629e-05 1.000000e+00 6.872237e-05 5.609138e-05
    ##    4 0.00000000 4.406399e-05 6.872237e-05 1.000000e+00 4.253602e-05
    ##    5 0.06807778 1.502096e-02 5.609138e-05 4.253602e-05 1.000000e+00

``` r
y <- which(cosine_dist_mat>0.5, arr.in = TRUE)
str(y)
```

    ##  int [1:1612, 1:2] 1 2 3 4 5 6 7 8 9 10 ...
    ##  - attr(*, "dimnames")=List of 2
    ##   ..$ : chr [1:1612] "1" "2" "3" "4" ...
    ##   ..$ : chr [1:2] "Docs" "Docs"

``` r
y
```

    ##      Docs Docs
    ## 1       1    1
    ## 2       2    2
    ## 3       3    3
    ## 4       4    4
    ## 5       5    5
    ## 6       6    6
    ## 7       7    7
    ## 8       8    8
    ## 9       9    9
    ## 10     10   10
    ## 11     11   10
    ## 12     12   10
    ## 13     13   10
    ## 10     10   11
    ## 11     11   11
    ## 12     12   11
    ## 13     13   11
    ## 10     10   12
    ## 11     11   12
    ## 12     12   12
    ## 13     13   12
    ## 10     10   13
    ## 11     11   13
    ## 12     12   13
    ## 13     13   13
    ## 14     14   14
    ## 15     15   15
    ## 16     16   16
    ## 17     17   17
    ## 18     18   18
    ## 19     19   19
    ## 179   179   19
    ## 20     20   20
    ## 21     21   21
    ## 22     22   22
    ## 23     23   23
    ## 24     24   24
    ## 25     25   25
    ## 26     26   26
    ## 27     27   26
    ## 26     26   27
    ## 27     27   27
    ## 28     28   28
    ## 29     29   29
    ## 30     30   30
    ## 729   729   30
    ## 31     31   31
    ## 32     32   32
    ## 33     33   33
    ## 34     34   34
    ## 55     55   34
    ## 35     35   35
    ## 36     36   36
    ## 37     37   37
    ## 38     38   38
    ## 597   597   38
    ## 39     39   39
    ## 40     40   40
    ## 41     41   41
    ## 42     42   42
    ## 43     43   43
    ## 124   124   43
    ## 130   130   43
    ## 132   132   43
    ## 136   136   43
    ## 142   142   43
    ## 190   190   43
    ## 194   194   43
    ## 44     44   44
    ## 45     45   45
    ## 46     46   46
    ## 47     47   47
    ## 48     48   48
    ## 49     49   49
    ## 50     50   50
    ## 51     51   51
    ## 201   201   51
    ## 52     52   52
    ## 53     53   53
    ## 54     54   54
    ## 34     34   55
    ## 55     55   55
    ## 56     56   56
    ## 57     57   57
    ## 484   484   57
    ## 58     58   58
    ## 59     59   59
    ## 60     60   60
    ## 61     61   61
    ## 62     62   62
    ## 63     63   63
    ## 64     64   64
    ## 65     65   65
    ## 67     67   65
    ## 66     66   66
    ## 65     65   67
    ## 67     67   67
    ## 68     68   68
    ## 69     69   69
    ## 70     70   70
    ## 71     71   71
    ## 72     72   72
    ## 113   113   72
    ## 73     73   73
    ## 445   445   73
    ## 633   633   73
    ## 74     74   74
    ## 75     75   75
    ## 76     76   76
    ## 77     77   77
    ## 78     78   78
    ## 79     79   79
    ## 80     80   80
    ## 81     81   81
    ## 82     82   82
    ## 84     84   82
    ## 86     86   82
    ## 83     83   83
    ## 82     82   84
    ## 84     84   84
    ## 86     86   84
    ## 85     85   85
    ## 82     82   86
    ## 84     84   86
    ## 86     86   86
    ## 87     87   87
    ## 88     88   88
    ## 89     89   89
    ## 140   140   89
    ## 90     90   90
    ## 91     91   91
    ## 163   163   91
    ## 334   334   91
    ## 365   365   91
    ## 463   463   91
    ## 464   464   91
    ## 522   522   91
    ## 732   732   91
    ## 92     92   92
    ## 93     93   93
    ## 94     94   94
    ## 95     95   95
    ## 96     96   96
    ## 97     97   97
    ## 98     98   98
    ## 99     99   99
    ## 100   100  100
    ## 101   101  101
    ## 102   102  102
    ## 307   307  102
    ## 360   360  102
    ## 426   426  102
    ## 517   517  102
    ## 599   599  102
    ## 103   103  103
    ## 104   104  104
    ## 105   105  105
    ## 106   106  106
    ## 107   107  107
    ## 108   108  108
    ## 109   109  109
    ## 110   110  110
    ## 111   111  111
    ## 112   112  112
    ## 72     72  113
    ## 113   113  113
    ## 114   114  114
    ## 115   115  115
    ## 116   116  116
    ## 117   117  117
    ## 118   118  118
    ## 370   370  118
    ## 119   119  119
    ## 120   120  120
    ## 121   121  121
    ## 122   122  122
    ## 123   123  123
    ## 43     43  124
    ## 124   124  124
    ## 130   130  124
    ## 132   132  124
    ## 136   136  124
    ## 142   142  124
    ## 190   190  124
    ## 194   194  124
    ## 125   125  125
    ## 128   128  125
    ## 126   126  126
    ## 127   127  127
    ## 125   125  128
    ## 128   128  128
    ## 129   129  129
    ## 43     43  130
    ## 124   124  130
    ## 130   130  130
    ## 132   132  130
    ## 136   136  130
    ## 142   142  130
    ## 131   131  131
    ## 43     43  132
    ## 124   124  132
    ## 130   130  132
    ## 132   132  132
    ## 136   136  132
    ## 142   142  132
    ## 190   190  132
    ## 194   194  132
    ## 133   133  133
    ## 134   134  134
    ## 135   135  135
    ## 43     43  136
    ## 124   124  136
    ## 130   130  136
    ## 132   132  136
    ## 136   136  136
    ## 453   453  136
    ## 137   137  137
    ## 138   138  138
    ## 139   139  139
    ## 89     89  140
    ## 140   140  140
    ## 141   141  141
    ## 43     43  142
    ## 124   124  142
    ## 130   130  142
    ## 132   132  142
    ## 142   142  142
    ## 190   190  142
    ## 192   192  142
    ## 194   194  142
    ## 198   198  142
    ## 200   200  142
    ## 143   143  143
    ## 144   144  144
    ## 145   145  145
    ## 146   146  146
    ## 147   147  147
    ## 148   148  148
    ## 149   149  149
    ## 184   184  149
    ## 225   225  149
    ## 287   287  149
    ## 339   339  149
    ## 341   341  149
    ## 344   344  149
    ## 351   351  149
    ## 353   353  149
    ## 150   150  150
    ## 151   151  151
    ## 152   152  152
    ## 153   153  152
    ## 152   152  153
    ## 153   153  153
    ## 154   154  154
    ## 155   155  155
    ## 729   729  155
    ## 894   894  155
    ## 156   156  156
    ## 157   157  157
    ## 158   158  158
    ## 159   159  159
    ## 160   160  160
    ## 161   161  161
    ## 162   162  162
    ## 91     91  163
    ## 163   163  163
    ## 334   334  163
    ## 365   365  163
    ## 463   463  163
    ## 464   464  163
    ## 522   522  163
    ## 732   732  163
    ## 164   164  164
    ## 165   165  165
    ## 166   166  166
    ## 167   167  167
    ## 168   168  168
    ## 169   169  169
    ## 170   170  170
    ## 171   171  171
    ## 172   172  172
    ## 173   173  173
    ## 174   174  174
    ## 175   175  175
    ## 176   176  176
    ## 177   177  177
    ## 178   178  178
    ## 19     19  179
    ## 179   179  179
    ## 180   180  180
    ## 181   181  181
    ## 450   450  181
    ## 182   182  182
    ## 183   183  183
    ## 149   149  184
    ## 184   184  184
    ## 225   225  184
    ## 287   287  184
    ## 339   339  184
    ## 341   341  184
    ## 344   344  184
    ## 351   351  184
    ## 353   353  184
    ## 185   185  185
    ## 186   186  186
    ## 187   187  187
    ## 729   729  187
    ## 188   188  188
    ## 189   189  189
    ## 43     43  190
    ## 124   124  190
    ## 132   132  190
    ## 142   142  190
    ## 190   190  190
    ## 192   192  190
    ## 194   194  190
    ## 198   198  190
    ## 199   199  190
    ## 200   200  190
    ## 461   461  190
    ## 191   191  191
    ## 142   142  192
    ## 190   190  192
    ## 192   192  192
    ## 194   194  192
    ## 198   198  192
    ## 199   199  192
    ## 200   200  192
    ## 461   461  192
    ## 193   193  193
    ## 43     43  194
    ## 124   124  194
    ## 132   132  194
    ## 142   142  194
    ## 190   190  194
    ## 192   192  194
    ## 194   194  194
    ## 198   198  194
    ## 199   199  194
    ## 200   200  194
    ## 449   449  194
    ## 453   453  194
    ## 461   461  194
    ## 195   195  195
    ## 196   196  196
    ## 197   197  197
    ## 142   142  198
    ## 190   190  198
    ## 192   192  198
    ## 194   194  198
    ## 198   198  198
    ## 199   199  198
    ## 200   200  198
    ## 461   461  198
    ## 190   190  199
    ## 192   192  199
    ## 194   194  199
    ## 198   198  199
    ## 199   199  199
    ## 200   200  199
    ## 142   142  200
    ## 190   190  200
    ## 192   192  200
    ## 194   194  200
    ## 198   198  200
    ## 199   199  200
    ## 200   200  200
    ## 461   461  200
    ## 51     51  201
    ## 201   201  201
    ## 202   202  202
    ## 203   203  203
    ## 204   204  204
    ## 592   592  204
    ## 205   205  205
    ## 206   206  206
    ## 601   601  206
    ## 207   207  207
    ## 208   208  208
    ## 500   500  208
    ## 209   209  209
    ## 210   210  209
    ## 214   214  209
    ## 209   209  210
    ## 210   210  210
    ## 214   214  210
    ## 211   211  211
    ## 212   212  212
    ## 213   213  213
    ## 209   209  214
    ## 210   210  214
    ## 214   214  214
    ## 215   215  215
    ## 216   216  216
    ## 217   217  217
    ## 218   218  218
    ## 219   219  219
    ## 220   220  220
    ## 221   221  221
    ## 222   222  222
    ## 223   223  223
    ## 224   224  224
    ## 149   149  225
    ## 184   184  225
    ## 225   225  225
    ## 287   287  225
    ## 339   339  225
    ## 341   341  225
    ## 344   344  225
    ## 351   351  225
    ## 353   353  225
    ## 226   226  226
    ## 227   227  227
    ## 228   228  228
    ## 229   229  229
    ## 230   230  230
    ## 231   231  231
    ## 232   232  231
    ## 231   231  232
    ## 232   232  232
    ## 233   233  233
    ## 234   234  234
    ## 235   235  235
    ## 236   236  236
    ## 237   237  237
    ## 238   238  238
    ## 239   239  239
    ## 240   240  240
    ## 241   241  241
    ## 242   242  242
    ## 243   243  243
    ## 244   244  244
    ## 246   246  244
    ## 247   247  244
    ## 569   569  244
    ## 632   632  244
    ## 245   245  245
    ## 244   244  246
    ## 246   246  246
    ## 244   244  247
    ## 247   247  247
    ## 569   569  247
    ## 248   248  248
    ## 249   249  249
    ## 559   559  249
    ## 250   250  250
    ## 251   251  251
    ## 252   252  252
    ## 253   253  253
    ## 254   254  254
    ## 255   255  255
    ## 256   256  256
    ## 257   257  257
    ## 258   258  257
    ## 257   257  258
    ## 258   258  258
    ## 259   259  259
    ## 260   260  260
    ## 261   261  261
    ## 262   262  262
    ## 931   931  262
    ## 263   263  263
    ## 264   264  264
    ## 265   265  265
    ## 266   266  266
    ## 267   267  267
    ## 729   729  267
    ## 268   268  268
    ## 269   269  269
    ## 270   270  270
    ## 271   271  271
    ## 272   272  272
    ## 273   273  273
    ## 274   274  274
    ## 275   275  275
    ## 276   276  276
    ## 447   447  276
    ## 858   858  276
    ## 277   277  277
    ## 278   278  278
    ## 279   279  278
    ## 281   281  278
    ## 282   282  278
    ## 283   283  278
    ## 278   278  279
    ## 279   279  279
    ## 281   281  279
    ## 282   282  279
    ## 283   283  279
    ## 280   280  280
    ## 278   278  281
    ## 279   279  281
    ## 281   281  281
    ## 282   282  281
    ## 283   283  281
    ## 278   278  282
    ## 279   279  282
    ## 281   281  282
    ## 282   282  282
    ## 283   283  282
    ##  [ reached getOption("max.print") -- omitted 1112 rows ]

``` r
print(coffee_tweets[[659]])
```

    ## [1] "It's Saturday morning, so you're probably a bit hungover yeah? Do not worry, we've got the cure for you! #Coffee"

``` r
print(coffee_tweets[[292]])
```

    ## [1] "Does your coffee help you #loseweight? Get a sample at #weightloss #coffee"

So really very similar...

and we can extract the values of the matrix with

``` r
cosine_dist_mat[y]
```

    ##    [1] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##    [7] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##   [13] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##   [19] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##   [25] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##   [31] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##   [37] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##   [43] 1.0000000 1.0000000 1.0000000 0.5780789 1.0000000 1.0000000
    ##   [49] 1.0000000 1.0000000 0.9009366 1.0000000 1.0000000 1.0000000
    ##   [55] 1.0000000 0.8113901 1.0000000 1.0000000 1.0000000 1.0000000
    ##   [61] 1.0000000 1.0000000 0.7985142 1.0000000 0.7971099 0.5252309
    ##   [67] 0.6552737 0.5059945 1.0000000 1.0000000 1.0000000 1.0000000
    ##   [73] 1.0000000 1.0000000 1.0000000 1.0000000 0.5453228 1.0000000
    ##   [79] 1.0000000 1.0000000 0.9009366 1.0000000 1.0000000 1.0000000
    ##   [85] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##   [91] 1.0000000 1.0000000 1.0000000 0.8847583 1.0000000 0.8847583
    ##   [97] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [103] 1.0000000 1.0000000 0.5747610 0.5455130 1.0000000 1.0000000
    ##  [109] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [115] 1.0000000 1.0000000 0.6807897 1.0000000 1.0000000 1.0000000
    ##  [121] 0.6807897 1.0000000 0.6807897 0.6807897 1.0000000 1.0000000
    ##  [127] 1.0000000 1.0000000 0.9355991 1.0000000 1.0000000 1.0000000
    ##  [133] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [139] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [145] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [151] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [157] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [163] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [169] 1.0000000 1.0000000 1.0000000 0.8322613 1.0000000 1.0000000
    ##  [175] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 0.7985142
    ##  [181] 1.0000000 0.7971099 0.5252309 0.6552737 0.5059945 1.0000000
    ##  [187] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [193] 0.7985142 0.7985142 1.0000000 0.7985142 0.7623418 0.5023215
    ##  [199] 1.0000000 1.0000000 1.0000000 0.7985142 1.0000000 0.7971099
    ##  [205] 0.5252309 0.6552737 0.5059945 1.0000000 1.0000000 1.0000000
    ##  [211] 0.7971099 0.7971099 0.7623418 0.7971099 1.0000000 0.5224589
    ##  [217] 1.0000000 1.0000000 1.0000000 0.9355991 1.0000000 1.0000000
    ##  [223] 0.5252309 0.5252309 0.5023215 0.5252309 1.0000000 0.5699403
    ##  [229] 0.5566438 0.6539572 0.5735905 0.5678209 1.0000000 1.0000000
    ##  [235] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [241] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [247] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [253] 1.0000000 1.0000000 1.0000000 0.5326942 0.5087987 1.0000000
    ##  [259] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [265] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [271] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [277] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [283] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [289] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [295] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [301] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [307] 0.5414174 1.0000000 1.0000000 0.6552737 0.6552737 0.6552737
    ##  [313] 0.5699403 1.0000000 0.8379020 0.7127685 0.6251743 0.5122376
    ##  [319] 0.6188859 0.5229224 1.0000000 0.5566438 0.8379020 1.0000000
    ##  [325] 0.6961399 0.6208430 0.5002872 0.6044475 0.5107228 1.0000000
    ##  [331] 0.5059945 0.5059945 0.5059945 0.6539572 0.7127685 0.6961399
    ##  [337] 1.0000000 0.7173335 0.5877483 0.7101180 0.6154253 0.5057279
    ##  [343] 0.7840057 1.0000000 1.0000000 1.0000000 0.5735905 0.6251743
    ##  [349] 0.6208430 0.7173335 1.0000000 0.5308004 0.6228496 0.5262715
    ##  [355] 0.5122376 0.5002872 0.5877483 0.5308004 1.0000000 0.8276769
    ##  [361] 0.5678209 0.6188859 0.6044475 0.7101180 0.6228496 0.8276769
    ##  [367] 1.0000000 0.5209779 0.5453228 1.0000000 1.0000000 1.0000000
    ##  [373] 1.0000000 1.0000000 1.0000000 1.0000000 0.8124207 1.0000000
    ##  [379] 1.0000000 0.8737549 1.0000000 0.9123661 0.9005015 0.9123661
    ##  [385] 1.0000000 0.9169851 1.0000000 1.0000000 1.0000000 0.9005015
    ##  [391] 0.9169851 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [397] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [403] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [409] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [415] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [421] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [427] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [433] 0.5093740 0.6084673 0.6220686 0.5541926 1.0000000 0.5093740
    ##  [439] 1.0000000 0.6084673 1.0000000 0.5260713 1.0000000 1.0000000
    ##  [445] 0.7981151 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [451] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [457] 1.0000000 1.0000000 1.0000000 1.0000000 0.6283654 1.0000000
    ##  [463] 1.0000000 1.0000000 1.0000000 1.0000000 0.5144751 1.0000000
    ##  [469] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [475] 1.0000000 1.0000000 0.8906475 1.0000000 1.0000000 1.0000000
    ##  [481] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [487] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [493] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [499] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [505] 1.0000000 1.0000000 1.0000000 1.0000000 0.7965334 1.0000000
    ##  [511] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [517] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [523] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [529] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [535] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [541] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [547] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [553] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [559] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 0.7571570
    ##  [565] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [571] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [577] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [583] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [589] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [595] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [601] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [607] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [613] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [619] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [625] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [631] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [637] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [643] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [649] 1.0000000 1.0000000 1.0000000 1.0000000 0.6784411 1.0000000
    ##  [655] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [661] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [667] 0.9579119 1.0000000 1.0000000 1.0000000 0.9579119 0.8322613
    ##  [673] 1.0000000 0.9579119 0.9579119 1.0000000 1.0000000 1.0000000
    ##  [679] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [685] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [691] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [697] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 0.5713227
    ##  [703] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [709] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [715] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [721] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [727] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [733] 0.7965334 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [739] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [745] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [751] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [757] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [763] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [769] 1.0000000 1.0000000 0.5747610 1.0000000 1.0000000 0.8906475
    ##  [775] 1.0000000 0.8906475 1.0000000 0.6154253 1.0000000 0.6545889
    ##  [781] 0.6277790 1.0000000 1.0000000 1.0000000 1.0000000 0.5224589
    ##  [787] 0.5057279 0.6545889 1.0000000 0.5181023 1.0000000 1.0000000
    ##  [793] 1.0000000 1.0000000 0.9951305 1.0000000 1.0000000 1.0000000
    ##  [799] 0.5229224 0.5107228 0.7840057 0.5262715 0.5209779 0.6277790
    ##  [805] 0.5181023 1.0000000 0.5616957 1.0000000 1.0000000 1.0000000
    ##  [811] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [817] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [823] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [829] 1.0000000 1.0000000 0.5616957 1.0000000 1.0000000 1.0000000
    ##  [835] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [841] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [847] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [853] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 0.9109665
    ##  [859] 0.9109665 1.0000000 1.0000000 1.0000000 1.0000000 0.8737549
    ##  [865] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [871] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [877] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [883] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [889] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [895] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [901] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [907] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [913] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [919] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [925] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [931] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [937] 0.7981151 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [943] 0.5512571 1.0000000 1.0000000 1.0000000 0.9951305 1.0000000
    ##  [949] 1.0000000 0.6220686 0.5260713 1.0000000 1.0000000 1.0000000
    ##  [955] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [961] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [967] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [973] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [979] 1.0000000 1.0000000 1.0000000 0.8113901 1.0000000 1.0000000
    ##  [985] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [991] 1.0000000 0.8124207 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [997] 1.0000000 1.0000000 1.0000000 1.0000000
    ##  [ reached getOption("max.print") -- omitted 612 entries ]

Another way of doing TF-IDF
===========================

Another way of doing TF-IDF is the following

``` r
dtm <- DocumentTermMatrix(coffee_corpus)
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[1:10, 1001:1010])
```

    ## <<DocumentTermMatrix (documents: 10, terms: 10)>>
    ## Non-/sparse entries: 0/100
    ## Sparsity           : 100%
    ## Maximal term length: 22
    ## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)
    ## Sample             :
    ##     Terms
    ## Docs #musicfestival #musician #musiclover #musthavemycaffeinefix
    ##   1               0         0           0                      0
    ##   10              0         0           0                      0
    ##   2               0         0           0                      0
    ##   3               0         0           0                      0
    ##   4               0         0           0                      0
    ##   5               0         0           0                      0
    ##   6               0         0           0                      0
    ##   7               0         0           0                      0
    ##   8               0         0           0                      0
    ##   9               0         0           0                      0
    ##     Terms
    ## Docs #mybabygirl #myfave #myhappyplace #myhome #mylittlegirl #mypeople
    ##   1            0       0             0       0             0         0
    ##   10           0       0             0       0             0         0
    ##   2            0       0             0       0             0         0
    ##   3            0       0             0       0             0         0
    ##   4            0       0             0       0             0         0
    ##   5            0       0             0       0             0         0
    ##   6            0       0             0       0             0         0
    ##   7            0       0             0       0             0         0
    ##   8            0       0             0       0             0         0
    ##   9            0       0             0       0             0         0

Keep Tweets Metadata
--------------------

Depending on what you are trying to accomplish, you may want to keep metadata about the document when you create a TDM or DTM. This metadata can be incorporated into the corpus fairly easily by creating a readerControl list and applying it to a DataframeSource when calling `VCorpus()`.

You will need to know the column names of the data frame containing the metadata to be captured. The `names()` function is helpful for this.

To capture the text column of the coffee tweets text along with a metadata column of unique numbers called num you would use the code below.

    custom_reader <- readTabular(
      mapping = list(content = "text", id = "num")
    )
    text_corpus <- VCorpus(
      DataframeSource(tweets), 
      readerControl = list(reader = custom_reader)
    )

An example is

``` r
df <- do.call("rbind", lapply(c_tweets, as.data.frame))

df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))


# Add author to custom reading list
custom_reader <- readTabular(mapping = list(content = "text", 
                                            id = "id",
                                            author = "screenName",
                                            date = "created"
                                            ))

# Make corpus with custom reading
text_corpus <- VCorpus(DataframeSource(df), readerControl = list(reader = custom_reader))

# Clean corpus
text_corpus <- clean_corpus(text_corpus)

# Print data
text_corpus[[1]][1]
```

    ## $content
    ## [1] "  know  make  dilorenzocaffe doughboxdiner doughbox coffeetime httpstcoqmi8pfixzc"

``` r
# Print metadata
text_corpus[[1]][2]
```

    ## $meta
    ##   id      : 876338214482714624
    ##   author  : DoughboxDiner
    ##   date    : 2017-06-18 07:18:06
    ##   language: en
