# server.R

library(streamR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(ROAuth)
library(bitops)
library(shiny)
library(twitteR)
library(plyr)
library(sqldf)
library(bitops)
library(streamR)

shinyServer(function(input, output) {
  

  output$electionPlot <- renderPlot({
    
    #prints selected variables
    zzz <- input$var1
    print(zzz)
    xxx <- input$var2
    print(xxx)
    
    #if input selected by user is weekly
    if(input$var2 == "Weekly"){
      
      #if input selected by user is weekly and (donald trump or hillary clinton or bernie sanders or ted cruz)
      if(input$var1 == "Donald Trump"){
        json_data1 <- readLines('tweetsdonaldtrump.json', warn = FALSE)
      } else if(input$var1 == "Hillary Clinton"){
        json_data1 <- readLines('tweetshillaryclinton.json', warn = FALSE)
      } else if(input$var1 == "Bernie Sanders"){
        json_data1 <- readLines('tweetsberniesanders.json', warn = FALSE)
      } else{
        json_data1 <- readLines('tweetstedcruz.json', warn = FALSE)
      }
      
      #performs sentiment file based on positive and negative words defined in files and constructs a dynamic barplot
      json_df <- jsonlite::fromJSON(json_data1)
      json_text <- json_df$text
      json_clean <- iconv(json_text, 'UTF-8', 'ASCII')
      pos = scan('positive-words.txt', what='character', comment.char=';')
      neg = scan('negative-words.txt', what='character', comment.char=';')
      source('sentiment_new.r')
      analysis = score.sentiment(json_clean, pos, neg)
      print(table(analysis$score))
      x <- sqldf('SELECT count(score) as A,score as B FROM analysis GROUP BY B')
      a <- x$B
      b <- x$A
      barplot(table(analysis$score), main = "US Election Trends 2016", xlab="Tweet Sentiment Score(Non-Streaming)", ylab="Supporting Tweets", las=2, col=rainbow(12), beside=TRUE, legend = c(a), args.legend = list(title = "Score", x = "topright", cex = .8))
      
    } else {
      
      #load twitter live streaming authentication file
      load("my_oauth.Rdata")
      
      search_keyword <- input$var1
      print(search_keyword)
      
      #if input selected by user is current and (donald trump or hillary clinton or bernie sanders or ted cruz)
      if(search_keyword == "Donald Trump"){
        
        filterStream(file.name = "tweetsdonaldtrump.json", # Save tweets in a json file
                     track = c(search_keyword, "election", "president"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
                     language = "en",
                     timeout = 15, # Keep connection alive for 60 seconds
                     oauth = my_oauth) # Use my_oauth file as the OAuth credentials
        
        tweets.df <- parseTweets("tweetsdonaldtrump.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
        tweets.df
        
      } else if(search_keyword == "Hillary Clinton"){
        
        filterStream(file.name = "tweetshillaryclinton.json", # Save tweets in a json file
                     track = c(search_keyword, "election", "president"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
                     language = "en",
                     timeout = 15, # Keep connection alive for 60 seconds
                     oauth = my_oauth) # Use my_oauth file as the OAuth credentials
        
        tweets.df <- parseTweets("tweetshillaryclinton.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
        tweets.df
        
      } else if(search_keyword == "Bernie Sanders"){
        
        filterStream(file.name = "tweetsberniesanders.json", # Save tweets in a json file
                     track = c(search_keyword, "election", "president"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
                     language = "en",
                     timeout = 15, # Keep connection alive for 60 seconds
                     oauth = my_oauth) # Use my_oauth file as the OAuth credentials
        
        tweets.df <- parseTweets("tweetsberniesanders.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
        tweets.df
        
      } else {
        
        filterStream(file.name = "tweetstedcruz.json", # Save tweets in a json file
                     track = c(search_keyword, "election", "president"), # Collect tweets mentioning either Affordable Care Act, ACA, or Obamacare
                     language = "en",
                     timeout = 15, # Keep connection alive for 60 seconds
                     oauth = my_oauth) # Use my_oauth file as the OAuth credentials
        
        tweets.df <- parseTweets("tweetstedcruz.json", simplify = FALSE) # parse the json file and save to a data frame called tweets.df. Simplify = FALSE ensures that we include lat/lon information in that data frame.
        tweets.df
        
      }
     
      #performs sentiment file based on positive and negative words defined in files and constructs a dynamic barplot
      tweets.text <- tweets.df$text
      tweets.text <- iconv(tweets.text, 'UTF-8', 'ASCII')
      getwd()
      pos = scan('positive-words.txt', what='character', comment.char=';')
      neg = scan('negative-words.txt', what='character', comment.char=';')
      source('sentiment_new.r')
      analysis = score.sentiment(tweets.text, pos, neg)
      print(table(analysis$score))
      x <- sqldf('SELECT count(score) as A,score as B FROM analysis GROUP BY B')
      a <- x$B
      b <- x$A
      barplot(table(analysis$score), main = "US Election Trends 2016", xlab="Tweet Sentiment Score(Streaming)", ylab="Supporting Tweets", las=2, col=rainbow(12), beside=TRUE, legend = c(a), args.legend = list(title = "Score", x = "topright", cex = .8))
    }
    
  })
  
})