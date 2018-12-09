#install.packages("twitteR")
#install.packages("leaflet")
#install.packages(DT)

library(twitteR)
library(stats)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)

to.plain <- function(s) {
  
  # 1 character substitutions
  old1 <- "źżśšžàáâãäåąçćèéêëęłìíîïðñńòóôõöùúûüý"
  new1 <- "zzsszaaaaaaacceeeeeliiiidnnooooouuuuy"
  s1 <- chartr(old1, new1, s)
  
  # 2 character substitutions
  old2 <- c("œ", "ß", "æ", "ø")
  new2 <- c("oe", "ss", "ae", "oe")
  s2 <- s1
  for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
  
  s2
}

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        textInput("searchkw", label = "search:", value = "#rstats"),
        dateInput("startDate", label = "Begin date", value = Sys.Date()-14),
        dateInput("endDate", label = "End date", value = Sys.Date()+1),
        selectInput("resultType", label="Result type", choices = c("popular", "recent", "mixed")),
        numericInput("numberOfTweets", label = "Tweets number", value=500 ),
        br(),
        selectInput("timestampsFreq", "Timestamp freqency", choices = c("15 mins", "30 mins", "45 mins", "1 hours",
                                                                        "1.5 hours", "2 hours", "3 hours", "6 hours", 
                                                                        "12 hours", "1 days", "3 days", "1 weeks")),
        br(),
        actionButton("startRender", label = "Generate"),
        #selectInput("dataset", "Select the dataset", choices = c("Aggregated", "Timestamps", "Users' actions")),
        #downloadButton("downloadData", "Export CSV"),
        width = 3
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Aggregated Data", plotOutput("plotAggr", width = "95%", height = "80vh")),
          tabPanel("Timestamps", plotOutput("plotDayByDay", width = "95%", height = "80vh")),
          tabPanel("Actions", plotOutput("actions", width = "95%", height = "80vh")),
          tabPanel("Table", dataTableOutput("table")),
          tabPanel("Locations", plotOutput("locations", width = "100%", height = "90vh")),
          type="pills"
        ),
        width = 9
      )
    )
  ),
  server = function(input, output) {
    
    #setup Twitter connections
    consumer_key <- "bGqjgT8oPgauAHghheqIgsuba"
    consumer_secret <- "7DxWQIulqPKALqRFSq4suDpwJwRB0d3aM2HkB9WLqrvj535GgH"
    access_token <- "337049853-yIoetwWCvW8MPn7YkAmF6JuRkjj2hZ3ERLMalJiy"
    access_secret <- "oPtmwAc2Ovp62wUSoX1xSQT6XK1vdqlatNppMup57mclS"
    options(httr_oauth_cache = TRUE)
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  
    #call observeEvent on startRender button click
    observeEvent(input$startRender, {
      
      #get parameters to find Tweets list and parse it to dataframe
      tweets <- twListToDF(searchTwitter(input$searchkw,
                                         n=input$numberOfTweets,
                                         since = as.character(input$startDate), 
                                         until = as.character(input$endDate),
                                         resultType = input$resultType,
                                         retryOnRateLimit = 5))
      #parse created date to readible format
      tweets$created <- as.character(tweets$created)
      
      print(colnames(tweets))
      
      #extract usernames to vector
      userNames <- tweets[,c("screenName")]
      
      #make dataframe from usernames
      users <- twListToDF(lookupUsers(userNames))
      
      #extract vector of users' locations
      locations <- users[,c("location")]
      
      #normalize and filter locations
      Encoding(locations)  <- "UTF-8"
      locations_corpus <- Corpus(VectorSource(locations))
      loc_clean <- tm_map(locations_corpus, removePunctuation)
      loc_clean <- tm_map(locations_corpus, content_transformer(tolower))
      loc_clean <- tm_map(loc_clean, removeWords, c(stopwords("english"), "worldwide", "world"))
      loc_clean <- tm_map(loc_clean, removeNumbers)
      loc_clean <- tm_map(loc_clean, stripWhitespace)
      loc_clean <- tm_map(loc_clean, to.plain)
      
      print(loc_clean)
      
      #merge tweets and users by screenName
      tweets <- merge(tweets, users, by="screenName")
      
      #make datetime sequence from begin to end date
      ts_seq <- seq(as.POSIXct(min(tweets$created.x)), as.POSIXct(max(tweets$created.x)), by=input$timestampsFreq)
      ts_vector <- vector()
      
      #control print
      print(input$searchkw)
      
      #assingment timestamp to tweet
      for(i in 1:nrow(tweets)){
        for(j in 2:length(ts_seq)){
          #print(tweets[i, 'created.x'])
          #print(ts_seq[j])
          if(tweets[i, 'created.x'] >= ts_seq[j-1] && tweets[i, 'created.x'] < ts_seq[j]){
            ts_vector <- c(ts_vector, as.character.Date(ts_seq[j-1]))
            break
          } else if(j == length(ts_seq) && tweets[i, 'created.x'] > ts_seq[j]){
            #print(j)
            ts_vector <- c(ts_vector, as.character.Date(ts_seq[j]))
          }
        }
      }
      
      #add column to tweets dataframe with timestamps
      tweets["timestamp"] <- ts_vector
      
      #count favourities and retweets to dataframes
      favs_df <- aggregate(tweets$favoriteCount , by = list(Date = tweets$timestamp), FUN = sum)
      retweets_df <- aggregate(tweets$retweetCount , by = list(Date = tweets$timestamp), FUN = sum)
      
      #merge both dfs into one
      actions_df <- merge(favs_df, retweets_df, by = "Date")
      
      #initialize empty vectors to aggregated data and day by day chart
      aggr_tweets_vector <- vector()
      day_by_day_tweets_vector <- vector()
      
      #count tweets by timestamps
      for(i in 1:length(ts_seq)){
        aggr_tweets_vector <- c(aggr_tweets_vector, length(which(tweets$created.x < ts_seq[i])))
        #print(which(tweets$created.x < ts_seq[i]))
        if(i < length(ts_seq)){
          day_by_day_tweets_vector <- c(day_by_day_tweets_vector,
                                        length(which(tweets$created.x > ts_seq[i] & tweets$created.x < ts_seq[i+1])))
          
        }
      }
      day_by_day_tweets_vector <- c(day_by_day_tweets_vector,
                                    length(which(tweets$created.x > ts_seq[length(ts_seq)-1])))
      
      #making dataframes to generate charts
      df_aggr <- data.frame(ts_seq, aggr_tweets_vector)
      df_day_by_day <- data.frame(ts_seq, day_by_day_tweets_vector)
      
      #generate aggregated chart
      output$plotAggr <- renderPlot({
        plot(df_aggr, type = "l", col = "blue")
      })
      
      #generate day by day chart
      output$plotDayByDay <- renderPlot({
        plot(df_day_by_day, type = "l", col = "blue")
      })
      
      #generate actions chart
      output$actions <- renderPlot({
        g<- ggplot(actions_df, aes(as.POSIXct(Date)))
        g <- g + geom_line(aes(y=x.x), colour="red")
        g <- g + geom_line(aes(y=x.y), colour="blue")
        plot(g)
        
      })
      
      #generate locations wordcloud
      output$locations <- renderPlot({
        wordcloud(loc_clean, 
                  scale = c(4, 3), 
                  min.freq = 3, 
                  colors = brewer.pal(4,"Blues"))
      })
      
      #render control dataTable
      output$table <- renderDataTable(
        tweets[c("text", "screenName", "created.x", "timestamp", "retweetCount", "favoriteCount", "location")]
      )
    })
    
    
  }
)