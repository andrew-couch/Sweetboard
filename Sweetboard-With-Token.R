library(shiny)
library(tidyverse)
library(tidytext)
library(rtweet)
library(rsconnect)
library(lexicon)
library(lubridate)
library(topicmodels)
library(scales)


rm(list = ls())
get_token()
options(scipen = 999)
options(shiny.launch.browser = .rs.invokePlumberWindowViewer)

ui <- fluidPage(
  h1(tags$div(class="header", checked=NA,
              tags$a(href="https://github.com/andrew-couch/Sweetboard", "Sweetboard"))),
  h3("Sentiment + Tweet Analytics Dashboard"),
  h4("Developed by Andrew Couch"),
  
  
  sidebarLayout(
    sidebarPanel(
      style = "position:fixed;width:20%;",
      width = 3,
      sliderInput(inputId = "top", label = "Top N", min = 1, max = 25, step = 1, value = 5),
      sliderInput(inputId = "ngram", label = "N-Grams", min = 1, max = 4, step = 1, value = 2),
      textInput("filterword", "Word to be filtered", ""),
      textInput("containword", "Ngram contains...", ""),
      textInput("negationword", "Negation word", "not"),
      checkboxInput("removestop", "Remove Stop Words", value = FALSE)),
    
    
    mainPanel(
    tabsetPanel(
      
      
      tabPanel("Tweet Data",
               value = 1,
               fluidRow(column(3,
                               fileInput("file", label = h3("File input")),
                               sliderInput(inputId = "tweetn", 
                                           label = "Amount of Tweets to Scrape", 
                                           min = 0, max = 3200, step = 50, value = 3200),
                               textInput("username", h3("Twitter Username"), NULL),
                               actionButton("go", "Scrape Tweets"),
                               textInput("downloadname", h2("File Name")),
                               downloadButton("downloadData", "Download"),
                               textOutput("minremain"),
                               textOutput("cooldown")),
                        column(5,
                               tableOutput("scrapedSummary"),
                               tableOutput("tweethead")))),
      
      
      tabPanel("Ngram Analysis",
               value = 2,
               plotOutput("ngramplot"), 
               plotOutput("ngramnegation")),
      
      
      tabPanel("Sentiment Analysis", 
               value = 3,
               selectInput("lexicon", label = h3("Select Lexicon"),
                           choices = list("Bing" = "bing","Afinn" = "afinn", "Loughran" = "loughran", "NRC" = "nrc"),
                           selected = "bing"),
               plotOutput("sentimentplot",  width = "100%", height = 700),
               plotOutput("ngramsentiment", height = 500),
               plotOutput("sentimenttime", height = 500),
               tableOutput("sentimenttable")),
      
      
      tabPanel("Tweet Metrics", 
               value = 4,
               plotOutput("TweetMetricPlot", height = 700), 
               sliderInput(inputId = "toptweetn", 
                           label = "Top N Tweets Common Words", 
                           min = 1, max = 250, step = 1, value = 100),
               plotOutput("toptweetcommon"),
               tableOutput("toptweets"),
               textInput("filterusername", "Username to be filtered", ""),
               plotOutput("mentionPlot"),
               textInput("wordmetric", "Word Metric Distribution", ""),
               plotOutput("metricDist", height = 800, width = "100%"),
               fluidRow(column(3, sliderInput(inputId = "highlowperformers", 
                                              label = "Compare Tweet Performance by Percentile", 
                                              min = 1, max = 100, step = 1, value = 50)),
                        column(5, selectInput("tweetmetric", 
                                             label = "Select Tweet Metrics",
                                             choices = list("Favorites" = "favorite_count", 
                                                            "Retweets" = "retweet_count"),
                           selected = "favorite_count"))),
               plotOutput("tweetHighLow")),
      
      
      tabPanel("Topic Modeling", value = 5,
               sliderInput(inputId = "topicmodels", 
                           label = "Amount of Topics", 
                           min = 2, max = 6, step  = 1, value = 4),
               dateRangeInput(inputId = "dateRange",
                              label = "Date Range",
                              start = Sys.Date()-10, 
                              end =  Sys.Date()+10,
                              min = Sys.Date()-10, 
                              max =  Sys.Date()+10,
                              format = "mm/dd/yy",
                              sep = " - "),
               selectInput("topicdivision", label = "Topic Division",
                           choices = list("Month" = "month","Biweekly" = "biweek", "Week" = "week", "Day" = "day"),
                           selected = "month"),
               plotOutput("LDA", height = 800),
               plotOutput("topicProbability", height = 800)),

      
      tabPanel("Raw Data", 
               value = 6,
               DT::dataTableOutput("rawdatatable"))
    )
  )
)
)

server <- function(input, output, session) {
 
  tweetData <- reactiveValues(data = NULL) #Set as null so tweetData can receive scraped or uploaded tweets

  
  #Tweet Data
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$downloadname, ".csv", sep = "")
    },
    content = function(file) {
      write_as_csv(tweetData$data, file)
    }
  )
  
  observeEvent(input$go, {
    tweetData$data <- get_timeline(input$username, n = input$tweetn) #When button is clicked the function executes a request for timeline from twitter api 
    tweetData$data <- tweetData$data %>% 
      mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% #Cleans the text by switching twitter links with the word 'link'
      mutate(text = str_replace_all(text, "https:", "")) %>% 
      mutate(text = str_replace_all(text, "<[^>]+>", ""),
             created_at = as_date(created_at)) 
    updateDateRangeInput(session, 
                         inputId = "dateRange", 
                         start = min(tweetData$data$created_at),
                         end = max((tweetData$data$created_at)),
                         min = min((tweetData$data$created_at)),
                         max = max((tweetData$data$created_at)))
  })

  observeEvent(input$file, {
    tweetData$data <- read.csv(input$file$datapath, encoding = "UTF-8") #Reads in csv 
    tweetData$data <- tweetData$data %>%  #Same string manipulations from twitter scraping 
      mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% 
      mutate(text = str_replace_all(text, "https:", "")) %>% 
      mutate(text = str_replace_all(text, "<[^>]+>", ""),
             created_at = as_date(created_at)) 
    updateDateRangeInput(session, 
                         inputId = "dateRange", 
                         start = min(tweetData$data$created_at),
                         end = max((tweetData$data$created_at)),
                         min = min((tweetData$data$created_at)),
                         max = max((tweetData$data$created_at)))
  })
  
  output$tweethead <- renderTable({
    req(is.null(tweetData$data) == FALSE) #Prevents function from executing until tweets are scraped or uploaded
    tweetData$data %>% #Outputs a glimpse of tweet text from uploaded or scraped tweets 
      select(`created_at`, `text`) %>% 
      mutate(`created_at` = as.character(`created_at`)) %>% 
      arrange() %>% 
      head()
  })
  
  output$minremain <- renderText({
    rate_limit() %>% #Shows how many tweet requests a user can execute
      filter(query == "statuses/user_timeline") %>% 
      select(remaining) %>%
      paste("Requests Remaining")
  })
  
  output$cooldown <- renderText({
    rate_limit() %>% #Shows time left in the api until it resets for more scraping 
      filter(query == "statuses/user_timeline") %>% 
      mutate(minutes = round(reset_at - Sys.time(),0)) %>% 
      select(minutes) %>% 
      paste("minutes until reset")
  })
  
  output$scrapedSummary <- renderTable({
    req(is.null(tweetData$data) == FALSE) #Displays a summary of scraped tweets with general metrics 
    tweetData$data %>% 
      filter(is_retweet == "FALSE") %>% 
      select(favorite_count, retweet_count) %>% 
      rename("FavoritesReceived" = favorite_count, "RetweetsReceived" = retweet_count) %>% 
      gather(key = "key", value = "value") %>% 
      group_by(key) %>% 
      summarise(min = min(value),
                median = median(value),
                mean = mean(value),
                max = max(value),
                sd = sd(value),
                TotalTweets = n())
  })
  
  #Ngram Analysis
  
  output$ngramplot <- renderPlot({
    req(is.null(tweetData$data) == FALSE) #Plot for ngram analysis 
    tweetData$data %>% 
      select(text) %>% 
      unnest_tokens("ngram", `text`, token = "ngrams", n = input$ngram) %>% #Allows user to select the ngram 
      filter(!str_detect(`ngram`, if_else(input$filterword == "", " AAA ", input$filterword))) %>% #Filters ngrams based off of a word
      filter(str_detect(ngram, if_else(input$containword == "", "", input$containword))) %>% #Selects ngrams that contain a specifc word
      filter(!str_detect(ngram, if_else(input$removestop == TRUE, paste(lexicon::sw_fry_100 ,collapse = '|'), "AAA"))) %>% #Removes ngrams that contain stop words 
      count(ngram) %>% 
      top_n(n, n = input$top) %>% 
      ggplot(aes(x = reorder(ngram, n), y = n, fill = ngram)) + 
      geom_col() + 
      coord_flip() +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 20),
            axis.text.x = element_text(size = 15)) +
      ylab("") +
      xlab("")
  })
 
  output$ngramnegation <- renderPlot({
    req(is.null(tweetData$data) == FALSE)
    tweetData$data %>% 
      select(text) %>% 
      mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% 
      unnest_tokens("ngram", `text`, token = "ngrams", n = input$ngram) %>% 
      mutate(key = row_number()) %>% 
      separate(`ngram`, into = "word") %>% 
      filter(`word` == input$negationword) %>% #Selects ngrams that begin with an input word 
      select(key) %>% 
      inner_join(tweetData$data %>% 
                   select(text) %>% 
                   mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% 
                   unnest_tokens("ngram", `text`, token = "ngrams", n = input$ngram) %>% 
                   mutate(key = row_number())) %>% #Joins words with the input word
      select(ngram) %>% 
      count(ngram) %>% 
      top_n(n, n = input$top) %>% 
      ggplot(aes(x = reorder(ngram, n), y = n)) + 
      geom_col(aes(fill  = n), size = 1) + 
      coord_flip() + 
      theme(legend.position = "none",
            axis.text.y = element_text(size = 20),
            axis.text.x = element_text(size = 15)) +
      ggtitle("Negation Ngram") +
      ylab("") +
      xlab("")
  })
  
  #Sentiment Analysis
  
  output$sentimentplot <- renderPlot({
    req(is.null(tweetData$data) == FALSE) #Plots top n sentiment based off of user selected sentiment 
    tweetData$data %>% 
      select(text) %>% 
      mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% 
      unnest_tokens("word", `text`) %>% 
      filter(!word == input$filterword) %>% 
      inner_join(get_sentiments(lexicon = input$lexicon)) %>% #Finds sentiment based on selected lexicon 
      rename(., "word" = 1, "sentiment" = 2) %>% 
      group_by(word, sentiment) %>% 
      count(sentiment) %>% 
      group_by(sentiment) %>% 
      top_n(n, n = input$top) %>% 
      ggplot(aes(x = reorder_within(word, n, sentiment), 
                 y = n, 
                 fill = sentiment)) + 
      geom_col() + 
      scale_x_reordered() + 
      theme(axis.text.y = element_text(size = 20),
            axis.text.x = element_text(size = 15)) +
      facet_wrap(~sentiment, scales = "free", ncol = 2) + 
      theme(legend.position = "none", strip.text = element_text(size=15)) + 
      coord_flip() + 
      xlab(label = "") + 
      ylab(label = "")
  })
  
  output$sentimenttime <- renderPlot({
    req(is.null(tweetData$data) == FALSE) #Genearl sentiment timeseries with the BING lexicon 
    tweetData$data %>% 
      select(text, created_at) %>% 
      mutate(created_at = as.Date(created_at)) %>% 
      unnest_tokens("word", `text`) %>% 
      inner_join(get_sentiments(lexicon = c("bing"))) %>% 
      group_by(sentiment, created_at) %>% 
      count(sentiment) %>% 
      ggplot(aes(x = created_at, y = n)) + 
      geom_smooth(se = FALSE, aes(group = sentiment,color = sentiment)) + 
      geom_smooth(data = tweetData$data %>% 
                    filter(is_retweet == "FALSE") %>% 
                    select(created_at) %>% 
                    mutate(created_at = as.Date(created_at)) %>% 
                    count(created_at), mapping = aes(x = created_at, y = n, color = "Tweets/Day"),
                  se = FALSE, inherit.aes = FALSE) + 
      scale_x_date(date_labels = "%b-%d") +
      xlab(label = "") + 
      ylab(label = "Tweets/Day") +
      scale_color_manual( values = c("#00BFC4", "#F8766D", "black")) +
      theme(axis.text.y = element_text(size = 20),
            axis.text.x = element_text(size = 15),
            legend.text=element_text(size=20),
            legend.position = c(.2,.9),
            legend.direction = "horizontal",
            legend.title = element_blank(),
            axis.title.y.left = element_text(size = 15),
            axis.title.y.right = element_text(size = 15)) +
      scale_y_continuous(sec.axis = sec_axis(trans = ~ . /2, name = "Senitment"))
  })
  
  output$ngramsentiment <- renderPlot({
    req(is.null(tweetData$data) == FALSE) #Finds top ngrams that contain sentiment words 
    tweetData$data %>% 
      select(text) %>% 
      mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% 
      unnest_tokens("ngram", `text`, token = "ngrams", n =input$ngram) %>% 
      filter(!str_detect(`ngram`, if_else(input$filterword == "", " AAA ", input$filterword))) %>% 
      mutate(key = row_number()) %>% 
      unnest_tokens("word", "ngram") %>% 
      inner_join(get_sentiments(lexicon = "bing")) %>% 
      inner_join(tweetData$data %>% #Filters sentiment based on an input word
                   select(text) %>% 
                   mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% 
                   unnest_tokens("ngram", `text`, token = "ngrams", n =input$ngram) %>% 
                   filter(!str_detect(`ngram`, if_else(input$filterword == "", " AAA ", input$filterword))) %>% 
                   mutate(key = row_number()), by = c("key" = "key")) %>% 
      select(-word) %>% 
      group_by(sentiment) %>% 
      count(ngram) %>% 
      top_n(n, n = input$top) %>% 
      mutate(n = if_else(sentiment == "negative",-n,n)) %>% 
      ggplot(aes(x = reorder(ngram, n), y = n, fill = sentiment)) + 
      geom_col() + 
      coord_flip() +
      xlab(label = "") + 
      ylab(label = "") +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 20),
            axis.text.x = element_text(size = 15))
  })
  
  output$sentimenttable <- renderTable({
    req(is.null(tweetData$data) == FALSE) #Generates table displaying top n text and sentiment
    tweetData$data %>%
      filter(is_retweet == "FALSE") %>% 
      select(text, status_id) %>% 
      unnest_tokens("word", text) %>% 
      inner_join(get_sentiments(lexicon = input$lexicon)) %>% 
      rename("status_id" = 1, "word" = 2, "sentiment" = 3) %>% 
      count(status_id, sentiment) %>% 
      group_by(sentiment) %>% 
      mutate(rank = rank(n, ties.method = "random")) %>% 
      top_n(rank, n = input$top) %>%
      select(-rank) %>% 
      inner_join(tweetData$data %>% select(text, status_id, created_at)) %>% 
      arrange(sentiment, -n) %>% 
      select(created_at, text, sentiment , n)
  })
  
  #Tweet Metrics
  
  output$TweetMetricPlot <- renderPlot({
    req(is.null(tweetData$data) == FALSE) #Creates timeseries displaying favorites, retweets, and daily tweets 
    tweetData$data %>% 
      filter(is_retweet == "FALSE") %>% 
      select(created_at, favorite_count, retweet_count) %>% 
      mutate(date = as.Date(created_at)) %>% 
      select(-created_at) %>% 
      group_by(date) %>% 
      summarise("Daily Favorites" = sum(favorite_count),
                "Daily Retweets" = sum(retweet_count),
                "Daily Tweets" = n()) %>% 
      gather(key = "type", value = "value", -date) %>% 
      ggplot(aes(x = date, y = value, color = type)) +
      geom_smooth(se = FALSE) + 
      geom_point(size = 3) + 
      facet_wrap(~type, scales = "free", ncol = 1) + 
      theme(legend.position = "none",
            axis.text.y = element_text(size = 15),
            axis.text.x = element_text(size = 15),
            strip.text = element_text(size = 15)) +
      xlab("") +
      ylab("")
  })
  
  output$toptweets <- renderTable({
    req(is.null(tweetData$data) == FALSE) #Creates table displaying top n tweets given tweet metric 
    tweetData$data %>% 
      filter(is_retweet == "FALSE") %>% 
      select(created_at, favorite_count, retweet_count, text) %>% 
      mutate(date = as.Date(created_at)) %>% 
      mutate(date = as.character(date)) %>% 
      select(-created_at) %>% 
      gather(key = "type", value = "value", -text, -date) %>% 
      group_by(type) %>% 
      filter(type == input$tweetmetric) %>% 
      top_n(value, n = input$top) %>% 
      arrange(-value)
  })
  
  output$toptweetcommon <- renderPlot({
    req(is.null(tweetData$data) == FALSE) #finds most frequent words given the top n tweets of a given metric 
    tweetData$data %>% 
      filter(is_retweet == "FALSE") %>% 
      select(favorite_count, retweet_count, text) %>% 
      gather(key = "type", value = "value", -text) %>% 
      group_by(type) %>% 
      top_n(value, n = input$toptweetn) %>% 
      select(text, type) %>% 
      mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% 
      unnest_tokens("ngram", `text`, token = "ngrams", n = input$ngram) %>% 
      count(ngram) %>% 
      filter(!str_detect(`ngram`, if_else(input$filterword == "", " AAA ", input$filterword))) %>% 
      filter(!str_detect(ngram, if_else(input$removestop == TRUE, paste(lexicon::sw_fry_100 ,collapse = '|'), "AAA"))) %>% 
      group_by(type) %>% 
      top_n(n, n = input$top) %>% 
      mutate(ranking = row_number()) %>% 
      filter(ranking <= input$top) %>% 
      ggplot(aes(x = reorder_within(ngram, n, type), y = n, fill = ngram)) + 
      geom_col() + 
      scale_x_reordered() +
      facet_wrap(~type, scales = "free") +
      coord_flip() + 
      theme(legend.position = "none",
            axis.text.y = element_text(size = 25),
            axis.text.x = element_text(size = 15),
            strip.text = element_text(size=15)) +
      xlab(label = "") + 
      ylab(label = "")
  })
  
  output$mentionPlot <- renderPlot({
    req(is.null(tweetData$data) == FALSE) #Counts mentions in tweets 
    tweetData$data %>% 
      filter(is_retweet == FALSE) %>% 
      select(text) %>% 
      mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% 
      unnest_tokens("word", `text`, token = "regex", pattern = "\\s+") %>% 
      filter(str_detect(word, "@")) %>% 
      mutate(word = str_replace_all(word, "[^a-z]", " ")) %>% 
      mutate(word = trimws(word)) %>% 
      count(word) %>% 
      arrange(-n) %>% 
      filter(word != input$filterusername) %>% 
      top_n(n, n =input$top) %>% 
      ggplot(aes(x = reorder(word, -n), y =n , color = word)) + 
      geom_point(size = 10) + 
      geom_segment(aes(x = reorder(word, -n), xend = word, yend = 0, y =n)) +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 15),
            axis.text.x = element_text(size = 20)) +
      xlab("") + 
      ylab("")
  })
  
  output$metricDist <- renderPlot({
    req(is.null(tweetData$data) == FALSE) #Craetes groupings of words that contain and don't contain
    tweetData$data %>%  #Plots metric distributions of the groupings 
      filter(is_retweet == "FALSE") %>% 
      select(text, favorite_count, retweet_count) %>% 
      mutate(text = str_to_lower(text)) %>% 
      mutate("Word" = if_else(str_detect(text, str_to_lower(input$wordmetric)) == TRUE, 
                              paste(c("contains",input$wordmetric), sep = "", collapse = " "),
                              paste(c("doesn't contain",input$wordmetric), collapse = " "))) %>% 
      select(-text) %>% 
      gather(key = "metric", value = "value", -Word) %>% 
      mutate(metric = if_else(metric == "favorite_count", "Favorites", "Retweets")) %>% 
      group_by(metric, Word) %>% 
      mutate(median = median(value)) %>% 
      ggplot(aes(x = value, fill = Word, group = Word)) + 
      geom_histogram(color = "white") + 
      geom_vline(aes(xintercept = median), linetype = "dashed", size = 1, color = "black") +
      facet_wrap(~metric + Word, scales = "free") +
      scale_x_log10() +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 15),
            axis.text.x = element_text(size = 15),
            strip.text = element_text(size=15)) +
      ylab("") +
      xlab("")
  })
  
  output$tweetHighLow <- renderPlot({
    req(is.null(tweetData$data) == FALSE) #Creates two groupings of a percentile and remaining percentile
    tweetData$data %>% #Weights ngrams by tf-idf and finds top n words for the 2 groupings
      filter(is_retweet == "FALSE") %>% 
      select(favorite_count, retweet_count, text) %>%
      gather(key = "type", value = "value", -text) %>% 
      group_by(type) %>% 
      mutate(percentile = 100*pnorm((log(value+1) - mean(log(value+1)))/sd(log(value+1)))) %>% 
      mutate(comparison = if_else(percentile > input$highlowperformers, 
                                  "High Performing Tweets", 
                                  "Low Performing Tweets")) %>% 
      select(text, comparison, type) %>% 
      filter(type == input$tweetmetric) %>% 
      unnest_tokens("ngram", text, token = "ngrams", n = input$ngram) %>% 
      count(comparison, ngram) %>% 
      bind_tf_idf(ngram, comparison, n) %>% 
      group_by(comparison) %>% 
      top_n(tf_idf, n = input$top) %>% 
      mutate(tf_idf = if_else(comparison == "Low Performing Tweets", -tf_idf,tf_idf)) %>% 
      ggplot(aes(x = reorder(ngram, tf_idf), y = tf_idf, fill = comparison)) +
      geom_col(color = "white") + 
      coord_flip() +
      scale_fill_manual( values = c("#00BFC4", "#F8766D")) + 
      theme(legend.position = "none",
            axis.text.y = element_text(size = 15),
            axis.text.x = element_text(size = 15),
            strip.text = element_text(size=15)) +
      ylab("") +
      xlab("")
  })
  
  #Topic Models 
  
  output$LDA <- renderPlot({
    
    req(is.null(tweetData$data) == FALSE) #Creates n topics by a given time grouping (monthly, biweekly, weekly, daily)
    
    #Format/wrangle the data for topic modeling   
    topicmodelData <- tweetData$data %>% 
      filter(created_at >= input$dateRange[1] & created_at <= input$dateRange[2]) %>% 
      select(created_at, text) %>% 
      mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% 
      mutate(text = str_replace_all(text, "https:", "")) %>% 
      mutate(text = str_replace_all(text, "<[^>]+>", "")) %>%
      mutate(date = as_date(created_at)) %>%
      mutate(month = paste(month(date), year(date), sep = "-"),
             day = paste(day(date), month(date), year(date), sep = "-"),
             week = week(date)) %>% 
      mutate(biweek = round(week/2, 0)) %>% 
      select(-created_at) %>% 
      gather(key = "key", value = "value", -text) %>%
      filter(key == paste(input$topicdivision)) %>% 
      unnest_tokens("ngram", "text", token = "ngrams", n = input$ngram) %>% 
      filter(!str_detect(`ngram`, if_else(input$filterword == "", " AAA ", input$filterword))) %>% 
      filter(!str_detect(ngram, if_else(input$removestop == TRUE, paste(lexicon::sw_fry_100 ,collapse = '|'), "AAA"))) %>% 
      select(-key) %>% 
      group_by(value) %>% 
      count(ngram) %>% 
      cast_dtm(value, ngram, n) 
    
    #Create the topic model and chart the output
    topicmodelData %>% 
      LDA(k = input$topicmodels) %>% 
      tidy() %>% 
      group_by(topic) %>% 
      top_n(beta, n = input$top) %>% 
      ggplot(aes(x = reorder_within(term, beta, topic), y = beta, fill = topic %>% as.factor())) + 
      geom_col() + 
      coord_flip() + 
      scale_x_reordered() + 
      facet_wrap(~topic, scales = "free") +
      theme(legend.position = "none",
            axis.text.y = element_text(size = 25),
            axis.text.x = element_text(size = 15),
            strip.text = element_text(size=15)) +
      xlab(label = "") + 
      ylab(label = "")
  })
  
  #output the topic model's probabilities 
  output$topicProbability <- renderPlot({
    req(is.null(tweetData$data) == FALSE) #Uses topics to display topic probabilities for the given time groupings 
    tweetData$data %>% 
      filter(created_at >= input$dateRange[1] & created_at <= input$dateRange[2]) %>% 
      select(created_at, text) %>% 
      mutate(text = str_replace_all(text, "(//t.co/)(.*)", " Link")) %>% 
      mutate(text = str_replace_all(text, "https:", "")) %>% 
      mutate(text = str_replace_all(text, "<[^>]+>", "")) %>%
      mutate(date = as_date(created_at)) %>%
      mutate(month = paste(month(date), year(date), sep = "-"),
             day = paste(day(date), month(date), year(date), sep = "-"),
             week = week(date)) %>% 
      mutate(biweek = round(week/2, 0)) %>% 
      select(-created_at) %>% 
      gather(key = "key", value = "value", -text) %>%
      filter(key == paste(input$topicdivision)) %>% 
      unnest_tokens("ngram", "text", token = "ngrams", n = input$ngram) %>% 
      filter(!str_detect(`ngram`, if_else(input$filterword == "", " AAA ", input$filterword))) %>% 
      filter(!str_detect(ngram, if_else(input$removestop == TRUE, paste(lexicon::sw_fry_100 ,collapse = '|'), "AAA"))) %>% 
      select(-key) %>% 
      group_by(value) %>% 
      count(ngram) %>% 
      cast_dtm(value, ngram, n) %>% 
      LDA(k = input$topicmodels) %>%
      tidy(matrix = "gamma") %>%
      mutate(topic = as.factor(topic)) %>%
      group_by(document, topic) %>%
      ggplot(aes(x = reorder(document, as.numeric(document)), y = gamma, color = topic)) +
      geom_point(size = 4) +
      geom_segment(aes(
        xend = document,
        yend = 0,
        y = gamma,
        x = document
      )) +
      scale_y_continuous(labels = scales::percent) + 
      coord_flip() + 
      facet_wrap( ~ topic, scales = "free") +
      theme(
        legend.position = "none",
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        strip.text = element_text(size = 15)) +
      xlab(label = "") +
      ylab(label = "")
  })
  #Raw Data
  
  output$rawdatatable <- DT::renderDataTable({
    tweetData$data %>%  #Creates data table and displays general tweet data
      select(created_at,
             screen_name, text,
             is_quote, is_retweet, 
             favorite_count, retweet_count, 
             followers_count, status_id) %>%
      mutate(`created_at` = as.character(`created_at`)) %>% 
      arrange(`created_at`)
  })
}

shinyApp(ui = ui, server = server)


#Testing Environemnt 
