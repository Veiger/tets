load("veiger.rdata")


library(tidytext)
library(tidyverse)
library(stringr)
library(dplyr)
library(scales)
library(reshape2)
library(wordcloud)
library(streamR)
library(grid)   
library(plotly)

#### frequency table for cap and com
#### cap
tidycap <- capdata %>% 
  dplyr::select(text) %>%
  unnest_tokens(word,text)
data("stop_words")
tidycap <-  tidycap %>% 
  anti_join(stop_words)




#### com
tidycom <- comdata %>% 
  dplyr::select(text) %>%
  unnest_tokens(word,text)
data("stop_words")
tidycom <-  tidycom %>% 
  anti_join(stop_words)



## frequency
frequency <- bind_rows(mutate(tidycap, ideology = "capitalism"),
                       mutate(tidycom, ideology = "communism")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(ideology, word) %>%
  group_by(ideology) %>%
  mutate(proportion = n / sum(n)) %>% 
  dplyr::select(-n) %>%
  spread(ideology, proportion) %>%
  gather(ideology, proportion, `communism`)


#### sentiment distribution
tidycap2 <- capdata %>% 
  dplyr::select(text) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text)

tidycom2 <- comdata %>% 
  dplyr::select(text) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text)


tidycap2 <- tidycap2%>%
  mutate(ideology = "capitalsim")
tidycom2 <- tidycom2%>%
  mutate(ideology = "communism")

com2cap2 <- rbind(tidycap2, tidycom2)

nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

joycap <- com2cap2 %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

com2cap2 %>%
  filter(ideology == "capitalism") %>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)

com2cap2 <- com2cap2 %>%
  inner_join(get_sentiments("bing")) %>%
  count(ideology, index = linenumber %/% 30, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)


#### senti frequency

capwords <- tidycap %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

comwords <- tidycom %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#####StreamR data
tweets.com <- parseTweets("tweets_com.json", verbose = F)
tweets.cap <- parseTweets("tweets_cap.json", verbose = F)
map.data <- map_data("state")   
cappoints <- data.frame(x = as.numeric(tweets.cap$lon),  
                        y = as.numeric(tweets.cap$lat))

cappoints <- cappoints[cappoints$y > 25, ]   

compoints <- data.frame(x = as.numeric(tweets.com$lon),  
                        y = as.numeric(tweets.com$lat))

compoints <- compoints[compoints$y > 25, ] 

cap_gplot <- ggplot(map.data) + 
  geom_map(aes(map_id = region),  
           map = map.data,  
           fill = "white",             
           color = "grey20", size = 0.25) + 
  expand_limits(x = map.data$long, y = map.data$lat) +            
  theme(axis.line = element_blank(),  
        axis.text = element_blank(),  
        axis.ticks = element_blank(),                     
        axis.title = element_blank(),  
        panel.background = element_blank(),  
        panel.border = element_blank(),                     
        panel.grid.major = element_blank(), 
        plot.background = element_blank(),                     
        plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
  geom_point(data = cappoints,             
             aes(x = x, y = y), size = 3,  
             alpha = 1/2, color = "skyblue")  


com_gplot <- ggplot(map.data) + 
  geom_map(aes(map_id = region),  
           map = map.data,  
           fill = "white",             
           color = "grey20", size = 0.25) + 
  expand_limits(x = map.data$long, y = map.data$lat) +            
  theme(axis.line = element_blank(),  
        axis.text = element_blank(),  
        axis.ticks = element_blank(),                     
        axis.title = element_blank(),  
        panel.background = element_blank(),  
        panel.border = element_blank(),                     
        panel.grid.major = element_blank(), 
        plot.background = element_blank(),                     
        plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
  geom_point(data = compoints,             
             aes(x = x, y = y), size = 3,  
             alpha = 1/2, color = "pink")


library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui = fluidPage(
  titlePanel("MA615 Final"),
  themeSelector(),
  navbarPage(
    "Content",
    tabPanel("Introduction",
             sidebarLayout(
               sidebarPanel(
                 h3("Weige Guo
                    U92586253")
               ),
               mainPanel(
                 p("Disputes between Capitalsim and Communism, two of the most adopted economic systems, have long existed. Given limited time these systems have been adopted as well as different measurements, it is hard to say which is a better system that could benefit a country to the most. However, exploring social data and finding out people's perception about the two systems in a society that allows freedom of speech can shed somne lights on the topic for future studies.

                    This project explored the online perpection, twitter exclusively, of these two systems and generated some interesting insights. Tne project contains four parts: Data Extraction, Text Analysis, Mapping, Emoji Analysis and Shiny Application. ")
               )
             )),
    tabPanel("EDA on twitter texts", 
               tabsetPanel(
                 tabPanel(
                   "Word Frequency",
                   fluidRow(
                     column(
                       width = 6,
                       plotOutput("histcap1"),
                       p("text analysis(capitalism) - split the text into seperate words, eliminate stop words, count the occurance of words and plot the ones that occur more than 120 times in all 5000 tweets")
                     ),
                     column(
                       width = 6,
                       plotOutput("histcom1"),
                       p("text analysis(communism) - split the text into seperate words, eliminate stop words, count the occurance of words and plot the ones that occur more than 120 times in all 5000 tweets")
                     )
                   )
                 ),
                 tabPanel("Word Proportion",
                          fluidRow(
                            column(
                              width = 4,
                              p("Compare the words that are frequently used in two sets of data(capitalsim and communism). The more close the word is to the top right, the more frequently it is used in both datasets. ")
                            ),
                            column(
                              width = 8,
                              plotOutput("scatterWordP")
                            )
                          )),
                 tabPanel("A glimpse in Sentiment",
                          fluidRow(
                            column(
                              width = 8,
                              plotOutput("sentimentCompare")
                            ),
                            column(
                              width = 4,
                              p("split the text into lines, set line number as row number, count sentiment scores for each row, and compare the result of two data sets with plots.")
                            )
                          )
                          ),
                 tabPanel("Contribution to sentiment",
                          fluidRow(
                            column(
                              width = 6,
                              plotOutput("sentimentFre_cap"),
                              p("Select sentimental words in 5000 tweets mentioning 'capitalism',plot the top 10 most used positive words and negative words respectively. ")
                            ),
                            column(
                              width = 6,
                              plotOutput("sentimentFre_com"),
                              p("Select sentimental words in 5000 tweets mentioning 'communism',plot the top 10 most used positive words and negative words respectively. ")
                            )
                          )
                 )
                 
               )
             ),
    tabPanel("Word Cloud of twitter", 
             tabsetPanel(
               tabPanel("sentiment cloud",
                        fluidRow(
                          sidebarPanel(sliderInput("max",
                                                   "Maximum Number of Words:",
                                                   min = 1,  max = 150,  value = 100))
                          ),
                          fluidRow(
                            column(
                              h3("CAP"),
                              width = 6,
                              plotOutput("capsentiWC"),
                              p("wordcloud of mosted used sentimental words in 5000 tweets mentioning 'capitalsim', distinguish the negative ones from the positive ones by using different colors. ")
                            ),
                            column(
                              h3("COM"),
                              width = 6,
                              plotOutput("comsentiWC"),
                              p("wordcloud of mosted used sentimental words in 5000 tweets mentioning 'communism', distinguish the negative ones from the positive ones by using different colors.")
                            )
                            )
                        )
             )
    ),
    tabPanel("Emoji", 
             splitLayout(
               img(src="cap_emoj.png",width=495,height=300),
               img(src="com_emoj.png",width=495,height=300)
             )
    ),
    tabPanel("Mapping",
        sidebarLayout(
          sidebarPanel(
            selectInput("input1",label = "Choose Economic System", choices = c("Capitalism", "Communism"))
          ),
          mainPanel(
            plotOutput("map")
          )
        )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$histcap1 <- renderPlot({
    tidycap %>%
      count(word, sort = TRUE) %>%
      filter(n>120) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col(fill = "skyblue") +
      xlab(NULL) +
      coord_flip()
  })
  
  output$histcom1 <- renderPlot({
    tidycom %>%
      count(word, sort = TRUE) %>%
      filter(n>120) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n)) +
      geom_col(fill = "pink") +
      xlab(NULL) +
      coord_flip()
  })
  
  output$scatterWordP <- renderPlot({
    ggplot(frequency, aes(x = proportion, y = `capitalism`, color = abs(`capitalism` - proportion))) +
      geom_abline(color = "purple", lty = 2) +
      geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
      geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
      scale_x_log10(labels = percent_format()) +
      scale_y_log10(labels = percent_format()) +
      scale_color_gradient(limits = c(0, 0.001), low = "purple", high = "darkslategray4") +
      theme(legend.position="none") 
  })
  
  output$sentimentCompare <- renderPlot({
    ggplot(com2cap2, aes(index, sentiment, fill = ideology)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ideology, ncol = 2, scales = "free_x")
  })
  
  output$sentimentFre_cap <- renderPlot({
    capwords %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  })
  
  output$sentimentFre_com <- renderPlot({
    comwords %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  })
  
  
  output$capsentiWC <- renderPlot({
    tidycap %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("darkgrey","red"),
                     max.words = input$max)
  })
  
  output$comsentiWC <- renderPlot({
    tidycom %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("darkgrey","red"),
                       max.words = input$max)
  })
  
  output$map <- renderPlot({
    if (input$input1 == "Capitalism") {
      cap_gplot
    } else {
      com_gplot
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

