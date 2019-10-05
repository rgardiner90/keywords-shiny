library(shiny)
library(pdftools)
library(tidyverse)
library(tidytext)
library(tidylo)


custom_stops <- c("https", "www", "google")




# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Automating Keyword Exploration of Journal Articles"),
   
   
   fileInput("file1", "Choose PDF File",
             multiple = FALSE,
             accept = c(".pdf")),
   
   
   sidebarLayout(
      sidebarPanel(
         radioButtons("ngrams",
                     "Number of words:",
                     choices = c(1, 2, 3),
                     selected = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(type = "tabs",
                    tabPanel("Most Common Words", plotOutput("contents")),
                    tabPanel("Unique Words", plotOutput("unique"))
         )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$contents <- renderPlot({
    
    req(input$file1)
    
    
    tryCatch(
      {
        df_raw <- pdftools::pdf_text(input$file1$datapath) %>%
          read_lines() %>%
          str_squish() %>%
          enframe()
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
    if(input$ngrams == 1) {
      
      df_unigram <- df_raw %>%
        unnest_tokens("word", value) %>%
        filter(!str_detect(word, "[:digit:]")) %>%
        anti_join(stop_words, by = "word") %>%
        filter(!word %in% custom_stops) %>%
        mutate(source = "Article")
      
      
      df_unigram %>%
        count(word, sort = TRUE) %>%
        head(15) %>%
        mutate(word = fct_reorder(word, n)) %>%
        ggplot(aes(x = word, y = n, fill = n)) +
        geom_col(show.legend = FALSE) +
        labs(x = "", y = "Number of Occurences",
             title = "Most Common Words") +
        coord_flip() +
        scale_fill_gradient(low = "lightgreen", high = "green") +
        theme_minimal() 

      
    } else if(input$ngrams == 2) {
      
      df_bigram <- df_raw %>%
        unnest_tokens("bigram", value, token = "ngrams", n = 2) %>%
        separate(bigram, c("word1", "word2"), sep = " ") %>%
        anti_join(stop_words, by = c("word1" = "word")) %>%
        anti_join(stop_words, by = c("word2" = "word")) %>%
        filter(!str_detect(word1, "[:digit:]"),
               !str_detect(word2, "[:digit:]"),
               !word1 %in% custom_stops,
               !word2 %in% custom_stops) %>%
        unite(bigram, c("word1", "word2"), sep = " ") %>%
        select(1:2)
      
      df_bigram %>%
        count(bigram, sort = TRUE) %>%
        head(15) %>%
        mutate(bigram = fct_reorder(bigram, n)) %>%
        ggplot(aes(x = bigram, y = n, fill = n)) +
        geom_col(show.legend = FALSE) +
        labs(x = "", y = "Number of Occurences",
             title = "Most Common Words") +
        coord_flip() +
        scale_fill_gradient(low = "lightgreen", high = "green") +
        theme_minimal() 
      
    } else {
      df_trigram <- df_raw %>%
        unnest_tokens("bigram", value, token = "ngrams", n = 3) %>%
        separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%
        anti_join(stop_words, by = c("word1" = "word")) %>%
        anti_join(stop_words, by = c("word2" = "word")) %>%
        anti_join(stop_words, by = c("word3" = "word")) %>%
        filter(!str_detect(word1, "[:digit:]"),
               !str_detect(word2, "[:digit:]"),
               !str_detect(word3, "[:digit:]"),
               !word1 %in% custom_stops,
               !word2 %in% custom_stops,
               !word3 %in% custom_stops) %>%
        unite(trigram, c("word1", "word2", "word3"), sep = " ") %>%
        select(1:2)
      
      df_trigram %>%
        count(trigram, sort = TRUE) %>%
        head(15) %>%
        mutate(trigram = fct_reorder(trigram, n)) %>%
        ggplot(aes(x = trigram, y = n, fill = n)) +
        geom_col(show.legend = FALSE) +
        labs(x = "", y = "Number of Occurences",
             title = "Most Common Words") +
        coord_flip() +
        scale_fill_gradient(low = "lightgreen", high = "green") +
        theme_minimal() 
    }
    
  })
  
  output$unique <- renderPlot({
    
    req(input$file1)
    
    
    tryCatch(
      {
        df_raw <- pdftools::pdf_text(input$file1$datapath) %>%
          read_lines() %>%
          str_squish() %>%
          enframe()
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    training <- read_csv("https://raw.githubusercontent.com/rgardiner90/keywords-shiny/master/unique_training.csv") 
    
    df_unigram <- df_raw %>%
      unnest_tokens("word", value) %>%
      filter(!str_detect(word, "[:digit:]")) %>%
      anti_join(stop_words, by = "word") %>%
      filter(!word %in% custom_stops) %>%
      mutate(source = "Article")
    
    combined <- rbind(df_unigram, training) %>%
      group_by(source) %>%
      count(word, sort = TRUE)
    
    combined %>%
      bind_log_odds(source, word, n) %>%
      filter(source == "Article") %>%
      arrange(desc(log_odds)) %>%
      head(15) %>%
      mutate(word = fct_reorder(word, log_odds)) %>%
      ggplot(aes(x = word, y = log_odds, fill = log_odds)) +
      geom_col(show.legend = FALSE) +
      labs(x = "", y = "Weighted Log-Odds",
           title = "Most Unique Words") +
      coord_flip() +
      scale_fill_gradient(low = "lightgreen", high = "green") +
      theme_minimal() 
    
  })
  
}
    

# Run the application 
shinyApp(ui = ui, server = server)

