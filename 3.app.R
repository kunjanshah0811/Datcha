# ===== COMPR - SOCIAL MEDIA COMPARISON TOOL ===== #
# File: app.R
# Structure:
# 1. LIBRARIES
# 2. TEXT PROCESSING MODULE
# 3. UI DEFINITION
# 4. SERVER LOGIC
#    - Data Loading
#    - Core Analysis Functions
#    - Output Rendering
#    - Event Handlers
# 5. APP LAUNCHER

# ====================== #
# 1. LIBRARY IMPORTS
# ====================== #
library(shiny)
library(shinyjs)        # For enhanced UI functionality
library(dplyr)          # Data manipulation
library(tm)             # Text mining
library(topicmodels)    # Topic modeling
library(sentimentr)     # Sentiment analysis
library(wordcloud2)     # Word cloud visualization
library(highcharter)    # Interactive charts
library(tidytext)       # Text processing
library(reshape2)       # Data reshaping
library(ggplot2)        # Visualization
library(DT)             # Interactive tables
library(stringdist)     # String distance calculation
library(shinyBS)        # For tooltips
library(quanteda)       # Quantitative text analysis
library(quanteda.textstats) # Text statistics
library(KeynessMeasures)    # Keyness analysis
library(SnowballC)      # For stemming
library(textstem)       # For lemmatization
library(LDAvis)         # LDA visualization
library(diffobj)        #For visual  text diffs
library(htmltools)

# ====================== #
# 2. TEXT PROCESSING MODULE
# ====================== #
text_processor = list(
  clean = function(text, use_stem = FALSE, use_lemma = FALSE) {
    corpus = Corpus(VectorSource(text)) %>%
      tm_map(content_transformer(tolower)) %>%
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeWords, stopwords(source = "smart")) %>%
      tm_map(stripWhitespace)
    
    # Apply stemming if requested
    if(use_stem) {
      corpus <- tm_map(corpus, stemDocument)
    }
    
    # Apply lemmatization if requested
    if(use_lemma) {
      text_vec <- sapply(corpus, as.character)
      text_vec <- lemmatize_strings(text_vec)
      return(text_vec)
    }
    
    sapply(corpus, as.character)
  },
  
  get_freq = function(text, gram_type = "Uni-gram") {
    if (gram_type == "Uni-gram") {
      corpus <- Corpus(VectorSource(text))
      dtm <- DocumentTermMatrix(corpus)
      freq <- colSums(as.matrix(dtm))
      data.frame(word = names(freq), freq = freq) %>% 
        arrange(desc(freq))
    } else {
      tibble(text = text) %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        count(bigram, sort = TRUE) %>%
        rename(word = bigram, freq = n)
    }
  }
)

# ====================== #
# 3. UI DEFINITION
# ====================== #
ui <- navbarPage("Datcha",
                 
  # 1. fiRST PAGE
  tabPanel("Dashboard",
  fluidPage(
  useShinyjs(), 
  tags$head(
    tags$style(HTML("          #placeholder for custom CSS
      .form-group, .shiny-input-container {
        margin-bottom: 8px;
      }
      .form-group {
        padding-bottom: 0px;
      }
      .shiny-date-input {
        padding-bottom: 0px;
        margin-bottom: 0px;
      }
    ")),
    tags$style(HTML("
  .tooltip-inner {
    max-width: 300px;
    padding: 10px;
    background-color: #f8f9fa;
    color: #212529;
    border: 1px solid #dee2e6;
    border-radius: 4px;
  }
  .fa-info-circle {
    color: #007bff;
    margin-left: 5px;
    cursor: pointer;
  }
")),
    
    tags$script(HTML('
    $(document).ready(function() {
      // Initialize tooltips
      $("[data-toggle=\'tooltip\']").tooltip(); 
    });
  ')),
    tags$style(HTML("
    .datatable {
      margin: 20px 0;
    }
    .datatable td {
      max-width: 300px;
      overflow-wrap: break-word;
    }
  ")),
    tags$link(rel = "stylesheet", type = "text/css", href = "diffobj.css"),
  ),

  
  # ----- Main Layout ----- #
  
  # Main Title
  titlePanel("Datcha"),  
  
  # Subheadline and Introduction Text
  h3("Compare Your Social Media Over Time"),
  p("This app allows users to compare two social media datasets collected at different times. 
     It identifies removed posts based on their IDs and analyzes them using NLP techniques, 
     including word clouds, topic modeling, and sentiment analysis."),
  
  sidebarLayout(
    sidebarPanel(
      # Improved Dataset Selection Section
      h4("Dataset Comparison"),
      wellPanel(
        # Dataset 1
        fluidRow(
          column(6, fileInput("file1", "Dataset 1", accept = ".csv", width = "100%")),
          column(6, dateInput("date1", "Collection Date", value = Sys.Date()-30, width = "100%"))
        ),
        # Dataset 2
        fluidRow(
          column(6, fileInput("file2", "Dataset 2", accept = ".csv", width = "100%")),
          column(6, dateInput("date2", "Collection Date", value = Sys.Date(), width = "100%"))
        ),
        actionButton("compare", "Compare Datasets", 
                     class = "btn-primary", width = "100%")
      ),
      
      uiOutput("date_validation_msg"),
      
      
      
      hr(),
      h4("Summary"),
      textOutput("dataset1_count"),
      textOutput("dataset2_count"),
      textOutput("removed_count"),
      textOutput("edited_post_count"),
      
      
      hr(),
      h4("Data Quality Indicators"),
      textOutput("completeness"),
      textOutput("data_loss"),
      textOutput("removed_per_day"),  # New line for daily removal rate
      textOutput("removal_rate_daily"),  
      
      hr(),
      h4("Data Editing"),
      uiOutput("data_editing_ui"),
      uiOutput("edited_post_ratio"),
      
      # conditionalPanel(
      #   condition = "input.tabset == 'Sentiment Analysis'",
      #   hr(),
      #   h4("Sentiment Comparison: Dataset 1 vs Dataset 2"),
      #   tableOutput("dataset_comparison_table") %>% 
      #     tagAppendAttributes(style = "border: 1px solid #ddd; width: 100%; text-align: center;")
      # )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel("Word Frequency", 
                 fluidRow(
                   column(6, h4("Removed Posts"), highchartOutput("word_freq_plot_removed")),
                   column(6, h4("Remaining Posts"), highchartOutput("word_freq_plot_remaining"))
                 )
        ),
        # tabPanel("Word Cloud",
        #          fluidRow(
        #            column(6,
        #                   h4("Removed Posts"),
        #                   wordcloud2Output("word_cloud_removed"),
        #                   htmlOutput("removed_word_stats")
        #            ),
        #            column(6,
        #                   h4("Remaining Posts"),
        #                   wordcloud2Output("word_cloud_remaining"),
        #                   htmlOutput("remaining_word_stats")
        #            )
        #          )
        # ),
        
        tabPanel("Keyness Analysis",
                 # In your UI section, add:
                 fluidRow(
                   column(12,
                          uiOutput("keyness_controls"),  # This will render the tab panel
                          highchartOutput("keyness_plot"),
                          uiOutput("keyness_interpretation")
                   )
                 )
        ),
        # tabPanel("Word Cloud",
        #          fluidRow(
        #            column(12,
        #                   radioButtons("word_cloud_gram", "Gram Type:",
        #                                choices = c("Uni-gram", "Bi-gram"), 
        #                                selected = "Uni-gram", inline = TRUE)
        #            )
        #          ),
        #          fluidRow(
        #            column(6,
        #                   h4("Removed Posts"),
        #                   wordcloud2Output("word_cloud_removed"),
        #                   htmlOutput("removed_word_stats")
        #            ),
        #            column(6,
        #                   h4("Remaining Posts"),
        #                   wordcloud2Output("word_cloud_remaining"),
        #                   htmlOutput("remaining_word_stats")
        #            )
        #          )
        # ),
        tabPanel("Topic Modeling",
                 fluidRow(
                   column(12,
                          sliderInput("num_topics", "Number of Topics:", 
                                      min = 2, max = 10, value = 5),
                          radioButtons("topic_dataset", "Show Topics For:",
                                       choices = c("Removed Posts", "Remaining Posts", "Combined View"),
                                       selected = "Removed Posts",
                                       inline = TRUE)
                          
                   )
                 ),
                 uiOutput("ldavis_output")
        ),
        tabPanel("Sentiment Analysis", 
                 fluidRow(
                   column(6, h4("Removed Posts Sentiment"), 
                          highchartOutput("sentiment_plot_removed")),
                   column(6, h4("Remaining Posts Sentiment"), 
                          highchartOutput("sentiment_plot_remaining"))
                 ),
                 hr(),
                 h4("Most Extreme Posts"),
                 fluidRow(
                   column(6, 
                          h5("Most Positive (Removed)"),
                          uiOutput("most_positive_removed_box")
                   ),
                   column(6, 
                          h5("Most Negative (Removed)"),
                          uiOutput("most_negative_removed_box")
                   )
                 ),
                 fluidRow(
                   column(6, 
                          h5("Most Positive (Remaining)"),
                          uiOutput("most_positive_remaining_box")
                   ),
                   column(6, 
                          h5("Most Negative (Remaining)"),
                          uiOutput("most_negative_remaining_box")
                   )
                 )
        ),
        tabPanel("Text Changes",
                 fluidRow(
                   column(12,
                          # h4("Text Edit Distances Between Datasets"),
                          # htmlOutput("edit_distance_summary_ui"),
                          # hr(),
                          h4("Most Edited Posts"),
                          dataTableOutput("most_edited_posts"),
                          # hr(),
                          # h4("Export Edited Posts"),
                          # fluidRow(
                          #   column(6, downloadButton("download_csv", "Download as CSV")),
                          #   column(6, downloadButton("download_pdf", "Download as PDF"))
                          # )
                   )
                 )
        ))
      )
    ),
        
        
        
      )  # Closing tabsetPanel
    ) , # Closing mainPanel,
  tabPanel("Information",  # <- new page
           fluidPage(
             h2("About Datcha"),
             p("Datcha compares social media datasets over time."),
             hr(),
             h4("How to Use"),
             tags$ul(
               tags$li("Upload two .csv files with ID and text columns."),
               tags$li("Click Compare to analyze removed and edited posts."),
               tags$li("Explore the tabs for sentiment, topics, word usage, and changes.")
             ),
             hr(),
             h4("Credits"),
             p("Built by Yannik using R and Shiny."),
             p("Packages used: quanteda, sentimentr, topicmodels, diffobj, and more."),
             hr(),
             h4("Contact"),
             p("For support, contact your team lead.")
           )
  )  # Closing sidebarLayout
)  # Closing fluidPage

# ====================== #
# 4. SERVER LOGIC
# ====================== #
server <- function(input, output) {
  
  # Create a reactive value to track if comparison was done
  comparison_done <- reactiveVal(FALSE)
  
  # Observe when compare button is pressed
  observeEvent(input$compare, {
    comparison_done(TRUE)
  })
  
  
  # ===== 4.1 Data Loading and Preparation
  
  # Load the first dataset
  data1 <- reactive({
    req(input$file1)  # Ensure the file is uploaded
    read.csv(input$file1$datapath, stringsAsFactors = FALSE)
  })
  
  # Load the second dataset
  data2 <- reactive({
    req(input$file2)  # Ensure the file is uploaded
    read.csv(input$file2$datapath, stringsAsFactors = FALSE)
  })
  
  # Function to detect the correct ID column
  detect_id_column <- function(df) {
    possible_ids <- c("id", "tweet_id", "comment_id")
    found_id <- intersect(possible_ids, names(df))
    
    if (length(found_id) > 0) {
      return(found_id[1])  # Use the first matching column
    } else {
      return(NULL)
    }
  }
  
  # ===== 4.2 Core Analysis Functions
  
  # Date validation message
  output$date_validation_msg <- renderUI({
    if (is.null(input$date1) || is.null(input$date2)) {
      return(div(style = "color: red;", "Please enter dates for both datasets"))
    }
    if (input$date2 <= input$date1) {
      return(div(style = "color: red;", "Error: Dataset 2 date must be after Dataset 1"))
    }
    return(NULL)
  })
  
  
  # In your server section (add these new outputs):
  output$removed_per_day <- renderText({
    req(comparison_done(), removed_posts(), input$date1, input$date2)
    days <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    if(days <= 0) return("Daily removal rate: N/A")
    paste("Daily Removed Posts:", round(nrow(removed_posts())/days, 1), "posts/day")
  })
  
  output$daily_removal_count <- renderText({
    req(removed_posts(), input$date1, input$date2)
    days <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    if(days <= 0) return("Total days: N/A")
    paste("Time period:", days, "days")
  })
  
  # Daily removal rate calculation - with percentage and total days
  output$removal_rate_daily <- renderText({
    req(comparison_done(), removed_posts(), input$date1, input$date2, data1())
    days_diff <- as.numeric(difftime(input$date2, input$date1, units = "days"))
    if(days_diff <= 0) return("Invalid date range")
    total_posts <- nrow(data1())
    daily_removal_percent <- round((nrow(removed_posts()) / days_diff / total_posts * 100), 2)
    paste("Daily Removal Rate:", daily_removal_percent, "% of total posts/day (over", days_diff, "days)")
  })
  
  
  output$mean_edit_distance <- renderText({
    req(edit_distances())
    paste("Mean Edit Distance:", round(mean(edit_distances()$edit_distance), 2))
  })
  
  output$mean_normalized_distance <- renderText({
    req(edit_distances())
    paste("Mean Normalized Distance:", round(mean(edit_distances()$normalized_distance), 3))
  })
  
  
  # Enable/disable compare button based on date validity
  observe({
    # Only enable compare button when dates are valid
    if (!is.null(input$date1) && !is.null(input$date2) && input$date2 > input$date1) {
      shinyjs::enable("compare")
    } else {
      shinyjs::disable("compare")
    }
  })
  
  # Calculate the number of posts in Dataset 1
  output$dataset1_count <- renderText({
    req(comparison_done(), data1())
    paste("Number of posts in Dataset 1:", nrow(data1()))
  })
  
  # Calculate the number of posts in Dataset 2
  output$dataset2_count <- renderText({
    req(comparison_done(), data2())
    paste("Number of posts in Dataset 2:", nrow(data2()))
  })
  

  
  # ===== 4.4
  
  # Identify removed posts (present in the first dataset but missing in the second)
  removed_posts <- eventReactive(input$compare,{
    req(data1(), data2())
    df1 <- data1()
    #write.csv(df1,"C:/Users/KUNJAN SHAH/Desktop/df1.csv", row.names = FALSE )
    df2 <- data2()
    #write.csv(df2)
    
    id_col_1 <- detect_id_column(df1)
    id_col_2 <- detect_id_column(df2)
    
    # Ensure both datasets contain at least one valid ID column
    if (is.null(id_col_1) || is.null(id_col_2)) {
      showNotification("Error: No valid ID column ('id', 'tweet_id', or 'comment_id') found in one or both datasets.", type = "error")
      return(NULL)
    }
    
    # Filter posts that are missing in the second dataset
    removed <- df1 %>% filter(!(!!sym(id_col_1) %in% df2[[id_col_2]]))
    
    return(removed)
  })
  
  # Get remaining posts
  remaining_posts <- eventReactive(input$compare, {
    req( data1(), data2())
    data1() %>% 
      filter(!!sym(detect_id_column(data1())) %in% data2()[[detect_id_column(data2())]])
  })
  
  
  # 4.5 Summmary stats
  
  # Display the number of removed posts
  output$removed_count <- renderText({
    req(comparison_done(), removed_posts())
    paste("Number of Removed Posts:", nrow(removed_posts()))
    
  })
  
  #Display the number of edited posts
  output$edited_post_count <- renderText({
    req(edit_distances())
    edited_count <- sum(edit_distances()$edit_distance > 0, na.rm = TRUE)
    paste("Number of Edited Posts:", edited_count)
  })
  
  
  
  # Calculate completeness
  output$completeness <- renderText({
    req(comparison_done(), data1(), data2())
    completeness <- (nrow(data2()) / nrow(data1())) * 100
    paste("Completeness:", round(completeness, 1), "%")
  })
  
  # Calculate data loss
  output$data_loss <- renderText({
    req(comparison_done(), removed_posts())
    data_loss <- (nrow(removed_posts()) / nrow(data1())) * 100
    paste("Data Loss:", round(data_loss, 1), "%")
  })  
  

  
  #===== Text Changes =====#
  # ensure www/ exists
  if (!dir.exists("www")) dir.create("www")
  # overwrite diffobj.css in www/
  file.copy(diffobj_css(), "www/diffobj.css", overwrite = TRUE)
  
  
  #-------Levenshtein distance
  # Function to calculate Levenshtein distance between matching posts
  calculate_edit_distance <- function(df1, df2) {
    id_col_1 <- detect_id_column(df1)
    id_col_2 <- detect_id_column(df2)
    
    if (is.null(id_col_1) || is.null(id_col_2)) return(NULL)
    
    # Get matching posts
    matching <- df1 %>%
      inner_join(df2, by = setNames(id_col_2, id_col_1),
                 suffix = c("_1", "_2"))
    
    if (nrow(matching) == 0) return(NULL)
    
    # Calculate edit distance for each pair
    matching %>%
      mutate(
        edit_distance = stringdist::stringdist(text_1, text_2, method = "lv"),
        normalized_distance = edit_distance / pmax(nchar(text_1), nchar(text_2))
      ) %>%
      select(!!sym(id_col_1), text_1, text_2, edit_distance, normalized_distance)
  }
  
  # Calculate edit distances between matching posts
  edit_distances <- reactive({
    req(data1(), data2())
    calculate_edit_distance(data1(), data2())
  })
  
  #Reactive Data Editing Section
  output$data_editing_ui <- renderUI({
    req(comparison_done())
    tagList(
      htmlOutput("edit_distance_summary_ui")
    )
  })
  
  output$most_edited_posts <- renderDT({
    req(edit_distances())
    
    df <- edit_distances()[order(-edit_distances()$edit_distance), ][1:20, ]
    
    # Apply HTML diff with inline styles for both columns
    apply_diff <- function(text_1, text_2) {
      html <- as.character(
        diffChr(text_1, text_2, format = "html", mode= 'sidebyside', color.mode = 'rgb', style = list(html.output = "diff.w.style"))
      )
      # Strip diff hunk headers like: <div class='diffobj-header'>@@ 1 @@</div>
      html <- gsub("<div class='diffobj-line'><div class='diffobj-header'>@@.*?@@</div></div>", "", html)
      paste0("<div style='white-space:pre-wrap;'>", html, "</div>")
    }
    
    # Clean assignment without names
    df$text_1 <- unname(mapply(apply_diff, df$text_1, df$text_2, SIMPLIFY = FALSE))
    print(df$text_1)
    df$text_2 <- unname(mapply(apply_diff, df$text_2, df$text_1, SIMPLIFY = FALSE))
    
    
    datatable(
      data.frame(
        ID = df[[detect_id_column(df)]],
        Differences = I(df$text_1),
        #text_2 = I(df$text_2),
        Edit_distance = df$edit_distance,
        Normalized_distance = df$normalized_distance
      ),
      escape = FALSE,
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        scrollX = FALSE,  # Let it wrap instead of force scroll
        columnDefs = list(
          list(width = '8%', targets = 0),
          list(width = '8%', targets = 1),   # ID column
          list(width = '72%', targets = 2),  # text_1
          list(width = '10%', targets = 3),  # edit_distance
          list(width = '10%', targets = 4)   # normalized_distance
        )
      ),
      class = 'cell-border stripe hover compact nowrap'
    )
  })
  
  

  
  #Calculate Levenshtein distance 
  output$edit_distance_summary_ui <- renderUI({
    req(edit_distances())
    dist_data <- edit_distances()
    
    if (is.null(dist_data)) return(HTML("No matching posts to compare"))
    
    
    # Calculate means only if we have valid data
    mean_edit <- ifelse(nrow(dist_data) > 0, mean(dist_data$edit_distance, na.rm = TRUE), 0)
    mean_norm <- ifelse(nrow(dist_data) > 0, mean(dist_data$normalized_distance, na.rm = TRUE), 0)
    
    tagList(
      tags$span("Mean Edit Distance: ", round(mean(dist_data$edit_distance), 2),
                tags$span(icon("info-circle"), id = "edit_distance_info"),
                tags$br(),
                tags$span("Mean Normalized Distance: ", round(mean(dist_data$normalized_distance), 3),
                          tags$span(icon("info-circle"), id = "normalized_info"),
                          
                          # Initialize tooltips
                          bsTooltip("edit_distance_info", 
                                    "Levenshtein distance counts the minimum number of single-character edits needed to change one text into another",
                                    placement = "right"),
                          bsTooltip("normalized_info", 
                                    "Normalized distance divides the edit distance by the length of the longer text (range 0-1)",
                                    placement = "right")
                )))
  })
  
  output$edited_post_ratio <- renderUI({
    req(edit_distances())
    total <- nrow(edit_distances())
    edited <- sum(edit_distances()$edit_distance > 0, na.rm = TRUE)
    ratio <- if (total > 0) round((edited / total) * 100, 1) else 0
    
    tagList(
      tags$span(paste("Edited Post Ratio:", ratio, "%"),
                tags$span(icon("info-circle"), id = "edited_ratio_info")),
      bsTooltip("edited_ratio_info", 
                "Percentage of matched posts that were changed between the two datasets", 
                placement = "right")
    )
  })
  
  
  
  
  # ===== Generate Word Frequency Plots ===== #
  output$word_freq_plot_removed <- renderHighchart({
    req(comparison_done(), removed_posts())
    
    text_data <- removed_posts()$text  # Assumption: The text column is named "text"
    
    if (is.null(text_data) || length(text_data) == 0) {
      showNotification("Error: No text data found in removed posts.", type = "error")
      return(NULL)
    }
    
    cleaned_text <- text_processor$clean(text_data, use_stem = FALSE, use_lemma = TRUE)
    word_freq <- text_processor$get_freq(cleaned_text) %>%
      filter(freq > 1) %>%
      slice(1:100)  # Add slicing to match original behavior
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Removed Posts") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                 formatter = JS(paste0("function() {
                                     var result='';
                                     result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.point.category+'</span>:<b> '
                                     +this.point.y + '</b>';
                                     return result;
    }"))) %>%
      hc_xAxis(categories = word_freq[1:100,]$word,
               labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
      hc_add_series(name = "Word", data = word_freq[1:100,]$freq, type = "column",
                    color = "#AA0114", showInLegend = FALSE)
  })
  
  output$word_freq_plot_remaining <- renderHighchart({
    req(comparison_done(), data1(), data2())
    
    remaining <- data1() %>% 
      filter(!!sym(detect_id_column(data1())) %in% data2()[[detect_id_column(data2())]])
    
    text_data <- remaining$text
    
    if (is.null(text_data) || length(text_data) == 0) {
      showNotification("Error: No text data found in remaining posts.", type = "error")
      return(NULL)
    }
    
    # Clean text first
    cleaned_text <- text_processor$clean(text_data,  use_stem = FALSE, use_lemma = TRUE)
    word_freq <- text_processor$get_freq(cleaned_text) %>%
      filter(freq > 1) %>%
      slice(1:100)
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_title(text = "Remaining Posts") %>%
      hc_tooltip(crosshairs = TRUE, shared = FALSE, useHTML = TRUE,
                 formatter = JS(paste0("function() {
                                     var result='';
                                     result='<br/><span style=\\'color:'+this.series.color+'\\'>'+this.point.category+'</span>:<b> '
                                     +this.point.y + '</b>';
                                     return result;
    }"))) %>%
      hc_xAxis(categories = word_freq[1:100,]$word,
               labels = list(style = list(fontSize = '11px')), max = 20, scrollbar = list(enabled = TRUE)) %>%
      hc_add_series(name = "Word", data = word_freq[1:100,]$freq, type = "column",
                    color = "#4472c4", showInLegend = FALSE)
  })
  
  
  # # ===== Generate Word Cloud ===== #
  # output$word_cloud_removed <- renderWordcloud2({
  #   req(comparison_done(), removed_posts())
  #   
  #   if (is.null(removed_posts()$text) || all(removed_posts()$text == "")) {
  #     return(div(class = "loading-placeholder", "No text data in removed posts"))
  #   }
  #   
  #   cleaned_text <- text_processor$clean(removed_posts()$text, use_stem = FALSE, use_lemma = TRUE )
  #   word_freq <- text_processor$get_freq(cleaned_text) %>%
  #     filter(freq > 1) %>%
  #     slice(1:100)
  #   
  #   if (nrow(word_freq) == 0) {
  #     return(div(class = "loading-placeholder", "No frequent words in removed posts"))
  #   }
  #   
  #   wc <- wordcloud2(
  #     word_freq,
  #     size = 0.8,
  #     minSize = 0.01,
  #     gridSize = 10,
  #     color = "#FF6B6B",
  #     shape = 'circle'
  #   )
  #   
  #   wc %>% htmlwidgets::onRender("
  #     function(el) {
  #       var canvas = el.querySelector('canvas');
  #       if (!canvas) return;
  #       
  #       canvas.addEventListener('mousemove', function(evt) {
  #         if (evt.target.tagName === 'CANVAS') {
  #           var word = evt.target.textContent;
  #           if (word) {
  #             Shiny.setInputValue('hover_word_removed', word);
  #           }
  #         }
  #       });
  #       
  #       canvas.addEventListener('mouseout', function(e) {
  #         Shiny.setInputValue('hover_word_removed', null);
  #       });
  #     }
  #   ")
  # })
  # 
  # output$word_cloud_remaining <- renderWordcloud2({
  #   req(comparison_done(), remaining_posts())
  #   
  #   if (is.null(remaining_posts()$text) || all(remaining_posts()$text == "")) {
  #     return(div(class = "loading-placeholder", "No text data in remaining posts"))
  #   }
  #  
  #   cleaned_text <- text_processor$clean(remaining_posts()$text,  use_stem = FALSE, use_lemma = TRUE)
  #   word_freq <- text_processor$get_freq(cleaned_text) %>%
  #     filter(freq > 1) %>%
  #     slice(1:100)
  #   
  #   if (nrow(word_freq) == 0) {
  #     return(div(class = "loading-placeholder", "No frequent words in remaining posts"))
  #   }
  #   
  #   wc <- wordcloud2(
  #     word_freq,
  #     size = 0.8,
  #     minSize = 0.01,
  #     gridSize = 10,
  #     color = "#4ECDC4",
  #     shape = 'circle'
  #   )
  #   
  #   wc %>% htmlwidgets::onRender("
  #     function(el) {
  #       var canvas = el.querySelector('canvas');
  #       if (!canvas) return;
  #       
  #       canvas.addEventListener('mousemove', function(evt) {
  #         if (evt.target.tagName === 'CANVAS') {
  #           var word = evt.target.textContent;
  #           if (word) {
  #             Shiny.setInputValue('hover_word_remaining', word);
  #           }
  #         }
  #       });
  #       
  #       canvas.addEventListener('mouseout', function(e) {
  #         Shiny.setInputValue('hover_word_remaining', null);
  #       });
  #     }
  #   ")
  # })
  # 
  # 
  # 
  # observeEvent(input$hover_word, {
  #   if (!is.null(input$hover_word)) {
  #     word <- input$hover_word$word
  #     source <- input$hover_word$source
  #     
  #     # Handle the hover based on the word and source
  #     if (source == "removed") {
  #       # Handle hover on removed word cloud
  #     } else if (source == "remaining") {
  #       # Handle hover on remaining word cloud
  #     }
  #   }
  # })
  
  #   output$word_cloud_removed <- renderWordcloud2({
  #     req(comparison_done(), removed_posts())
  #     
  #     if (is.null(removed_posts()$text) || all(removed_posts()$text == "")) {
  #       return(div(class = "loading-placeholder", "No text data in removed posts"))
  #     }
  #     
  #     cleaned_text <- text_processor$clean(removed_posts()$text)
  #     word_freq <- text_processor$get_freq(cleaned_text) %>%
  #       filter(freq > 1) %>%
  #       slice(1:100)
  #     
  #     if (nrow(word_freq) == 0) {
  #       return(div(class = "loading-placeholder", "No frequent words in removed posts"))
  #     }
  #     
  #     wc <- wordcloud2(
  #       word_freq,
  #       size = 0.8,
  #       minSize = 0.01,
  #       gridSize = 10,
  #       color = "#FF6B6B",
  #       shape = 'circle'
  #     )
  #     
  #     wc %>% htmlwidgets::onRender(JS("
  #     function(el) {
  #       var canvas = el.querySelector('canvas');
  #       if (!canvas) return;
  #       
  #       canvas.addEventListener('mousemove', function(evt) {
  #         var rect = canvas.getBoundingClientRect();
  #         var x = evt.clientX - rect.left;
  #         var y = evt.clientY - rect.top;
  #         
  #         if (evt.target.tagName === 'CANVAS') {
  #           var word = evt.target.textContent;
  #           Shiny.setInputValue('hover_word_removed', word);
  #         }
  #       });
  #       
  #       canvas.addEventListener('mouseout', function(e) {
  #         Shiny.setInputValue('hover_word_removed', null);
  #       });
  #     }
  #   "))
  #   })
  #   
  #   output$word_cloud_remaining <- renderWordcloud2({
  #     req(comparison_done(), remaining_posts())
  #     
  #     if (is.null(remaining_posts()$text) || all(remaining_posts()$text == "")) {
  #       return(div(class = "loading-placeholder", "No text data in remaining posts"))
  #     }
  #     
  #     cleaned_text <- text_processor$clean(remaining_posts()$text)
  #     word_freq <- text_processor$get_freq(cleaned_text) %>%
  #       filter(freq > 1) %>%
  #       slice(1:100)
  #     
  #     if (nrow(word_freq) == 0) {
  #       return(div(class = "loading-placeholder", "No frequent words in remaining posts"))
  #     }
  #     
  #     wc <- wordcloud2(
  #       word_freq,
  #       size = 0.8,
  #       minSize = 0.01,
  #       gridSize = 10,
  #       color = "#4ECDC4",
  #       shape = 'circle'
  #     )
  #     
  #     wc %>% htmlwidgets::onRender(JS("
  #     function(el) {
  #       var canvas = el.querySelector('canvas');
  #       if (!canvas) return;
  #       
  #       canvas.addEventListener('mousemove', function(evt) {
  #         var rect = canvas.getBoundingClientRect();
  #         var x = evt.clientX - rect.left;
  #         var y = evt.clientY - rect.top;
  #         
  #         if (evt.target.tagName === 'CANVAS') {
  #           var word = evt.target.textContent;
  #           Shiny.setInputValue('hover_word_remaining', word);
  #         }
  #       });
  #       
  #       canvas.addEventListener('mouseout', function(e) {
  #         Shiny.setInputValue('hover_word_remaining', null);
  #       });
  #     }
  #   "))
  #   })
  #   
  #   # Add CSS
  #   tags$style(HTML("
  #   .loading-placeholder {
  #     height: 400px;
  #     display: flex;
  #     align-items: center;
  #     justify-content: center;
  #     color: #666;
  #     font-style: italic;
  #   }
  #   .word-highlight {
  #     font-weight: 900 !important;
  #     font-size: 1.5em !important;
  #     fill: #FFA500 !important;
  #     cursor: pointer;
  #   }
  # "))
  #   
  #   # Observers for hover effects
  #   observeEvent(input$hover_word_removed, {
  #     if (is.null(input$hover_word_removed)) {
  #       runjs('
  #       document.querySelector("#word_cloud_removed").querySelectorAll(".word-highlight").forEach(el => {
  #         el.classList.remove("word-highlight");
  #       });
  #     ')
  #     } else {
  #       runjs(sprintf('
  #       document.querySelector("#word_cloud_removed").querySelectorAll(".word-highlight").forEach(el => {
  #         el.classList.remove("word-highlight");
  #       });
  #       document.querySelector("#word_cloud_removed").querySelectorAll("span").forEach(el => {
  #         if(el.textContent === "%s") {
  #           el.classList.add("word-highlight");
  #         }
  #       });
  #     ', input$hover_word_removed))
  #     }
  #   }, ignoreNULL = FALSE)
  #   
  #   observeEvent(input$hover_word_remaining, {
  #     if (is.null(input$hover_word_remaining)) {
  #       runjs('
  #       document.querySelector("#word_cloud_remaining").querySelectorAll(".word-highlight").forEach(el => {
  #         el.classList.remove("word-highlight");
  #       });
  #     ')
  #     } else {
  #       runjs(sprintf('
  #       document.querySelector("#word_cloud_remaining").querySelectorAll(".word-highlight").forEach(el => {
  #         el.classList.remove("word-highlight");
  #       });
  #       document.querySelector("#word_cloud_remaining").querySelectorAll("span").forEach(el => {
  #         if(el.textContent === "%s") {
  #           el.classList.add("word-highlight");
  #         }
  #       });
  #     ', input$hover_word_remaining))
  #     }
  #   }, ignoreNULL = FALSE)
  
  
  
  
  
  #   process_text_data <- function(text_data, gram_type = "Uni-gram") {
  #     if (is.null(text_data) || all(text_data == "")) return(NULL)
  #     
  #     # Clean once, use everywhere
  #     cleaned_text <- text_processor$clean(text_data)
  #     
  #     # Dynamic max words based on dataset size
  #     total_words <- length(unlist(strsplit(cleaned_text, "\\s+")))
  #     max_words <- ifelse(total_words < 50, floor(total_words/2), 100)
  #     
  #     if (gram_type == "Uni-gram") {
  #       # Uni-gram processing
  #       corpus <- Corpus(VectorSource(cleaned_text))
  #       dtm <- DocumentTermMatrix(corpus)
  #       freq <- colSums(as.matrix(dtm))
  #       word_freq <- data.frame(word = names(freq), freq = freq) |> 
  #         arrange(desc(freq)) |> 
  #         filter(freq > 1) |> 
  #         slice(1:max_words)
  #     } else {
  # 
  #       # Bi-gram processing with dynamic adjustment
  #       text_df <- tibble(text = cleaned_text)
  #       word_freq <- text_df |>
  #         unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  #         count(bigram, sort = TRUE) |>
  #         filter(n > 1) |>
  #         rename(word = bigram, freq = n)
  # 
  #       # Adjust for small datasets
  #       available_bigrams <- nrow(word_freq)
  #       if (available_bigrams > 0) {
  #         word_freq <- word_freq |>
  #           slice(1:min(max_words, available_bigrams))
  #       }
  #     }
  #     return(word_freq)
  #   }
  #   
  #   # Replace the existing reactive calculations with:
  #   removed_word_freq <- reactive({
  #     req(comparison_done(), removed_posts())
  #     cleaned <- text_processor$clean(removed_posts()$text)
  #     text_processor$get_freq(cleaned, gram_type = "Uni-gram") %>%
  #       filter(freq > 1) %>%  # Add your existing filters
  #       slice(1:100)          # Add your existing slicing
  #   }) 
  #   
  #   remaining_word_freq <- reactive({
  #     req(comparison_done(), remaining_posts())
  #     cleaned <- text_processor$clean(remaining_posts()$text)
  #     text_processor$get_freq(cleaned, gram_type = "Uni-gram") %>%
  #       filter(freq > 1) %>%  # Add your existing filters
  #       slice(1:100)          # Add your existing slicing
  #   }) 
  #   
  #   # Word cloud renders with dynamic sizing
  #   output$word_cloud_removed <- renderWordcloud2({
  #     req(comparison_done(), removed_posts())
  #     # Show loading placeholder
  #     if(is.null(removed_word_freq())) {
  #       return(div(class = "loading-placeholder", "Generating word cloud..."))
  #     }
  #     
  #     if (nrow(removed_word_freq()) == 0) {
  #       showNotification("No valid bigrams in removed posts", type = "warning")
  #       return(NULL)
  #     }
  #     
  #     wordcloud2(
  #       removed_word_freq(),
  #       size = ifelse(input$word_cloud_gram == "Uni-gram", 0.8, 0.6),
  #       minSize = ifelse(input$word_cloud_gram == "Uni-gram", 0.01, 0.005),
  #       gridSize = ifelse(input$word_cloud_gram == "Uni-gram", 10, 5),
  #       color = "#FF6B6B",
  #       shape = 'circle'
  #     )
  #   })
  #   
  #   # Remaining posts word cloud - fixed hover issue
  #   output$word_cloud_remaining <- renderWordcloud2({
  #     req(comparison_done(), remaining_posts())
  #     # Show loading placeholder
  #     if(is.null(remaining_word_freq())) {
  #       return(div(class = "loading-placeholder", "Generating word cloud..."))
  #     }
  #     
  #     if (nrow(remaining_word_freq()) == 0) {
  #       showNotification("No valid bigrams in remaining posts", type = "warning")
  #       return(NULL)
  #     }
  #     
  #     wordcloud2(
  #       remaining_word_freq(),
  #       size = ifelse(input$word_cloud_gram == "Uni-gram", 0.8, 0.6),
  #       minSize = ifelse(input$word_cloud_gram == "Uni-gram", 0.01, 0.005),
  #       gridSize = ifelse(input$word_cloud_gram == "Uni-gram", 10, 5),
  #       color = "#4ECDC4",
  #       shape = 'circle'
  #     )
  #   })
  #   # Add CSS for loading placeholder
  #   tags$style(HTML("
  #   .loading-placeholder {
  #     height: 400px;
  #     display: flex;
  #     align-items: center;
  #     justify-content: center;
  #     color: #666;
  #     font-style: italic;
  #   }
  # "))
  
  
  
  # ===== Topic Modeling ===== #
  # Helper: run LDA on cleaned text (backup helper function)
  run_lda <- function(text_data, k) {
    cleaned <- text_processor$clean(text_data, use_stem = FALSE, use_lemma = TRUE)
    if (length(cleaned) < 5) return(NULL)
    
    
    corpus <- Corpus(VectorSource(cleaned))
    dtm <- DocumentTermMatrix(corpus)
    
    # Find non-empty docs
    non_empty_docs <- rowSums(as.matrix(dtm)) > 0
    
    # Filter both corpus and cleaned text
    dtm <- dtm[non_empty_docs, ]
    cleaned <- cleaned[non_empty_docs]
    
    
    
    if (nrow(dtm) < 5 || ncol(dtm) < 5) return(NULL)
    tryCatch(LDA(dtm, k = k, control = list(seed = 123, verbose = 0)),
             error = function(e) NULL)
  }
  
  # Helper: Create JSON for LDAvis easily
  topicmodels_json_ldavis <- function(fitted, text_vector, doc_term) {
    library(dplyr)
    library(stringi)
    
    phi <- posterior(fitted)$terms %>% as.matrix()
    theta <- posterior(fitted)$topics %>% as.matrix()
    vocab <- colnames(phi)
    
    doc_length <- sapply(text_vector, function(x) {
      stri_count(x, regex = "\\S+")
    })
    
    temp_frequency <- as.matrix(doc_term)
    freq_matrix <- data.frame(ST = colnames(temp_frequency),
                              Freq = colSums(temp_frequency))
    
    # TRY createJSON with cmdscale, fallback to PCA if error
    json <- tryCatch({
      LDAvis::createJSON(phi = phi, theta = theta,
                         vocab = vocab,
                         doc.length = doc_length,
                         term.frequency = freq_matrix$Freq,
                         mds.method = stats::cmdscale)
    }, error = function(e) {
      LDAvis::createJSON(phi = phi, theta = theta,
                         vocab = vocab,
                         doc.length = doc_length,
                         term.frequency = freq_matrix$Freq,
                         mds.method = function(x) { prcomp(x)$x[,1:2] })
    })
    
    return(json)
  }
  
  
  output$ldavis_output <- renderUI({
    req(comparison_done(), input$num_topics)
    
    dataset_list <- list(
      "Removed Posts" = removed_posts(),
      "Remaining Posts" = remaining_posts(),
      "Combined View" = bind_rows(
        removed_posts() %>% mutate(group = "removed"),
        remaining_posts() %>% mutate(group = "remaining")
      )
    )
    
    dataset <- dataset_list[[input$topic_dataset]]
    
    if (nrow(dataset) < 5 || all(dataset$text == "")) {
      return(div(class = "alert alert-warning",
                 "Not enough text data for topic modeling (minimum 5 documents required)"))
    }
    
    withProgress(message = 'Generating topics...', value = 0.5, {
      cleaned <- text_processor$clean(dataset$text, use_stem = FALSE, use_lemma = TRUE)
      corpus <- Corpus(VectorSource(cleaned))
      dtm <- DocumentTermMatrix(corpus)
      dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
      
      if (nrow(dtm) < 5 || ncol(dtm) < 5) {
        return(div(class = "alert alert-danger",
                   "Topic modeling failed - insufficient meaningful text patterns"))
      }
      
      lda_model <- tryCatch({
        LDA(dtm, k = input$num_topics, control = list(seed = 1234))
      }, error = function(e) NULL)
      
      if (is.null(lda_model)) {
        return(div(class = "alert alert-danger",
                   "LDA Model could not be built."))
      }
      
      # Get the LDAvis JSON directly
      json <- topicmodels_json_ldavis(lda_model, cleaned, dtm)
      
      # Render directly without file writing
      LDAvis::renderVis(json)
    })
  })
  
  
  
  # ===== Sentiment Analysis ===== #
  get_sentiment_distribution <- function(text_vector) {
    if (is.null(text_vector)) {
      return(data.frame(
        category = c("Negative", "Neutral", "Positive"),
        percentage = c(0, 0, 0)
      ))
    }
    
    # Process in chunks for large datasets
    chunk_size <- 500
    chunks <- split(text_vector, ceiling(seq_along(text_vector)/chunk_size))
    
    results <- lapply(chunks, function(chunk) {
      sentences <- get_sentences(chunk)
      sentiment(sentences)
    })
    
    all_scores <- unlist(lapply(results, function(x) x$sentiment))
    
    category <- cut(all_scores, 
                    breaks = c(-Inf, -0.01, 0.01, Inf),
                    labels = c("Negative", "Neutral", "Positive"))
    
    counts <- table(factor(category, levels = c("Negative", "Neutral", "Positive")))
    percentages <- prop.table(counts) * 100
    
    data.frame(
      category = names(percentages),
      percentage = as.numeric(percentages)
    )
  }
  
  # Optimized version of sentiment_by for extreme posts
  get_extreme_posts <- function(df, n = 1, type = "positive") {
    if (nrow(df) == 0) return("No data")
    
    # Process in chunks
    chunk_size <- 500
    chunks <- split(df, ceiling(seq_len(nrow(df))/chunk_size))
    
    all_scores <- lapply(chunks, function(chunk) {
      sentences <- get_sentences(chunk$text)
      scores <- sentiment_by(sentences)
      data.frame(text = chunk$text, score = scores$ave_sentiment)
    }) %>% bind_rows()
    
    if (type == "positive") {
      all_scores %>% 
        arrange(desc(score)) %>% 
        slice_head(n = n) %>% 
        pull(text) %>% 
        as.character()
    } else {
      all_scores %>% 
        arrange(score) %>% 
        slice_head(n = n) %>% 
        pull(text) %>% 
        as.character()
    }
  }
  
  # Sentiment distribution plots 
  output$sentiment_plot_removed <- renderHighchart({
    req(comparison_done(), removed_posts())
    
    withProgress(message = 'Analyzing sentiment...', value = 0.5, {
      sentiment_data <- get_sentiment_distribution(removed_posts()$text)
    })
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = c("Negative", "Neutral", "Positive")) %>%
      hc_yAxis(title = list(text = "Percentage"), labels = list(format = "{value}%")) %>%
      hc_add_series(name = "Removed Posts", 
                    data = sentiment_data$percentage, 
                    color = "#AA0114") %>%
      hc_tooltip(pointFormat = "<b>{point.category}</b>: {point.y:.1f}%") %>%
      hc_plotOptions(series = list(pointPadding = 0.1, groupPadding = 0.1))
  })
  
  output$sentiment_plot_remaining <- renderHighchart({
    req(comparison_done(), remaining_posts())
    
    withProgress(message = 'Analyzing sentiment...', value = 0.5, {
      sentiment_data <- get_sentiment_distribution(remaining_posts()$text)
    })
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = c("Negative", "Neutral", "Positive")) %>%
      hc_yAxis(title = list(text = "Percentage"), labels = list(format = "{value}%")) %>%
      hc_add_series(name = "Remaining Posts", 
                    data = sentiment_data$percentage, 
                    color = "#4472c4") %>%
      hc_tooltip(pointFormat = "<b>{point.category}</b>: {point.y:.1f}%") %>%
      hc_plotOptions(series = list(pointPadding = 0.1, groupPadding = 0.1))
  })
  
  # # Sentiment comparison table
  # output$dataset_comparison_table <- renderTable({
  #   req(comparison_done(), data1(), data2())
  #   
  #   withProgress(message = 'Comparing datasets...', value = 0, {
  #     incProgress(0.3, detail = "Analyzing Dataset 1")
  #     sent1 <- get_sentiment_distribution(data1()$text)
  #     
  #     incProgress(0.6, detail = "Analyzing Dataset 2")
  #     sent2 <- get_sentiment_distribution(data2()$text)
  #     
  #     data.frame(
  #       Category = c("Negative", "Neutral", "Positive"),
  #       Dataset1 = paste0(round(sent1$percentage, 1), "%"),
  #       Dataset2 = paste0(round(sent2$percentage, 1), "%")
  #     )
  #   })
  # })
  
  # ----- Most Extreme Posts - Dynamic Boxes ----- #
  # Reactive expressions for text
  most_positive_removed_text <- reactive({
    req(comparison_done(), removed_posts())
    withProgress(message = 'Finding most positive...', value = 0.5, {
      get_extreme_posts(removed_posts(), type = "positive")
    })
  })
  
  most_negative_removed_text <- reactive({
    req(comparison_done(), removed_posts())
    withProgress(message = 'Finding most negative...', value = 0.5, {
      get_extreme_posts(removed_posts(), type = "negative")
    })
  })
  
  most_positive_remaining_text <- reactive({
    req(comparison_done(), remaining_posts())
    withProgress(message = 'Finding most positive...', value = 0.5, {
      get_extreme_posts(remaining_posts(), type = "positive")
    })
  })
  
  most_negative_remaining_text <- reactive({
    req(comparison_done(), remaining_posts())
    withProgress(message = 'Finding most negative...', value = 0.5, {
      get_extreme_posts(remaining_posts(), type = "negative")
    })
  })
  
  # Dynamic UI boxes
  output$most_positive_removed_box <- renderUI({
    text <- most_positive_removed_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #f0f8ff; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_negative_removed_box <- renderUI({
    text <- most_negative_removed_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #fff0f0; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_positive_remaining_box <- renderUI({
    text <- most_positive_remaining_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #f0f8ff; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  output$most_negative_remaining_box <- renderUI({
    text <- most_negative_remaining_text()
    if(is.null(text) || text == "No data") return(div("No data"))
    
    div(style = paste0("background: #fff0f0; padding: 10px; border-radius: 5px;",
                       "min-height: 50px; max-height: 300px;",
                       "overflow-y: auto; white-space: pre-wrap;"),
        text)
  })
  
  # ===== Keyness Analysis Module ===== #
  keyness_analyzer <- list(
    prepare_data = function(removed_posts, remaining_posts) {
      combined_df <- data.frame(
        text = c(text_processor$clean(removed_posts$text), 
                 text_processor$clean(remaining_posts$text)),
        group = c(rep("removed", nrow(removed_posts)), 
                  rep("remaining", nrow(remaining_posts)))
      )
      
      frequency_table_creator(
        df = combined_df,
        text_field = "text",
        grouping_variable = "group",
        grouping_variable_target = "removed",
        remove_punct = TRUE,
        remove_symbols = TRUE,
        remove_numbers = TRUE,
        lemmatize = TRUE
      )
    },
    
    calculate_keyness = function(frequency_table) {
      keyness_measure_calculator(
        frequency_table,
        log_likelihood = TRUE,
        ell = TRUE,
        bic = TRUE,
        perc_diff = TRUE,
        relative_risk = TRUE,
        log_ratio = TRUE,
        odds_ratio = TRUE,
        sort = "decreasing",
        sort_by = "ell"  # Changed to sort by effect size
      )
    }
  )
  
  # Reactive keyness analysis
  keyness_results <- reactive({
    req(removed_posts(), remaining_posts())
    
    withProgress(message = 'Analyzing key terms...', value = 0.5, {
      freq_table <- keyness_analyzer$prepare_data(removed_posts(), remaining_posts())
      measures <- keyness_analyzer$calculate_keyness(freq_table)
      
      # Helper function for filtering
      filter_terms <- function(use_type, n = 5) {
        measures %>%
          filter(word_use == use_type, 
                 log_likelihood > 3.84) %>%  # Remove ell > 0 condition
          arrange(desc(log_likelihood)) %>%  # Sort by log_likelihood first
          slice(1:n)
      }
      
      list(
        overuse = filter_terms("overuse"),
        underuse = filter_terms("underuse"),
        all = measures %>% 
          filter(log_likelihood > 3.84) %>%
          arrange(desc(ell))
      )
    })
  })
  
  # UI controls remain the same
  output$keyness_controls <- renderUI({
    req(keyness_results())
    
    tabsetPanel(
      id = "keyness_tabs",
      tabPanel("Removed Posts", value = "removed"),
      tabPanel("Remaining Posts", value = "remaining"),
      tabPanel("Combined View", value = "combined")
    )
  })
  
  
  # Update the plot to show ELL values
  output$keyness_plot <- renderHighchart({
    req(keyness_results(), input$keyness_tabs)
    
    # Create keyness_data based on tab selection
    if(input$keyness_tabs == "removed") {
      keyness_data <- keyness_results()$overuse %>%
        mutate(
          color = "#AA0114",
          y = ell  # Use ELL instead of log_likelihood
        )
      title_text <- "Terms Distinctive of Removed Posts (by Effect Size)"
      
    } else if(input$keyness_tabs == "remaining") {
      keyness_data <- keyness_results()$underuse %>%
        mutate(
          color = "#4472C4",
          y = ell
        )
      title_text <- "Terms Distinctive of Remaining Posts (by Effect Size)"
      
    } else {
      overuse_top <- keyness_results()$overuse %>% 
        slice(1:5) %>%
        mutate(
          color = "#AA0114",
          y = ell
        )
      
      underuse_top <- keyness_results()$underuse %>% 
        slice(1:5) %>%
        mutate(
          color = "#4472C4",
          y = ell   # Negative ELL for visualization
        )
      
      keyness_data <- bind_rows(overuse_top, underuse_top) %>%
        arrange(desc(abs(y)))
      
      title_text <- "Keyness Analysis: Effect Size Comparison"
    }
    
    # Create the highchart object
    highchart() %>%
      hc_chart(
        type = "bar",
        height = 500,
        marginLeft = 100,
        marginBottom = 100
      ) %>%
      hc_title(
        text = title_text
      ) %>%
      hc_subtitle(
        text = paste0("Comparing ", nrow(removed_posts()), " removed posts to ", 
                      nrow(remaining_posts()), " remaining posts")
      ) %>%
      hc_xAxis(
        categories = keyness_data$word,
        labels = list(
          style = list(fontSize = "11px"),
          rotation = 0
        )
      ) %>%
      hc_yAxis(
        title = list(text = "Effect Size (ELL) [0-1]"),
        labels = list(format = "{value:.6f}"),  # Show more decimal places
        plotLines = if(input$keyness_tabs == "combined") 
          list(list(value = 0, color = "#666", width = 1, zIndex = 5)) else NULL
      ) %>%
      hc_tooltip(
        formatter = JS("function() {
    var corpus = this.point.y > 0 ? 'Removed' : 'Remaining';
    var ell = (this.point.y).toFixed(6);  // Just format directly, no scaling needed
    var ll = this.point.log_likelihood.toFixed(2);
    var ratio = this.point.log_ratio ? this.point.log_ratio.toFixed(2) : 'N/A';
    return '<b>' + this.point.category + '</b><br>' +
           'More frequent in: <b>' + corpus + '</b><br>' +
           'Effect Size (ELL): ' + ell + '<br>' +
           'Log-likelihood: ' + ll + '<br>' +
           'Log Ratio: ' + ratio;
  }")
      ) %>%
      hc_plotOptions(
        series = list(
          colorByPoint = TRUE,
          minPointLength = 3
        ),
        bar = list(
          groupPadding = 0.1,
          pointPadding = 0.1
        )
      ) %>%
      hc_add_series(
        data = lapply(1:nrow(keyness_data), function(i) {
          list(
            y = keyness_data$y[i],
            color = keyness_data$color[i],
            log_likelihood = keyness_data$log_likelihood[i],
            log_ratio = keyness_data$log_ratio[i]
          )
        }),
        showInLegend = FALSE
      )
  })
  
  # Updated interpretation with ELL
  output$keyness_interpretation <- renderUI({
    req(keyness_results(), input$keyness_tabs)
    
    if(input$keyness_tabs == "removed") {
      top_terms <- keyness_results()$overuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
      
      HTML(paste0(
        "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h5>Understanding Key Terms in Removed Posts</h5>",
        "<div style='color: #AA0114;'>",
        paste("- ", top_terms$info, collapse = "<br>"),
        "</div>",
        "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
        "These words appear much more often in removed posts than in the remaining ones.<br>",
        "<strong>Example:</strong> If the word 'protest' appears a lot in removed posts but not in remaining ones, it will show up here.<br>",
        "<strong>LL (Log-likelihood)</strong> tells us how statistically significant the difference is (a value above 3.84 means it's important).<br>",
        "<strong>ELL (Effect Size)</strong> shows how strong that difference is (closer to 1 = bigger difference).",
        "</p>",
        "</div>"
      ))
      
      
    } else if(input$keyness_tabs == "remaining") {
      top_terms <- keyness_results()$underuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
      
      HTML(paste0(
        "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h5>Understanding Key Terms in Remaining Posts</h5>",
        "<div style='color: #4472C4;'>",
        paste("- ", top_terms$info, collapse = "<br>"),
        "</div>",
        "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
        "These words show up more often in remaining posts compared to removed ones.<br>",
        "<strong>Example:</strong> If the word 'update' is common in remaining posts but not in removed ones, it will appear here.<br>",
        "LL (Log-likelihood) and ELL (Effect Size) explain how important and how strong the difference is.",
        "</p>",
        "</div>"
      ))
      
    } else {
      top_removed <- keyness_results()$overuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
      
      top_remaining <- keyness_results()$underuse %>%
        slice(1:5) %>%
        mutate(info = paste0(word, " (LL: ", round(log_likelihood, 1), ", ELL: ", sprintf("%.6f", ell), ")"))
      
      HTML(paste0(
        "<div style='margin-top: 20px; background: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h5>Quick Guide: Comparing Key Terms</h5>",
        "<div style='columns: 2;'>",
        "<div style='color: #AA0114;'>",
        "<strong>Common in Removed Posts:</strong><br>",
        paste("- ", top_removed$info, collapse = "<br>"),
        "</div>",
        "<div style='color: #4472C4; margin-left: 30px;'>",
        "<strong>Common in Remaining Posts:</strong><br>",
        paste("- ", top_remaining$info, collapse = "<br>"),
        "</div>",
        "</div>",
        "<p style='margin-top: 10px; font-size: 0.9em; color: #666;'>",
        "This helps you understand which words are more typical in each group.<br>",
        "LL tells us if it's a meaningful difference (above 3.84 = likely real).<br>",
        "ELL shows how big the difference is (0 to 1 scale, closer to 1 = bigger).<br>",
        "<strong>Example:</strong> 'banned' might appear more in removed posts, while 'sale' might appear more in remaining posts.",
        "</p>",
        "</div>"
      ))
    }
  })
  
}

# ======================= #
#       APPLICATION       #
#       LAUNCHER          #
# ======================= #
shinyApp(ui, server)