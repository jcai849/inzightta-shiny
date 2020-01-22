imported <- reactive({
  ############################
  if (input$import_from == "Spotify/Genius"){
    
    cleaned <- raw_data()[[1]] %>% clean_for_app()
    
  }
  else{
    
    if (input$import_from == "Twitter"){
      cleaned <- raw_data()
      cleaned$text <- plain_tweets(cleaned$text)
      cleaned$text <- gsub("[/].+", "", cleaned$text)
      cleaned$text <- textclean::replace_hash(cleaned$text)
      cleaned$text <- textclean::replace_word_elongation(cleaned$text)
      cleaned$text <- gsub("@\\w+", "", cleaned$text)
      
      cleaned <- cleaned %>% clean_for_app()
      cleaned
    }
    
    else if (input$import_from == "The Guardian Articles"){
      
      cleaned <- raw_data() %>% clean_for_app()

      cleaned$id <- iconv(cleaned$id, from = "UTF-8", to = "ASCII//TRANSLIT")
      
      cleaned <- cleaned %>%
        mutate(
          id = str_replace_all(id, "<.+?>", ""))
      
      # Fix the pound sign becoming ?
      cleaned$text <- gsub("[?](\\d+)", "£\\1", cleaned$text)
    }
    
    else if (input$import_from == "" ){
      
    }
    
    else {
      cleaned <- raw_data() %>% clean_for_app()
      
      # for imported text files
      cleaned$text <- gsub('""', "", cleaned$text)
      cleaned$text <- gsub('" "', " ", cleaned$text)
    }
  }
  cleaned
  
})

#### output$pre_processed_show <- renderDataTable(imported()) in raw_data.R

output$downloadData_pre_processed <- downloadHandler(
  filename = function() {
    paste("preprocessed", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(imported(), file, row.names = FALSE)
  }
)