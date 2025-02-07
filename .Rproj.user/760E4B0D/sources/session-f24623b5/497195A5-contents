library(pdftools)
library(stringr)
library(tidyverse)


ProcessPDF <- function(FilePath){
  doc <- pdf_text(FilePath)
  docText <- character()
  for(page in doc){
    #remove all new lines
    lines <- str_split(page, '\n')[[1]][str_split(page, '\n')[[1]] != ""] %>%
      #strip all numbers and whitespace then capitalize
      str_remove_all(pattern = '[:digit:]|[:punct:]') %>%
      str_trim() %>%
      toupper() %>%
      str_c(collapse = ' ')
    # collapse into a single string
    docText <- append(docText, lines)
  }
  docText <- str_c(docText, collapse = ' ')
  return(docText)
}

load('./data/WokeWords.Rda')

HowWoke <- function(FilePath){
  docText <- ProcessPDF(FilePath)
  return(WokeDF %>% 
    mutate(Frequency = str_count(docText, pattern = Terms)) %>%
    filter(Frequency > 0) %>%
    arrange(desc(Frequency))
  )
}


Mission <- HowWoke('./docs/fvcc_mission.pdf')

view(Mission)



