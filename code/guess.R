library(tidyverse)
library(here)

dat <- read_delim(here("data", "wordle-answers-alphabetical.txt"),
                  delim = "\n",
                  col_names = "value",
                  col_types = 'c') %>% 
  # filter(str_detect(value,
  #                   "^[:alpha:]{5}$")) %>% 
  mutate(unique_letters = str_count(value, str_sub(value, 1, 1)) == 1 &
           str_count(value, str_sub(value, 2, 2)) == 1 &
           str_count(value, str_sub(value, 3, 3)) == 1 &
           str_count(value, str_sub(value, 4, 4)) == 1 &
           str_count(value, str_sub(value, 5, 5)) == 1,
         vowels = str_count(value, "a|e|i|o|u"),
         weight = 0)

letter_count <- function(x) {
  sum(str_count(dat$value, x))
}

letter_counts <- tibble(letter = letters, count = 0)

for(i in 1:26) {
  letter_counts$count[i] <- letter_count(letter_counts$letter[i]) 
}

word_weight <- function(word) {
  weight <- 0
  for(i in 1:5) {
   weight <- weight + letter_counts$count[letter_counts$letter == str_sub(word, i, i)]
  }
  return(weight)
}       

for(i in 1:nrow(dat)) {
  dat$weight[i] <- word_weight(dat$value[i])
}

dat$weight <- rank(-dat$weight)

get_results <- function(Word) {
  while(TRUE) {
    results <- readline(str_c(Word, "\tresult? (x,i,y): "))
    if(str_detect(results, "^[xiy]{5}$")) {
      return(results)
      break
    }
    if(nchar(results) != 5) {
      cat("Incorrect number of characters entered!\n")
    } else {
      cat("Characters must be x (not in word), i (in word), or y (in word and in correct position)!\n")
    }
  }
}

guess <- function() {
  
  possible_words <- dat
  
  start_words <- dat %>% 
    filter(unique_letters, vowels >= 3)
  
  Word <- sample(start_words$value, 1, prob = start_words$weight)
  
  results <- get_results(Word)
  
  while(nrow(possible_words) > 0) {
    if(results == "yyyyy") {
      cat("\n\tCongradulations!!!\n")
      break
    }
    for(i in 1:5) {
      result <- str_sub(results, i, i)
      letter <- str_sub(Word, i, i)
      if(result == "y") {
        possible_words <- possible_words %>% 
          filter(str_sub(value, i, i) == letter)
      }
      else if(result == "i") {
        possible_words <- possible_words %>% 
          filter(!str_sub(value, i, i) == letter,
                 str_detect(value, letter))
      }
      else{
        possible_words <- possible_words %>% 
          filter(!str_detect(value, letter))
      }
    }
    
    Word <- sample(possible_words$value, 1, prob = possible_words$weight)
    results <- get_results(Word)
  }
}

guess()
