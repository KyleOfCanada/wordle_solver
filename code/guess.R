library(tidyverse)
library(here)

# Get list of words
dat <- read_delim(here("data", "wordle-answers-alphabetical.txt"),
                  delim = "\n",
                  col_names = "value",
                  col_types = 'c') %>% 
  mutate(unique_letters = str_count(value, str_sub(value, 1, 1)) == 1 &
           str_count(value, str_sub(value, 2, 2)) == 1 &
           str_count(value, str_sub(value, 3, 3)) == 1 &
           str_count(value, str_sub(value, 4, 4)) == 1 &
           str_count(value, str_sub(value, 5, 5)) == 1,
         vowels = str_count(value, "a|e|i|o|u"),
         weight = 0)

# Count how often each letter appears
letter_counts <- tibble(letter = letters, first = 0, second = 0, third = 0, fourth = 0, fifth = 0)

for(i in 1:26) {
  letter <- letter_counts$letter[i]
  
  for(ii in 1:5) {
    cnt <- sum(str_count(str_sub(dat$value, ii, ii), letter))
    letter_counts[i, ii + 1] <- cnt
  }
}

word_weight <- function(word) {
  weight <- 0
  lttrs <- list(NULL)
  for(i in 1:5) {
    letter <- str_sub(word, i, i)
    if(letter %in% lttrs) {
      weight <- weight + sum(letter_counts[letter_counts$letter == letter, i + 1])
    } else {
      lttrs <- c(lttrs, letter)
      weight <- weight + sum(letter_counts[letter_counts$letter == letter, i + 1]) 
      weight <- weight + sum(letter_counts[letter_counts$letter == letter, 2:6])/5
    }
  }
  return(weight)
}

for(i in 1:nrow(dat)) {
  dat$weight[i] <- word_weight(dat$value[i])
}

# Main function that guesses
guess <- function() {
  
  possible_words <- dat
  
  solved_letters <- c(NA, NA, NA, NA, NA)
  
  # solved_count <- 0
  
  # Get a good starting word
  start_words <- dat %>%
    filter(weight == max(weight))
  
  Word <- sample(start_words$value, 1)
  
  guess_count <- 1
  
  cat("Round\tWords Left\tGuess\t\t\t Result")
  
  # Sub-function to take user input
  get_results <- function(Word, guess_count, possible) {
    while(TRUE) {
      results <- readline(str_c(guess_count, "/6\t",nrow(possible), "\t\t\"", Word, "\"\tresult? (x,i,y): "))
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
  
  # Make first guess
  results <- get_results(Word, guess_count, possible_words)
  
  # Loop through guesses until solved
  while(nrow(possible_words) > 0) {
    
    # Win condition
    if(results == "yyyyy") {
      cat("\n\tCongradulations!!!\n\n")
      break
    }
    
    # Fail condition
    if(guess_count == 6) {
      cat("\n\tOut of Guesses\n\n")
      break
    }
    
    included_letters <- NULL
    y_positions <- str_locate_all(results, "y|i")[[1]][,1]
    for(ii in (1:5)[(1:5 %in% y_positions)] ) {
      included_letters <- str_c(included_letters, str_sub(Word, ii, ii))
    }

    # Filter possible words based on results of guess
    for(i in 1:5) {
      result <- str_sub(results, i, i)
      letter <- str_sub(Word, i, i)
      if(result == "y") {
        solved_letters[i] <- letter
        
        possible_words <- possible_words %>% 
          filter(str_sub(value, i, i) == letter)
      } else if(result == "i") {
        possible_words <- possible_words %>% 
          filter(!str_sub(value, i, i) == letter,
                 str_detect(value, letter))
      } else {
        if(length(included_letters) == 0) {
          possible_words <- possible_words %>% 
            filter(!str_detect(value, letter))
        } else if(!str_detect(included_letters, letter)){
          possible_words <- possible_words %>% 
            filter(!str_detect(value, letter))
        } else {
          possible_words <- possible_words %>%
            filter(!(str_sub(value, i, i) == letter))
        }
      }
    }
    
    for(i in 1:26) {
      letter <- letter_counts$letter[i]
      
      for(ii in 1:5) {
        if(is.na(solved_letters[ii])) {
          cnt <- sum(str_count(str_sub(possible_words$value, ii, ii), letter))
          letter_counts[i, ii + 1] <- cnt
        } else if(solved_letters[ii] == letter){
          letter_counts[i, ii + 1] <- 0
        } else {
          cnt <- sum(str_count(str_sub(possible_words$value, ii, ii), letter))
          letter_counts[i, ii + 1] <- cnt
        }
      }
    }
    
    word_weight <- function(word) {
      weight <- 0
      lttrs <- list(NULL)
      for(i in 1:5) {
        letter <- str_sub(word, i, i)
        if(letter %in% lttrs) {
          weight <- weight + sum(letter_counts[letter_counts$letter == letter, i + 1])
        } else {
          lttrs <- c(lttrs, letter)
          weight <- weight + sum(letter_counts[letter_counts$letter == letter, i + 1])
          weight <- weight + sum(letter_counts[letter_counts$letter == letter, 2:6])/5
        }
      }
      return(weight)
    }
    
    for(i in 1:nrow(dat)) {
      dat$weight[i] <- word_weight(dat$value[i])
    }
    
    # Make next guess
    guess_count <- guess_count + 1
    
    # Select solved word or a high weight word
    if(guess_count == 6) {
      Word <- sample(possible_words$value, 1)
    } else if(sum(is.na(solved_letters)) == 0) {
      Word <- str_c(solved_letters, collapse = "")
    } else if (nrow(possible_words) <= 2) {
      Word <- sample(possible_words$value, 1)
    } else {
      guess_words <- dat %>%
        filter(weight == max(weight))
      
      Word <- sample(guess_words$value, 1)
    }
    results <- get_results(Word, guess_count, possible_words)
  }
}

guess()
