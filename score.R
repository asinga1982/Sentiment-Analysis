scores <- function(data, pos, neg, .progress = 'none'){
  
  score = laply(data, function(data, pos, neg)
  {
    data <- gsub("[[:punct:]]", "",data)
    data <- gsub("d+", "", data )
    data <- gsub("[[:cntrl:]]", "",data)
    trytolower <- function(y){
      x <- NA
      try_error <- tryCatch(tolower(y), error=function(e) e)
      
      if (!inherits(try_error, "error"))
      x <- tolower(y)
      return(x)
      
    }
    
    data <- sapply(data, trytolower)
    
    word.list <- str_split(data, "\\s+")
    words <- unlist(word.list)
    
    pos.match <- !is.na(match(words, pos))
    neg.match <- !is.na(match(words, neg))
    
    s <- sum(pos.match) - sum(neg.match)
    return(s)
  }, pos, neg, .progress = .progress)               
    
    score.df <- data.frame(data, score)
    return(score.df)
  
}