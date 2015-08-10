library(dplyr)
stratSample <-  function(my.df, sample.col, num.group, w = NULL, method = 'num.group'){

  #For Testing
#       my.df <- target.pool
#       sample.col <- c('State', 'PriorAttempt', 'ReserveGroup')
#       num.group <- 3
#       method <- 'num.group'
#       w <- c(0.4, 0.4, 0.2)
  
  my.df <- my.df %>% mutate(join.key = row_number())
  #Declare default cell name first
  my.df$cell.name <- NA

  for(i in 1:length(sample.col)){
    if(i == 1){
      
      my.df$cell.name <- paste(sample.col[i], ':', my.df[, sample.col[i]])
      
    }else{
       
      my.df$cell.name <- paste(my.df$cell.name, sample.col[i], ':', my.df[, sample.col[i]], sep=" ")

    }
  }
    
  output          <- NULL
  unique.cells    <- unique(my.df$cell.name)
  
  if(method == 'num.group'){
  
    num.group <- min(num.group, 50)
    
    for(i in 1:length(unique.cells)){
      
      sub.sample <- subset(my.df, my.df$cell.name == unique.cells[i])
      
      if(nrow(sub.sample) >= num.group){
        
        int.sam <- sample(1:nrow(sub.sample), nrow(sub.sample))
        
        label.drawer <- sample(0:(num.group - 1), num.group) #rotate the group
    
        sub.sample$group <- NA
        
        for(j in 1:num.group){
          
          temp.flag <- int.sam %% num.group == j - 1
          sub.sample$group[temp.flag] <- label.drawer[j]
          
        }
        output <- rbind(output, sub.sample)
      }
      
    }
  }else{
    if(sum(w) == 1 & sum(w>0) == length(w) & method == 'weight'){
    
      num.group <- length(w)
      
      for(i in 1:length(unique.cells)){
        
        sub.sample <- subset(my.df, my.df$cell.name == unique.cells[i])
        n <- floor(w * nrow(sub.sample))
        n.cumsum <- c(0, cumsum(n))
        
        if(nrow(sub.sample) > 0 & sum(n>0) == length(n)){
          
          int.sam <- sample(1:nrow(sub.sample), sum(n))
          sub.sample$group <- NA
          
          for(j in 1:(length(n.cumsum) - 1)){
            
            sub.sample$group[(n.cumsum[j]+1):(n.cumsum[j+1])] <- j
            
          }
          
          output <- rbind(output, sub.sample)
        }
        
      }
    }
  }
  
    output.join  <- output %>% dplyr::select(join.key, group)
    group.sorted <- my.df %>% dplyr::select(join.key) %>% left_join(output.join, by = "join.key") %>%
      select(group)
    group.sorted <- as.vector(unlist(group.sorted))
    group.sorted[is.na(group.sorted)] <- num.group + 1 #Give the Not Selected(=NA) a group index 
    group.sorted <- factor(group.sorted, labels = c(LETTERS[1:num.group], "Not Selected"))
  
  return(group.sorted)
    
}








