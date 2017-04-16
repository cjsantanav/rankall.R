#Assigment3
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
# you can see how many cols and row there are at outcome
ncol(outcome)
nrow(outcome)
# the names of each column
names(outcome)
#
# To make a histogram of the 30-day death rates from heart attack
outcome[, 11] <- as.numeric(outcome[, 11]) 
## You may get a warning about NAs being introduced; that is okay 
hist(outcome[, 11])
#
rates <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
hist(as.numeric(rates[, 11]))
#
#The function should use the following template.
best <- function(state, outcome) { ## Read outcome data ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death ## rate
}
require(plyr)
#
source("best.R")
best("TX", "heart attack")
#
hosp_sort <- function(state,outcome){
  #setting NA to be 0
  rates[rates == "Not Available"] <- 0
  index <- c(grep("^Hospital.*Death*", names(rates)))
  mortality_rates <- rates[,c(2,7,index)]
  names(mortality_rates)[3:5] <- c("heart attack", "heart failure", "pneumonia")
  
  #Making rates numeric
  mortality_rates[,3:5] <- apply(mortality_rates[,3:5],2,as.numeric)
  
  mortality_rates[mortality_rates == 0] <- NA
  selected_state <- mortality_rates[mortality_rates$State == state,]
  
  order_selected <- arrange(selected_state, selected_state[,outcome], Hospital.Name, na.last=TRUE)
  order_selected <- order_selected[complete.cases(order_selected[,outcome]),]
  return(order_selected)
  #na.omit(selected_state[order(c(selected_state[,outcome]), na.last = TRUE),])
}

best <- function(state, outcome, st = "a") {
  if (!state %in% unique(rates$State))
    return("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  
  order_selected <- hosp_sort(state,outcome)
  return(order_selected[1,1])
  
}
### Test
best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")


###
# Write a function called rankhospital that takes three arguments: 
# the 2-character abbreviated name of a state (state), an outcome (outcome), 
# and the ranking of a hospital in that state for that outcome (num). 
# The function reads the outcome-of-care-measures.csv file and 
# returns a character vector with the name of the hospital that has the 
# ranking specified by the num argument. For example, the call
worst <- function(state, outcome, st = "a") {
  if (!state %in% unique(rates$State))
    return("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  
  order_selected <- hosp_sort(state,outcome)
  
  return(order_selected[nrow(order_selected),1])
}

rankhospital <- function(state, outcome, num = "best", st = "a"){
  if (num == "best")
    return(best(state,outcome))
  if (num == "worst")
    return(worst(state,outcome))       
  else {order_selected <- hosp_sort(state,outcome) 
  return(order_selected[num,1])}
}

###
# Write a function called rankall that takes two arguments: 
# an outcome name (outcome) and a hospital rank- ing (num). 
# The function reads the outcome-of-care-measures.csv file and 
# returns a 2-column data frame containing the hospital in each state 
# that has the ranking specified in num. 
# For example the function call rankall(“heart attack”, “best”) 
# would return a data frame containing the names of the hospitals 
# that are the best in their respective states for 30-day heart attack 
# death rates.

hosp_sort <- function(state,outcome){
  #setting NA to be 0
  rates[rates == "Not Available"] <- 0
  index <- c(grep("^Hospital.*Death*", names(rates)))
  mortality_rates <- rates[,c(2,7,index)]
  names(mortality_rates)[3:5] <- c("heart attack", "heart failure", "pneumonia")
  
  #Making rates numeric
  mortality_rates[,3:5] <- apply(mortality_rates[,3:5],2,as.numeric)
  
  mortality_rates[mortality_rates == 0] <- NA
  selected_state <- mortality_rates[mortality_rates$State == state,]
  
  order_selected <- arrange(selected_state, selected_state[,outcome], Hospital.Name, na.last=TRUE)
  order_selected <- order_selected[complete.cases(order_selected[,outcome]),]
  return(order_selected)
  #na.omit(selected_state[order(c(selected_state[,outcome]), na.last = TRUE),])
}

best <- function(state, outcome, st = "a") {
  if (!state %in% unique(rates$State))
    return("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  
  order_selected <- hosp_sort(state,outcome)
  return(order_selected[1,c(1,2)])
  
}

worst <- function(state, outcome, st = "a") {
  if (!state %in% unique(rates$State))
    return("Invalid State")
  if (!outcome %in% c("heart attack", "heart failure", "pneumonia"))
    return("Invalid Outcome")
  
  order_selected <- hosp_sort(state,outcome)
  
  return(order_selected[nrow(order_selected),c(1,2)])
}

rankhospital <- function(state, outcome, num = "best", st = "a"){
  if (num == "best")
    return(best(state,outcome))
  if (num == "worst")
    return(worst(state,outcome))       
  else {order_selected <- hosp_sort(state,outcome) 
  return(order_selected[num,c(1,2)])}
}

rankall <- function(outcome, num = "best") {
  #print(lapply(unique(rates$State),hosp_sort, outcome))
  results <- unlist(lapply(sort(unique(rates$State)), rankhospital, outcome, num),use.names=FALSE)
  hosp <- results[c(TRUE,FALSE)]
  state <- results[c(FALSE,TRUE)]
  all <- data.frame(hosp,state)
  names(all) <- c("Hospital.Name", "state")
  all
  
}
# testing
head(rankall("heart attack", 20), 10)

###
tail(rankall("pneumonia", "worst"), 3)
###
tail(rankall("heart failure"), 10)
###
# With For Loops
rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  states <- unique(data[, 7])
  outcomes <- c("heart attack", "heart failure", "pneumonia") 
  
  if (!state %in% states) {
    stop("invalid state")
  }
  
  if (!outcome %in% outcomes) {
    stop("invalid outcome")
  }
  
  if (outcome == "heart attack") {
    outcome <- 11
  }
  
  if (outcome == "heart failure") {
    outcome <- 17
  }
  
  if (outcome == "pneumonia") {
    outcome <- 23
  }
  
  data[, 11] <- suppressWarnings(as.numeric(data[, 11])) # heart attack
  data[, 17] <- suppressWarnings(as.numeric(data[, 17])) # heart failure
  data[, 23] <- suppressWarnings(as.numeric(data[, 23])) # pneumonia
  
  stateframe <- data[data[, 7] == state, ]
  
  
  statesort <- stateframe[order(stateframe[outcome], stateframe[2]),]
  
  if (num == "best") {num <- 1}
  if (num == "worst") {
    num <- nrow(statesort)
    ret <- c(statesort[num, 2], state)
    return(ret)
    num = "worst"}
  else{
    ret <- c(statesort[num, 2], state)
    return(ret)
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
}


rankall <- function(outcome, num = "best") {
  
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  states <- sort(unique(data[, 7]))
  
  mat <- matrix(, nrow = 0, ncol = 2)
  
  hospital <- rep("", length(states))
  
  for (i in (1:length(states))) {
    out <- matrix(rankhospital(states[i], outcome, num), ncol = 2)
    mat <- rbind(mat, out)
  }
  
  mat
  
}