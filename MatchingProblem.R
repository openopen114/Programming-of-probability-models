#Example 3.14 (The Matching Rounds Problem)
#page. 111
#openopen  "Mon Jan 19  2015"
#openopen114@gmail.com


#run
#>source("MatchingProblem.R")
#>MatchingProblem(n)
#output:
#[1] num of total round 
#[2] tabulate selection
#[3] num of total selection


# n : num of people
MatchingProblem <- function(n){
    library(dplyr)
    # set up initial variable
    people  <-  1:n
    stay.people  <-  n
    round  <-  0
    
    # record the people that stay on each round
    list.people <- list()
    result <- list()
    
    
    # select people who choosing wrong hat ie. stay 
    StayPeople <- function(x){
        people <- people[ x != people ]
        assign("people", people, envir = .GlobalEnv)
       
        #output
        people
    }
    
    
    # simulation loop 
    while( stay.people > 0 ){
        round <- round +1       
        people %>%
            sample() %>%
            StayPeople() ->people
        stay.people = length(people)
        list.people[[round]] <- people 
        assign("list.people", list.people, envir = .GlobalEnv)      
    }
    
    
    
    result[[1]] <- paste("num of total round = ", round, sep="")
    
    
    # get the log on each list ie. num of stay 
    num.stay.people  <- 1:n
    for ( i in 1:length(list.people)){
        num.stay.people  <- c(num.stay.people, list.people[[i]])
       
    }
    result[[2]] <- table(num.stay.people)
    
    result[[3]] <- paste("num of total selection = ", sum(table(num.stay.people)), sep="")
    
    result
}