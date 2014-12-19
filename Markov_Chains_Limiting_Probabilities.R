#install.packages("markovchain")
library(markovchain)

################
###  example ###
################
statesNames=c("a","b","c")
markovB<-new("markovchain", states=statesNames, transitionMatrix=
                 matrix(c(0.2,0.5,0.3,
                          0,1,0,
                          0.1,0.8,0.1),nrow=3, byrow=TRUE, dimnames=list(statesNames,statesNames)
                 ))
steadyStates(markovB)

######  exercises4.33 ######
statesNames=c("type1","type2","type3")
markov33<-new("markovchain", states=statesNames, transitionMatrix=
                 matrix(c(0.8,0.1,0.1,
                          0.6,0.2,0.2,
                          0.4,0.3,0.3),nrow=3, byrow=TRUE, dimnames=list(statesNames,statesNames)
                 ))
 s33<- steadyStates(markov33)




######  exercises4.22 ######
statesNames=c("0","1","2","3","4","5","6","7","8","9","10","11","12")
markov22<-new("markovchain", states=statesNames, transitionMatrix=
                  matrix(c(rep(0,1), rep(1/6,6), rep(0,6),
                           rep(0,2), rep(1/6,6), rep(0,5),
                           rep(0,3), rep(1/6,6), rep(0,4),
                           rep(0,4), rep(1/6,6), rep(0,3),
                           rep(0,5), rep(1/6,6), rep(0,2),
                           rep(0,6), rep(1/6,6), rep(0,1),
                           rep(0,7), rep(1/6,6),
                           rep(1/6,1), rep(0,7), rep(1/6,5),
                           rep(1/6,2), rep(0,7), rep(1/6,4),
                           rep(1/6,3), rep(0,7), rep(1/6,3),
                           rep(1/6,4), rep(0,7), rep(1/6,2),
                           rep(1/6,5), rep(0,7), rep(1/6,1),
                           rep(1/6,6), rep(0,7) 
                           ),nrow=13, byrow=TRUE, dimnames=list(statesNames,statesNames)
                  ))
 s22<- steadyStates(markov20)



######  multiple 4 ######
statesNames=c("0","1","2","3")
markov4<-new("markovchain", states=statesNames, transitionMatrix=
                  matrix(c(1/6, 2/6, 2/6, 1/6,
                           1/6, 1/6, 2/6, 2/6,
                           2/6, 1/6, 1/6, 2/6,
                           2/6, 2/6, 1/6, 1/6),nrow=4, byrow=TRUE, dimnames=list(statesNames,statesNames)
                  ))
s4<- steadyStates(markov4)


