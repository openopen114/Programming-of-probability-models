#Demo for  " The Gambler's Ruin Problem to drug testing" Page.232
#openopen, "Fri Dec 19 2014"
#openopen114@gmail.com


### 1.> source('Drug_Testing.R')
### 2.> Simulation_Drug_Testing(M, p1, p2, times)
# M  : threshould of test stop ;
# p1 : probability of receive drug number1
# p2 : probability of receive drug number2
# times : number of simulation repeat

### example ###
### testing on M =  5 ; p1 =  0.6 ; p2 =  0.4 ; times =  1000
### >Simulation_Drug_Testing(5, 0.6, 0.4, 1000)



####################
##### function #####
####################
Drug_Testing_func  <- function(M, p1, p2){
    #print setting parameters 
    ###print(paste("testing on M = ", M, "; p1 = ", p1, "; p2 = ", p2))
              
    m = 0
    while( abs(sum(m)) < M ){
        #random Xi & Yi
        x = rbinom(1, 1, p1)
        y = rbinom(1, 1, p2)
                  
        d = x - y
        m = c(m, d)
    }
              
    # print result for M, and n
    ###print(paste("result M = ",sum(m))
    ###print(paste("n = ",length(m)))
             
    #set up global variables "result_M" for simulation using
    assign("result_M",sum(m), envir = .GlobalEnv)
      
}



#######################
##### simulation  #####
#######################

Simulation_Drug_Testing <- function(M, p1, p2, times){
    #print setting parameters 
    print(paste("testing on M = ", M, "; p1 = ", p1, "; p2 = ", p2, "; times = ", times))
    
    #simulation 1000 times
    result_M_list = c()
    for (i in 1:times){
        Drug_Testing_func(M, p1, p2)
        result_M_list = c(result_M_list,result_M)
    }
    
    #probability of an incorrect decision
    if (p1 > p2){
        p_incorrect = sum(result_M_list == -M)/times
        print(paste("probability of an incorrect decision = ", p_incorrect))
    }else{
        p_incorrect = sum(result_M_list == M)/times
        print(paste("probability of an incorrect decision = ", p_incorrect))
    }
    
}
