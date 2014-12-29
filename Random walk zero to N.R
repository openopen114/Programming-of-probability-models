#"4.5.3 Using a Random Walk to Analyze a Probabilistic Algorithm 
#for the Satisfiability Problem","page.237"
#openopen  "Mon Dec 29 2014"
#openopen114@gmail.com

#function: Random_Walk_Zero2N()
#default : 
#N=10, p=0.6

#run
#>Random_Walk_Zero2N(N=10, p=0.6)
#output: animation plot, GIF, HTML


Random_Walk_Zero2N <- function( N = 10, p = 0.6 ){
    
    # initial position = 0
    position <- 0
    
    #record 
    position_list = 0
    
    # while position >= N => STOP
    while( position < N ){
        
        #P(0,1) = 1
        if ( position == 0 ){
            position <- 1
        }else{
            #P(i,i+1) = p
            position <- position + ( 2*rbinom(1,1,p) ) - 1
        }
        
        
        position_list = c(position_list, position )
        assign("position_list", position_list, envir = .GlobalEnv)
    } 
    
    
    #animation
    #set up option
    library(animation)
    aniopt <- ani.options(nmax = length(position_list),
                          interval = 0.2)
    
    #animation using ani.pause()
    for (i in 1:length(position_list)) {
        plot(position_list, type='n')
        
        x <- seq(1:i)
        y <- position_list[1:i]
        points(x,y)
        lines(x,y)
        ani.pause()}
    
    #GIF
    saveGIF({
        #animation using ani.pause()
        for (i in 1:length(position_list)) {
            plot(position_list, type='n')
        
            x <- seq(1:i)
            y <- position_list[1:i]
            points(x,y)
            lines(x,y)
            ani.pause()}
             }, interval = 0.2,
                movie.name = "randomwalk_demo.gif",
                ani.width = 600, 
                ani.height = 600
        )
    
    
    #save HTML
    saveHTML({
        #animation using ani.pause()
        for (i in 1:length(position_list)) {
            plot(position_list, type='n')
            
            x <- seq(1:i)
            y <- position_list[1:i]
            points(x,y)
            lines(x,y)
            ani.pause()}
    }, img.name = "randomwalk",
       htmlfile = "Random_Walk_Zero2N.html",
       ani.height = 400, ani.width = 600,  
       interval = 0.2,
       title = "demo",
       description = c("4.5.3 Using a Random Walk to Analyze a Probabilistic Algorithm for the Satisfiability Problem",
                       "page.237")
          )    
}