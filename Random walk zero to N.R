#"4.5.3 Using a Random Walk to Analyze a Probabilistic Algorithm 
#for the Satisfiability Problem","page.237"
#openopen  "Mon Dec 29 2014"
#openopen114@gmail.com

#function: Random_Walk_Zero2N()
#default : 
#N=10, p=0.6

#run
#>Random_Walk_Zero2N(N=10, p=0.6)
#output: animation plot, GIF, HTML;ggplot gif


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
    
    ######################
    ### animation plot ###
    ######################
    #set up option of animation
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
    
    ########################
    ### animation -> GIF ###
    ########################
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
                movie.name = "randomwalkz2n_demo1.gif",
                ani.width = 600, 
                ani.height = 600
        )
    
    #########################
    ### animation -> HTML ###
    #########################
    saveHTML({
        #animation using ani.pause()
        for (i in 1:length(position_list)) {
            plot(position_list, type='n')
            
            x <- seq(1:i)
            y <- position_list[1:i]
            points(x,y)
            lines(x,y)
            ani.pause()}
    }, img.name = "randomwalk_deom1",
       htmlfile = "Random_Walk_Zero2N.html",
       ani.height = 400, ani.width = 600,  
       interval = 0.2,
       title = "demo",
       description = c("4.5.3 Using a Random Walk to Analyze a Probabilistic Algorithm for the Satisfiability Problem",
                       "page.237")
          )    
    
    
    
    ########################
    ### animation ggplot ###
    ########################
    #animation using ggplot animation
    #data
    library(ggplot2)
    df.randwalkz2n <- data.frame(x = 1:length(position_list),
                     y = position_list)
    
    
    # ggplot single graph
    draw.p.plot<-function(i){
        p<-ggplot(df.randwalkz2n, aes(x,y)) +
            geom_point(data=df.randwalkz2n[1:i,]) +
            geom_line(data=df.randwalkz2n[1:i,], aes(x,y)) + 
            scale_x_continuous(limits=c(0, length(position_list))) + 
            scale_y_continuous(limits=c(0, N))
        
        print(p)
    }
    
    
    #loop all graph
    loop.p.animate <- function() {
        lapply(1:length(position_list), function(i) {
            draw.p.plot(i)
        })
    }
    
    
    ################################
    ### animation -> GIF(ggplot) ###
    ################################
    saveGIF(loop.p.animate(), interval = .3,
            movie.name="randomwalkz2n_demo2.gif")
    
    
    
    #################################
    ### animation -> HTML(ggplot) ###
    #################################
    saveHTML(loop.p.animate(), img.name = "randomwalk_deom2",
             htmlfile = "Random_Walk_Zero2N_deom2.html",
             ani.height = 400, ani.width = 600,  
             interval = 0.2,
             title = "demo_2",
             description = c("4.5.3 Using a Random Walk to Analyze a Probabilistic Algorithm for the Satisfiability Problem",
                "page.237")
             )    
    
    
}