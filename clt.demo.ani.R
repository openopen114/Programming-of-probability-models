#"Demonstration of the Central Limit Theorem"
#openopen  "Mon Jan 12 2015"
#openopen114@gmail.com

#function: clt.demo.ani()
#default : 
#nsample = 50, ntrial = 30
# data form rnorm( nsample, mean=0, sd=1)


#run
#>source("clt.demo.ani.R")
#>clt.demo.ani(30,15)

clt.demo.ani <- function( nsample = 50, ntrial = 30){
    
    
    # data form rnorm( nsample, mean=0, sd=1)
    rdata  <-  c()
    for ( i in 1:ntrial) {
        rdata <- c( rdata, rnorm( nsample, mean=0, sd=1) )
    }
    
    #global "rdata"
    assign("rdata", rdata, envir = .GlobalEnv)
    assign("nsample", nsample, envir = .GlobalEnv)
    assign("ntrial", ntrial, envir = .GlobalEnv)
    
    #animation using ggplot animation
    #data
    library(ggplot2)
    library(animation)
    
    
    # ggplot single graph
    draw.p.plot <- function(i) {
        
        n <- (nsample*i)
        
        df.data <- data.frame( plotdata = rdata[1:n] )
        
        p <- ggplot(df.data, aes(x = plotdata ) ) + 
            geom_histogram(aes(y=..density..), binwidth=.2,
                           colour="#007ecd", fill="#b9e4ff") +
            geom_density(alpha=.2, colour="#db347e")  
        
        print(p)
    }
    
    
    
    
    #loop all graph
    loop.p.animate <- function() {
        lapply(1:ntrial, function(i) {
            draw.p.plot(i)
        })
    }
    
    
    #save HTML
    
    daterecord <- date()
    dts <- gsub(" {0,}","",daterecord) # no " space "
    dt <- gsub(":",replacement="_",dts) # no " Colon "
    saveHTML(loop.p.animate(), img.name = dt,
             htmlfile = "cltdemo.html",
             ani.height = 400, ani.width = 600,  
             interval = 0.2,
             title = "cltdeom",
             description = "Demonstration of the Central Limit Theorem"
    )    
    
    
    
    
    ### use animation package function "clt.ani"
    oopt = ani.options(interval = 0.2, nmax = ntrial)
    daterecord <- paste("cltani",date(),sep=" ")
    dts <- gsub(" {0,}","",daterecord) # no " space "
    dt <- gsub(":",replacement="_",dts) # no " Colon "
    saveHTML(clt.ani(obs = nsample, FUN = rnorm, mean = 0, sd = 1),
             img.name = dt,
             htmlfile = "clt.ani.func.demo.html",
             ani.height = 400, ani.width = 600,  
             interval = 0.2,
             title = "clt.ani.func.demo",
             description = "Demonstration of the Central Limit Theorem"
    )  
    
}



