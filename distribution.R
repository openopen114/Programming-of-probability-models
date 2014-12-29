#Plotting distributions (normal +normal, log normal + log normal)
#openopen  "Mon Dec 22 2014"
#openopen114@gmail.com


########### norm_norm  ###########
#function: norm_norm()
#default : 
#num_1 = 1000, mean_1 = 0, sd_1 = 1, 
#num_2 = 1000, mean_2 = 0, sd_2 = 1

#run
#>source("distribution.R")
#>str(norm_norm)
#>norm_norm(1000,1,2,1000,3,4)
#output:
#        ____________
#       |  pn1  pn3  |
#       |  pn2  pn4  |
#        ____________



########### lognorm_lognorm  ###########
#function: lognorm_lognorm()
#default :
#num_1 = 1000, mean_1 = 0, sd_1 = 1, 
#num_2 = 1000, mean_2 = 0, sd_2 = 1

#run
#>source("distribution.R")
#>str(lognorm_lognorm)
#>lognorm_lognorm(1000,0.1,1.1,1000,0.2,1.2)
#output:
#        ____________
#       |  pl1  pl3  |
#       |  pl2  pl4  |
#        ____________


#######################
######Distribution#####
#######################
#rnorm(n, mean=0, sd=1)                     [ Normal ]
#rexp(n, rate=1)                       [ Exponential ]
#rgamma(n, shape, scale=1)                   [ Gamma ]
#rpois(n, lambda)                          [ Poisson ] 
#rweibull(n, shape, scale = 1)             [ Weibull ]
#rcauchy(n, location = 0, scale = 1)        [ Cauchy ]
#rbeta(n, shape1, shape2, ncp = 0)            [ Beta ]
#rt(n, df, ncp)                                  [ t ]
#rf(n, df1, df2, ncp)                            [ F ]
#rchisq(n, df, ncp = 0)  (non-central) [ Chi-Squared ] 
#rbinom(n, size, prob)                    [ Binomial ] 
#rgeom(n, prob)                          [ Geometric ]
#rhyper(nn, m, n, k)                [ Hypergeometric ]
#rlogis(n, location = 0, scale = 1)       [ Logistic ]
#rlnorm(n, meanlog = 0, sdlog = 1)      [ Log Normal ] 
#rnbinom(n, size, prob, mu)      [ Negative Binomial ] 
#runif(n, min = 0, max = 1)                [ Uniform ]
#rwilcox(nn, m, n)     [ Wilcoxon Rank Sum Statistic ]

#r for random number generation
#d for density
#p for cumulative distribution
#q for quantile function



#######################
### normal + normal ###
#######################
#default num=1000, mean=0, sd=1
#>source("distribution.R")
#>norm_norm(1000,1,2,1000,3,4)

set.seed(1234)
norm_norm <- function(num_1=1000, mean_1=0, sd_1=1,
                      num_2=1000, mean_2=0, sd_2=1){

    sample_norm_1 <- rnorm(num_1, mean_1, sd_1)
    sample_norm_2 <- rnorm(num_2, mean_2, sd_2) 
    sample_norm_add <- sample_norm_1 + sample_norm_2

    df_norm <- data.frame(tags = factor( c(rep(paste("normal 1, mean =", mean_1, "sd =", sd_1),num_1),
                                           rep(paste("normal 2, mean =", mean_2, "sd =", sd_2),num_2),
                                           rep("normal add",num_1))),
                         sample = c(sample_norm_1, sample_norm_2, sample_norm_add)
                         )
                 
    
    #plot
    library(ggplot2)
    library(plyr)
    library(grid)
    
    sample_norm_mean <- ddply(df_norm, "tags", summarise, sample.mean=mean(sample))
    
    pn1 <- ggplot(df_norm, aes(x=sample, colour=tags)) + 
                 geom_density() +
                 geom_vline(data=sample_norm_mean, aes(xintercept=sample.mean,  colour=tags),
                   linetype="dashed", size=1)
    pn2 <- ggplot(df_norm, aes(x=sample, colour=tags)) + 
                 stat_ecdf() +
                  geom_vline(data=sample_norm_mean, aes(xintercept=sample.mean,  colour=tags),
                   linetype="dashed", size=1)
    
    
    pn3 <- ggplot(df_norm, aes(x=sample, colour=tags)) + 
                 geom_density() +
                 stat_ecdf() +
                 geom_vline(data=sample_norm_mean, aes(xintercept=sample.mean,  colour=tags),
                   linetype="dashed", size=1)
                
    pn4 <- ggplot(df_norm, aes(x=sample, colour=tags)) + 
                 geom_density() +
                 stat_ecdf() +
                 facet_grid(. ~ tags) +
        geom_vline(data=sample_norm_mean, aes(xintercept=sample.mean,  colour=tags),
                   linetype="dashed", size=1)
    
    
    # Multiple plot function
    #
    # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
    # - cols:   Number of columns in layout
    # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
    #
    # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
    # then plot 1 will go in the upper left, 2 will go in the upper right, and
    # 3 will go all the way across the bottom.
    #
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        require(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
            # Make the panel
            # ncol: Number of columns of plots
            # nrow: Number of rows needed, calculated from # of cols
            layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                             ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
            print(plots[[1]])
            
        } else {
            # Set up the page
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
            
            # Make each plot, in the correct location
            for (i in 1:numPlots) {
                # Get the i,j matrix positions of the regions that contain this subplot
                matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                
                print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                layout.pos.col = matchidx$col))
            }
        }
    }
    
    
    assign("pn1", pn1, envir = .GlobalEnv)
    assign("pn2", pn2, envir = .GlobalEnv)
    assign("pn3", pn3, envir = .GlobalEnv)
    assign("pn4", pn4, envir = .GlobalEnv)
    
    multiplot(pn1, pn2 ,pn3, pn4 ,cols=2)
    
}







###############################
### log normal + log normal ###
###############################
#default num=1000, meanlog=0, sdlog=1

set.seed(1234)
lognorm_lognorm <- function(num_1=1000, meanlog_1=0, sdlog_1=1,
                            num_2=1000, meanlog_2=0, sdlog_2=1){
    
    sample_nlorm_1 <- rlnorm(num_1, meanlog_1, sdlog_1)
    sample_nlorm_2 <- rlnorm(num_2, meanlog_2, sdlog_2) 
    sample_nlorm_add <- sample_nlorm_1 + sample_nlorm_2
    
    df_nlorm <- data.frame(tags = factor( c(rep(paste("log normal 1, meanlog =", meanlog_1, "sdlog =", sdlog_1),num_1),
                                           rep(paste("log normal 2, meanlog =", meanlog_2, "sdlog =", sdlog_2),num_2),
                                           rep("lognormal add",num_1))),
                          sample = c(sample_nlorm_1, sample_nlorm_2, sample_nlorm_add)
    )
    
    
    #plot
    library(ggplot2)
    library(plyr)
    library(grid)
    
    sample_nlorm_mean <- ddply(df_nlorm, "tags", summarise, sample.mean=mean(sample))
    
    pl1 <- ggplot(df_nlorm, aes(x=sample, colour=tags)) + 
                  geom_density() +
                  geom_vline(data=sample_nlorm_mean, aes(xintercept=sample.mean,  colour=tags),
                   linetype="dashed", size=1)
    pl2 <- ggplot(df_nlorm, aes(x=sample, colour=tags)) + 
                  stat_ecdf() +
                  geom_vline(data=sample_nlorm_mean, aes(xintercept=sample.mean,  colour=tags),
                   linetype="dashed", size=1)
    
    pl3 <- ggplot(df_nlorm, aes(x=sample, colour=tags)) + 
                 geom_density() +
                 stat_ecdf() +
                 geom_vline(data=sample_nlorm_mean, aes(xintercept=sample.mean,  colour=tags),
                 linetype="dashed", size=1)
    
    pl4 <- ggplot(df_nlorm, aes(x=sample, colour=tags)) + 
                 geom_density() +
                 stat_ecdf() +
                 facet_grid(. ~ tags) +
                 geom_vline(data=sample_nlorm_mean, aes(xintercept=sample.mean,  colour=tags),
                   linetype="dashed", size=1)
    
    
    
    # Multiple plot function
    #
    # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
    # - cols:   Number of columns in layout
    # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
    #
    # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
    # then plot 1 will go in the upper left, 2 will go in the upper right, and
    # 3 will go all the way across the bottom.
    #
    multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        require(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
            # Make the panel
            # ncol: Number of columns of plots
            # nrow: Number of rows needed, calculated from # of cols
            layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                             ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
            print(plots[[1]])
            
        } else {
            # Set up the page
            grid.newpage()
            pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
            
            # Make each plot, in the correct location
            for (i in 1:numPlots) {
                # Get the i,j matrix positions of the regions that contain this subplot
                matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                
                print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                layout.pos.col = matchidx$col))
            }
        }
    }
    
    assign("pl1", pl1, envir = .GlobalEnv)
    assign("pl2", pl2, envir = .GlobalEnv)
    assign("pl3", pl3, envir = .GlobalEnv)
    assign("pl4", pl4, envir = .GlobalEnv)
    
    multiplot(pl1, pl2,pl3, pl4, cols=2)
    
}





