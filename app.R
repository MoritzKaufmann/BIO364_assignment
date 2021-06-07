#This script is part of the asignment for BIO 364: The physics of life from the university of Zürich
#The script was written by Moritz Kaufmann 07.06.21

#load the needed packages
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(Rfast)
library(shiny)

#set a seed for reproducible results
set.seed(1234)

#
#create the function for the logistic map
#the function was written by Nicole Radziwill and found on
#https://qualityandinnovation.com/wp-content/uploads/2019/09/logistic-growth.html
logistic.map <- function(r, x, N, M) {
  ## r: bifurcation parameter
  ## x: initial value, something greater than 0 and less than 1
  ## N: number of iterations (total)
  ## M: number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i] * (1 - z[i])
  }
  ## Return the last M iterations 
  z[c((N-M):N)]
}


#create a function to draw a cobweb diagram
#the function was written by Nicole Radziwill and found on
#https://qualityandinnovation.com/wp-content/uploads/2019/09/logistic-growth.html
logistic.cobweb <- function(r, N=100) {
  # code adapted from http://bayesianbiologist.com/tag/cobweb-plot/
  x<- seq(0,1,length=100)
  x_next <- lapply(2:N, function(i) r*x[i]*(1-x[i]))
  orbit.df <- as_tibble(cbind(x[2:N], unlist(x_next)))
  
  p <- orbit.df %>% ggplot() + geom_line(aes(x=V1, y=V2), col='red', size=2) + 
    xlab(expression(x[t])) + ylab(expression(x[t+1])) + 
    theme(axis.title.x=element_text(colour='black', size=18)) +
    theme(axis.title.y=element_text(colour='black', size=18)) +
    theme(legend.position="none") 
  
  start=runif(1,0,1)
  
  # start at your random spot on the x-axis and start with a vertical line:
  vert=FALSE
  seg_df <- data.frame(x=start, xend=start, y=0, yend=r*start*(1-start))
  for(i in 1:(2*N)) {
    if(vert) {
      seg_df <- rbind(seg_df, data.frame(x=start, xend=start, y=start, yend=r*start*(1-start)))
      vert=FALSE
    } else {
      seg_df <- rbind(seg_df, data.frame(x=start, xend=r*start*(1-start), y=r*start*(1-start), yend=r*start*(1-start)))
      vert=TRUE
      start=r*start*(1-start)
    }
  }
  p + geom_segment(data=seg_df, aes(x=x, xend=xend, y=y, yend=yend))+theme_bw()
}

#create the layout for the shiny app
# Add the title to the Shiny app
ui = shinyUI(fluidPage(
  # Application title
  column(3, offset = 4,
         titlePanel("Visualization of the logistic map")
  ),
  br(),
  
  #add describtion of the application
  fluidRow(
    column(8, offset = 2,
           h3('This app shows the bifurcation diagram, the Lyapanuv exponent, the logistic map and the cobweb plot
                  of the function F(x) = Rx(1-x). The growth factor r can be adjusted via the sliders. This app was created 
                  by Moritz Kaufmann for the assignemt of the course Physisc of Life from UZH.'))
  ),
  br(),
  
  #Add the layout to show the bifurcation diagram
  h3('Bifurcation diagram of the logistic map'),
  fluidRow(
    column(10, offset = 2,
           img(src="bifurcation.png", height="700" ) )),
  br(),
  
  #Add the laxout for the slider and the Lyapunuv exponent
  h3('Lyapunuv Exponent', position = 'center'),
  br(),
  fluidRow(
    column(2, sliderInput("r",
                          label="Growth rate r:",
                          min = 0,
                          max = 4,
                          value=c(0,4),
                          step = 0.1),
    ),
    column(10,plotOutput("distPlot"))
  ),
  br(),
  
  #Add the layout and the slider for the Coweb plot
  h3('Coweb Plot'),
  br(),
  fluidRow(
    column(2,sliderInput("coweb",
                         label="Growth rate r:",
                         min = 0,
                         max = 4,
                         value=c(2),
                         step = 0.1
    )),
    column(10,plotOutput('coweb'))
  ),
  br(),
  
  #Add the layout and the slider for the logistic map
  h3('Logistic Map'),
  br(),
  fluidRow(
    column(2,sliderInput("conv",
                         label="Growth rate r:",
                         min = 0,
                         max = 4,
                         value=c(2),
                         step = 0.1
    )),
    column(10, plotOutput('conv'))
  )
))

#creat the server function for the shiny app
server = shinyServer(function(input, output) {
  
  #add a seed for constant results
  set.seed(1234)
  
  output$distPlot <- renderPlot({
    
    #create the plot for the lyapanuv exponent
    #the function was written by Nicole Radziwill and found on
    #https://qualityandinnovation.com/wp-content/uploads/2019/09/logistic-growth.html
    x <- seq(min(input$r),max(input$r),0.01)
    
    #define the starting points as 0
    XI <- lya <- 0
    
    # Create 401 different logistic maps for r=0.00 to 4.00 and get one Lyapunov exponent (lya) from each
    for (i in 1:length(x)) {
      xi <- logistic.map(x[i],.01,500,500)
      XI <- rbind(XI,xi)
      lya[i] <- sum(log(abs(x[i]-(2*x[i]*XI[i,]))))/length(x) 
    }
    
    #create a dataframe with the Lyapunov Exponents and the corresponding growth rate
    df_lya = as.data.frame(cbind(lya,x))
    
    #create the plot
    ggplot() + geom_line(data=df_lya, aes(x=x,y=lya), size=1) + 
      geom_hline(yintercept=0, col="red") +
      scale_y_continuous(limits = c(Rfast::nth(df_lya$lya,2, descending = F),
                                    max(df_lya$lya))) + 
      ylab(expression(paste('Lyapunov exponent  ', lambda)))+
      xlab(expression(paste('Growth rate ', italic('r'))))+
      theme_bw()
    
  })
  
  #add the coweb plot to the shiny app
  output$coweb = renderPlot({
    logistic.cobweb(input$coweb)
    
  })
  
  #add the logistic map to the shiny app
  output$conv= renderPlot({
    iter <- logistic.map(input$conv,.01,20,20) 
    ggplot(data.frame(index=1:length(iter), logistic.map=iter), aes(x=index,y=logistic.map)) + geom_line()+theme_bw()
    
  })
})

shinyApp(ui, server)
