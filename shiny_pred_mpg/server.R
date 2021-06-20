library(shiny)
library(plotly)
shinyServer(function(input, output) {
#few change to mtcars dataset
    #create weight² variable 
    mtcars$wt2<-mtcars$wt**2
    
    #transform the am variable in a factor one
    mtcars2 <- mtcars
    mtcars2[mtcars$am == 1,9]<-"manual"
    mtcars2[mtcars$am == 0,9]<-"auto"
    mtcars2$am<-as.factor(mtcars2$am)
    
    #transform the disp continuous variable into a factor one with 10 classes
    disp_seq <- round(seq(min(mtcars$disp)-0.01, max(mtcars$disp), length.out = 10),digits=2);o=1;
    for(i in  mtcars$disp){d<-which(order(c(i,disp_seq))==1);mtcars2$disp[o]<-paste(">",as.character(disp_seq[d-1]));o=o+1}
    mtcars2$disp <- as.factor(mtcars2$disp)
    
    #transform the hp continuous variable into a factor one with 10 classes
    hp_seq <- round(seq(min(mtcars$hp)-0.01, max(mtcars$hp), length.out = 10),digits=2);o=1;
    for(i in  mtcars$hp){d<-which(order(c(i,hp_seq))==1);mtcars2$hp[o]<-paste(">",as.character(hp_seq[d-1]));o=o+1}
    mtcars2$hp <- as.factor(mtcars2$hp)
    
    #fit the two models from selected points
    model1 <- reactive({
        brushed_data <- brushedPoints(mtcars, input$brush1,
                                      xvar = "wt", yvar = "mpg")
        if(nrow(brushed_data) < 2){
            return(lm(mpg ~ wt,data = mtcars))
        }
        lm(mpg ~ wt, data = brushed_data)
        
    })

           
    
    
    model2 <- reactive({
        brushed_data <- brushedPoints(mtcars, input$brush1,
                                      xvar = "wt", yvar = "mpg")
        if(nrow(brushed_data) < 2){
            return(lm(mpg ~ wt + wt2,data = mtcars))
        }
        brushed_data$wt2 <- brushed_data$wt**2
       lm(mpg ~ wt + wt2, data = brushed_data)
    })
    
    
    #use both model to build prediction form the input weigth
    model1pred <- reactive({
        wt_input <- input$w1
        predict(model1(), newdata = data.frame(wt = wt_input))
    })
    
    model2pred <- reactive({
        wt_input <- input$w1
        predict(model2(), newdata = 
                    data.frame(wt = wt_input,
                               wt2 = wt_input**2))
    })
    
    #out prediction variable
    output$pred_mod1 <- renderText({
        model1pred()
    })
    
    output$pred_mod2 <- renderText({
        model2pred()
    })
    
    #plot part
     output$gaz_plot <- renderPlot({
         wt_input <- input$w1
         mtcars2<-mtcars2[order(mtcars[,input$ch1]),]
         plot(mtcars2$wt, mtcars2$mpg, xlab = "Weigth",
              ylab = "Miles Per Gallon", bty = "n", pch = 16,
              xlim = c(0, 6), ylim = c(10, 35),col=mtcars2[,input$ch1])
         if(input$showModel1){
             abline(model1(), col = "red", lwd = 2)
         }
         if(input$showModel2){
             model2lines <- predict(model2(), newdata = data.frame(
                 wt = seq(1.5, 6, by=0.05), wt2 = seq(1.5, 6, by=0.05)**2 ))
             lines(seq(1.5, 6, by=0.05), model2lines, col = "blue", lwd = 2)
         }
    
         legend(5, 30, c("Model 1 Prediction", "Model 2 Prediction"), pch = 15,
                col = c("red", "blue"), bty = "n", cex = 1.2)
    
         points(wt_input, model1pred(), col = "red", pch = 15, cex = 2)
         points(wt_input, model2pred(), col = "blue", pch = 15, cex = 2)
    
         legend('topright', as.character(unique(mtcars2[,input$ch1])),
                col=unique(mtcars2[,input$ch1]) , title = input$ch1 ,pch=16)
         
     
    })
 
        
    
})