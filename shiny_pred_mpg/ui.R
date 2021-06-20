library(shiny)
library(plotly)
# Define application 
shinyUI(fluidPage(
    
    # Define application's title
    titlePanel("Predict the gazoline consumption (Miles per gallon) from car's weight"),
    
    # Define sidebar 
    sidebarLayout(
        sidebarPanel(
            h2(helpText("Prediction section")),
            helpText("Estimate the car's gazoline consumption only with weight variable. You can select some points on the graphic to use only these ones in the estimation models."),
            h3(helpText("Select car's weight:")),
            numericInput("w1", label = h4("Weight"), step = 0.01, value = 3),
            checkboxInput("showModel1", "Show/Hide Model 1 ( mpg ~ a1*w + b )", value = TRUE),
            checkboxInput("showModel2", "Show/Hide Model 2 (mpg ~ a1*w + a2*w² + b)", value = TRUE),
            h2(helpText("Exploratory graphic color parameter")),
            helpText("show others characteristics like horse power, or manual/automatic in color on the graphic."),
            h3(helpText("Select a characteristic:")),
            selectInput("ch1", label = h4("Characteristic"),
                        choices = list("automatic/manual" = "am", "horse power" = "hp","cylinder" = "cyl", "disp"="disp"))
        ),
    # Plot mpg estimation from weight + curves of the two models used
    mainPanel(
        plotOutput("gaz_plot", brush = brushOpts(
            id = "brush1"
        )),
        h4("The model1 predicted gazoline consumption value is:"),
        h3(textOutput("pred_mod1")),
        h4("The model2 predicted gazoline consumption value is:"),
        h3(textOutput("pred_mod2"))
    )
)))