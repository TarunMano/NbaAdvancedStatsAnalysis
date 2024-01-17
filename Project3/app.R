library(shiny)
library(colourpicker)
library(ggplot2)
library(dplyr)

nbaStats = read.csv("nbaStats.csv")

ui <- fluidPage(
  
  titlePanel("NBA Advanced Stats Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      p("Analysis of the NBA's advanced stats, to determine the effect of truth that the advanced stats truly have."),
      hr(),
      helpText("Choose color value for graph"),
      colourInput("col", "Color:", "skyblue",showColour = "background"),
      helpText("Choose color value for Usage variable gradient."),
      colourInput("col2", "Usage Color Gradient:", "red",showColour = "background"),
      helpText("Choose X variable for graph"),
      radioButtons("xChoice", "Choose X:",choices = list( "Offensive Rating" = "OFFRTG", "Defensive Rating" = "DEFRTG","Win Percentage" = "WINPCT", "Team" = "TEAM", "Player Impact Estimate" = "PIE"), selected = "OFFRTG"),
      helpText("Choose Y variable for graph"),
      radioButtons("yChoice", "Choose Y:",choices = list( "Offensive Rating" = "OFFRTG" , "Defensive Rating" = "DEFRTG", "Win Percentage" = "WINPCT" ,"Team Win Percentage" = "TEAMWIN", "Player Impact Estimate" = "PIE"), selected = "WINPCT"),
      helpText("Color graph in terms of player usage."),
      checkboxInput("usage", "In terms of usage.", value = FALSE),
      helpText("Filter minimum amount of games a player has played; Must be a minimum of 40 games or maximum of 83 games."),
      sliderInput("games","Games Played", value = min(nbaStats$GP), min = min(nbaStats$GP), max = max(nbaStats$GP)),
      hr(),
      div("References and Other Info:"),
      div("Image: Jesse D. Garrabrant / NBAE via Getty Images"),
      br(),
      div("Dataset:", a("https://www.nba.com/stats/players/advanced?Season=2022-23"))
    
    
  ),
  
    mainPanel(
      plotOutput("distPlot"),
      hr(),
      fluidRow( 
        column(5, verbatimTextOutput("cor")
               ),
      img(src='https://cdn.theathletic.com/app/uploads/2019/11/02034806/GettyImages-1179394907-1024x683.jpg',width = 575, height = 427, align = "left"),
        
        
      )
      
    )
  )
) 
  



server <- function(input,output){
  
  
 
   output$distPlot = renderPlot({
     filterStats = filter(nbaStats,GP >= input$games)
     if(input$xChoice == 'TEAM'){
         ggplot(data = filterStats, aes_string(x = input$xChoice, y = input$yChoice )) + geom_boxplot( fill = input$col) + theme_classic() + scale_color_gradient(low = input$col2, high = input$col) + labs(title = paste("Team grouped by", input$yChoice))
       
      
       
     }
     else if(input$usage == TRUE & input$xChoice != 'TEAM'){
       ggplot(data = filterStats, aes_string(x = input$xChoice, y = input$yChoice)) + geom_point( aes(col = USG.)) + theme_classic() + scale_color_gradient(low = input$col2, high = input$col) + labs(title = paste("Correlation Between Usage, ",input$xChoice, ", and", input$yChoice))

     }
     else{
       ggplot(data = filterStats, aes_string(x = input$xChoice, y = input$yChoice)) + geom_point(col = input$col ) + theme_classic() + labs(title = paste("Correlation Between",input$xChoice, "and", input$yChoice))
     }
      
  })
   
   
   output$cor = renderPrint({
     filterStats = filter(nbaStats, GP >= input$games)
      if(input$xChoice == "TEAM"){
        y = input$yChoice
        print("Aggregate Mean:")
        aggregate(formula(paste0(y,"~TEAM")), data = filterStats, FUN = function(x) round(mean(x),2) )
        
      }
      else{
       
       paste("Correlation:",round(cor(select(filterStats, input$xChoice), select(filterStats, input$yChoice)),2), sep = " ")
       
      }
     
   })
  
  
}

shinyApp(ui = ui, server = server)