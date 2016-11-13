shinyUI(fluidPage(
  titlePanel("US Election Trends 2016"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("US Presidential Election Trends 2016"),
      
      selectInput("var1", 
                  label = "Election Candidate",
                  choices = c("Donald Trump", "Hillary Clinton",
                              "Bernie Sanders", "Ted Cruz"),
                  selected = "Donald Trump"),
      
      selectInput("var2", 
                  label = "Result Type",
                  choices = c("Weekly", "Current"),
                  selected = "Weekly")
    ),
    
    mainPanel(
      plotOutput("electionPlot")
    )
  )
))