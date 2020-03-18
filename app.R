#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(igraph)
library(tidyverse)
library(patchwork)

simulateCovid <- function(
  Population = 10000, 
  averageDegree = 10,
  numInitInfected = 3, 
  daysOfSimulation = 100, 
  probGoingOut = 0.5, 
  transmissionProb = 0.1, 
  daysToHospital = 5,
  propAsymptomatic = 0,
  propSelfQuarantine = 0
) {
  ws <- watts.strogatz.game(1, Population, averageDegree %/% 2, 0.8)
  init_infected <- sample(1:Population, numInitInfected)
  edgs <- as_edgelist(ws) %>% 
    as_tibble() %>%
    arrange(V1, V2) %>%
    bind_rows(
      (.) %>%
        rename(V3=V2, V2=V1) %>%
        rename(V1=V3)
    )
  
  infected <- tibble(People=init_infected, Time=0)
  hospitalised <- tibble() 
  
  # A vector that holds who is infected
  infectVec <- rep(F, Population)
  infectVec[init_infected] <- T
  
  asymptomatic <- sample(1:Population, round(Population*propAsymptomatic))
  selfQuarantine <- sample(1:Population, round(Population*propSelfQuarantine))
  
  for(d in 1:daysOfSimulation) {
    edgs %>%
      filter(!V2 %in% selfQuarantine) %>%
      sample_frac(probGoingOut) %>%
      filter(!V2 %in% infected$People) %>%  # once someone's been infected they are immune
      filter(V1 %in% which(infectVec)) %>%
      sample_frac(transmissionProb) %>%
      distinct(V2) %>%
      pull(V2) -> 
      newInfected
    infectVec[newInfected] <- T
    tibble(
      People=newInfected, 
      Time = d
    ) %>%
      bind_rows(infected) ->
      infected
    
    infected %>%
      filter(!People %in% asymptomatic) %>%
      filter(Time <= (d - daysToHospital)) %>%
      bind_rows(hospitalised) %>%
      distinct() ->
      hospitalised
    
    if(nrow(hospitalised) > 0)
      infectVec[hospitalised$People] <- F  # these people have been hospitalised so are out of the system
  }
  return(infected)
}

simulateCovidMult <- function(
  Population = 10000, 
  averageDegree = 10,
  numInitInfected = 3, 
  daysOfSimulation = 100, 
  probGoingOut = 0.5, 
  transmissionProb = 0.1, 
  daysToHospital = 5,
  propAsymptomatic = 0,
  propSelfQuarantine = 0
) {
  sims <- crossing(
    Population=Population, 
    AverageDegree=averageDegree,
    NumInitInfected=numInitInfected,
    daysOfSimulation=daysOfSimulation,
    ProbGoingOut=probGoingOut,
    TransmissionProb=transmissionProb,
    DaysToHospital=daysToHospital,
    PropAsymptomatic=propAsymptomatic,
    PropSelfQuarantine=propSelfQuarantine
  )
  
  uniqueVars <- c()
  variableVars <- c('Index') 
  
  if(length(Population) > 1) 
    variableVars <- c(variableVars, "Population") else
      uniqueVars <- c(uniqueVars, 'Population')
  
  if(length(averageDegree) > 1) 
    variableVars <- c(variableVars, "AverageDegree") else
      uniqueVars <- c(uniqueVars, 'AverageDegree')
  
  if(length(numInitInfected) > 1) 
    variableVars <- c(variableVars, "NumInitInfected") else
      uniqueVars <- c(uniqueVars, 'NumInitInfected')
  
  if(length(probGoingOut) > 1) 
    variableVars <- c(variableVars, "ProbGoingOut") else
      uniqueVars <- c(uniqueVars, 'ProbGoingOut')
  
  if(length(transmissionProb) > 1) 
    variableVars <- c(variableVars, "TransmissionProb") else
      uniqueVars <- c(uniqueVars, 'TransmissionProb')
  
  if(length(daysToHospital) > 1) 
    variableVars <- c(variableVars, "DaysToHospital") else
      uniqueVars <- c(uniqueVars, 'DaysToHospital')
 
  if(length(propAsymptomatic) > 1) 
    variableVars <- c(variableVars, "PropAsymptomatic") else
      uniqueVars <- c(uniqueVars, 'PropAsymptomatic')
  
  if(length(propSelfQuarantine) > 1) 
    variableVars <- c(variableVars, "PropSelfQuarantine") else
      uniqueVars <- c(uniqueVars, 'PropSelfQuarantine')
  
  
  sims %>%
    mutate(Index=1:n()) ->
    sims
  
  shortForms <- tibble(
    Variable=c('Population', 'AverageDegree', 'NumInitInfected', 'ProbGoingOut', 'TransmissionProb', 'DaysToHospital', "PropAsymptomatic", "PropSelfQuarantine"),
    Short=c('N', 'd', 'n0', 'p', 'q', 'Days', 'asymp', 'selfQ')
  )
  
  if(length(variableVars) > 1) {
    sims[,variableVars] %>%
      gather(Variable, Value, -Index) %>%
      inner_join(shortForms, by='Variable') %>%
      arrange(Index, Short) %>%
      group_by(Index) %>%
      summarise(Name=paste(paste(Short, Value, sep=': '), collapse=', ')) ->
      simNames
    
    sims %>%
      inner_join(simNames, by='Index') ->
      sims
  } else
    sims$Name <- ''
  
  sims[,uniqueVars] %>%
    distinct() %>%
    gather(Variable, Value) %>%
    inner_join(shortForms, by='Variable') %>%
    arrange(Short) %>%
    mutate(Name=paste(Short, Value, sep=': ')) %>%
    pull(Name) %>%
    paste(collapse=', ') ->
    Title
  
  simResults <- tibble()
  for(i in 1:nrow(sims)) {
    simulateCovid(
      Population=sims$Population[i],
      averageDegree = sims$AverageDegree[i],
      numInitInfected = sims$NumInitInfected[i],
      daysOfSimulation = sims$daysOfSimulation[i],
      probGoingOut = sims$ProbGoingOut[i],
      transmissionProb = sims$TransmissionProb[i],
      daysToHospital = sims$DaysToHospital[i],
      propAsymptomatic = sims$PropAsymptomatic[i],
      propSelfQuarantine = sims$PropSelfQuarantine[i]
    ) %>%
      crossing(sims[i,]) %>%
      bind_rows(simResults) -> 
      simResults
  }
  
  return(
    wrap_elements(
      (
        simResults %>%
          count(Time, Name) %>%
          arrange(Name, Time) %>%
          group_by(Name) %>%
          mutate(Infected=cumsum(n)) %>%
          ggplot(aes(x=Time, y=Infected/Population, col=Name)) + geom_point() + geom_line() + theme_minimal() + ggtitle("Cumulative Proportion Infected") + scale_y_continuous('', labels=scales::percent, lim=c(0,1)) + scale_color_brewer(palette="Set1") + theme(legend.title = element_blank(), legend.position = 'top')
      ) +
        (
          simResults %>%
            count(Time, Name) %>%
            ggplot(aes(x=Time, y=n, col=Name)) + geom_point() + geom_line() + theme_minimal() + ggtitle("New Infections Per Day") + xlab('')  + scale_color_brewer(palette="Set1")  + theme(legend.title = element_blank(), legend.position = 'top')
        )
    ) + ggtitle(Title)
  )
}


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Covid-19 Simulator, by Karthik Shashidhar"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput('pop', "Population", '10000'),
      textInput('avgDeg', "Average Connections Per Person", '10, 20'),
      textInput('initInfect', 'Number of people initially infected', '3'),
      textInput('pgo', "Probability of not social distancing", '1'),
      textInput('t', "Probability of transmission between meeting people", '0.1'),
      textInput('dys', 'Number of days before infected goes to hospital', '5'),
      textInput('asymp', "Proportion of infected people who are asymptomatic", '0'),
      textInput('selfq', 'Proportion of people who self-quarantine', '0'),
      
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    Population <- as.numeric(trimws(unlist(strsplit(input$pop, ','))))
    averageDegree <- as.numeric(trimws(unlist(strsplit(input$avgDeg, ','))))
    numInitInfected <- as.numeric(trimws(unlist(strsplit(input$initInfect, ','))))
    probGoingOut <- as.numeric(trimws(unlist(strsplit(input$pgo, ','))))
    transmissionProb <- as.numeric(trimws(unlist(strsplit(input$t, ','))))
    daysToHospital <- as.numeric(trimws(unlist(strsplit(input$dys, ','))))
    propAsymptomatic <- as.numeric(trimws(unlist(strsplit(input$asymp, ','))))
    propSelfQuarantine <- as.numeric(trimws(unlist(strsplit(input$selfq, ','))))
    p <- simulateCovidMult(
      Population = Population,
      averageDegree = averageDegree,
      numInitInfected = numInitInfected,
      daysOfSimulation = 100,
      probGoingOut = probGoingOut,
      transmissionProb = transmissionProb,
      daysToHospital = daysToHospital,
      propAsymptomatic = propAsymptomatic,
      propSelfQuarantine = propSelfQuarantine
    )
    p
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

