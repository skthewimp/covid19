require(patchwork)
require(sf)
require(tidyverse)

### The following lines are to load your twitter credentials and then create a token that will allow you to tweet automatically. For obvious reasons I'm not sharing my credentials here

load('twittercred.RData') # This is the file where I have my credentials. You can have a similar file
require(rtweet)

token <- create_token(app = '<your-app-name>', consumer_key =consumer, consumer_secret = consumerSecret,
                      access_token = access_token, access_secret = access_secret)
#### From here onwards you can use the code verbatim

covidindia <- read_csv('https://api.covid19india.org/csv/latest/raw_data.csv', col_types = cols()) %>%
  mutate(`Age Bracket`=as.numeric(`Age Bracket`))

rd3 <- read_csv('https://api.covid19india.org/csv/latest/raw_data3.csv', col_types = cols())

read_csv('https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv', col_types = cols()) %>%
  mutate(
    Date=as.Date(`Updated On`, '%d/%m/%Y'),
    Tested=as.numeric(`Total Tested`)
  ) %>%
  rename(state=State) ->
  covidtests

kar <- st_read('2011_Dist.shp') %>%
  filter(ST_NM=='Karnataka')

covidindia %>%
  mutate(Date=as.Date(`Date Announced`, '%d/%m/%Y')) %>%
  rename(State=`Detected State`, District=`Detected District`, City=`Detected City`) %>%
  count(Date, State, District, City, name = 'Cases') %>%
  bind_rows(
    rd3 %>%
      filter(`Current Status` != "Recovered") %>%
      mutate(Date=as.Date(`Date Announced`, '%d/%m/%Y')) %>%
      rename(State=`Detected State`, District=`Detected District`, City=`Detected City`) %>%
      group_by(Date, State, District, City) %>%
      summarise(Cases=sum(`Num Cases`))
  ) ->
  dailyCases

dailyCases %>%
  group_by(Date) %>%
  summarise(n=sum(Cases)) %>%
  arrange(Date) %>%
  mutate(Cumul=cumsum(n)) %>%
  filter(Cumul >= 30) %>%
  mutate(
    Growth3=log(Cumul/lag(Cumul,3)),
    DoublingTime3=log(2)/Growth3*3
  ) %>%
  filter(!is.na(DoublingTime3)  & Date < Sys.Date()) ->
  overall

newTitle <- paste("Overall, cases in India are now doubling every", overall %>%
                    filter(Date==max(Date)) %>%
                    pull(DoublingTime3) %>%
                    first() %>%
                    round(1), "days")

overall %>%
  ggplot(aes(x=Date, y=DoublingTime3)) + geom_point() + geom_line(lwd=1)  + scale_y_continuous("Doubling Time (number of days)", breaks=c()) + theme_minimal() + geom_text(aes(label=round(DoublingTime3,1)), nudge_y=0.2, fontface='bold', size=3) + labs(title=newTitle, subtitle = "Calculated using 3-day lagging compounded daily growth rate\nThe higher this number, the better for us", caption="© Karthik Shashidhar (@karthiks)") + scale_x_date('',date_breaks = '2 days', date_labels = '%d-%m') + theme(legend.position='none', panel.grid.minor = element_blank(), axis.text = element_text(face='bold'), title=element_text(face='bold', size=16))  ->
  p1

dailyCases %>%
  group_by(Date, State) %>%
  summarise(n=sum(Cases)) %>%
  ungroup() %>%
  complete(Date, State, fill=list(n=0)) %>%
  group_by(State) %>%
  arrange(Date) %>%
  mutate(Cumul=cumsum(n),Total=sum(n)) %>%
  filter(Cumul >= 5 & Total >= 50) %>%  # ignore small states or states without much data
  mutate(
    Growth3=log(Cumul/lag(Cumul,3)),
    DoublingTime3=log(2)/Growth3*3,
    LastGrowth=Growth3[Date==max(Date)],
    LastDouble=DoublingTime3[Date==max(Date)]
  ) %>%
  filter(!is.na(DoublingTime3) & Date >= as.Date('2020-03-26')) %>%
  mutate(StateCase=paste(State, paste(Total, 'total cases'), paste("Now doubling every", round(LastDouble, 1), "days"),  sep='\n')) %>%
  ggplot(aes(x=Date, y=DoublingTime3)) + geom_point() + geom_line()  + scale_y_continuous("Doubling Time (number of days)", breaks=c()) + theme_minimal() + geom_text(aes(label=round(DoublingTime3,1), y=1.1*DoublingTime3), fontface='bold', size=3) + labs(title="Covid Cases In India: Days to Double", subtitle = "Calculated using 3-day lagging compounded daily growth rate", caption="© Karthik Shashidhar (@karthiks)" ) + scale_x_date('') + facet_wrap(~reorder(StateCase, LastGrowth), scales='free') + theme(strip.text = element_text(face='bold', hjust=0), panel.grid.minor = element_blank(), axis.text = element_text(face='bold')) ->
  stateWise

districtTitle <- "In fact, there is wide divergence within states as well\nWithin states, we see high divergence in doubling rates across districts"

dailyCases %>%
  group_by(Date, State, District) %>%
  summarise(n=sum(Cases)) %>%
  ungroup() %>%
  mutate(State=paste0(District, ' (', State, ')')) %>%
  group_by(Date, State) %>%
  summarise(n=sum(n)) %>%
  ungroup() %>%
  complete(Date, State, fill=list(n=0)) %>%
  group_by(State) %>%
  arrange(Date) %>%
  mutate(Cumul=cumsum(n),Total=sum(n)) %>%
  filter(Cumul >= 5 & Total >= 150) %>%  # ignore small states or states without much data
  mutate(
    Growth3=log(Cumul/lag(Cumul,3)),
    DoublingTime3=log(2)/Growth3*3,
    LastGrowth=Growth3[Date==max(Date)],
    LastDouble=DoublingTime3[Date==max(Date)]
  ) %>%
  filter(!is.na(DoublingTime3) & Date==max(Date) & DoublingTime3 < 50) %>%
  mutate(StateCase=paste0(State, ' (',Total, ')'))%>%
  ggplot(aes(x=reorder(StateCase, LastDouble), y=LastDouble, fill=log(Total), label=round(LastDouble, 1))) + geom_col() + geom_text(size=2.3, aes(hjust=ifelse(LastDouble < max(LastDouble)/2, 0, 1), col=ifelse(LastDouble < max(LastDouble)/2, 'black', 'white')), fontface='bold') + scale_y_continuous("Days to double", breaks=c()) + theme_minimal() + labs(title="Covid Cases By District In India: Days to Double", subtitle = "Calculated using 3-day lagging compounded daily growth rate", caption="© Karthik Shashidhar (@karthiks)\n Data source: covid19india.org" ) + xlab('') + coord_flip() + theme(panel.grid.minor = element_blank(), axis.text = element_text(face='bold'), legend.position = 'none') + scale_fill_gradient() + scale_colour_identity() + geom_hline(yintercept = 14, col='red', lwd=0.5) ->
  districtsIndia

dailyCases %>%
  filter(!is.na(Date) & State %in% c('Uttar Pradesh',"Bihar", "Tamil Nadu", "Gujarat", "Rajasthan", "Madhya Pradesh")) %>%
  group_by(Date, State, District) %>%
  summarise(n=sum(Cases)) %>%
  group_by(State) %>%
  complete(Date, District, fill=list(n=0)) %>%
  group_by(State, District) %>%
  arrange(Date) %>%
  mutate(Cumul=cumsum(n),Total=sum(n)) %>%
  filter(Cumul >= 3 & Total >= 10) %>%  # ignore small states or states without much data
  mutate(
    Growth3=log(Cumul/lag(Cumul,3)),
    DoublingTime3=log(2)/Growth3*3,
    LastGrowth=Growth3[Date==max(Date)],
    LastDouble=DoublingTime3[Date==max(Date)]
  ) %>%
  filter(!is.na(DoublingTime3) & Date==max(Date) & DoublingTime3 <= 50) %>%
  mutate(StateCase=paste0(District, ' (',Total, ')'))%>%
  ggplot(aes(x=reorder(StateCase, LastDouble), y=LastDouble, fill=log(Total), label=round(LastDouble, 1))) + geom_col() + geom_text(size=2.3, aes(hjust=ifelse(LastDouble < max(LastDouble)/2, 0, 1), col=ifelse(LastDouble < max(LastDouble)/2, 'black', 'white')), fontface='bold') + scale_y_continuous("Days to double", breaks=c()) + theme_minimal() + labs(title="Covid Cases By District In India: Days to Double", subtitle = "Calculated using 3-day lagging compounded daily growth rate", caption="© Karthik Shashidhar (@karthiks)\n Data source: covid19india.org" ) + xlab('') + coord_flip() + theme(panel.grid.minor = element_blank(), axis.text = element_text(face='bold'), legend.position = 'none') + scale_fill_gradient() + scale_colour_identity() + facet_wrap(~State, scales='free') + geom_hline(yintercept = 14, col='red', lwd=0.5) ->
  districtsMajorStates

dailyCases %>%
  filter(!is.na(Date)) %>%
  group_by(Date, State) %>%
  summarise(n=sum(Cases)) %>%
  ungroup() %>%
  complete(Date, State, fill=list(n=0)) %>%
  group_by(State) %>%
  arrange(State, Date) %>%
  mutate(
    Cumul=cumsum(n),
    Today=sum(n),
    Lag7=Cumul[Date==(max(Date)-7)],
    Growth7=log(Today/Lag7)/7,
    Lag3=Cumul[Date==(max(Date)-3)],
    Growth3=log(Today/Lag3)/3,
    Lag2=Cumul[Date==(max(Date)-2)],
    Growth2=log(Today/Lag2)/2,
    GrowthRate=pmax(Growth7, Growth3, Growth2),
    Colour=ifelse(GrowthRate <= log(2)/30, 'yellow', ifelse(GrowthRate <= log(2)/7, 'orange', 'red'))
  ) %>%
  select(Date, State, Cumul, GrowthRate, Colour, Today) %>%
  inner_join(
    (.) %>%
      filter(Date==max(Date)) %>%
      select(Date, State, Today, GrowthRate) %>%
      crossing(Projection=0:14) %>%
      mutate(
        ProjectedDate=Date + Projection, 
        ProjectedCases=Today * exp(Projection * GrowthRate),
        Date=ProjectedDate-14
      ) %>%
      select(-GrowthRate, -Today, -Projection),
    by=c("State", "Date")
  ) %>%
  mutate(
    LiveCases=ProjectedCases-Cumul,
    StateLabel=paste(State, paste(signif(round(Today*exp(14*GrowthRate)-Today),3), 'active cases on', format(Sys.Date()+14, '%d-%b')), sep='\n')
  ) %>%
  filter(Cumul >= 5 & Today >= 50) %>%
  ggplot(aes(x=ProjectedDate, y=LiveCases, col=Colour)) + geom_point() + geom_line() + scale_y_continuous("Number of cases", labels=scales::label_number_si()) + theme_minimal() + geom_text(aes(label=round(LiveCases), y=1.1*LiveCases), fontface='bold', size=2) + facet_wrap(~reorder(StateLabel, -Today*exp(GrowthRate*14)), scales='free') + labs(title="Covid Cases In India: Growth by state", subtitle="14 day projection based on maximum of 2-day, 3-day and 7-day growth rate in cases\nAssuming a constant 14-day recovery period", caption="© Karthik Shashidhar (@karthiks)") + scale_x_date('')  + theme(strip.text = element_text(face='bold', hjust=0)) + scale_colour_identity() ->
  growthProjections


dailyCases %>% 
  mutate(Region=coalesce(City, District, State)) %>%
  arrange(desc(Date), desc(Cases)) %>%
  group_by(Date) %>%
  mutate(
    Prop=Cases/sum(Cases),
    Total=sum(Cases),
    Cumul=cumsum(Prop),
    Rank=rank(-Cases)
  ) %>% 
  ungroup() %>%
  filter(Total > 100 & Date >= max(Date)-9) %>%
  group_by(Region) %>%
  mutate(minRank=min(Rank)) %>%
  filter(minRank <= 5) %>%
  mutate(Label=paste0(Cases, ' (', scales::percent(Prop, 1), ')')) %>%
  group_by(Date) %>%
  ggplot(aes(x=reorder(Region, Prop), y=Cases, label=Label)) + geom_col() + geom_text(aes(hjust=ifelse(Cases >= max(Cases)/4, 1, 0), col=ifelse(Cases >= max(Cases)/4, 'white', 'black')), fontface='bold', size=3) + scale_colour_identity()  + facet_wrap(~reorder(format(Date, '%d-%b'), desc(Date)), scales='free') + coord_flip() + theme_minimal() + xlab('') + theme(text=element_text(face='bold'), title=element_text(face='bold')) + scale_y_continuous("Daily Cases", breaks=c()) + labs(title='Regions contributing to highest number of daily cases', caption="© Karthik Shashidhar (@karthiks)\n Data source: covid19india.org") ->
  bigRegions



covidtests %>%
  distinct(state, Date, Tested) %>%
  filter(!is.na(Tested)) %>%
  group_by(state) %>%
  arrange(state, Date) %>%
  mutate(
    TestedToday=Tested-lag(Tested, 1),
    DateDiff=as.numeric(Date-lag(Date, 1), units='days')
  ) %>%
  filter(Date==max(Date)) %>%
  arrange(-Tested) ->
  mostTests

covidtests %>%
  distinct(state, Date, Tested) %>%
  filter(!is.na(Tested)) %>%
  group_by(state) %>%
  arrange(state, Date) %>%
  mutate(
    TestedToday=Tested-lag(Tested, 1),
    DateDiff=as.numeric(Date-lag(Date, 1), units='days')
  ) %>%
  filter(n() > 5 & !is.na(DateDiff) & DateDiff==1) %>%
  filter(Date %in% c(max(Date), max(Date)-3)) %>%
  group_by(state) %>%
  filter(Tested > 1000) %>%
  filter(n() == 2) %>%
  summarise(Growth=log(Tested[Date==max(Date)]/Tested[Date==max(Date)-3])/3, LastDate=max(Date)) %>%
  ungroup() %>%
  filter(LastDate==max(LastDate)) %>%
  arrange(-Growth) ->
  mostGrowth



testingTitle <- paste(mostTests$state[1], "leads the pack in terms of testing with", mostTests$Tested[1], "tests.\nIn the last three days,", mostGrowth$state[1], "has shown the most impressive growth in testing, increasing by an average of", scales::percent(mostGrowth$Growth[1]), "per day")


covidtests %>%
  distinct(state, Date, Tested) %>%
  filter(!is.na(Tested)) %>%
  group_by(state) %>%
  arrange(state, Date) %>%
  mutate(
    TestedToday=Tested-lag(Tested, 1),
    DateDiff=as.numeric(Date-lag(Date, 1), units='days')
  ) %>%
  filter(n() > 5 & !is.na(DateDiff) & DateDiff==1) %>%
  ggplot(aes(x=Date, y=Tested, label=Tested)) + geom_point() + geom_line() + geom_text(aes(y=1.1*Tested), fontface='bold', size=2.5) + theme_minimal() + facet_wrap(~state, scales='free') + xlab('') + scale_y_continuous("Cumulative tested", labels=scales::label_number_si()) + labs(title=testingTitle, caption ="© Karthik Shashidhar (@karthiks)\n Data source: covid19india.org" )+ theme(title=element_text(face='bold', size=14), text=element_text(face='bold')) ->
  testingGraph


covidtests %>%
  distinct(state, Date, Tested) %>%
  arrange(state, Date) %>%
  filter(!is.na(Tested)) %>%
  group_by(state) %>%
  mutate(
    NewTests=Tested-lag(Tested, 1)
  ) %>%
  filter(!is.na(NewTests) & NewTests > 0) %>%
  mutate(
    DateDiff=as.numeric(Date-lag(Date,1))
  ) %>%
  filter(DateDiff==1) %>%
  filter(n() >= 5) %>%
  inner_join(
    dailyCases %>%
      filter(!is.na(Date)) %>%
      group_by(Date, State) %>%
      summarise(Positive=sum(Cases)) %>%
      ungroup() %>%
      rename(state=State) %>%
      arrange(state, Date) %>%
      group_by(state) %>%
      mutate(Positive=cumsum(Positive)),
    by=c("Date", "state")
  ) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  group_by(state) %>%
  arrange(state,Date) %>%
  mutate(
    TestGrowth=log(Tested/lag(Tested,3))/3, 
    CaseGrowth=log(Positive/lag(Positive,3))/3, 
    Slope=CaseGrowth/TestGrowth,
    LastBig=as.numeric(Sys.Date()-max(Date[!is.na(Slope) & Slope >= 0.5]))
  ) %>%
  distinct(state, LastBig) %>%
  ungroup() %>%
  mutate(
    LastBig=ifelse(LastBig==Inf,NA,LastBig),
    LastBig=coalesce(LastBig, max(LastBig, na.rm=T)),
    BlueNess=(LastBig-min(LastBig))/(max(LastBig)-min(LastBig)),
    Colour=rgb(1,0.4+BlueNess*0.6,0.4-BlueNess*0.4)
  )%>%
  distinct(state, Colour) ->
  stateColours


covidtests %>%
  distinct(state, Date, Tested) %>%
  inner_join(
    dailyCases %>%
      filter(!is.na(Date)) %>%
      group_by(Date, State) %>%
      summarise(Positive=sum(Cases)) %>%
      ungroup() %>%
      rename(state=State) %>%
      arrange(state, Date) %>%
      group_by(state) %>%
      mutate(Positive=cumsum(Positive)),
    by=c("Date", "state")
  ) %>%
  filter(complete.cases(.)) %>%
  group_by(state) %>%
  filter(n() > 5) ->
  stateTestCases

stateColours %>%
  filter(state %in% stateTestCases$state) ->
  stateColours

stateGraphs <- list()

for(i in 1:nrow(stateColours)) {
  stateTestCases %>%
    filter(state==stateColours$state[i]) %>%
    ggplot(aes(x=Tested, y=Positive, label=format(Date, '%d-%m'))) + geom_point() + geom_smooth(method='loess', formula='y~x') + theme_minimal()  + scale_x_continuous("Total tests", labels=scales::label_number_si()) + scale_y_continuous("Total Positive Cases") + ggtitle(stateColours$state[i]) + theme(panel.background = element_rect(fill=stateColours$Colour[i]), title=element_text(face='bold', size=16), text=element_text(face='bold', size=10)) ->
    stateGraphs[[i]]
}

stateGraphs %>% wrap_plots(nrow=3, guides = 'collect') + plot_annotation(title="Comparing testing numbers to number of positive cases by state", caption="© Karthik Shashidhar (@karthiks)\n Data source: covid19india.org", theme=theme(title=element_text(size=16, face='bold'))) + plot_layout() ->
  testingGraph2

stateTestCases %>%
  group_by(state) %>%
  mutate(Label=ifelse(Date==max(Date), state, '')) %>%
  inner_join(stateColours, by='state') %>%
  ggplot(aes(x=Tested, y=Positive, group=state, col=Colour, label=Label)) + geom_line(lwd=1) + geom_point(pch=2) + geom_text(nudge_x=10,fontface='bold', col='black', size=3, hjust=0)+ scale_colour_identity() + theme_minimal() + scale_x_continuous("Total Tests", labels=scales::label_number_si(0.1)) + scale_y_continuous("Total Positive Cases") + theme(text=element_text(face='bold'),title=element_text(face='bold'))  ->
  testingGraph4

stateTestCases %>%
  group_by(state) %>%
  mutate(
    Ratio=Tested/Positive,
    Label=ifelse(Date==max(Date), round(Ratio), '')
  ) %>%
  ggplot(aes(x=Date, y=Ratio, label=Label)) + geom_point() + geom_line() + geom_text(hjust=1, nudge_y=1, fontface='bold', size=2.5) + facet_wrap(~state, scales='free') + theme_minimal() + xlab('') + ylab("Tests per positive case")+ labs(title="Tests per positive case", subtitle='Higher the graph, better the state is at testing', caption="© Karthik Shashidhar (@karthiks)\n Data source: covid19india.org" ) + theme(title=element_text(face='bold'), text=element_text(face='bold')) ->
  testingGraph5


covidtests %>%
  distinct(state, Date, Tested) %>%
  inner_join(
    dailyCases %>%
      filter(!is.na(Date)) %>%
      group_by(Date, State) %>%
      summarise(Positive=sum(Cases)) %>%
      ungroup() %>%
      rename(state=State) %>%
      arrange(state, Date) %>%
      group_by(state) %>%
      mutate(Positive=cumsum(Positive)),
    by=c("Date", "state")
  ) %>%
  filter(complete.cases(.) & Date >= as.Date('2020-04-07')) %>%
  group_by(state) %>%
  filter(n() > 5) %>%
  ggplot(aes(x=Date, y=Tested, col='tested')) + geom_line() + geom_point() + geom_point(aes(y=Positive*20, col='Positive')) + geom_line(aes(y=Positive*20, col='Positive')) + scale_y_continuous("Cumulative Tests (Left Axis)", sec.axis = sec_axis(~./20, name='Cumulative Positive (Right Axis)'), labels=scales::label_number_si()) + facet_wrap(~state, scales='free') + scale_colour_manual(values=c('blue', 'red')) + theme_minimal() + theme(axis.line.y.left = element_line(colour='red'), axis.text.y.left = element_text(colour='red', face='bold'), axis.ticks.y.left = element_line(colour='red'), axis.line.y.right = element_line(colour='blue'), axis.text.y.right = element_text(colour='blue', face='bold'), axis.ticks.y.right = element_line(colour='blue'), axis.title.y.right = element_text(colour='blue', face='bold'), axis.title.y.left=element_text(colour='red', face='bold'), legend.position='none', strip.text = element_text(face='bold', size=12, hjust=0), axis.text.x = element_text(face='bold')) + xlab('') + ggtitle("Total tests and positive cases in major states") ->
  testingGraph3

karnatakaTitle <- "Within Karnataka as well, we see vastly different growth rates by district"






dailyCases %>%
  filter(!is.na(Date)) %>%
  group_by(Date, District, State) %>%
  summarise(n=sum(Cases)) %>%
  ungroup() %>%
  filter(State=="Karnataka") %>%
  ungroup() %>%
  complete(District, Date, fill = list(n=0)) %>%
  arrange(District, Date) %>%
  group_by(District) %>%
  mutate(
    Total=cumsum(n),
    Latest=Total[Date==max(Date)],
    Growth7=Total/lag(Total,7)-1,
    Growth3=Total/lag(Total,3)-1,
    colour=ifelse(Growth7==0, 'No new cases in last 1 week', ifelse((lag(Total,7)==0 & Total > 0) | Growth7 > 1 | Growth3 > 0.7, 'Doubled in 1 week OR 70% growth in last 3 days', 'Low growth in last 1 week')),
    colour2=ifelse(Growth7==0, 'Yellow', ifelse((lag(Total,7)==0 & Total > 0) | Growth7 > 1 | Growth3 > 0.7, 'Red', 'Orange'))
  ) %>%
  filter(Date >= (max(Date) -1)) ->
  karTodayYesterday

karTodayYesterday %>%
  select(District, colour2, Date) %>%
  spread(Date, colour2) %>%
  ungroup() %>%
  filter(complete.cases(.)) %>%
  mutate(Different=.[,2] != .[,3]) ->
  karDiff

names(karDiff)[2] <- 'Today'
names(karDiff)[3] <- 'Yesterday'

if(sum(karDiff$Different)==0)
  karMapTitle <- 'No change in district colour codes from yesterday' else
    karDiff %>%
  filter(Different) %>%
  mutate(Annotation=paste0(District, " moves from ", Today, ' to ', Yesterday)) %>%
  ungroup() %>%
  summarise(Commentary=paste(Annotation, collapse=', ')) ->
  karMapTitle

kar %>%
  mutate(DISTRICT=as.character(DISTRICT)) %>%
  left_join(
    tibble(
      District=c('Bagalkote', 'Ballari', 'Belagavi', 'Bengaluru', "Bengaluru Rural", "Kalaburagi", 'Mysuru', 'Tumakuru', 'Vijayapura'),
      DISTRICT=c('Bagalkot', 'Bellary', 'Belgaum', 'Bangalore', 'Bangalore Rural', 'Gulbarga', 'Mysore', 'Tumkur' , 'Bijapur')
    ),
    by='DISTRICT'
  )  %>%
  mutate(District=coalesce(District, DISTRICT)) %>%
  left_join(
    karTodayYesterday %>%
      filter(Date==max(Date)) %>%
      distinct(District, colour),
    by='District'
  ) %>%
  mutate(colour=coalesce(colour, 'Zero cases')) %>%
  ggplot() + geom_sf(aes(fill=colour), col='black') + 
  geom_text(
    data= kar %>%
      st_centroid() %>%
      st_coordinates() %>%
      as_tibble() %>% 
      mutate(DISTRICT=as.character(kar$DISTRICT)) %>%
      left_join(
        tibble(
          District=c('Bagalkote', 'Ballari', 'Belagavi', 'Bengaluru', "Bengaluru Rural", "Kalaburagi", 'Mysuru', 'Tumakuru', 'Vijayapura'),
          DISTRICT=c('Bagalkot', 'Bellary', 'Belgaum', 'Bangalore', 'Bangalore Rural', 'Gulbarga', 'Mysore', 'Tumkur' , 'Bijapur')
        ),
        by='DISTRICT'
      )  %>%
      mutate(District=coalesce(District, DISTRICT)), 
    aes(x=X, y=Y, label=District), fontface='bold', size=4) -> p


p + scale_fill_manual(values = c('red', 'orange', 'yellow', 'darkgreen'))  + theme_minimal() + theme(legend.position = 'bottom', legend.title = element_blank()) + labs(title="Prevalence and growth of covid-19 cases in Karnataka", subtitle = format(Sys.Date(), '%d %B, %Y'), caption="© Karthik Shashidhar (@karthiks)") + xlab('') + ylab('')+guides(fill=guide_legend(nrow=2,byrow=TRUE)) ->
  karnatakaMap

mainMessage <- paste("This will go out as an automated update henceforth.", newTitle, sep='\n')

tmp <- tempfile(fileext = ".png")
ggsave(tmp, p1, height = 10, width = 16, units = 'in')

post_tweet(mainMessage, media=tmp, token=token)

my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

dailyCases %>%
  filter(!is.na(Date)) %>%
  group_by(Date, State) %>%
  summarise(n=sum(Cases)) %>%
  ungroup() %>%
  ungroup() %>%
  complete(Date, State, fill=list(n=0)) %>%
  group_by(State) %>%
  arrange(Date) %>%
  mutate(Cumul=cumsum(n),Total=sum(n)) %>%
  filter(Cumul >= 5 & Total >= 100) %>%  # ignore small states or states without much data
  mutate(
    Growth3=log(Cumul/lag(Cumul,3)),
    DoublingTime3=log(2)/Growth3*3,
    LastGrowth=Growth3[Date==max(Date)],
    LastDouble=DoublingTime3[Date==max(Date)]
  ) %>%
  filter(!is.na(DoublingTime3) & Date >= as.Date('2020-03-26') & Date < Sys.Date()) %>%
  mutate(StateCase=paste(State, paste(Total, 'total cases'), paste("Now doubling every", round(LastDouble, 1), "days"),  sep='\n')) %>%
  group_by(State) %>%
  filter(Date==max(Date)) %>%
  arrange(DoublingTime3) ->
  stateDouble

stateMessage <- paste("There is still a massive divergence in the rate of doubling by state.",  stateDouble$State[1], "is the biggest concern as of today, with cases doubling every", round(stateDouble$LastDouble[1],1), "days")

ggsave(tmp, stateWise, height = 12, width = 20, units = 'in')


post_tweet(stateMessage, media=tmp, token=token, in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]


ggsave(tmp, districtsIndia + districtsMajorStates+ plot_layout(nrow=1, widths=c(1,2)) + plot_annotation(title=districtTitle, theme = theme(plot.title = element_text(size = 16, face='bold'))), height = 12, width = 20, units = 'in')

post_tweet(districtTitle, media=tmp, token=token, in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

growthMessage <- "These are projections (NOT predictions) of the number of active cases in each state 2 weeks from now. Doesn't include cases expected to have been cured by then"

ggsave(tmp, growthProjections, height = 12, width = 20, units = 'in')

post_tweet(paste(growthMessage, '\nThis graph has changed from yesterday'), media=tmp, token=token, in_reply_to_status_id = reply_id)


my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

ggsave(tmp, bigRegions, height = 12, width = 20, units = 'in')
post_tweet("Some regions are contributing to a lion's share of daily cases in India", media=tmp, token=token, in_reply_to_status_id = reply_id)


my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

ggsave(tmp, testingGraph, height = 12, width = 20, units = 'in')

post_tweet(paste('Now coming to testing.', testingTitle), media=tmp, token=token, in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]


ggsave(tmp, testingGraph2, height = 12, width = 24, units = 'in')

post_tweet("Comparing testing numbers to positive numbers. If this curve is flattening out, that state is testing enough. I've shaded the plots in a yellow-red range. Yellow >> red", media=tmp, token=token, in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]


ggsave(tmp, testingGraph4, height = 12, width = 20, units = 'in')

post_tweet("The same graph with all states on the same axis, so we can directly compare their testing trajectories", media=tmp, token=token, in_reply_to_status_id = reply_id)


my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]


ggsave(tmp, testingGraph5, height = 12, width = 20, units = 'in')

post_tweet("In light of my recent blogpost on tests per case (https://t.co/joeN8Mutyr), here is the latest graph showing that", media=tmp, token=token, in_reply_to_status_id = reply_id)

my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

ggsave(tmp, karnatakaMap, height = 20, width = 16, units = 'in')
post_tweet(paste('Coming to Karnataka,',karMapTitle), media=tmp, token=token, in_reply_to_status_id = reply_id)


my_timeline <- get_timeline(rtweet:::home_user())
reply_id <- my_timeline$status_id[1]

ggsave(tmp, testingGraph3, height = 12, width = 20, units = 'in')
post_tweet("Here is a double Y-axis plot comparing testing and case rates by state. Not sure what conclusions you can draw fromt his. \n\nThe End", media=tmp, token=token, in_reply_to_status_id = reply_id)


