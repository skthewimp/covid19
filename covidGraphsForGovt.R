setwd('~/Documents/work/data work/')
require(patchwork)
require(sf)
require(tidyverse)

jsonlite::fromJSON('https://raw.githubusercontent.com/covid19india/api/master/raw_data.json', simplifyVector = T)  %>% 
  .[[1]] %>%
  as_tibble() %>%
  mutate(Date=as.Date(dateannounced, '%d/%m/%Y')) ->
  covidindia

covidindia %>%
  count(Date) %>%
  filter(!is.na(Date)) %>%
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


covidindia %>%
  filter(!is.na(Date)) %>%
  rename(State=detectedstate) %>%
  count(Date, State) %>%
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
  ggplot(aes(x=Date, y=DoublingTime3, group=1, col=log(DoublingTime3))) + geom_line(lwd=1)  + scale_y_continuous("Doubling Time (number of days)") + theme_minimal() + labs(title="There is a massive divergence in how the disease is spreading in different states", subtitle = "Days to double, calculated using 3-day lagging compounded daily growth rate\nHigher the better", caption="© Karthik Shashidhar (@karthiks)" ) + scale_x_date('') + facet_wrap(~reorder(StateCase, LastGrowth), scales='free') + theme(legend.position = 'none', strip.text = element_text(face='bold', hjust=0), panel.grid.minor = element_blank(), axis.text = element_text(face='bold'), title = element_text(size=16, face='bold')) + scale_colour_gradient2(low='red', high='darkgreen', mid='yellow', midpoint = log(20)) ->
  stateWise


districtTitle <- "In fact, there is wide divergence within states as well\nWithin states, we see high divergence in doubling rates across districts"

covidindia %>%
  filter(!is.na(Date) & Date < Sys.Date()) %>%
  mutate(State=paste0(detecteddistrict, ' (', detectedstate, ')')) %>%
  count(Date, State) %>%
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
  filter(!is.na(DoublingTime3) & Date==max(Date) & DoublingTime3 < 50) %>%
  mutate(StateCase=paste0(State, ' (',Total, ')'))%>%
  ggplot(aes(x=reorder(StateCase, LastDouble), y=LastDouble, fill=log(Total), label=round(LastDouble, 1))) + geom_col() + geom_text(size=2.3, aes(y=ifelse(LastDouble < 6, LastDouble + 1, LastDouble - 1), col=ifelse(LastDouble < 6, 'black', 'white')), fontface='bold') + scale_y_continuous("Days to double", breaks=c()) + theme_minimal() + labs(title="Covid Cases By District In India: Days to Double", subtitle = "Calculated using 3-day lagging compounded daily growth rate\n Longer bar implies doing better", caption="© Karthik Shashidhar (@karthiks)\n Data source: covid19india.org" ) + xlab('') + coord_flip() + theme(panel.grid.minor = element_blank(), axis.text = element_text(face='bold'), legend.position = 'none') + scale_fill_gradient() + scale_colour_identity() ->
  districtsIndia



covidindia %>%
  filter(!is.na(Date) & Date < Sys.Date() & detectedstate %in% c('Uttar Pradesh',"Maharashtra", "Tamil Nadu", "Gujarat", "Rajasthan", "Madhya Pradesh")) %>%
  mutate(District=detecteddistrict, State=detectedstate) %>%
  count(Date, State, District) %>%
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
  filter(!is.na(DoublingTime3) & Date==max(Date)) %>%
  mutate(StateCase=paste0(District, ' (',Total, ')'))%>%
  ggplot(aes(x=reorder(StateCase, LastDouble), y=LastDouble, fill=log(Total), label=round(LastDouble, 1))) + geom_col() + geom_text(size=2.3, aes(y=ifelse(LastDouble < 6, LastDouble + 2, LastDouble - 2), col=ifelse(LastDouble < 6, 'black', 'white')), fontface='bold') + scale_y_continuous("Days to double", breaks=c()) + theme_minimal() + labs(caption="© Karthik Shashidhar (@karthiks)\n Data source: covid19india.org", title='Six major Covid-19 affected states') + xlab('') + coord_flip() + theme(panel.grid.minor = element_blank(), axis.text = element_text(face='bold'), legend.position = 'none') + scale_fill_gradient() + scale_colour_identity() + facet_wrap(~State, scales='free')  ->
  districtsMajorStates

covidindia %>%
  rename(State=detectedstate) %>%
  filter(!is.na(Date) & Date < Sys.Date()) %>%
  count(Date, State) %>%
  complete(Date, State, fill=list(n=0)) %>%
  filter(!is.na(Date)) %>%
  group_by(State) %>%
  arrange(Date) %>%
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
    Forecast2weeks=Today*exp(14*GrowthRate)  - Today,  # People infected until today would have bee cured in 2 weeks' time,
    Colour=ifelse(GrowthRate <= log(2)/30, 'yellow', ifelse(GrowthRate <= log(2)/7, 'orange', 'red'))
  ) %>%
  ungroup() %>%
  mutate(StateLabel=paste(State, paste(signif(round(Forecast2weeks),3), 'active cases on', format(Sys.Date()+14, '%d-%b')), sep='\n')) %>%
  filter(Cumul >= 5 & Today >= 50) ->  # ignore small states or states without much data
  stateCumul
stateCumul %>%
  ggplot(aes(x=Date, y=Cumul, col=Colour)) + geom_point() + geom_line() + scale_y_log10("Number of cases") + theme_minimal() + geom_text(aes(label=n, y=1.5*Cumul), fontface='bold', size=2) + facet_wrap(~reorder(StateLabel, -Forecast2weeks), scales='free') + labs(title="Covid Cases In India: Growth by state", subtitle="14 day projection based on maximum of 2-day, 3-day and 7-day growth rate in cases", caption="© Karthik Shashidhar (@karthiks)") + scale_x_date('')  + theme(strip.text = element_text(face='bold', hjust=0)) + scale_colour_identity() ->
  growthProjections


  
jsonlite::fromJSON('https://raw.githubusercontent.com/covid19india/api/master/state_test_data.json') %>%
  .[[1]] %>%
  as_tibble() %>%
  mutate(
    Date=as.Date(updatedon, '%d/%m/%Y'),
    Tested=as.numeric(totaltested)
  ) ->
  covidtests

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
  filter(n() ==2 & Tested > 1000) %>%
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
  ggplot(aes(x=Date, y=Tested, label=Tested)) + geom_point() + geom_line() + geom_text(aes(y=1.1*Tested), fontface='bold', size=2.5) + theme_minimal() + facet_wrap(~state, scales='free') + xlab('') + scale_y_continuous("Cumulative tested", breaks=c()) + labs(title=testingTitle, caption ="© Karthik Shashidhar (@karthiks)\n Data source: covid19india.org" )+ theme(title=element_text(face='bold', size=14), text=element_text(face='bold')) -> 
  testingGraph

covidtests %>%
  distinct(state, Date, Tested) %>%
  inner_join(
    covidindia %>%
      count(Date, state=detectedstate, name = 'Positive') %>%
      arrange(state, Date) %>%
      group_by(state) %>%
      mutate(Positive=cumsum(Positive)),
    by=c("Date", "state")
  ) %>%
  filter(complete.cases(.)) %>%
  group_by(state) %>%
  filter(n() > 5) %>%
  ggplot(aes(x=Tested, y=Positive, label=format(Date, '%d-%m'))) + geom_point() + geom_smooth(method='loess', formula='y~x') + facet_wrap(~state, scales='free') + theme_minimal()  + xlab("Cumulative people tested") + ylab("Cumulative people found positive") ->
  testingGraph2

covidtests %>%
  distinct(state, Date, Tested) %>%
  inner_join(
    covidindia %>%
      count(Date, state=detectedstate, name = 'Positive') %>%
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

karnatakaTitle <- "Within Karnataka as well, we see vastly different growth rates by district.\nCurrently, our major concern is a cluster of districts in North Karnataka which are showing rapid growth"
covidindia %>%
  filter(!is.na(Date) & Date < Sys.Date() & detectedstate=='Karnataka') %>%
  mutate(State=detecteddistrict) %>%
  count(Date, State) %>%
  complete(Date, State, fill=list(n=0)) %>%
  group_by(State) %>%
  arrange(Date) %>%
  mutate(Cumul=cumsum(n),Total=sum(n)) %>%
  filter(Cumul >= 3 & Total >= 3) %>%  # ignore small states or states without much data
  mutate(
    Growth3=log(Cumul/lag(Cumul,3)),
    DoublingTime3=log(2)/Growth3*3,
    LastGrowth=Growth3[Date==max(Date)],
    LastDouble=DoublingTime3[Date==max(Date)]
  ) %>%
  filter(!is.na(DoublingTime3) & Date==max(Date)) %>%
  mutate(StateCase=paste0(State, ' (',Total, ')'))%>%
  ggplot(aes(x=reorder(StateCase, LastDouble), y=LastDouble, fill=log(Total), label=round(LastDouble, 1))) + geom_col() + geom_text(size=2.3, aes(y=ifelse(LastDouble < 6, LastDouble + 2, LastDouble - 2), col=ifelse(LastDouble < 6, 'black', 'white')), fontface='bold') + scale_y_continuous("Days to double", breaks=c()) + theme_minimal() + labs(title="Covid Cases By District In Karnataka: Days to Double", subtitle = "Calculated using 3-day lagging compounded daily growth rate", caption="© Karthik Shashidhar (@karthiks)\n Data source: covid19india.org" ) + xlab('') + coord_flip() + theme(panel.grid.minor = element_blank(), axis.text = element_text(face='bold'), legend.position = 'none') + scale_fill_gradient() + scale_colour_identity() ->
  karnatakaBar

kar <- st_read('~/Documents/work/data work/maps/maps/Districts/Census_2011/2011_Dist.shp') %>%
  filter(ST_NM=='Karnataka')

kar %>%
  mutate(DISTRICT=as.character(DISTRICT)) %>%
  left_join(
    covidindia %>%
      rename(State=detectedstate, District=detecteddistrict) %>%
      filter(State=="Karnataka" & !is.na(Date)) %>%
      count(District, Date) %>%
      complete(District, Date, fill = list(n=0)) %>%
      arrange(District, Date) %>%
      group_by(District) %>%
      mutate(
        Total=cumsum(n),
        Latest=Total[Date==max(Date)],
        Back1week=Total[Date==(max(Date)-7)],
        Back3days=Total[Date==(max(Date)-3)],
        WeekGrowth=Latest/Back1week-1,
        Growth3=Latest/Back3days-1,
        colour=ifelse(Latest==Back1week, 'No new cases in last 1 week', ifelse((Back1week==0 & Latest > 0) | WeekGrowth > 1 | Growth3 > 0.7, 'Doubled in 1 week OR 70% growth in last 3 days', 'Low growth in last 1 week'))
      ) %>%
      distinct(District, colour) %>%
      left_join(
        tibble(
          District=c('Bagalkote', 'Ballari', 'Belagavi', 'Bengaluru', "Bengaluru Rural", "Kalaburagi", 'Mysuru', 'Tumakuru', 'Vijayapura'),
          DISTRICT=c('Bagalkot', 'Bellary', 'Belgaum', 'Bangalore', 'Bangalore Rural', 'Gulbarga', 'Mysore', 'Tumkur' , 'Bijapur')
        ),
        by='District'
      ) %>%
      mutate(DISTRICT=coalesce(DISTRICT, District)) %>%
      ungroup() %>%
      distinct(DISTRICT, colour),
    by='DISTRICT'
  ) %>%
  mutate(colour=coalesce(colour, 'Zero cases')) %>%
  ggplot() + geom_sf(aes(fill=colour), col='black') + geom_text(data=st_coordinates(st_centroid(kar)) %>% as_tibble() %>% mutate(DISTRICT=kar$DISTRICT), aes(x=X, y=Y, label=DISTRICT), fontface='bold', size=3)+  scale_fill_manual(values = c('red', 'orange', 'yellow', 'darkgreen'))+ theme_minimal() + theme(legend.position = 'bottom', legend.title = element_blank()) + labs(title="Prevalence and growth of covid-19 cases in Karnataka", subtitle = format(Sys.Date(), '%d %B, %Y'), caption="© Karthik Shashidhar (@karthiks)") + xlab('') + ylab('')+guides(fill=guide_legend(nrow=2,byrow=TRUE)) ->
  karnatakaMap

pdf('~/Documents/work/data work/covid_doc.pdf', 18, 32)
p1 / grid::textGrob(newTitle) /  stateWise / grid::textGrob(districtTitle) / (districtsIndia | districtsMajorStates) / grid::textGrob(karnatakaTitle) / (karnatakaBar | karnatakaMap)
dev.off()

pdf('~/Documents/work/data work/covid_doc2.pdf', 16, 9)
print(p1)
print(stateWise)
print(districtsIndia + districtsMajorStates+ plot_layout(nrow=1, widths=c(1,2)) + plot_annotation(title=districtTitle, theme = theme(plot.title = element_text(size = 16, face='bold')))) 
#print(testingGraph + (testingGraph2 + testingGraph3))
print(growthProjections)
print(testingGraph)
print(testingGraph2)
print(testingGraph3)

print(karnatakaMap + karnatakaBar + plot_annotation(title=karnatakaTitle, theme = theme(plot.title = element_text(size = 16, face='bold'))))
dev.off()


