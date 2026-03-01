setwd("~/Documents/work/data work")
require(tidyverse)
require(rvest)
url <- 'https://en.wikipedia.org/wiki/COVID-19_testing'
testing <- url %>%
  xml2::read_html() %>%
  html_table(fill=T)
testing <- testing[[2]]
testing %>% as_tibble() -> testing

testing %>% 
  transmute(
    Country=`Country or region`, 
    Tests=parse_number(Tests),
    PerMillion=parse_number(`Tests /millionpeople`), 
    Positive=parse_number(Positive), 
    PositiveRate=Positive/Tests
    ) %>%  
  filter(!str_detect(Country, ':')) %>% 
  ggplot(aes(x=PerMillion, y=PositiveRate, label=Country)) + geom_point() + geom_text() + scale_x_log10("Tests per million people") + theme_minimal() + geom_smooth(method='lm') + scale_y_continuous("Proportion of tests found positive", labels=scales::percent) + ggtitle("Rate of positive cases for covid-19 tests, against tests per million people")

testing %>% transmute(Country=`Country or region`, Tests=parse_number(Tests), PerMillion=parse_number(`Tests /millionpeople`), Positive=parse_number(Positive), PositiveRate=Positive/Tests) %>%  filter(!str_detect(Country, ':')) %>% filter(Country != 'Philippines') %>% ggplot(aes(x=PerMillion, y=PositiveRate, label=Country)) + geom_point() + geom_text() + scale_x_log10("Tests per million people") + theme_minimal() + geom_smooth(method='lm') + scale_y_continuous("Proportion of tests found positive", labels=scales::percent) + ggtitle("Rate of positive cases for covid-19 tests, against tests per million people")

testing %>% transmute(Country=`Country or region`, Tests=parse_number(Tests), PerMillion=parse_number(`Tests /millionpeople`), Positive=parse_number(Positive), PositiveRate=Positive/Tests) %>%  filter(!str_detect(Country, ':')) %>% filter(Country != 'Philippines') %>% arrange(-PerMillion)  %>% head(20) %>% summarise(Tests=sum(Tests), Positive=sum(Positive), PerMillion=mean(PerMillion)) %>% mutate(PositiveRate=Positive/Tests, India=1.3e9*PerMillion / 1e6 * PositiveRate)

testing %>% transmute(Country=`Country or region`, Tests=parse_number(Tests), PerMillion=parse_number(`Tests /millionpeople`), Positive=parse_number(Positive), PositiveRate=Positive/Tests) %>%  filter(!str_detect(Country, ':')) %>% filter(Country != 'Philippines') %>% arrange(-PerMillion)  %>% head(10) %>% summarise(Tests=sum(Tests), Positive=sum(Positive), PerMillion=mean(PerMillion)) %>% mutate(PositiveRate=Positive/Tests, India=1.3e9*PerMillion / 1e6 * PositiveRate)

testing %>% transmute(Country=`Country or region`, Tests=parse_number(Tests), PerMillion=parse_number(`Tests /millionpeople`), Positive=parse_number(Positive), PositiveRate=Positive/Tests) %>%  filter(!str_detect(Country, ':')) %>% filter(Country != 'Philippines') %>% arrange(-PerMillion)  %>% filter(Tests > 100000) %>% head(10) %>% summarise(Tests=sum(Tests), Positive=sum(Positive), PerMillion=mean(PerMillion)) %>% mutate(PositiveRate=Positive/Tests, India=1.3e9*PerMillion / 1e6 * PositiveRate)
