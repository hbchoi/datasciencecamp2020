setwd('C:/Users/PRIME#93/Google 드라이브/한동대 문서_/4학년/데이터 외전 캠프')
load("C:/Users/PRIME#93/Google 드라이브/한동대 문서_/4학년/데이터 외전 캠프/day1.RData")


head(country_info)
data1
head(covid)
save(country_info, data1, covid, file = 'day1.RData')

#<<Data Handling>>
#<tidyr>
library(tidyr)

data1 <- gather(data1, education, value, midleschool:university)
data1
data1 <- separate(data1, col = birth, sep = '-', into = c('year', 'month', 'day') )

data1 <- spread(data1, education, value)
data1 <- unite(data1, year, month, day, sep = '/', col = date)
data1

#--------------Tidyr 예제 ------------------------------------
#코로나데이터 
#날짜 분리시키기 
separate(covid, col = Date, sep = '-', 
                   into = c('year','month', 'day'))
head(covid)
#type으로 합치기 
gather(covid, type, number, confirmed:recovered)
head(covid)


#type별로 행을 분리시키기 
spread(covid, type, number) # 정렬 순서가 달라질 수 있음 
head(covid)

#year, month, day변수를 Date로 합치기 
unite(covid, year, month, date, 
                col = Date, sep = '-')

#위도와 경도를 /를 기준으로 합쳐보시오 
unite(covid, long, lat, sep = '/', col = location)

#-------------------------------------------------------------


#<dplyr>

library(dplyr)

#코로나 dplyr 연습 

# 2020년 4월 14일 프랑스의 확진자 수는? 
covid %>% filter(Date == '2020-04-14', country == 'France') %>% select(Date, country, confirmed)

#미국의 사망자 수가 제일 높은 날은 언제? 
covid %>% filter(country == 'US') %>% arrange(desc(death)) %>% head

#나라별 전체 확진자 수는? 
covid %>% group_by(country) %>% summarise(sum(confirmed))

# 유럽 대륙의 코로나 코로나 완치자 행을 내림차순 정렬
#join() 
covid %>% inner_join(country_info,  by= 'country') %>% 
  group_by(country) %>% summarise(total = sum(confirmed))
covid %>% left_join(country_info) %>% head()
covid %>% right_join(country_info, by= 'country')%>% 
  group_by(country) %>% summarise(total = sum(confirmed))
covid %>% full_join(country_info, by= 'country')%>% 
  group_by(country) %>% summarise(total = sum(confirmed))
covid %>% inner_join(country_info, by= 'country')%>% 
  group_by(country) %>% summarise(total = sum(confirmed))

covid %>% inner_join(country_info,  by= 'country')%>% 
  filter(continent == 'Europe') %>% select(Date, country, recovered) %>% 
  arrange(desc(recovered))

# 나라별 현재 순수 확진자 수만을 계산하시오 (전체 확진자 수 - 사망자수 - 완치자 수 )
covid %>% group_by(country) %>% summarise(sum(confirmed) - sum(death) - sum(recovered))

# 시간당 확진자 수 증가량 gain_per_hour 추가 / 증가비율이 가장 높은 날, 국가? 
covid %>% mutate(covid19_tidy, gain_per_hour = confirmed/24) %>% 
  arrange(desc(gain_per_hour)) %>%head

#코로나 확진자의 회복률이 가장 높은 나라는 어디? 
covid %>% group_by(country) %>% summarise(total_confirmed =sum(confirmed), total_recovered = sum(recovered)) %>% 
  mutate(cure_rating = (total_recovered/total_confirmed)*100) %>% 
  arrange(desc(cure_rating)) %>% head

#우리나라와 일본의 3월달 확진자 수 
covid %>% separate(col = Date, sep = '-', into = c('year', 'month', 'date')) %>% 
  filter(month == '03', country == 'Korea, South'|country == 'Japan') %>% 
  group_by(country) %>% summarise(total = sum(confirmed))


#6월달의 일일 확진자 증가비율이 가장 낮은 하위 5개 국가는 어디인가? 
covid %>% separate(col = Date, sep = '-', into = c('year', 'month', 'date')) %>% 
  filter(month == '06') %>% group_by(country) %>% 
  summarise(increase_rate = sum(confirmed)/30) %>% 
  arrange(increase_rate) %>% head(5)




#------------------------------------------
#Data Visualization 

#<ggplot>
library(ggplot2)

str(covid)
covid$Date <- as.Date(covid$Date)


#1) scatter plot (geom_point) 대륙별(date - confirmed / confirmed - death)
#basic: data + aes + geom
#aes - color, shape, size, fill, alpha ... 

#ggplot()에 aes 추가 옵션 = geom_point() 안에 aes 옵션 추가가 같은 결과? 
#basic
ggplot(covid, aes(x = Date, y = confirmed)) + 
  geom_point()
ggplot(covid, aes(x = Date, y = confirmed)) + 
  geom_point(col = 'red')
covid %>% left_join(country_info, by = 'country') %>% 
  ggplot(aes(x = Date, y = confirmed)) + geom_point(aes(col = continent))

#대륙별로 알아보기 쉽게 col 
covid %>% left_join(country_info, by = 'country') %>% 
  ggplot(aes(x = Date, y = confirmed)) + 
  geom_point(aes(col = continent),alpha= 0.5, shape = 5)

#aes, geom 옵션 추가 

covid %>% left_join(country_info, by = 'country') %>% 
  ggplot(aes(x = Date, y = confirmed)) + 
  geom_point(aes(col = continent, size = death))
covid %>% left_join(country_info, by = 'country') %>% 
  ggplot( aes(x = Date, y = confirmed)) + 
  geom_point(aes(col = continent), alpha = 0.2)
covid %>% left_join(country_info, by = 'country') %>% 
  ggplot( aes(x = Date, y = confirmed)) + 
  geom_point(aes(col = continent,alpha = death))
covid %>% left_join(country_info, by = 'country') %>% 
  ggplot(aes(x = Date, y = confirmed)) + 
  geom_point(aes(col = continent),alpha= 0.7, shape = 5, size = 0.4)



# 다른 layer 추가 

#scale
covid %>% left_join(country_info, by = 'country') %>% 
  ggplot( aes(x = Date, y = confirmed)) + 
  geom_point(aes(col = continent)) + scale_y_log10()
library(RColorBrewer)
covid %>% left_join(country_info, by = 'country') %>% 
  ggplot(aes(x = Date, y = confirmed)) + 
  geom_point(aes(col = confirmed)) + 
  scale_color_gradient(low = 'blue', high = 'red') #col변수가 수치형이여야 

#facet
covid %>% left_join(country_info, by = 'country') %>% 
  ggplot(aes(x =Date, y = confirmed)) +  
  geom_point(aes(col = continent))+ facet_wrap(continent~.)
d<- covid %>% left_join(country_info, by = 'country') %>% 
  ggplot(aes(x =Date, y = confirmed)) +  
  geom_point(aes(col = continent))
d+ geom_point(aes(col = continent))+ facet_wrap(~country)
d + facet_wrap(~country, scales = 'free')

install.packages('ggthemes')
#coordinate
d+  coord_flip()

#theme
d+ theme_classic()
d+ ggtitle("Covid19 patients")+ xlab("날짜") + ylab('확진자 수') 




#2) barplot 
covid_continent<- covid %>% left_join(country_info) %>% 
  group_by(continent)%>% 
  summarise(confirmed = sum(confirmed))
covid_continent

#geom_bar (범주형 데이터)
# stat = 'identity' 절대값형의 막대형 차트를 그릴때 
#stat = 'bin'일때는 항목의 빈도값을 출력 (x나 y값만 필요)

ggplot(covid_continent, aes(x =continent, y = confirmed)) +
  geom_bar(stat = 'identity')
ggplot(covid_continent, aes(x =continent, y = confirmed)) +
  geom_bar(stat = 'identity', aes(fill = continent))
ggplot(covid_continent, aes(x =continent, y = confirmed)) +
  geom_bar(stat = 'identity', aes(color = continent), size = 3)

#coord
ggplot(covid_continent, aes(x =continent, y = confirmed)) +
  geom_bar(stat = 'identity') + coord_flip()
#파이차트 
ggplot(covid_continent, aes(x ="", y = confirmed)) +
  geom_bar(stat = 'identity',aes(fill = continent))
ggplot(covid_continent, aes(x ="", y = confirmed)) +
  geom_bar(stat = 'identity',aes(fill = continent)) +coord_polar('y')

#scale
ggplot(covid_continent, aes(x =continent, y = confirmed)) +
  geom_bar(stat = 'identity', aes(fill = continent)) + 
  scale_fill_brewer()
ggplot(covid_continent, aes(x =continent, y = confirmed)) +
  geom_bar(stat = 'identity', aes(fill = continent)) + 
  scale_fill_brewer(palette = 'Paired')
ggplot(covid_continent, aes(x =continent, y = confirmed)) +
  geom_bar(stat = 'identity', aes(fill = continent)) + 
  scale_fill_brewer(palette = 'Paired') + scale_y_continuous(trans = 'log10')

#theme 
install.packages('ggthemes')
library(ggthemes)
ggplot(covid_continent, aes(x =continent, y = confirmed)) +
  geom_bar(stat = 'identity', aes(fill = continent)) + 
  ggtitle("Covid19 patients")+ 
  xlab("날짜") + ylab('확진자 수') +theme_economist()




#3) line plot (geom_line) 시간 흐름에 따라 
ggplot(covid, aes(x = Date, y = confirmed, group = country)) + 
  geom_line()
ggplot(covid, aes(x = Date, y = confirmed, group = country)) + 
  geom_line(aes(col = country))

ggplot(covid, aes(x = Date, y = confirmed, group = country)) + 
  geom_line(aes(col = continent))

ggplot(covid, aes(x = Date, y = confirmed, group = country)) + 
  geom_line(linetype = 'dashed')
ggplot(covid, aes(x = Date, y = confirmed, group = country)) + 
  geom_line(aes(col = country)) + geom_point(aes(col = country))
ggplot(covid, aes(x = Date, y = confirmed, group = country, col = country)) + 
  geom_line() + geom_point()


#facet 
ggplot(covid, aes(x = Date, y = confirmed)) + 
  geom_line(aes(col = continent)) + facet_wrap(~country)

ggplot(covid, aes(x = Date, y = confirmed)) + 
  geom_line(aes(col = continent)) + facet_wrap(~country, scales = 'free')

#theme 
ggplot(covid, aes(x = Date, y = confirmed)) + 
  geom_line(aes(col = continent)) + facet_wrap(~country, scales = 'free') + 
  theme_classic() + lab(title = '', x = ' ', y = ' ')


levels(as.factor(covid$country))

library(shiny)
library(dplyr)
library(ggplot2)
covid <- read.csv('https://raw.githubusercontent.com/joypark88/datacamp/master/covid_data.csv', stringsAsFactors = F)
covid$Date <- as.Date(covid$Date)
#practice3 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Number of confimed cases"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel( 
      dateRangeInput("date_range", 
                     label = "Date Range: ", 
                     start = '2020-01-22', 
                     end = '2020-07-08',
                     format = 'yyyy-mm-dd'
      ),
      checkboxGroupInput("country_selection", 
                         label = 'Choose countries', 
                         c('Belgium', 'Brazil', 'Chile', 'Ecuador', 
                           'Egypt', 'France', 'Germany', 'Ghana', 'India', 
                           'Indonesia', 'Iran', 'Iraq', 'Ireland', 'Italy',
                           'Japan', 'Korea, South', 'New Zealand', 'South Africa', 
                           'US'))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot")
      
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  covid_data <-reactive({covid %>% filter( country %in% input$country_selection, 
                                           Date >=input$date_range[1], Date <=input$date_range[2])})
  
  
  output$Plot <- renderPlot({
    data <- covid_data()
    ggplot(data, aes(x = Date, y = confirmed, group = country)) + 
      geom_line(aes(col = country))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)