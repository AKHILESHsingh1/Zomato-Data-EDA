
library(tidyverse)
library(ggplot2)
library(ggridges)
library(plotly)
library(tidytext)


zomato <- read.csv('zomato.csv')




zomato %>%
  str()



WorldData <- map_data('world')


WorldData %>% filter(region != "Antarctica") -> WorldData
WorldData <- fortify(WorldData)

zomato_world <- zomato %>% select(Latitude,Longitude,Price.range) %>% mutate(Price.range=as.factor(Price.range))


p <- ggplot()
p <- p + geom_map(data=WorldData, map=WorldData,
                  aes(x=long, y=lat, group=group, map_id=region),
                  fill="White", colour="#7f7f7f", size=0.5)


p <- p + geom_point(data=zomato_world,aes(x=Longitude,y=Latitude,color=Price.range))

p+theme(panel.background = element_blank(),axis.title = element_blank(),
        axis.text=element_blank(),legend.position = 'bottom',legend.background = element_blank(),
        plot.title = element_text(hjust=0.5,face='bold',color='#2d2d2d'),
        legend.key = element_rect(fill='white',color = 'transparent'))+labs(title='Positions of Profiled Restaurants',
                                                                            color='Price Range')

zomato %>%
  filter(Country.Code==1) %>%
  select(Restaurant.ID) %>%
  unique() %>%
  nrow()



zomato %>% filter(Country.Code==1) %>%
  select(Restaurant.ID,City) %>%
  unique() %>%
  group_by(City) %>%
  summarise(n=n()) %>%
  ggplot(aes(x=reorder(City,n),y=n))+geom_bar(stat = 'identity',fill='#cb202d')+
  coord_flip()+
  theme(panel.background = element_blank(),
        strip.background = element_blank(),
        axis.title = element_text(color = '#2d2d2d'),
        strip.text.x = element_text(color='#2d2d2d',face='bold',size=10),
        plot.title = element_text(hjust=0.5,face='bold',size=15))+
  labs(x='City',y='Number of Restaurants',title="Number of Restaurants by City")


zomato %>%
  filter(Country.Code==1) %>%
  select(Restaurant.ID,Has.Online.delivery,City) %>%
  unique() %>%
  group_by(City,Has.Online.delivery) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  rename(`Online Delivery Service`=Has.Online.delivery) %>%
  ggplot(aes(x=reorder(City,n),y=n,fill=`Online Delivery Service`))+
  geom_bar(stat='identity',position = 'dodge',width = 0.5)+
  labs(x='City',y='Number of Restaurants')+coord_flip()+
  theme(plot.title = element_text(hjust=0.5,face='bold',color='#2d2d2d'),panel.background = element_blank(),
        strip.text = element_text(face='bold',color='#2d2d2d'),axis.text.x = element_text(face='bold',color='#2d2d2d'),
        axis.text = element_text(face='bold',color='#2d2d2d'))

library(Rcpp)

zomato %>%
  filter(Country.Code==1) %>%
  select(Restaurant.ID,Cuisines,Average.Cost.for.two) %>%
  unique() %>%
  mutate(Cuisines=as.character(Cuisines)) %>%
  unnest_tokens(ngram,Cuisines,token='ngrams',n=1) %>%
  group_by(ngram) %>%
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  top_n(10) %>%
  ggplot(aes(x=reorder(ngram,n),y=n))+geom_bar(stat='identity',fill='#cb202d')+
  theme(plot.title = element_text(hjust=0.5,face='bold',color='#2d2d2d'),panel.background = element_blank(),
        strip.text = element_text(face='bold',color='#2d2d2d'),axis.text.x = element_text(face='bold',color='#2d2d2d'),
        axis.text = element_text(face='bold',color='#2d2d2d'))+
  coord_flip()+labs(x='Cuisine',y='Number of Mentions',title='Popular Cuisines by Mentions')


#Conclustion
#A majority of restaurants are located in India
#A majority of Indian restaurants are located in New Delhi
#A majority of Indian restaurants do not have online delivery
