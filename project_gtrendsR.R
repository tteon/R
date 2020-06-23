library(tidyverse)
library(lubridate)
library(gtrendsR)
library(prophet)
library(maps)

# web version
?gtrends
trends <- gtrends(keyword=c('JoeBiden','Trump'), geo='US',time="2020-01-01 2020-06-18")

names(trends)

trends$interest_by_region

trends$interest_by_region %>%
  filter(keyword=='Trump') %>%
  arrange(desc(hits)) %>%
  head(10)

trends$interest_by_region %>%
  filter(keyword=='JoeBiden') %>%
  arrange(desc(hits)) %>%
  head(10)

state <- map_data("state")
state

region_df_JoeBiden <- trends$interest_by_region %>%
  mutate(region = tolower(location)) %>%
  filter(region %in% state$region,
         keyword == 'JoeBiden') %>%
  select(region, hits) %>%
  arrange(desc(hits))
region_df_JoeBiden
# Biden version
plot_JoeBiden <- ggplot() +
  geom_map(data = state,
           map = state,
           aes(x = long, y = lat, map_id = region),
           fill="#ffffff", color="#ffffff", size=0.15) +
  geom_map(data = region_df_korea,
           map = state,
           aes(fill = hits, map_id = region),
           color='#ffffff', size=0.15) +
  scale_fill_continuous(low = 'grey', high = 'red') +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())
plot_JoeBiden

# Trump version
region_df_Trump <- trends$interest_by_region %>%
  mutate(region = tolower(location)) %>%
  filter(region %in% state$region,
         keyword == 'Trump') %>%
  select(region, hits) %>%
  arrange(desc(hits))
region_df_Trump

plot_Trump <- ggplot() +
  geom_map(data = state,
           map = state,
           aes(x = long, y = lat, map_id = region),
           fill="#ffffff", color="#ffffff", size=0.15) +
  geom_map(data = region_df_korea,
           map = state,
           aes(fill = hits, map_id = region),
           color='#ffffff', size=0.15) +
  scale_fill_continuous(low = 'grey', high = 'red') +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())
plot_Trump

plot_JoeBiden
plot_Trump


# coronavirus

region_df_corona <- trends$interest_by_region %>%
  mutate(region = tolower(location)) %>%
  filter(region %in% state$region,
         keyword == 'coronavirus') %>%
  select(region, hits) %>%
  arrange(desc(hits))
region_df_corona

plot_corona <- ggplot() +
  geom_map(data = state,
           map = state,
           aes(x = long, y = lat, map_id = region),
           fill="#ffffff", color="#ffffff", size=0.15) +
  geom_map(data = region_df_corona,
           map = state,
           aes(fill = hits, map_id = region),
           color='#ffffff', size=0.15) +
  scale_fill_continuous(low = 'grey', high = 'red') +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())
plot_corona

write.csv(k, "C:/Users/user/Documents/data/k.csv")

k <- trends$interest_over_time
region_df_korea <- trends$interest_by_region %>%
  mutate(region = tolower(location)) %>%
  filter(region %in% state$region,
         keyword == 'korea') %>%
  select(region, hits) %>%
  arrange(desc(hits))

region_df_korea

# time aspect
?lubridate

Trump_timeseries <- trends$interest_over_time %>%
  filter(keyword=='Trump') %>%
  select(date,hits)%>%
  arrange(date)
Trump_timeseries

  
trends_timeseries <- as_tibble(trends$interest_over_time) %>%
  mutate(date = ymd(date)) %>%
  select(date, hits, keyword) %>%
  arrange(desc(hits))
  
trends_timeseries

trends_timeseries %>%
  ggplot() +
  geom_line(aes(date,hits,color=keyword),size=1)




# news version






# youtube version

























