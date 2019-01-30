library(tidyverse)
library(ggmap)
library(rgeos)
library(maptools)
library(ggplot2)
library(plotly)

library(shiny)
library(leaflet)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

#Read in data from Global Terror Database
terrorism <- read_csv('data/terrorism.csv')
saveRDS(terrorism, 'data/terrorism.RDS')
state_codes <- read_csv('data/State_abbrev.csv')

#Import map to use
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
mp <- ggplot() +   mapWorld
plot(mp)

#Drop dupblicates
terrorism <- terrorism %>% distinct(eventid, .keep_all = TRUE)
  

#Subset Dataframe to U.S. only and reorder dataframe in descending order by total deaths
terrorism_US <- terrorism %>% 
  filter(country_txt == 'United States') %>%
  arrange(desc(nkill)) 
  saveRDS(terrorism_US, 'data/terrorism_US.RDS')

#Bar plot to visualize top 10 terror attacks by deaths and state
terrorism_US %>%
  top_n(10, nkill) %>%
  ggplot(aes(x= provstate, y= nkill)) + geom_col() +
  xlab('State') +
  ylab('Total Deaths') +
  ggtitle('Death Counts for top 10 Deadliest Terror Attacks in U.S. by State') 

#Subset to hijacks
terrorism_hj <- terrorism %>%
  filter(attacktype1 == 4, nkill > 0) %>%
  arrange(desc(nkill))

#Bar plot to visualize hijack deaths for 10 biggest years
terrorism_hj %>%
  ggplot(aes(x= iyear, y = nkill)) + geom_col() + 
  xlab('Year') +
  ylab('Total Deaths') +
  ggtitle('Death Counts for Hijackings')
  

#Group by year and then add up all hijackings in year
# terrorism_hj_year <- terrorism_hj %>%
#   mutate(iyear= as.numeric(iyear))
#   group_by(iyear) %>%
#   summarise(iyear)
  
hj_freq <- data.frame(table(terrorism_hj$iyear))%>%
  rename(Year = Var1)

sum(hj_freq$Freq)

hj_freq <- hj_freq %>%
  mutate(Rel_freq = Freq/ 659)

#Bar plot to look at frequency of hijacks for 10 biggest years
hj_freq %>%
  top_n(10, Freq) %>%
  ggplot(aes(x= Year, y = Freq)) + geom_col() + 
  xlab('Year') +
  ylab('Total Hijackings') +
  ggtitle('Top 10 Years for Total Hijackings')
  
#Line plot to look at frequency of total hijackings
  hj_freq %>%
    ggplot(aes(x= Year, y = Freq, group = Year )) + geom_line(linetype= "solid", color="black", size=.5) + geom_point(size=3) +
    xlab('Year') +
    ylab('Total Hijackings') +
    ggtitle('Frequency of Hijackings from 1970 - 2017')

#Subset deadly terror attacks in U.S.
US_deaths <- terrorism_US %>%
  filter(nkill > 0) %>%
  arrange(desc(iyear))

#Subset US deaths dataframe to look at frequency of deadly attacks by year 
US_freq <- data.frame(table(US_deaths$iyear)) %>%
  rename(Year = Var1)


#Visualize with line plot
ggplot(US_freq, aes(x= Year, y = Freq, group = 1)) +
  geom_line(linetype= "solid", color="black", size=.5) + 
  geom_point(size=1) +
  xlab('Year') +
  ylab('# of Deadly Terror Attacks') +
  # scale_x_discrete(limits = c(1970, 2017)) 
  scale_x_discrete(breaks = seq(1970, 2017, by = 5))+
  ggtitle('Frequency of Deadly Terror Attacks in the U.S. from 1970 to 2017')


#subset terrorist attacks by type of attack
#Bomb
terrorism_bomb <- terrorism %>%
  filter(attacktype1 == 3, nkill > 0) %>%
  arrange(desc(nkill))

bomb_freq <- data.frame(table(terrorism_bomb$iyear)) %>%
  rename(Year = Var1)

#Assassination
terrorism_assassination <- terrorism %>%
  filter(attacktype1 == 1, nkill > 0) %>%
  arrange(desc(nkill))

assassination_freq <- data.frame(table(terrorism_assassination$iyear)) %>%
  rename(Year = Var1)

#Armed Assault
terrorism_armed_assault <- terrorism %>%
  filter(attacktype1 == 2, nkill > 0) %>%
  arrange(desc(nkill))

armed_assault_freq <- data.frame(table(terrorism_armed_assault$iyear)) %>%
  rename(Year = Var1)

#Hostage
terrorism_hostage <- terrorism %>%
  filter(attacktype1 == 5, nkill > 0) %>%
  arrange(desc(nkill))

hostage_freq <- data.frame(table(terrorism_hostage$iyear)) %>%
  rename(Year = Var1)

#Kidnapping
terrorism_kidnapping <- terrorism %>%
  filter(attacktype1 == 6, nkill > 0) %>%
  arrange(desc(nkill))

kidnapping_freq <- data.frame(table(terrorism_kidnapping$iyear)) %>%
  rename(Year = Var1)

#Facility Attack
terrorism_facility_attack <- terrorism %>%
  filter(attacktype1 == 7, nkill > 0) %>%
  arrange(desc(nkill))

facility_attack_freq <- data.frame(table(terrorism_facility_attack$iyear)) %>%
  rename(Year = Var1)

#Unarmed Assault
terrorism_unarmed_assault <- terrorism %>%
  filter(attacktype1 == 8, nkill > 0) %>%
  arrange(desc(nkill))

unarmed_assault_freq <- data.frame(table(terrorism_unarmed_assault$iyear)) %>%
  rename(Year = Var1)

#Unknown
terrorism_unknown <- terrorism %>%
  filter(attacktype1 == 9, nkill > 0) %>%
  arrange(desc(nkill))

unknown_freq <- data.frame(table(terrorism_unknown$iyear)) %>%
  rename(Year = Var1)

#Visualizing trends overtime with  different types of terror methods 
ggplot(bomb_freq, aes(x= Year, y = Freq, group=1)) +
  geom_line(size=1, linetype= "solid", color="blue") +
  geom_point(color="black", size= 1) +
  xlab('Year') +
  ylab('Total Bombings') +
  scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
  ggtitle('Frequency of Deadly Terror Bombings from 1970 - 2017')

ggplot(armed_assault_freq, aes(x= Year, y = Freq, group=1)) +
  geom_line(size=1, linetype= "solid", color="blue") +
  geom_point(color="black", size= 1) +
  xlab('Year') +
  ylab('Total Bombings') +
  scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
  ggtitle('Frequency of Deadly Terror Armed Assaults from 1970 - 2017')

ggplot(assassination_freq, aes(x= Year, y = Freq, group=1)) +
  geom_line(size=1, linetype= "solid", color="blue") +
  geom_point(color="black", size= 1) +
  xlab('Year') +
  ylab('Total Bombings') +
  scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
  ggtitle('Frequency of Deadly Terror Assassinations from 1970 - 2017')

ggplot(facility_attack_freq, aes(x= Year, y = Freq, group=1)) +
  geom_line(size=1, linetype= "solid", color="blue") +
  geom_point(color="black", size= 1) +
  xlab('Year') +
  ylab('Total Bombings') +
  scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
  ggtitle('Frequency of Deadly Terror Facility Attacks from 1970 - 2017')

ggplot(hj_freq, aes(x= Year, y = Freq, group=1)) +
  geom_line(size=1, linetype= "solid", color="blue") +
  geom_point(color="black", size= 1) +
  xlab('Year') +
  ylab('Total Bombings') +
  scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
  ggtitle('Frequency of Deadly Terror Hijackings from 1970 - 2017')

ggplot(hostage_freq, aes(x= Year, y = Freq, group=1)) +
  geom_line(size=1, linetype= "solid", color="blue") +
  geom_point(color="black", size= 1) +
  xlab('Year') +
  ylab('Total Bombings') +
  scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
  ggtitle('Frequency of Deadly Terror Hotage Plots from 1970 - 2017')

ggplot(kidnapping_freq, aes(x= Year, y = Freq, group=1)) +
  geom_line(size=1, linetype= "solid", color="blue") +
  geom_point(color="black", size= 1) +
  xlab('Year') +
  ylab('Total Bombings') +
  scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
  ggtitle('Frequency of Deadly Terror Kidnappings from 1970 - 2017')

ggplot(unarmed_assault_freq, aes(x= Year, y = Freq, group=1)) +
  geom_line(size=1, linetype= "solid", color="blue") +
  geom_point(color="black", size= 1) +
  xlab('Year') +
  ylab('Total Bombings') +
  scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
  ggtitle('Frequency of Deadly Terror Unarmed Assaults from 1970 - 2017')

ggplot(unknown_freq, aes(x= Year, y = Freq, group=1)) +
  geom_line(size=1, linetype= "solid", color="blue") +
  geom_point(color="black", size= 1) +
  xlab('Year') +
  ylab('Total Bombings') +
  scale_x_discrete(breaks = seq(1970, 2017, by = 5)) +
  ggtitle('Frequency of Deadly Terror Attacks with Unknown Method from 1970 - 2017')


world_deaths <- terrorism %>%
  filter(nkill > 0)

#Subset to look at total deadly attacks, cumulative deaths, and average death rate by region of the world.
terrorism_summary_byregion <- terrorism %>%
  filter(nkill > 0) %>%
  group_by(region_txt) %>%
  summarize(total_deadly_attacks = n(), total_deaths= sum(nkill), death_rate= sum(nkill/n())) 


terrorism_summary_byregion %>%
  ggplot(aes(x= region_txt, y= total_deadly_attacks), fill = region_txt) + geom_col(color="blue", fill="white") +
  xlab('Region') +
  ylab('Total Deadly Attacks') +
  coord_flip() +
  ggtitle('Frequency of Deadly Attacks by Region 1970 to 2017') 


terrorism_summary_byregion %>%
  ggplot(aes(x= region_txt, y= total_deaths)) + geom_col() +
  xlab('Region') +
  ylab('Total Deaths from Terror Attacks') +
  ggtitle('Cumulative Deaths from Terror Attacks by Region 1970 to 2017') 

terrorism_summary_byregion %>%
  ggplot(aes(x= region_txt, y= death_rate)) + geom_col() +
  xlab('Region') +
  ylab('Total Deaths from Terror Attacks') +
  ggtitle('Cumulative Deaths from Terror Attacks by Region 1970 to 2017') 


#unique list of countries and save as one column data frame for both data and plotly and use a match 
# plotly_codes <- read_csv("data/plotly_countries_and_codes.csv")
# Countries_plotly <- plotly_codes %>% select(COUNTRY) %>% unique()


#Merge state code names into US Terrorism Dataframe
match(terrorism_US$provstate, state_codes$State_name)
unique_states <- terrorism_US %>% select(provstate) %>% unique()
terrorism_US <- terrorism_US %>%
  rename(State_name = provstate)
terrorism_US <- merge(terrorism_US, state_codes, by=("State_name"))

terrorism_US <- terrorism_US %>% distinct(eventid, .keep_all = TRUE)


#Tally total deaths, injuries, and types of attacks in U.S.
US_summary <- terrorism_US %>%
  dplyr::group_by(State) %>%
  dplyr::summarise(nkill = sum(nkill, na.rm = TRUE), 
                   nwound = sum(nwound, na.rm = TRUE), 
                   total_hijackings = sum(attacktype1 == 4),
                   total_armedassaults = sum(attacktype1 == 2),
                   total_assassinations = sum(attacktype1 == 1),
                   total_bombs = sum(attacktype1 == 3),
                   total_hostages = sum(attacktype1 == 5),
                   total_kidnappings = sum(attacktype1 == 6), 
                   total_facilityattacks = sum(attacktype1 == 7),
                   total_unarmedassaults = sum(attacktype1 == 8), 
                   total_unknowns = sum(attacktype1 == 9)
                   )
#Log certain columns to make choropleth more normalized
US_summary <- US_summary %>%
  mutate(log_kills = log(nkill), 
         log_nwound = log(nwound), 
         log_bombs = log(total_bombs), 
         log_facilityattacks = log(total_facilityattacks)) 

US_summary$log_kills[!is.finite(US_summary$log_kills)] <- 0
US_summary$log_nwound[!is.finite(US_summary$log_nwound)] <- 0
US_summary$log_bombs[!is.finite(US_summary$log_bombs)] <- 0
US_summary$log_facilityattacks[!is.finite(US_summary$log_facilityattacks)] <- 0


# hijacking_summary <- terrorism_US %>%
#   dplyr::group_by(State) %>%
#   mutate(total_hijackings = length(which(attacktype1 == 4)))
# 
# hijack_test <- terrorism_US %>%
#   add_count(attacktype1) %>%
#   filter (attacktype1 == 4)


# US_attack_type <- terrorism_US %>%
#   dplyr::group_by(State, attacktype1) %>%
#   dplyr::summarise(Most_Common_Attack_Count=n()) %>%
#   dplyr:: ungroup() %>%
#   dplyr::group_by(State) %>%
#   dplyr::filter(Most_Common_Attack_Count == max(Most_Common_Attack_Count)) 
# 
# 
# US_attack_type <- mutate(US_attack_type, 
#       Attack_type = case_when(
#         attacktype1 == 2 ~ 'Armed Assault', 
#         attacktype1 == 3 ~ "Bomb", 
#         attacktype1 == 7 ~ "Facility Attack"
#       )
# )


US_summary$hover1 <- with(US_summary, paste(State, '<br>', "Total Number Killed", nkill))
US_summary$hover2 <- with(US_summary, paste(State, '<br>', "Total Number Wounded", nwound))
US_summary$hoverhijackings <- with(US_summary, paste(State, '<br>', "Total Number of Hijacks", total_hijackings))
US_summary$hoverarmedassaults <- with(US_summary, paste(State, '<br>', "Total Number of Armed Assaults", total_armedassaults))
US_summary$hoverassassinations <- with(US_summary, paste(State, '<br>', "Total Number of Assassinations", total_assassinations))
US_summary$hoverbombs <- with(US_summary, paste(State, '<br>', "Total Number of Bombings", total_bombs))
US_summary$hoverhostages <- with(US_summary, paste(State, '<br>', "Total Number of Hostages", total_hostages))
US_summary$hoverkidnappings <- with(US_summary, paste(State, '<br>', "Total Number of Kidnappings", total_kidnappings))
US_summary$hoverfacilityattacks <- with(US_summary, paste(State, '<br>', "Total Number of Facility Attacks", total_facilityattacks))
US_summary$hoverunarmedassaults <- with(US_summary, paste(State, '<br>', "Total Number of Unarmed Assaults", total_unarmedassaults))
US_summary$hoverunknowns <- with(US_summary, paste(State, '<br>', "Total Number of Attacks w/ Unknown Classification", total_unknowns))



g1 <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

kill_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~log_kills, text = ~hover1, locations = ~State,
    color = ~log_kills, colors = 'Purples'
  ) %>%
  colorbar(title = "Logged Color Scale") %>%
  layout(
    title = 'Terrorism Deaths from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

kill_map

injury_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~log_nwound, text = ~hover2, locations = ~State,
    color = ~log_nwound, colors = 'Greens'
  ) %>%
  colorbar(title = "Logged Color Scale") %>%
  layout(
    title = 'Total People Injured by Terrorism from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

injury_map

hijack_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total_hijackings, text = ~hoverhijackings, locations = ~State,
    color = ~total_hijackings, colors = 'Oranges'
  ) %>%
  colorbar(title = "Total Incidents") %>%
  layout(
    title = 'Total Number of Terrorist Attacks Classified as a Hijacking from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

hijack_map

armedassault_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total_armedassaults, text = ~hoverarmedassaults, locations = ~State,
    color = ~total_armedassaults, colors = 'Blues'
  ) %>%
  colorbar(title = "Total Incidents") %>%
  layout(
    title = 'Total Number of Terrorist Attacks Classified as an Armed Assault from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

armedassault_map


assassination_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total_assassinations, text = ~hoverassassinations, locations = ~State,
    color = ~total_assassinations, colors = 'Purples'
  ) %>%
  colorbar(title = "Total Incidents") %>%
  layout(
    title = 'Total Number of Terrorist Attacks Classified as an Assassination from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

assassination_map

bomb_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~log_bombs, text = ~hoverbombs, locations = ~State,
    color = ~log_bombs, colors = 'Greens'
  ) %>%
  colorbar(title = "Logged Color Scale") %>%
  layout(
    title = 'Total Number of Terrorist Attacks Classified as Bombing from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

bomb_map

hostage_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total_hostages, text = ~hoverhostages, locations = ~State,
    color = ~total_hostages, colors = 'Oranges'
  ) %>%
  colorbar(title = "Total Incidents") %>%
  layout(
    title = 'Total Terrorist Attacks Classified as Hostage Taking from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

hostage_map

kidnapping_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total_kidnappings, text = ~hoverkidnappings, locations = ~State,
    color = ~total_kidnappings, colors = 'Blues'
  ) %>%
  colorbar(title = "Total Incidents") %>%
  layout(
    title = 'Total Terrorist Attacks Classified as a Kidnapping from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

kidnapping_map

facilityattack_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~log_facilityattacks, text = ~hoverfacilityattacks, locations = ~State,
    color = ~log_facilityattacks, colors = 'Purples'
  ) %>%
  colorbar(title = "Logged Color Scale") %>%
  layout(
    title = 'Total Terrorist Attacks Classified as a Facility Attack from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

facilityattack_map

unarmedassault_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total_unarmedassaults, text = ~hoverunarmedassaults, locations = ~State,
    color = ~total_unarmedassaults, colors = 'Greens'
  ) %>%
  colorbar(title = "Total Incidents") %>%
  layout(
    title = 'Total Terrorist Attacks Classified as an Unarmed Assault from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

unarmedassault_map

unknown_map <- plot_geo(US_summary, locationmode = 'USA-states') %>%
  add_trace(
    z = ~total_unknowns, text = ~hoverunknowns, locations = ~State,
    color = ~total_unknowns, colors = 'Oranges'
  ) %>%
  colorbar(title = "Total Incidents") %>%
  layout(
    title = 'Total Number of Terrorist Attacks with Unknown Classification from 1970 to 2017 by State<br>(Hover for breakdown)',
    geo = g1 
  )

unknown_map

#Word map for most common types of targets
corpus <- Corpus(VectorSource(terrorism$targtype1_txt))
corpus <- tm_map(corpus, removePunctuation)
# corpus <- tm_map(corpus, function(x) removeWords(x, stopwords()))
tdm <-TermDocumentMatrix(corpus, control=list(wordLengths=c(1,Inf)))
freq <- slam::row_sums(tdm)
words <- names(freq)    

wordcloud(words, freq, min.freq=1)


#Most frequent attacker groups
top_10_groups <- terrorism %>%
  dplyr::filter(gname != "Unknown") %>%
  dplyr::group_by(gname) %>%
  dplyr::summarise(appearances = n()) %>%
  dplyr::top_n(10, appearances) %>%
  dplyr:: pull(gname)  




