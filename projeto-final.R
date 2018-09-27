#loading library

library(readr)
library(dplyr)
library(ggplot2)

#loading data

us_elections <- read_csv("CountyDataMOD4.csv")
us_elections$FIPS <- sprintf("%05d",us_elections$FIPS)

#cleaning data

us50 <- us_elections %>%
  filter(State != "DC")

#replacing decimmal comma with dot and converting variable to numeric

us50$BlackPerc <- gsub("\\,", "\\.", us50$BlackPercentage)
us50$BlackPerc <- as.numeric(us50$BlackPerc)
us50$AsianPerc <- gsub("\\,", "\\.", us50$AsianPercentage)
us50$AsianPerc <- as.numeric(us50$AsianPerc)
us50$HispanicPerc <- gsub("\\,", "\\.", us50$HispanicPercentage)
us50$HispanicPerc <- as.numeric(us50$HispanicPerc)

us50$PopDen <- gsub("\\,", "\\.", us50$PopulationDensity)
us50$PopDen <- as.numeric(us50$PopDen)

#showing data in percentage

us50 <- mutate(us50, black100=BlackPerc*100)
us50 <- mutate(us50, PercAsianPop=AsianPerc*100)
us50 <- mutate(us50, PercHispanicPop=HispanicPerc*100)

#creating labels for legend

us50label <- us50 %>%
  mutate(label=case_when(DemocratOrRepublican == "0" ~ "Trump won",
                               DemocratOrRepublican == "1" ~ "Hillary won"))

us50label <- us50label %>%
  filter(label != "NA")

# renaming cols

us50label2 <- us50label %>%
  rename(PercAfroAmericanPop = black100,
         PercTrumpVotes = PerRepROUND,
         PopDensity = PopDen,
         Result = label)

#creating chart #1 for Afro-American population

n <- ggplot(data = us50label2) +
  geom_point(mapping=aes(x=PercAfroAmericanPop, y=PercTrumpVotes, size=VotesTotal2016,
                         color=Result,
                         text=sprintf(CountyState),
                         alpha=.5)) + 
  expand_limits(x = 0, y = 0) +
  facet_wrap(~State, ncol=5) +
  scale_color_manual(values=c("#0099CC", "#FF3333"))+
  theme_minimal() +
  guides(size = FALSE) +
  guides(alpha = FALSE) +
  labs(x = "Percentage of Afro-Americans per county",
       y = "Percentage of votes for Trump per county",
       title = "Did minorities voted for Trump?",
       subtitle = "In the 2016 elections",
       color = "",
       size = "Votes Total")

n

#going interactive

ggplotly(n, width = 1200, height = 1200)


#chart for hispanics

h <- ggplot(data = us50label2) +
  geom_point(mapping=aes(x=PercHispanicPop, y=PercTrumpVotes, size=VotesTotal2016,
                         color=Result,
                         text=sprintf(CountyState),
                         alpha=.5)) + 
  expand_limits(x = 0, y = 0) +
  facet_wrap(~State, ncol=5) +
  scale_color_manual(values=c("#0099CC", "#FF3333"))+
  theme_minimal() +
  guides(size = FALSE) +
  guides(alpha = FALSE) +
  labs(x = "Percentage of Hispnanics per county",
       y = "Percentage of votes for Trump per county",
       title = "Did Hispanics voted for Trump?",
       subtitle = "In the 2016 elections",
       color = "",
       size = "Votes Total")

h

ggplotly(h, width = 1200, height = 1200)

# chart for Asian population

a <- ggplot(data = us50label2) +
  geom_point(mapping=aes(x=PercAsianPop, y=PercTrumpVotes, size=VotesTotal2016,
                         color=Result,
                         text=sprintf(CountyState),
                         alpha=.5)) + 
  expand_limits(x = 0, y = 0) +
  facet_wrap(~State, ncol=5) +
  scale_color_manual(values=c("#0099CC", "#FF3333"))+
  theme_minimal() +
  guides(size = FALSE) +
  guides(alpha = FALSE) +
  labs(x = "Percentage of Asian population per county",
       y = "Percentage of votes for Trump per county",
       title = "Did Asian population voted for Trump?",
       subtitle = "In the 2016 elections",
       color = "",
       size = "Votes Total")

a

ggplotly(a, width = 1200, height = 1200)