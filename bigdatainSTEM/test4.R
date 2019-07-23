ggplot(data = TEDS_2016) +
  stat_count(mapping = aes(x = PartyID)) +
  scale_y_continuous(labels=scales::percent)


ggplot(TEDS_2016, aes(PartyID)) +
  geom_bar(aes(y = (..count..)/sum(..count..)),fill = factor(PartyID))) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Party support (%)") +
  theme_bw()

ggplot(TEDS_2016, aes(PartyID)) +
  geom_bar(aes(y = (..count..)/sum(..count..),fill=factor(PartyID)),reorder(y)) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Party support (%)") +
  theme_bw() +
  scale_fill_manual(values=c("steel blue","forestgreen","khaki1","orange","goldenrod","yellow","grey"))

ggplot(TEDS_2016, aes(PartyID)) +
  geom_histogram(stat = "count") +
  ylab("Party support (%)") +
  theme_bw()

+
  scale_fill_manual(values=c("steel blue","forestgreen","khaki1","orange","goldenrod","yellow","grey"))

ggplot(TEDS_2016, aes(PartyID)) +
  geom_bar()

ggplot(TEDS_2016, aes(PartyID)) +
  geom_bar(aes(y = (..count..)/sum(..count..),fill=PartyID)) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Party Support (%)") + xlab("Taiwan Political Parties")

+
  theme_bw()

ggplot(TEDS_2016, aes(PartyID)) +
  geom_bar(aes(y = (..count..)/sum(..count..),fill=PartyID)) +
  scale_y_continuous(labels=scales::percent) +
  ylab("Party Support (%)") +
  xlab("Taiwan Political Parties") +
  theme_bw() +
  scale_fill_manual(values=c("steel blue","forestgreen","khaki1","orange","goldenrod","yellow","grey"))



library(scales)
TEDS_2016 %>%
  count(PartyID) %>%
  mutate(perc = n / nrow(TEDS_2016)) -> T2

ggplot(T2, aes(x = PartyID, y = perc)) + geom_bar(stat = "identity")
ggplot(T2, aes(x = reorder(PartyID, -perc),y = perc,fill=PartyID)) +
  geom_bar(stat = "identity") +
  ylab("Party Support (%)") +
  xlab("Taiwan Political Parties") +
  theme_bw() +
  scale_fill_manual(values=c("steel blue","forestgreen","khaki1","orange","goldenrod","yellow","grey"))


TEDS_2016$Tondu <- factor(TEDS_2016$Tondu, labels=c("Unification now","Status quo, unif. in future","Status quo, decide later","Status quo forever", "Status quo, indep. in future", "Independence now","No response"))
(TEDS_2016$Tondu)
install.packages("descr")
library(descr)
freq(TEDS_2016$Tondu)
prop.table(table(TEDS_2016$Tondu))
ggplot(T2, aes(x = reorder(PartyID, -perc),y = perc,fill=PartyID)) +
    geom_bar(stat = "identity")

+
    ylab("Party support (%)") +
    theme_bw() +
    scale_fill_manual(values=c("steel blue","forestgreen","khaki1","orange","goldenrod","yellow","grey"))


#    scale_color_identity()

#    scale_fill_manual(values=c("steel blue","forestgreen","khaki1","orange","goldenrod","yellow","grey"))


TEDS_2016$PartyID <- factor(TEDS_2016$PartyID, labels=c("KMT","DPP","NP","PFP", "TSU", "NPP","Others/Neutral"))
TaiwanParty= %>% mutate()
TEDS_2016$PartyID
TEDS_2016 <- read_stata("https://github.com/datageneration/home/blob/master/DataProgramming/data/TEDS_2016.dta?raw=true")
rm(TEDS_2016)
summary(TEDS_2016$PartyID)
install.packages("tidyverse")
library(tidyverse)
library(dplyr)
attach(TEDS_2016)
KMTman=filter(PartyID,female==0)
class(PartyID)
