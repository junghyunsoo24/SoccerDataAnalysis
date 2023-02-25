library(mapproj)
library(ggiraphExtra)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(ggbeeswarm)
library(skimr)
library(dplyr)
library(reshape)

soccer <- read_csv('soccer.csv')


# 포지션별 선수 나이대(남자) -------------------------------------------------------------
FranceMenSoccer <-
  soccer %>%
  filter(country == "France") %>%
  filter(WorldCup == "Men")

CroatiaMenSoccer <-
  soccer %>%
  filter(country == "Croatia")%>%
  filter(WorldCup == "Men")

BelgiumMenSoccer <-
  soccer %>%
  filter(country == "Belgium")%>%
  filter(WorldCup == "Men")

UpperMenSoccer <- bind_rows(FranceMenSoccer, CroatiaMenSoccer)

UpperMenSoccer <- bind_rows(UpperMenSoccer, BelgiumMenSoccer)

UpperMenSoccer<-
  UpperMenSoccer%>%
  mutate(Age = (2018-Year+1))

UpperMenSoccer <-
  UpperMenSoccer %>%
  mutate(AgeRange = (case_when(Age <= 19 ~ "10대",
                               19 < Age & Age <= 29 ~ "20대", 
                               29 < Age & Age <= 39 ~ "30대", 
                               39 < Age & Age <= 49 ~ "40대",
                               49 < Age & Age <= 50 ~ "50대")))
UpperMenSoccer1 <-
  UpperMenSoccer %>%
  group_by(AgeRange, pos) %>%
  summarise(n = n()) %>%
  mutate(Age_total = sum(n)) %>%
  mutate(percent = round(n/Age_total*100, 2))

PanamaMenSoccer <-
  soccer %>%
  filter(country == "Panama") %>%
  filter(WorldCup == "Men")

EgyptMenSoccer <-
  soccer %>%
  filter(country == "Egypt") %>%
  filter(WorldCup == "Men")

IcelandMenSoccer <-
  soccer %>%
  filter(country == "Iceland") %>%
  filter(WorldCup == "Men")

LowerMenSoccer <- bind_rows(PanamaMenSoccer, EgyptMenSoccer)


LowerMenSoccer <- bind_rows(LowerMenSoccer, IcelandMenSoccer) 

LowerMenSoccer <-
  LowerMenSoccer%>%
  mutate(Age = (2018-Year+1))

LowerMenSoccer <-
  LowerMenSoccer%>%
  mutate(AgeRange = (case_when(Age <= 19 ~ "10대",
                               19 < Age & Age <= 29 ~ "20대", 
                               29 < Age & Age <= 39 ~ "30대", 
                               39 < Age & Age <= 49 ~ "40대",
                               49 < Age & Age <= 50 ~ "50대")))

LowerMenSoccer <-
  LowerMenSoccer%>%
  group_by(AgeRange, pos) %>%
  summarise(n = n()) %>%
  mutate(Age_total = sum(n)) %>%
  mutate(percent = round(n/Age_total*100, 2))

lowerfig1 <-
  LowerMenSoccer %>%
  ggplot(aes(x = pos, y = n, fill = pos)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = n), vjust = 1, hjust = 2) +
  coord_flip() +
  facet_wrap(~AgeRange, 3)

upperfig <- 
  UpperMenSoccer1 %>%
  ggplot(aes(x = pos, y = n, fill = pos)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = n), vjust = 1, hjust = 2) +
  coord_flip() +
  facet_wrap(~AgeRange, 3)

lowerfig1
ggsave('남자 하위 3개팀 포지션별 선수 나이대.png', width = 5, height = 7.5)

upperfig <- 
  UpperMenSoccer1 %>%
  ggplot(aes(x = pos, y = n, fill = pos)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = n), vjust = 1, hjust = 2) +
  coord_flip() +
  facet_wrap(~AgeRange, 3)

upperfig
ggsave('남자 상위 3개팀 포지션별 선수 나이대.png', width = 5, height = 7.5)

# 포지션별 선수 나이대(여자) ------------------------------------------------------------
USAWomenSoccer <-
  soccer %>%
  filter(country == "United States") %>%
  filter(WorldCup == "Women")

NetherlandsMenSoccer <-
  soccer %>%
  filter(country == "Netherlands")%>%
  filter(WorldCup == "Women")

SwedenWomenSoccer <-
  soccer %>%
  filter(country == "Sweden")%>%
  filter(WorldCup == "Women")

UpperWomenSoccer <- bind_rows(USAWomenSoccer, NetherlandsMenSoccer)

UpperWomenSoccer <- bind_rows(UpperWomenSoccer, SwedenWomenSoccer)

UpperWomenSoccer<-
  UpperWomenSoccer%>%
  mutate(Age = (2018-Year+1))

UpperWomenSoccer <-
  UpperWomenSoccer %>%
  mutate(AgeRange = (case_when(Age <= 19 ~ "10대",
                               19 < Age & Age <= 29 ~ "20대", 
                               29 < Age & Age <= 39 ~ "30대", 
                               39 < Age & Age <= 49 ~ "40대",
                               49 < Age & Age <= 50 ~ "50대")))

UpperWomenSoccer1 <-
  UpperWomenSoccer %>%
  group_by(AgeRange, pos) %>%
  summarise(n = n()) %>%
  mutate(Age_total = sum(n)) %>%
  mutate(percent = round(n/Age_total*100, 2))

upperWomenfig1 <-
  UpperWomenSoccer1 %>%
  ggplot(aes(x = pos, y = n, fill = pos)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = n), vjust = 1, hjust = 2) +
  coord_flip() +
  facet_wrap(~AgeRange, 3)

ggsave('여자 상위 3개팀 포지션별 나이대.png', width = 5, height = 7.5)

JamaicaWomenSoccer <-
  soccer %>%
  filter(country == "Jamaica") %>%
  filter(WorldCup == "Women")

KoreaWomenSoccer <-
  soccer %>%
  filter(country == "South Korea") %>%
  filter(WorldCup == "Women")

SouthAfricaWomenSoccer <-
  soccer %>%
  filter(country == "South Africa") %>%
  filter(WorldCup == "Women")

LowerWomenSoccer <- bind_rows(JamaicaWomenSoccer, KoreaWomenSoccer)

LowerWomenSoccer <- bind_rows(LowerWomenSoccer, SouthAfricaWomenSoccer) 

LowerWomenSoccer<-
  LowerWomenSoccer%>%
  mutate(Age = (2018-Year+1))

LowerWomenSoccer <-
  LowerWomenSoccer%>%
  mutate(AgeRange = (case_when(Age <= 19 ~ "10대",
                               19 < Age & Age <= 29 ~ "20대", 
                               29 < Age & Age <= 39 ~ "30대", 
                               39 < Age & Age <= 49 ~ "40대",
                               49 < Age & Age <= 50 ~ "50대")))

LowerWomenSoccer <-
  LowerWomenSoccer%>%
  group_by(AgeRange, pos) %>%
  summarise(n = n()) %>%
  mutate(Age_total = sum(n)) %>%
  mutate(percent = round(n/Age_total*100, 2))

lowerWomenfig1 <-
  LowerWomenSoccer %>%
  ggplot(aes(x = pos, y = n, fill = pos)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = n), vjust = 1, hjust = 2) +
  coord_flip() +
  facet_wrap(~AgeRange, 3)

lowerWomenfig1
ggsave('여자 하위 3개팀 포지션별 나이대.png', width = 5, height = 7.5)

# 포지션별 선수의 수(남자) ----------------------------------------------------------

FranceMenSoccer <-
  soccer %>%
  filter(country == "France") %>%
  filter(WorldCup == "Men")

CroatiaMenSoccer <-
  soccer %>%
  filter(country == "Croatia")%>%
  filter(WorldCup == "Men")

BelgiumMenSoccer <-
  soccer %>%
  filter(country == "Belgium")%>%
  filter(WorldCup == "Men")

UpperMenSoccer <- bind_rows(FranceMenSoccer, CroatiaMenSoccer)

UpperMenSoccer <- bind_rows(UpperMenSoccer, BelgiumMenSoccer)

#상위 3팀 데이터만 추출

PanamaMenSoccer <-
  soccer %>%
  filter(country == "Panama") %>%
  filter(WorldCup == "Men")

EgyptMenSoccer <-
  soccer %>%
  filter(country == "Egypt") %>%
  filter(WorldCup == "Men")

IcelandMenSoccer <-
  soccer %>%
  filter(country == "Iceland") %>%
  filter(WorldCup == "Men")

LowerMenSoccer <- bind_rows(PanamaMenSoccer, EgyptMenSoccer)

LowerMenSoccer <- bind_rows(LowerMenSoccer, IcelandMenSoccer) 

#하위 3팀 데이터만 추출

UpperMenSoccer1 <- UpperMenSoccer %>%
  group_by(pos) %>%
  summarise(n = n())

UpperMenSoccer1 %>%
  ggplot(aes(x = pos, y = n, fill = pos)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = n), vjust = 1, hjust = 2) +
  labs(x = '포지션',
       y = '인원 수',
       title = '남자 상위 3개팀 포지션 별 인원수') +
  coord_flip()

ggsave2('남자 상위 포지션별 인원수.png', width = 6, height = 4)

LowerMenSoccer1 <- LowerMenSoccer %>%
  group_by(pos) %>%
  summarise(n = n())

LowerMenSoccer1 %>%
  ggplot(aes(x = pos, y = n, fill = pos)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = n), vjust = 1, hjust = 2) +
  labs(x = '포지션',
       y = '인원 수',
       title = '남자 하위 3개팀 포지션 별 인원수') +
  coord_flip()

ggsave2('남자 하위 포지션별 인원수.png', width = 6, height = 4)

# 포지션별 출전 횟수(남자) ----------------------------------------------------------

fig_10 <- soccer %>%
  filter(country %in% c("France","Croatia","Belgium")) %>%
  filter(WorldCup == "Men") %>%
  group_by(country,pos) %>%
  mutate(label_1 = median(caps)) %>%
  mutate(label_2 = max(caps)) %>%
  ggplot(aes(x = caps, y = pos, color = country)) +
  geom_boxplot() +
  geom_text(aes(x = label_1, label = label_1), position = position_dodge(width = 1.35)) +
  geom_text(aes(x = label_2, label = label_2), position = position_dodge(width = 1.35)) +
  labs(
    x = '출전 횟수',
    y = '포지션',
    title = '남자 상위 3개팀 포지션 별 출전 수')+
  theme(legend.position = c(0.88,0.88)) +
  xlim(0,150)

fig_10
ggsave("남자 상위 3개팀 포지션 별 출전 수,median.png", width = 15, height = 10)

fig_11 <- soccer %>%
  filter(country %in% c("Egypt","Iceland","Panama")) %>%
  filter(WorldCup == "Men") %>%
  group_by(country,pos) %>%
  mutate(label_1 = median(caps)) %>%
  mutate(label_2 = max(caps)) %>%
  ggplot(aes(x = caps, y = pos, color = country)) +
  geom_boxplot() +
  geom_text(aes(x = label_1, label = label_1), position = position_dodge(width = 1.35)) +
  geom_text(aes(x = label_2, label = label_2), position = position_dodge(width = 1.35)) +
  labs(    x = '출전 횟수',
           y = '포지션',
           title = '남자 하위 3개팀 포지션 별 출전 수')+
  theme(legend.position = c(0.88,0.88))
fig_11
ggsave("남자 하위 3개팀 포지션 별 출전 수,median.png", width = 15, height = 10)

# 포지션별 골 수(남자) ------------------------------------------------------------

fig_7 <- soccer %>%
  filter(country %in% c("France","Croatia","Belgium")) %>%
  filter(WorldCup == "Men") %>%
  group_by(country,pos) %>%
  mutate(label_1 = median(goals)) %>%
  mutate(label_2 = max(goals)) %>%
  ggplot(aes(x = goals, y = pos, color = country)) +
  geom_boxplot() +
  xlim(0,40) +
  geom_text(aes(x = label_1, label = label_1), position = position_dodge(width = 1.35)) +
  geom_text(aes(x = label_2, label = label_2), position = position_dodge(width = 1.35)) +
  theme(legend.position = c(0.88,0.88)) +
  labs(title = "남자 상위 3팀 포지션별 골 수",
       x = '골 수',
       y = '포지션')
fig_7 

ggsave("포지션 별 골 수(남자 상위팀).png", width = 15, height = 10)

fig_8 <- soccer %>%
  filter(country %in% c("Panama","Egypt","Iceland")) %>%
  filter(WorldCup == "Men") %>%
  group_by(country,pos) %>%
  mutate(label_1 = median(goals)) %>%
  mutate(label_2 = max(goals)) %>%
  ggplot(aes(x = goals, y = pos, color = country)) +
  geom_boxplot() +
  xlim(0,40) +
  geom_text(aes(x = label_1, label = label_1), position = position_dodge(width = 1.35)) +
  geom_text(aes(x = label_2, label = label_2), position = position_dodge(width = 1.35)) +
  theme(legend.position = c(0.88,0.88)) +
  labs(title = "남자 하위 3팀 포지션별 골 수",
       x = '골 수',
       y = '포지션')
fig_8

ggsave("포지션 별 골 수(남자 하위팀).png", width = 15, height = 10)


# 포지션별 선수의 수(여자) ----------------------------------------------------------


USAWomenSoccer <-
  soccer %>%
  filter(country == "United States") %>%
  filter(WorldCup == "Women")

NetherlandsMenSoccer <-
  soccer %>%
  filter(country == "Netherlands")%>%
  filter(WorldCup == "Women")

SwedenWomenSoccer <-
  soccer %>%
  filter(country == "Sweden")%>%
  filter(WorldCup == "Women")

UpperWomenSoccer <- bind_rows(USAWomenSoccer, NetherlandsMenSoccer)

UpperWomenSoccer <- bind_rows(UpperWomenSoccer, SwedenWomenSoccer)

#상위 3팀 데이터만 추출

UpperWomenSoccer1 <- UpperWomenSoccer %>%
  group_by(pos) %>%
  summarise(n = n())

UpperWomenSoccer1 %>%
  ggplot(aes(x = pos, y = n, fill = pos)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = n), vjust = 1, hjust = 2) +
  labs(x = '포지션',
       y = '인원 수',
       title = '여자 상위 3개팀 포지션 별 인원수') +
  coord_flip()

ggsave2('여자 상위 포지션별 인원수.png', width = 6, height = 4)

JamaicaWomenSoccer <-
  soccer %>%
  filter(country == "Jamaica") %>%
  filter(WorldCup == "Women")

KoreaWomenSoccer <-
  soccer %>%
  filter(country == "South Korea") %>%
  filter(WorldCup == "Women")

SouthAfricaWomenSoccer <-
  soccer %>%
  filter(country == "South Africa") %>%
  filter(WorldCup == "Women")

LowerWomenSoccer <- bind_rows(JamaicaWomenSoccer, KoreaWomenSoccer)

LowerWomenSoccer <- bind_rows(LowerWomenSoccer, SouthAfricaWomenSoccer) 

#하위 3팀 데이터만 추출

LowerWomenSoccer1 <- LowerWomenSoccer %>%
  group_by(pos) %>%
  summarise(n = n())

LowerWomenSoccer1 %>%
  ggplot(aes(x = pos, y = n, fill = pos)) +
  geom_bar(stat='identity') +
  geom_text(aes(label = n), vjust = 1, hjust = 2) +
  labs(x = '포지션',
       y = '인원 수',
       title = '여자 하위 3개팀 포지션 별 인원수') +
  coord_flip()

ggsave2('여자 하위 포지션별 인원수.png', width = 6, height = 4)


# 포지션별 출전 횟수(여자) ----------------------------------------------------------

fig_12 <- soccer %>%
  filter(country %in% c("United States","Netherlands","Sweden")) %>%
  filter(WorldCup == "Women") %>%
  group_by(country,pos) %>%
  mutate(label_1 = median(caps)) %>%
  mutate(label_2 = max(caps)) %>%
  ggplot(aes(x = caps, y = pos, color = country)) +
  geom_boxplot() +
  geom_text(aes(x = label_1, label = label_1), position = position_dodge(width = 1.35)) +
  geom_text(aes(x = label_2, label = label_2), position = position_dodge(width = 1.35)) +
  labs(    x = '출전 횟수',
           y = '포지션',
           title = '여자 상위 3개팀 포지션 별 출전 수')+
  theme(legend.position = c(0.88,0.88))
fig_12
ggsave("여자 상위 3개팀 포지션 별 출전 수,median.png", width = 15, height = 10)

fig_13 <- soccer %>%
  filter(country %in% c("Jamaica","South Korea","South Africa")) %>%
  filter(WorldCup == "Women") %>%
  group_by(country,pos) %>%
  mutate(label_1 = median(caps)) %>%
  mutate(label_2 = max(caps)) %>%
  ggplot(aes(x = caps, y = pos, color = country)) +
  geom_boxplot() +
  geom_text(aes(x = label_1, label = label_1), position = position_dodge(width = 1.35)) +
  geom_text(aes(x = label_2, label = label_2), position = position_dodge(width = 1.35)) +
  labs(    x = '출전 횟수',
           y = '포지션',
           title = '여자 하위 3개팀 포지션 별 출전 수')+
  theme(legend.position = c(0.88,0.88)) +
  xlim(0,250)
fig_13
ggsave("여자 하위 3개팀 포지션 별 출전 수,median.png", width =15, height = 10)


# 포지션별 골 수(여자) ------------------------------------------------------------

fig_14 <- soccer %>%
  filter(country %in% c("United States","Netherlands","Sweden")) %>%
  filter(WorldCup == "Women") %>%
  group_by(country,pos) %>%
  mutate(label_1 = median(goals)) %>%
  mutate(label_2 = max(goals)) %>%
  ggplot(aes(x = goals, y = pos, color = country)) +
  geom_boxplot() +
  xlim(0,40) +
  geom_text(aes(x = label_1, label = label_1), position = position_dodge(width = 1.35)) +
  geom_text(aes(x = label_2, label = label_2), position = position_dodge(width = 1.35)) +
  theme(legend.position = c(0.88,0.88)) +
  labs(title = "여자 하위 3팀 포지션별 골 수",
       x = '골 수',
       y = '포지션')
fig_14
ggsave("포지션 별 골 수(여자 상위팀)median.png", width = 15, height = 10)

fig_9 <- soccer %>%
  filter(country %in% c("Jamaica","South Korea","South Africa")) %>%
  filter(WorldCup == "Women") %>%
  group_by(country,pos) %>%
  mutate(label_1 = median(goals)) %>%
  mutate(label_2 = max(goals)) %>%
  ggplot(aes(x = goals, y = pos, color = country)) +
  geom_boxplot() +
  xlim(0,40) +
  geom_text(aes(x = label_1, label = label_1), position = position_dodge(width = 1.35)) +
  geom_text(aes(x = label_2, label = label_2), position = position_dodge(width = 1.35)) +
  theme(legend.position = c(0.88,0.88)) +
  labs(title = "여자 하위 3팀 포지션별 골 수",
       x = '골 수',
       y = '포지션')
fig_9
ggsave("포지션 별 골 수(여자 하위팀)median.png", width = 15, height = 10)

# 출전 횟수에 따른 골 수(남자) -------------------------------------------------------

Mensoccer <- 
  soccer %>%
  filter(WorldCup == "Men")

cor.test(Mensoccer$caps, Mensoccer$goals)

Menfig <- soccer %>%
  filter(WorldCup == "Men") %>%
  ggplot(aes(x = caps, y = goals)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = '출전 수',
       y = '골 수',
       title = '남자 출전 수에 따른 골 수')

Menfig
ggsave2('남자 출전 수에 따른 골 수.png', width = 5, height = 7.5)


# 출전 횟수에 따른 골 수(여자) -------------------------------------------------------

Womensoccer <- 
  soccer %>%
  filter(WorldCup == "Women")

cor.test(Womensoccer$caps, Womensoccer$goals)

Womenfig <- soccer %>%
  filter(WorldCup == "Women") %>%
  ggplot(aes(x = caps, y = goals)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = '출전 수',
       y = '골 수',
       title = '여자 출전 수에 따른 골 수')

Womenfig
ggsave2('여자 출전 수에 따른 골 수.png', width = 5, height = 7.5)

# 골 결정력에 따른 승점(남자) --------------------------------------------------------

soccer_point <- read.csv('soccer_point.csv')

Mensoccer <- soccer_point %>%
  mutate(goals_determination = goals / caps) %>%
  filter(WorldCup == "Men") %>%
  filter(!pos == "GK")

countryD <- aggregate(goals_determination ~country, Mensoccer, sum)

countryP <- aggregate(point ~country, Mensoccer, sum)

MensoccerP <- cbind(countryD, countryP)

MensoccerP <- subset(MensoccerP, select=-country)

cor.test(MensoccerP$goals_determination, MensoccerP$point/20)

Mfig1 <- MensoccerP %>%
  ggplot(aes(x = goals_determination, y = point/20)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "경기 당 골 수",
       y = "승점",
       title = "남자 경기당 골 수에 따른 승점") +
  ylim(0, 25)

Mfig1
ggsave('남자 경기당 골 수에 따른 승점.png', width = 5, height = 7.5)

# 골 결정력에 따른 승점(여자) --------------------------------------------------------

soccer_point <- read.csv('soccer_point.csv')

Womensoccer <- soccer_point %>%
  mutate(goals_determination = goals / caps) %>%
  filter(WorldCup == "Women") %>%
  filter(!pos == "GK")

countryD <- aggregate(goals_determination ~country, Womensoccer, sum)

countryP <- aggregate(point ~country, Womensoccer, sum)

countryP <- countryP %>%
  filter(!country == "Thailand")

WomensoccerP <- cbind(countryD, countryP)

WomensoccerP <- subset(WomensoccerP, select=-country)

cor.test(WomensoccerP$goals_determination, WomensoccerP$point/20)

Wfig1 <- WomensoccerP %>%
  ggplot(aes(x = goals_determination, y = point/20)) +
  geom_point() +
  stat_smooth(method = "lm") +
  labs(x = "경기 당 골 수",
       y = "승점",
       title = "여자 경기당 골 수에 따른 승점") +
  ylim(0, 25)

Wfig1
ggsave('여자 경기당 골 수에 따른 승점.png', width = 5, height = 7.5)

