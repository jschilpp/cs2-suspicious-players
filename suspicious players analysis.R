# Suspicious players analysis

library(data.table)
library(stringr)
library(dplyr)
library(furrr)
library(readr)
library(lubridate)
library(ggplot2)
library(ggh4x)
library(purrr)
library(kableExtra)
library(tidyr)
library(haven)
library(ggrepel)

rm(list = ls())
gc()

# set your working directory with the csv file here
setwd("XXX")

# read in data
match_infos <- read_csv("Data/match_infos_prepared.csv") %>% 
  mutate(mm = ifelse(mm == "Premier Matchmaking","Premier","Faceit"))

# make date and time column as such
match_infos$match_datetime <- dmy_hms(match_infos$match_datetime)
match_infos$match_date <- as.Date(match_infos$match_datetime)
match_infos$match_hour <- match_infos$match_date
hour(match_infos$match_hour) <- hour(match_infos$match_datetime)
match_infos <- filter(match_infos, match_date >= as.Date("2024-01-01") & match_date <= as.Date("2024-01-07"))

# filter out matches with failed extraction
complete_matches <- match_infos %>% 
  group_by(match) %>% 
  summarise(players = n()) %>% 
  filter(players == 10) %>% 
  pull(match)

match_infos <- match_infos %>% 
  filter(match %in% complete_matches)

#### Plot mean headshot percentage over time ####
# get summary statistics per match and day
perf_aggr <- match_infos %>% 
  filter(match_date >= as.Date("2024-01-01") & match_date <= as.Date("2024-01-07")) %>%
  group_by(match) %>% 
  mutate(hs_pct10 = ifelse(kills >= 10,hs_pct/100,NA),
         no_rank = ifelse(is.na(rank) | rank == 1,1,0)) %>% 
  group_by(mm,match_date) %>% 
  summarize(hs_pct10 = mean(hs_pct10,na.rm=T),
            matches = n_distinct(match))
perf_aggr$mm <- factor(perf_aggr$mm,levels = c("Premier","Faceit"))

# adjust faceit hs pct to be able to plot faceit hs% and premier hs% in more comparable manner
faceit_hs_max <- max(filter(perf_aggr,mm=="Faceit")$hs_pct10)
premier_hs_max <- max(filter(perf_aggr,mm=="Premier")$hs_pct10)

perf_aggr <- perf_aggr %>% 
  mutate(hs_pct10_adjusted = ifelse(mm=="Faceit",hs_pct10 * premier_hs_max / faceit_hs_max,hs_pct10),
         hs_pct10_label = round(hs_pct10 * 100,2))

# plot hs pct (with two axis to account for difference in hs%)
ggplot(perf_aggr, aes(match_date, hs_pct10_adjusted,color = mm,shape=mm,linetype=mm)) +
  geom_point() +
  geom_line() +
  geom_text_repel(aes(y=hs_pct10_adjusted,label=hs_pct10_label),color="black") +
  scale_color_manual(values = c("firebrick","cornflowerblue")) +
  scale_y_continuous(labels = scales::percent,name = "Premier Headshot %",sec.axis = sec_axis(~.*faceit_hs_max / premier_hs_max, name="Faceit Headshot %",labels = scales::percent)) +
  xlab("Date") +
  geom_vline(xintercept=as.Date("2024-01-04")) +
  labs(title = "Mean Headshot Percentage around VAC Ban Wave Jan 04, 2024",
       caption = str_wrap(paste0("Data from csstats.gg. Matches = ",format(length(unique(match_infos$match)),big.mark=","),". Players = ",format(length(match_infos$player_name),big.mark=",")," (unique players = ",format(length(unique(match_infos$player_name)),big.mark=","),"). Mean Premier rank = ",format(round(mean(filter(match_infos,mm=="Premier")$rank,na.rm=T),0),big.mark=","),". Mean Faceit level = ",format(round(mean(filter(match_infos,mm=="Faceit")$rank,na.rm=T),2),big.mark=","),". Only players with at least ten kills are considered."),123)) +
  theme_bw() +
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        plot.margin=margin(c(1,1,1,1))) +
  theme(strip.text.x = element_text(face = "bold"), strip.text.y = element_text(face = "bold")) +
  theme(text = element_text(family = "sans"))

  # save
ggsave("Writing/illustrations/hs_pct.png",width=20,height=10,unit="cm")

#### Preparation for Stata to check for statistical significance ####
match_infos_dta <- match_infos %>%
  filter(match_date >= as.Date("2024-01-01") & match_date <= as.Date("2024-01-07")) %>%
  group_by(mm,match_rank_group) %>%
  mutate(mean_hs_pct = mean(hs_pct,na.rm=T),sd_hs_pct = sd(hs_pct,na.rm=T),
         mean_kills = mean(kills,na.rm=T),sd_kills = sd(kills,na.rm=T),
         hs_outlier = ifelse(kills >= 10 & hs_pct > mean_hs_pct + 2*sd_hs_pct,1,0),
         kills_outlier = ifelse(kills > mean_kills + 2*sd_kills,1,0),
         outlier = ifelse(hs_outlier == 1 & kills_outlier == 1,1,0),
         treated = ifelse(mm=="Premier",1,0),
         after=ifelse(match_date > as.Date("2024-01-04"),1,0),
         period = as.numeric(match_date - as.Date("2024-01-01"))) %>%
  select(c("kills","hs_pct","rank","treated","after","player_name","period"))

write_dta(match_infos_dta, "Data/match_infos.dta")

#### Plot density of HS Percentage ####
# plot distribution of hs pct before and after
perf_aggr <- match_infos_dta %>% 
  filter(period != 3 & kills >= 10) %>% 
  group_by(mm,hs_pct,after) %>% 
  summarise(n = n()) %>% 
  arrange(mm,after,hs_pct) %>% 
  group_by(mm,after) %>% 
  mutate(cum_n = cumsum(n),
         total_n = sum(n),
         n_pct = n/total_n,
         cum_n_pct = cum_n/total_n,
         hs_pct = hs_pct/100) %>% 
  group_by(mm,hs_pct) %>% 
  arrange(mm,hs_pct,after) %>% 
  mutate(pct_diff = (cum_n_pct - lag(cum_n_pct))*100,
         pos_diff = ifelse(pct_diff > 0,1,0))

perf_aggr$mm <- factor(perf_aggr$mm,levels = c("Premier","Faceit"))

ggplot(perf_aggr, aes(hs_pct,cum_n_pct, color = factor(after), linetype = factor(after))) +
  geom_line(linewidth=.25) +
  facet_wrap2(~mm) +
  scale_color_manual(values = c("firebrick","darkgreen"),
                     labels = c("Jan 01 - Jan 03","Jan 05 - Jan 07")) +
  scale_linetype_manual(values = c("solid","dashed"),labels = c("Jan 01 - Jan 03","Jan 05 - Jan 07")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  ylab("Cumulative Percentage of Players") +
  xlab("Headshot %") +
  labs(title = "Cumulative Distribution of Headshot Percentages around VAC Ban Wave Jan 04, 2024",
       caption = str_wrap(paste0("Data from csstats.gg. Matches = ",format(length(unique(match_infos$match)),big.mark=","),". Players = ",format(length(match_infos$player_name),big.mark=",")," (unique players = ",format(length(unique(match_infos$player_name)),big.mark=","),"). Mean Premier rank = ",format(round(mean(filter(match_infos,mm=="Premier")$rank,na.rm=T),0),big.mark=","),". Mean Faceit level = ",format(round(mean(filter(match_infos,mm=="Faceit")$rank,na.rm=T),2),big.mark=","),". Only players with at least ten kills are considered."),123)) +
  theme_bw() +
  theme(legend.position="bottom", legend.title=element_blank(),
        legend.margin=margin(c(0,0,0,0)),
        plot.margin=margin(c(1,1,1,1))) +
  theme(strip.text.x = element_text(face = "bold"), strip.text.y = element_text(face = "bold")) +
  theme(text = element_text(family = "sans"))

# save
ggsave("Writing/illustrations/hs_density.png",width=20,height=10,unit="cm")

# plot difference between expected and actual
ggplot(perf_aggr, aes(hs_pct,pct_diff,fill=factor(pos_diff))) +
  geom_col() +
  facet_wrap2(~mm) +
  scale_fill_manual(values = c("darkgreen","firebrick"),
                     labels = c("Jan 01 - Jan 03","Jan 05 - Jan 07")) +
  scale_x_continuous(labels = scales::percent) +
  ylab("Excess cumulative percentage points") +
  xlab("Headshot %") +
  labs(title = "Excess Cumulative Percentages of Players before VAC Ban Wave Jan 04, 2024",
       caption = str_wrap(paste0("Data from csstats.gg. Value for X% Headshots calculated by: Cumulative Percentage of Players with X% Headshots before ban wave - Cumulative Percentage of Players with X% Headshots after ban wave. Matches = ",format(length(unique(match_infos$match)),big.mark=","),". Players = ",format(length(match_infos$player_name),big.mark=",")," (unique players = ",format(length(unique(match_infos$player_name)),big.mark=","),"). Mean Premier rank = ",format(round(mean(filter(match_infos,mm=="Premier")$rank,na.rm=T),0),big.mark=","),". Mean Faceit level = ",format(round(mean(filter(match_infos,mm=="Faceit")$rank,na.rm=T),2),big.mark=","),". Only players with at least ten kills are considered."),123)) +
  theme_bw() +
  theme(legend.position="none",
        plot.margin=margin(c(1,1,1,1))) +
  theme(strip.text.x = element_text(face = "bold"), strip.text.y = element_text(face = "bold")) +
  theme(text = element_text(family = "sans"))

# save
ggsave("Writing/illustrations/excess_hs_density.png",width=20,height=10,unit="cm")

#### Compute percentage of suspicious people by using cumulative hs pct ####
# compute expected number of suspicious players (assuming that suspicious players are performing above median)
# get expected number of people above median (in pre treatment periods) vs actual
# difference, divided by the number of people playing in before treatment periods
# should be number of suspicious players that are now performing worse / are not present any more
density <- filter(match_infos_dta,period != 3 & kills >= 10) %>% 
  mutate(hs_pct = round(hs_pct)) %>% 
  group_by(mm,after,hs_pct) %>% 
  summarise(n = n()) %>% 
  arrange(mm,after,hs_pct) %>% 
  mutate(total = sum(n),
         cum_n = cumsum(n),
         pct = n/total,
         cum_pct = cum_n/total) %>% 
  pivot_wider(names_from="after",values_from=c("n","cum_n","pct","total","cum_pct"),id_cols = c("mm","hs_pct"))

# get median hs pct
median_before <- median(filter(match_infos_dta,mm=="Premier",after==0)$hs_pct,na.rm=T)

# expected number of players in after periods that perform better than median
exp_pct <- 1-filter(density, hs_pct == median_before & mm == "Premier")$cum_pct_0[1]
exp_pl <- exp_pct * filter(density, hs_pct == median_before & mm == "Premier")$total_1[1]

# get actual number of players
act_pct <- 1-filter(density, hs_pct == median_before & mm == "Premier")$cum_pct_1[1]
act_pl <- act_pct * filter(density, hs_pct == median_before & mm == "Premier")$total_1[1]

# compute difference
sus <- exp_pl - act_pl
sus_pct_premier <- (sus / filter(density, hs_pct == median_before & mm == "Premier")$total_0[1]) * 100
sus_pct_premier

# do same thing for Faceit
# get median hs pct
median_before <- median(filter(match_infos_dta,mm=="Faceit",after==0)$hs_pct,na.rm=T)

# expected number of players in after periods that perform better than median
exp_pct <- 1-filter(density, hs_pct == median_before & mm == "Faceit")$cum_pct_0[1]
exp_pl <- exp_pct * filter(density, hs_pct == median_before & mm == "Faceit")$total_1[1]

# get actual number of players
act_pct <- 1-filter(density, hs_pct == median_before & mm == "Faceit")$cum_pct_1[1]
act_pl <- act_pct * filter(density, hs_pct == median_before & mm == "Faceit")$total_1[1]

# compute difference
sus <- exp_pl - act_pl
sus_pct_faceit <- (sus / filter(density, hs_pct == median_before & mm == "Faceit")$total_0[1]) * 100
sus_pct_faceit
