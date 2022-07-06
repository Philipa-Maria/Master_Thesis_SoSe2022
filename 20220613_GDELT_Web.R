# ---- Administration ----
setwd("~/Documents/ESB/Semester 4/Masterarbeit/Quantitative Auswertung/Daten und R")

library(tidyverse) 
library(lubridate) 
library(stringr)
library(ggplot2)
library(here)
library(tidyr)
library(dplyr)
library(histogram)
library(plyr)
library(gridExtra)
library(Kendall)
library(xts)
library(psych)
library(sjmisc)
library(coin)
library(ggpubr)
library(rstatix)

options(scipen = 9999, digits = 4)

# Basic Business Articles reading
## source: https://gdelt.github.io/#api=doc&query=(business%20OR%20corporation%20OR%20firm%20OR%20enterprise%20OR%20company%20OR%20multinational%20OR%20venture%20OR%20%22joint%20venture%22)&sourcecountry=CH,GM&timelinemode=TimelineVol&timelinesmooth=7&startdatetime=20200101000000&enddatetime=20220601235959
## articles: (business OR corporation OR firm OR enterprise OR company OR multinational OR venture OR "joint venture")
## source countries: Germany, China
## Date range: 20-Sept-2017 to 20-May-2022 (announcement +/- 28 months)
## Smooth: 7 days
## TIMELINE %Vol of global news coverage

# GENERAL

df0 <- read.csv(url("https://api.gdeltproject.org/api/v2/doc/doc?query=(business%20OR%20corporation%20OR%20firm%20OR%20enterprise%20OR%20company%20OR%20multinational%20OR%20venture%20OR%20%22joint%20venture%22)%20(sourcecountry:CH%20OR%20sourcecountry:GM)&mode=TimelineVol&timelinesmooth=7&startdatetime=20170920000000&enddatetime=20220520235959&timezoom=yes&FORMAT=csv")
                )

df0$dates <- gsub("-", "", df0$Date)
df0$dates <- as.Date(df0$dates, "%Y%m%d")

df<- df0[ ,-c(1,2)]

names(df)[names(df) == 'Value'] <- 'Value_b'


# Basic Data, key words:
# travelx3
dft0 <- read.csv(url("https://api.gdeltproject.org/api/v2/doc/doc?query=repeat3:%22travel%22%20(business%20OR%20corporation%20OR%20firm%20OR%20enterprise%20OR%20company%20OR%20multinational%20OR%20venture%20OR%20%22joint%20venture%22)%20(sourcecountry:CH%20OR%20sourcecountry:GM)&mode=TimelineVol&timelinesmooth=7&startdatetime=20170920000000&enddatetime=20220520235959&timezoom=yes&FORMAT=csv")
                )

dft0$dates <- gsub("-", "", dft0$Date)
dft0$dates <- as.Date(dft0$dates, "%Y%m%d")

dft<- dft0[ ,-c(1,2)]

# set off effect by business articles
dft_b <- join(dft, df, by = "dates")

dft_rel <- mutate(dft_b, Value_rel = Value / Value_b)
p1 <- ggplot(dft_rel)+
  geom_line(aes(dates, Value_rel, color = "Relative Values (adjusted)"))+
  geom_line(aes(dates, Value, color = "Values of travel (not adjusted)"))+
  geom_line(aes(dates, Value_b, color = "Only Business Articles"))+
  scale_color_manual(values = c("#13C5D1", "#797F80" , "#000000"))+
  labs(title = "All")
p2 <- ggplot(dft_rel)+
  geom_line(aes(dates, Value_rel, color = "Relative Values (adjusted)"))+
  scale_color_manual(values = c("#797F80" ))+
  labs(title = "Relative Values (adjusted)")
p3 <- ggplot(dft_rel)+
  geom_line(aes(dates, Value, color = "Values of travel (not adjusted)"))+
  scale_color_manual(values = c("#000000"))+
  labs(title = "Values of travel (not adjusted)")
p4 <- ggplot(dft_rel)+
  geom_line(aes(dates, Value_b, color = "Only Business Articles"))+
  scale_color_manual(values = c("#13C5D1"))+
  labs(title = "Only Business Articles")

p1_4_travel <- grid.arrange(p1, p2, p3, p4)

# effect set off successfully 


# informx3 and provide information
dfi0 <- read.csv(url("https://api.gdeltproject.org/api/v2/doc/doc?query=(%22provide%20information%22%20OR%20repeat3:%22inform%22)%20%20(business%20OR%20corporation%20OR%20firm%20OR%20enterprise%20OR%20company%20OR%20multinational%20OR%20venture%20OR%20%22joint%20venture%22)%20(sourcecountry:CH%20OR%20sourcecountry:GM)&mode=TimelineVol&timelinesmooth=7&startdatetime=20170920000000&enddatetime=20220520235959&timezoom=yes&FORMAT=csv")
                 )

dfi0$dates <- gsub("-", "", dfi0$Date)
dfi0$dates <- as.Date(dfi0$dates, "%Y%m%d")

dfi<- dfi0[ ,-c(1,2)]

# set off effect by business articles
dfi_b <- join(dfi, df, by = "dates")

dfi_rel <- mutate(dfi_b, Value_rel = Value / Value_b)
p1i <- ggplot(dfi_rel)+
  geom_line(aes(dates, Value_rel, color = "Ratio"))+
  geom_line(aes(dates, Value, color = "Values (not adjusted)"))+
  geom_line(aes(dates, Value_b, color = "Only Business Articles"))+
  scale_color_manual(values = c("#13C5D1", "#797F80" , "#000000"))+
  labs(title = "All inform3x, provide information")
p2i <- ggplot(dfi_rel)+
  geom_line(aes(dates, Value_rel, color = "Ratio"))+
  scale_color_manual(values = c("#797F80" ))+
  labs(title = "Ratio")
p3i <- ggplot(dfi_rel)+
  geom_line(aes(dates, Value, color = "Values (not adjusted)"))+
  scale_color_manual(values = c("#000000"))+
  labs(title = "Values (not adjusted)")
p4i <- ggplot(dfi_rel)+
  geom_line(aes(dates, Value_b, color = "Only Business Articles"))+
  scale_color_manual(values = c("#13C5D1"))+
  labs(title = "Only Business Articles")

p1_4_inform <- grid.arrange(p1i, p2i, p3i, p4i)
# set off effect: successful


# strengthen OR boost OR intensify) ties (business OR corporation OR firm OR enterprise OR company OR multinational OR venture OR "joint venture") 
dfs0 <- read.csv(url("https://api.gdeltproject.org/api/v2/doc/doc?query=(strengthen%20OR%20boost%20OR%20intensify)%20ties%20(business%20OR%20corporation%20OR%20firm%20OR%20enterprise%20OR%20company%20OR%20multinational%20OR%20venture%20OR%20%22joint%20venture%22)%20(sourcecountry:CH%20OR%20sourcecountry:GM)&mode=TimelineVol&timelinesmooth=7&startdatetime=20170920000000&enddatetime=20220520235959&timezoom=yes&FORMAT=csv ")
)

dfs0$dates <- gsub("-", "", dfs0$Date)
dfs0$dates <- as.Date(dfs0$dates, "%Y%m%d")

dfs<- dfs0[ ,-c(1,2)]

# set off effect by business articles
dfs_b <- join(dfs, df, by = "dates")

dfs_rel <- mutate(dfs_b, Value_rel = Value / Value_b)
p1s <- ggplot(dfs_rel)+
  geom_line(aes(dates, Value_rel, color = "Ratio"))+
  geom_line(aes(dates, Value, color = "Value (not adjusted)"))+
  geom_line(aes(dates, Value_b, color = "Only Business Articles"))+
  scale_color_manual(values = c("#13C5D1", "#797F80" , "#000000"))+
  labs(title = "All Strengthen Ties")
p2s <- ggplot(dfs_rel)+
  geom_line(aes(dates, Value_rel, color = "Ratio)"))+
  scale_color_manual(values = c("#797F80" ))+
  labs(title = "Ratio")
p3s <- ggplot(dfs_rel)+
  geom_line(aes(dates, Value, color = "Values (not adjusted)"))+
  scale_color_manual(values = c("#000000"))+
  labs(title = "Values (not adjusted)")
p4s <- ggplot(dfs_rel)+
  geom_line(aes(dates, Value_b, color = "Only Business Articles"))+
  scale_color_manual(values = c("#13C5D1"))+
  labs(title = "Only Business Articles")

p1_4_strengthen <- grid.arrange(p1s, p2s, p3s, p4s)
# set off effect: successful



# "conflict of interest" (business OR corporation OR firm OR enterprise OR company OR multinational OR venture OR "joint venture") 
dfc0 <- read.csv(url("https://api.gdeltproject.org/api/v2/doc/doc?query=%22conflict%20of%20interest%22%20(business%20OR%20corporation%20OR%20firm%20OR%20enterprise%20OR%20company%20OR%20multinational%20OR%20venture%20OR%20%22joint%20venture%22)%20(sourcecountry:GM%20OR%20sourcecountry:CH)&mode=TimelineVol&timelinesmooth=7&startdatetime=20170920000000&enddatetime=20220520235959&timezoom=yes&FORMAT=csv ")
)

dfc0$dates <- gsub("-", "", dfc0$Date)
dfc0$dates <- as.Date(dfc0$dates, "%Y%m%d")

dfc<- dfc0[ ,-c(1,2)]

# set off effect by business articles
dfc_b <- join(dfc, df, by = "dates")

dfc_rel <- mutate(dfc_b, Value_rel = Value / Value_b)
p1c <- ggplot(dfc_rel)+
  geom_line(aes(dates, Value_rel, color = "Ratio"))+
  geom_line(aes(dates, Value, color = "Values (not adjusted)"))+
  geom_line(aes(dates, Value_b, color = "Only Business Articles"))+
  scale_color_manual(values = c("#13C5D1", "#797F80" , "#000000"))+
  labs(title = "All conflict of interest")
p2c <- ggplot(dfc_rel)+
  geom_line(aes(dates, Value_rel, color = "Ratio"))+
  scale_color_manual(values = c("#797F80" ))+
  labs(title = "Ratio")
p3c <- ggplot(dfc_rel)+
  geom_line(aes(dates, Value, color = "Values (not adjusted)"))+
  scale_color_manual(values = c("#000000"))+
  labs(title = "Values (not adjusted)")
p4c <- ggplot(dfc_rel)+
  geom_line(aes(dates, Value_b, color = "Only Business Articles"))+
  scale_color_manual(values = c("#13C5D1"))+
  labs(title = "Only Business Articles")

p1_4_confl <- grid.arrange(p1c, p2c, p3c, p4c)
# set off effect: successful



## ---- Mann-Kendall TREND Test ---- (data only since 2020-Jan-20)
# travelx3
tst_rel <- xts(dft_rel[dft_rel$dates >= '2020-01-20',-c(1,2,3)], order.by=as.Date(dft_rel[dft_rel$dates >= '2020-01-20',2], "%Y%m%d"))

MannKendall(tst_rel)
plot(tst_rel)

#Add a smooth line to visualize the trend 
tst_rel <- data.frame(date=index(tst_rel), coredata(tst_rel))

plot_tst_rel <- ggplot(tst_rel, aes(date, coredata.tst_rel.)) + 
  geom_line() + 
  stat_smooth()+
  theme_minimal()+
  xlab('Date')+
  ylab('"Travel" themed business articels, adjusted %')+
  labs(title = "Travel themed News Articles: 2020-01-20 to 2022-05-20",
       subtitle = "Mann-Kendall Trend Test, tau = -0.329, 2-sided pvalue =<0.0000000000000002",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
       )+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  scale_color_manual(values = c("#13C5D1", "#797F80"))
plot_tst_rel


# informx3 and provide information M-K
tsi_rel <- xts(dfi_rel[dfi_rel$dates >= '2020-01-20',-c(1,2,3)], order.by=as.Date(dfi_rel[dfi_rel$dates >= '2020-01-20',2], "%Y%m%d"))

MannKendall(tsi_rel)
plot(tsi_rel)

#Add a smooth line to visualize the trend 
tsi_rel <- data.frame(date=index(tsi_rel), coredata(tsi_rel))

plot_tsi_rel <- ggplot(tsi_rel, aes(date, coredata.tsi_rel.)) + 
  geom_line() + 
  stat_smooth()+
  theme_minimal()+
  xlab('Date')+
  ylab('"Inform, provide information" business articels, ratio')+
  labs(title = "'Inform, provide information' themed News Articles: 2020-01-20 to 2022-05-20",
       subtitle = "Mann-Kendall Trend Test: tau = 0.355, 2-sided pvalue =<0.0000000000000002",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  scale_color_manual(values = c("#13C5D1", "#797F80"))
plot_tsi_rel



# (strengthen OR boost OR intensify) ties (business OR corporation OR firm OR enterprise OR company OR multinational OR venture OR "joint venture") 
tfs_rel <- xts(dfs_rel[dfs_rel$dates >= '2020-01-20',-c(1,2,3)], order.by=as.Date(dfs_rel[dfs_rel$dates >= '2020-01-20',2], "%Y%m%d"))

MannKendall(tfs_rel)
plot(tfs_rel)

#Add a smooth line to visualize the trend 
tfs_rel <- data.frame(date=index(tfs_rel), coredata(tfs_rel))

plot_tfs_rel <- ggplot(tfs_rel, aes(date, coredata.tfs_rel.)) + 
  geom_line() + 
  stat_smooth()+
  theme_minimal()+
  xlab('Date')+
  ylab('"Strenghtening ties" business articels, ratio')+
  labs(title = "'Strengthening ties' themed News Articles: 2020-01-20 to 2022-05-20",
       subtitle = "Mann-Kendall Trend Test: tau = 0.409, 2-sided pvalue =<0.0000000000000002",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  scale_color_manual(values = c("#13C5D1", "#797F80"))
plot_tfs_rel



# "conflict of interest" (business OR corporation OR firm OR enterprise OR company OR multinational OR venture OR "joint venture") 
tsc_rel <- xts(dfc_rel[dfc_rel$dates >= '2020-01-20',-c(1,2,3)], order.by=as.Date(dfc_rel[dfc_rel$dates >= '2020-01-20',2], "%Y%m%d"))

MannKendall(tsc_rel)
plot(tsc_rel)

#Add a smooth line to visualize the trend 
tsc_rel <- data.frame(date=index(tsc_rel), coredata(tsc_rel))

plot_tsc_rel <- ggplot(tsc_rel, aes(date, coredata.tsc_rel.)) + 
  geom_line() + 
  stat_smooth()+
  theme_minimal()+
  xlab('Date')+
  ylab('"Conflict of interest" business articels, ratio')+
  labs(title = "'Conflict of interest' themed News Articles: 2020-01-20 to 2022-05-20",
       subtitle = "Mann-Kendall Trend Test: tau = 0.242, 2-sided pvalue =<0.0000000000000002",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  scale_color_manual(values = c("#13C5D1", "#797F80"))
plot_tsc_rel



# ---- Mann Whitney U-Test / Wilcoxon Test (unpaired) ----
# travelx3
x <- nrow(dft_rel)
dft_rel$ID <- seq.int(x)
dft_rel <-  mutate (dft_rel, part = if_else(dft_rel$ID < x/2, 1, 2))
count(dft_rel$part)
if(is_odd(x) == TRUE) { dft_rel <- subset(dft_rel, dft_rel$ID != x )}

## analyse 
tapply(dft_rel$Value_rel, dft_rel$part, summary)

ggplot()+
  geom_boxplot(data = dft_rel, aes(x = as.factor(part), y = Value_rel))+
  theme_minimal()+
  xlab('1 = before 2020-01-20; 2 = since 2020')+
  ylab('"Travel" themed business articels, adjusted %')+
  labs(title = "Travel themed News Articles: 2017-09-20 to 2022-05-20",
       subtitle = "Group 1: mean 0.0173, median 0.0162; Group 2: mean 0.0223, median 0.0215",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  scale_color_manual(values = c("#13C5D1", "#797F80"))

# Achtung: nach Corona höherer Durchschnittswert, aber negativer Trend

dft_rel$part <- as.factor(dft_rel$part)
a_dft_rel <- dft_rel %>% wilcox_test(Value_rel~part, exact = FALSE,
            paired = FALSE) %>%
  add_significance()
a_dft_rel

# Effektstärke
z <- qnorm(a_dft_rel$p/2)
r <- abs(z/sqrt(nrow(dft_rel)))
r




# inform3x and provide information
x <- nrow(dfi_rel)
dfi_rel$ID <- seq.int(x)
dfi_rel <-  mutate (dfi_rel, part = if_else(dfi_rel$ID < x/2, 1, 2))
count(dfi_rel$part)
if(is_odd(x) == TRUE) { dfi_rel <- subset(dfi_rel, dfi_rel$ID != x )}

## analyse 
tapply(dfi_rel$Value_rel, dfi_rel$part, summary)

ggplot()+
  geom_boxplot(data = dfi_rel, aes(x = as.factor(part), y = Value_rel))+
  theme_minimal()+
  xlab('1 = before 2020-01-20; 2 = since 2020')+
  ylab('"Inform/prov. Inf." business articels, adjusted %')+
  labs(title = "Inform/prov. Inf. News Articles: 2017-09-20 to 2022-05-20",
       subtitle = "Group 1: mean 0.0036, median 0.0034; Group 2: mean 0.00624, median 0.00611",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  scale_color_manual(values = c("#13C5D1", "#797F80"))


dfi_rel$part <- as.factor(dfi_rel$part)
a_dfi_rel <- dfi_rel %>% wilcox_test(Value_rel~part, exact = FALSE,
                                     paired = FALSE) %>%
  add_significance()
a_dfi_rel

# Effektstärke
zi <- qnorm(a_dfi_rel$p/2)
ri <- abs(zi/sqrt(nrow(dfi_rel)))
ri


# (strengthen OR boost OR intensify) ties (business OR corporation OR firm OR enterprise OR company OR multinational OR venture OR "joint venture") 
x <- nrow(dfs_rel)
dfs_rel$ID <- seq.int(x)
dfs_rel <-  mutate (dfs_rel, part = if_else(dfs_rel$ID < x/2, 1, 2))
count(dfs_rel$part)
if(is_odd(x) == TRUE) { dfs_rel <- subset(dfs_rel, dfs_rel$ID != x )}

## analyse 
tapply(dfs_rel$Value_rel, dfs_rel$part, summary)

ggplot()+
  geom_boxplot(data = dfs_rel, aes(x = as.factor(part), y = Value_rel))+
  theme_minimal()+
  xlab('1 = before 2020-01-20;                    2 = since 2020')+
  ylab('"Strenghten ties" business articels, ratio')+
  labs(title = "'Strenghten ties' News Articles: 2017-09-20 to 2022-05-20",
       subtitle = "Group 1: mean 0.00091, median 0.00083; Group 2: mean 0.000819, median 0.00079",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  scale_color_manual(values = c("#13C5D1", "#797F80"))


dfs_rel$part <- as.factor(dfs_rel$part)
a_dfs_rel <- dfs_rel %>% wilcox_test(Value_rel~part, exact = FALSE,
                                     paired = FALSE) %>%
  add_significance()
a_dfs_rel

# Effektstärke
zs <- qnorm(a_dfs_rel$p/2)
rs <- abs(zs/sqrt(nrow(dfs_rel)))
rs




# conflict of interest
x <- nrow(dfc_rel)
dfc_rel$ID <- seq.int(x)
dfc_rel <-  mutate (dfc_rel, part = if_else(dfc_rel$ID < x/2, 1, 2))
count(dfc_rel$part)
if(is_odd(x) == TRUE) { dfc_rel <- subset(dfc_rel, dfc_rel$ID != x )}

## analyse 
tapply(dfc_rel$Value_rel, dfc_rel$part, summary)

ggplot()+
  geom_boxplot(data = dfc_rel, aes(x = as.factor(part), y = Value_rel))+
  theme_minimal()+
  xlab('1 = before 2020-01-20; 2 = since 2020')+
  ylab('"Conflict of interest" business articels, adjusted %')+
  labs(title = "'Conflict of interest' News Articles: 2017-09-20 to 2022-05-20",
       subtitle = "Group 1: mean 0.000707, median 0.000621; Group 2: mean 0.001157, median 0.001123",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  scale_color_manual(values = c("#13C5D1", "#797F80"))


dfc_rel$part <- as.factor(dfc_rel$part)
a_dfc_rel <- dfc_rel %>% wilcox_test(Value_rel~part, exact = FALSE,
                                     paired = FALSE) %>%
  add_significance()
a_dfc_rel

# Effektstärke
zc <- qnorm(a_dfc_rel$p/2)
rc <- abs(zc/sqrt(nrow(dfc_rel)))
rc







# ---- Sonstiges ----


# Trying & Testing
# BASICS

dft_t0 <- read.csv(url("https://api.gdeltproject.org/api/v2/doc/doc?query=(repeat2:%22trust%22%20OR%20repeat2:%22trusted%22)%20-%22anti-trust%22%20(business%20OR%20corporation%20OR%20firm%20OR%20enterprise%20OR%20company%20OR%20multinational%20OR%20venture%20OR%20%22joint%20venture%22)%20(sourcecountry:CH%20OR%20sourcecountry:GM)&mode=TimelineVol&timelinesmooth=7&startdatetime=20170920000000&enddatetime=20220520235959&timezoom=yes&FORMAT=csv")
)

dft_t0$dates <- gsub("-", "", dft_t0$Date)
dft_t0$dates <- as.Date(dft_t0$dates, "%Y%m%d")
dft_t<- dft_t0[ ,-c(1,2)]

# set off effect by business articles
dft_t_b <- join(dft_t, df, by = "dates")
dft_t_rel <- mutate(dft_t_b, Value_rel = Value / Value_b)
p1_tt <- ggplot(dft_t_rel)+
  geom_line(aes(dates, Value_rel, color = "Relative Values (adjusted)"))+
  geom_line(aes(dates, Value, color = "'google' (not adjusted)"))+
  geom_line(aes(dates, Value_b, color = "Only Business Articles"))+
  scale_color_manual(values = c("#13C5D1", "#797F80" , "#000000"))+
  labs(title = "1. Overview (all)")+
  ylab(' ')
p2_tt <- ggplot(dft_t_rel)+
  geom_line(aes(dates, Value_rel, color = "Relative Values (adjusted)"))+
  scale_color_manual(values = c( "#000000" ))+
  labs(title = "2. Relative Values (adjusted) (ratio)")+
  ylab('Ratio')
p3_tt <- ggplot(dft_t_rel)+
  geom_line(aes(dates, Value, color = "'google' (not adjusted)"))+
  scale_color_manual(values = c("#13C5D1"))+
  labs(title = "3. 'google' (not adjusted) in %")
p4_tt <- ggplot(dft_t_rel)+
  geom_line(aes(dates, Value_b, color = "Only Business Articles"))+
  scale_color_manual(values = c("#797F80"))+
  labs(title = "4. Only Business Articles in %")

p1_4_tt <- grid.arrange(p1_tt, p2_tt, p3_tt, p4_tt)

# effect set off successfully 


## MK Trend
tst_t_rel <- xts(dft_t_rel[dft_t_rel$dates >= '2020-01-20',-c(1,2,3)], order.by=as.Date(dft_t_rel[dft_t_rel$dates >= '2020-01-20',2], "%Y%m%d"))

MannKendall(tst_t_rel)
plot(tst_t_rel)

#Add a smooth line to visualize the trend 
tst_t_rel <- data.frame(date=index(tst_t_rel), coredata(tst_t_rel))

plot_tst_t_rel <- ggplot(tst_t_rel, aes(date, coredata.tst_t_rel.)) + 
  geom_line() + 
  stat_smooth()+
  theme_minimal()+
  xlab('Date')+
  ylab('Reliable/reliability  themed business articels, adjusted %')+
  labs(title = "Reliable/reliability  News Articles: 2020-01-20 to 2022-05-20",
       subtitle = "Mann-Kendall Trend Test, tau = 0.242, 2-sided pvalue =<0.0000000000000002 ",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  scale_color_manual(values = c("#13C5D1", "#797F80"))
plot_tst_t_rel


#MW-U/W group comp.
x <- nrow(dft_t_rel)
dft_t_rel$ID <- seq.int(x)
dft_t_rel <-  mutate (dft_t_rel, part = if_else(dft_t_rel$ID < x/2, 1, 2))
count(dft_t_rel$part)
if(is_odd(x) == TRUE) { dft_t_rel <- subset(dft_t_rel, dft_t_rel$ID != x )}

## analyse 
tapply(dft_t_rel$Value_rel, dft_t_rel$part, summary)

ggplot()+
  geom_boxplot(data = dft_t_rel, aes(x = as.factor(part), y = Value_rel))+
  theme_minimal()+
  xlab('1 = before 2020-01-20; 2 = since 2020-01-20')+
  ylab('Reliable/reliability business articels, adjusted %')+
  labs(title = "Reliable/reliability News Articles: 2017-09-20 to 2022-05-20",
       subtitle = "Group 1: mean 0.000707, median 0.000621; Group 2: mean 0.001157, median 0.001123",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  scale_color_manual(values = c("#13C5D1", "#797F80"))


dft_t_rel$part <- as.factor(dft_t_rel$part)
a_dft_t_rel <- dft_t_rel %>% wilcox_test(Value_rel~part, exact = FALSE,
                                     paired = FALSE) %>% add_significance()
a_dft_t_rel

# Effektstärke
zt_t <- qnorm(a_dft_t_rel$p/2)
rt_t <- abs(zt_t/sqrt(nrow(dft_t_rel)))
rt_t






# REMOTE WORK conclusion (tone)----

df_end0 <- read.csv(url("https://api.gdeltproject.org/api/v2/doc/doc?query=%22remote%20work%22%20(business%20OR%20corporation%20OR%20firm%20OR%20enterprise%20OR%20company%20OR%20multinational%20OR%20venture%20OR%20%22joint%20venture%22)%20(sourcecountry:GM%20OR%20sourcecountry:CH)&mode=TimelineTone&timelinesmooth=7&startdatetime=20170920000000&enddatetime=20220520235959&timezoom=yes&FORMAT=csv")
)

df_end0$dates <- gsub("-", "", df_end0$Date)
df_end0$dates <- as.Date(df_end0$dates, "%Y%m%d")
df_end<- df_end0[ ,-c(1,2)]

## MK Trend
ts_end <- xts(df_end0[df_end0$dates >= '2020-01-20',-c(1,2,4)], order.by=as.Date(df_end0[df_end0$dates >= '2020-01-20',4], "%Y%m%d"))

MannKendall(ts_end)
plot(ts_end)

#Add a smooth line to visualize the trend 
ts_end <- data.frame(date=index(ts_end), coredata(ts_end))

plot_end <- ggplot(ts_end, aes(date, coredata.ts_end.)) + 
  geom_line() + 
  stat_smooth()+
  theme_minimal()+
  xlab('Date')+
  ylab('Avergae Tone')+
  labs(title = "Remote work themed News Articles: 2020-01-20 to 2022-05-20",
       subtitle = "Mann-Kendall Trend Test, tau = 0.197, 2-sided pvalue =<0.0000000000000002",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  scale_color_manual(values = c("#13C5D1", "#797F80"))
plot_end






#MW-U/W group comp.
x <- nrow(df_end)
df_end$ID <- seq.int(x)
df_end <-  mutate (df_end, part = if_else(df_end$ID < x/2, 1, 2))
count(df_end$part)
if(is_odd(x) == TRUE) { df_end <- subset(df_end, df_end$ID != x )}

## analyse 
tapply(df_end$Value, df_end$part, summary)

ggplot()+
  geom_boxplot(data = df_end, aes(x = as.factor(part), y = Value))+
  theme_minimal()+
  xlab('1 = before 2020-01-20; 2 = since 2020')+
  ylab('Average Tone')+
  labs(title = "Remote Work News Articles: 2017-09-20 to 2022-05-20",
       subtitle = "Group 1: mean 1.045, median 0.921; Group 2: mean 0.5180, median 0.5338",
       caption = "Data Source: https://gdelt.github.io/#api=doc&query=&contentmode=ArtList&maxrecords=75&timespan=1d"
  )+
  scale_color_manual(values = c("#13C5D1", "#797F80"))


df_end$part <- as.factor(df_end$part)
a_df_end <- df_end %>% wilcox_test(Value~part, exact = FALSE,
                                         paired = FALSE) %>%
  add_significance()
a_df_end

# Effektstärke
z_end <- qnorm(a_df_end$p/2)
r_end <- abs(z_end/sqrt(nrow(df_end)))
r_end










