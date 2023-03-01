# 2023 Eurostat data: Brexit plots
# Maarten Vink, EUI | @maartenpvink
#This script builds on adjusted code from https://david-reichel.github.io/posts/citizenship-acquisition-of-brits-in-the-eu/

#start with clean workspace
rm(list=ls(all=TRUE))

library(tidyverse)
library(eurostat)

# download data on citizenship acquisitions from Eurostat
d1 <- get_eurostat("migr_acq")
View(d1)
# Export dataset
# write.csv(d1, file = paste0("Eurostat_cit_data_", Sys.Date(), ".csv"), row.names = FALSE)
# d1 <- read.csv("Eurostat_cit_data_2023-03-01.csv", stringsAsFactors = FALSE)

#plot with totals
d2 <- d1 %>%
  filter(geo != 'UK' & geo != 'EU27_2020') %>%
  filter(age == "TOTAL") %>%
  filter(agedef == "COMPLET") %>%
  filter(unit == "NR") %>%
  filter(sex == "T") %>%
  filter(citizen == "UK") %>% 
  dplyr::select(iso2_d = geo, time, cit_acq = values, iso2_o = citizen, sex) %>% 
  mutate(year = format(time, format="%Y")) %>% 
  filter(year > 2009) %>%
  mutate(brexit = ifelse(year > 2015, 1, 
                         ifelse(year < 2016, 0, NA))) %>%
  group_by(year, brexit) %>% 
  summarise(total = sum(cit_acq))
View(d2)

#calculate total
total_pre_post <- d2 %>%
  group_by(brexit) %>% 
  summarise(total_pre = sum(total)) 
total_pre_post 

#brexit total_pre
#   0     16263
#   1    101517

#plot
fig1 <- d2 %>% 
  ggplot(aes(x=year, y=total, fill = brexit)) + 
  geom_bar(stat = "identity")+
  theme_minimal()+
  geom_vline(xintercept = 2016, linetype="dotted", size = 0.8) +  
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  geom_text(aes(label = total), vjust = -0.2, colour = "black")+
  geom_vline(aes(xintercept = 7)) +
  geom_text(aes(x = 5.8, y = 32000, label = "Brexit referendum"), hjust = 0.2) +
  labs(title = "Over a hundred thousand British nationals acquired citizenship elsewhere in Europe in the 6 years since the Brexit referendum",
       subtitle = "(and less than twenty thousand did so in the 6 years before)",
       caption = "Data source: Eurostat [migr_acq], 01-03-2023 | @maartenpvink")
fig1

#Save as jpeg file
jpeg("Fig_citaq_UK_total.jpeg", width=12, height=8, units="in", res=500)
fig1
dev.off()

#make plot for selected countries

#select EU countries and associated countries Iceland, Norway, Switzerland
#remove countries with missing data in various years: BG, EE, HR, LI, RO
EUplus <- c("AT","BE","CY", "CZ","DK","FI","FR","DE","EL","HU","IE","IT",
        "LV","LU","MT","NL","PL","PT","SK", "SI","ES","SE",
        "CH", "IS","NO")
EUplus

#select data on UK nationals cit acq in selected countries 
d3 <- d1 %>%
  filter(geo %in% EUplus) %>% #remove countries with substantial missing data: BG, EE, HR, LT, RO
  filter(age == "TOTAL") %>%
  filter(agedef == "COMPLET") %>%
  filter(unit == "NR") %>%
  filter(sex == "T") %>%
  filter(citizen == "UK") %>% 
  dplyr::select(iso2_d = geo, time, cit_acq = values, iso2_o = citizen, sex) %>% 
  mutate(year = format(time, format="%Y")) %>% 
  filter(year > 2009) %>%
#  mutate(year = as.numeric(time)) %>%
  mutate(brexit = ifelse(year > 2015, 1, 
                         ifelse(year < 2016, 0, NA)))
View(d3)

# Export dataset
# write.csv(d3, file = paste0("Eurostat_cit_UK_data_", Sys.Date(), ".csv"), row.names = FALSE)
# import UK data
# d3 <- read.csv("Eurostat_cit_UK_data_2023-03-01.csv", stringsAsFactors = FALSE)

#calculate top3 post-brexit citizenship granting countries by citizenship granting country
d3a <- d1 %>%
  filter(geo != 'UK' & geo != 'EU27_2020') %>%
  filter(age == "TOTAL") %>%
  filter(agedef == "COMPLET") %>%
  filter(unit == "NR") %>%
  filter(sex == "T") %>%
  filter(citizen == "UK") %>% 
  select(iso2_d = geo, time, cit_acq = values, iso2_o = citizen, sex) %>% 
  mutate(year = format(time, format="%Y")) %>% 
  filter(year > 2009) %>%
  mutate(brexit = ifelse(year > 2015, 1, 
                         ifelse(year < 2016, 0, NA))) %>%
  group_by(iso2_d, brexit) %>% 
  summarise(total = sum(cit_acq))
View(d3a)

top3 <- d3a %>%
  filter(brexit==1) %>% 
  filter(total > 10000)
top3 
#iso2_d brexit total
#DE          1 36133
#FR          1 12915
#SE          1 11317
# 59.5 percent of all cit acq in 6 years from 2016 are in these 3 countries

#plot
fig2 <- d3 %>% 
  ggplot(aes(x=time, y=cit_acq, fill = brexit)) + 
  geom_bar(stat = "identity")+
  facet_wrap(~iso2_d, scales = "free")+
  theme_minimal()+
  geom_vline(xintercept = 2016, linetype="dotted", size = 0.8) +  
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  labs(title = "60 percent of 100k post-Brexit referendum naturalisations by Brits in 3 countries: Germany, France, Sweden",
       subtitle = "(36 thousand in Germany, 13 thousand in France, 11 thousand in Sweden)",
       caption = "Data source: Eurostat [migr_acq], 01-03-2023 | @maartenpvink\n
       (excluded EU countries due to missing data: BG, EE, HR, LT, RO, included associated countries CH, IS, NO)")

#Save as jpeg file
jpeg("Fig_citaq_UK_iso_d.jpeg", width=12, height=8, units="in", res=500)
fig2
dev.off()

#End