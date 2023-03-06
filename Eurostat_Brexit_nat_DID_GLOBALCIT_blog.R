# 2023 Eurostat data: Brexit DID analysis
# Maarten Vink, EUI | @maartenpvink
# This script contains code for a blog post on https://globalcit.eu titled 'The Brexit Naturalisation Effect: Six Years After the Referendum'
# March 2023
# DiD analysis (Figures 3 and 4)

#start with clean workspace
rm(list=ls(all=TRUE))

library(tidyverse)
library(eurostat)
library(did)
library(lubridate)

# download data on citizenship acquisitions from Eurostat
# d1 <- get_eurostat("migr_acq")
# Export dataset
# write.csv(d1, file = paste0("Eurostat_cit_data_", Sys.Date(), ".csv"), row.names = FALSE)
d1 <- read.csv("Eurostat_cit_data_2023-03-01.csv", stringsAsFactors = FALSE)
View(d1)

#make variable for 28 EU member states + 3 associated countries
EUplus <- c("AT","BE","CY", "CZ","DK","FI","FR","DE","EL","HU","IE","IT",
            "LV","LU","MT","NL","PL","PT","SK", "SI","ES","SE",
            "CH", "IS","NO", "BG", "HR", "LT", "RO", "UK", "EE")
EUplus

#plot with totals
d2 <- d1 %>%
  filter(geo %in% EUplus) %>%
  filter(age == "TOTAL") %>%
  filter(agedef == "COMPLET") %>%
  filter(unit == "NR") %>%
  filter(sex == "T") %>%
  filter(citizen %in% EUplus) %>% 
  select(iso2_o = citizen, time, cit_acq = values, sex) %>% 
  mutate(year = year(time)) %>% 
  transform(country = factor(iso2_o, labels = c(1:31))) %>%
  mutate(treatment = ifelse(iso2_o == 'UK', 2016, 0)) %>% 
  filter(year > 2009) %>%
  mutate(brexit = ifelse(year > 2015, 1, 
                         ifelse(year < 2016, 0, NA))) %>%
  group_by(iso2_o, year, brexit, treatment, country) %>% 
  summarise(total = as.numeric(sum(cit_acq)))
View(d2)

d2$country <- as.numeric(as.character(d2$country))
class(d2$country)

#descriptive plot
d3 <- d2 %>%
  group_by(year, treatment) %>%
  summarise(citacq = sum(total)) # calculate cit acq by UK and rest of Europe
View(d2)

d4 <- d3 %>%
  mutate(citacq_c = ifelse(treatment == 0, citacq/30,
                           citacq)) #calculate mean cit acq for rest of Europe
View(d4)

p1 <- d4 %>% 
  ggplot(aes(x=year, y=citacq_c, group=treatment, colour=treatment))+
  geom_line()+
  theme_minimal()+
  geom_vline(xintercept = 2016, linetype="dotted", size = 0.8) +  
  ylab("")+
  xlab("")+
  scale_x_continuous(breaks=seq(2010,2021,1))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  geom_label(label="UK nationals", 
    x=2020,
    y=30000,
    label.size = 0.35,
    color = "black",
    fill="#69b3a2")+
  geom_label(label="Nationals from rest of Europe", 
             x=2019,
             y=7000,
             label.size = 0.35,
             color = "black",
             fill="#69b3a2")+
  geom_label(label="Brexit referendum", 
             x=2016.8,
             y=30000,
             label.size = 0.25,
             color = "black")+
  labs(title = "Number of annual citizenship acquisitions in other European countries by UK nationals and nationals from rest of Europe*",
       subtitle = "*27 EU member states + Iceland, Norway, Switzerland, 2010-2021",
       caption = "Data source: Eurostat [migr_acq], 01-03-2023 | @maartenpvink")
p1

#Save as jpeg file
jpeg("brexit.did.p1.jpeg", width=12, height=6, units="in", res=200)
p1
dev.off()

#alternative plot with polynomial
p1b <- d2 %>% 
  ggplot(aes(x = year, y = total, color = treatment)) +
  geom_point(size = 1)+
  theme_minimal()+
  geom_smooth(aes(group=treatment), method = "lm", formula = y ~ poly(x, 4))+
  geom_vline(xintercept = 2016, linetype="dotted", size = 0.8) +  
  ylab("")+
  xlab("")+
  scale_x_continuous(breaks=seq(2010,2021,1))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  geom_label(label="UK nationals", 
             x=2020,
             y=30000,
             label.size = 0.35,
             color = "black",
             fill="#69b3a2")+
  geom_label(label="Nationals from rest of Europe", 
             x=2019,
             y=7000,
             label.size = 0.35,
             color = "black",
             fill="#69b3a2")+
  geom_label(label="Brexit referendum", 
             x=2016.8,
             y=30000,
             label.size = 0.25,
             color = "black")+
  labs(title = "Figure 3. Number of annual citizenship acquisitions in other European countries by UK nationals and nationals from rest of Europe*",
       subtitle = "*27 EU member states + Iceland, Norway, Switzerland, 2010-2021 | polynomial regression lines indicate trends of two groups",
       caption = "Data source: Eurostat [migr_acq], 01-03-2023 | @maartenpvink")
p1b

#Save as jpeg file
jpeg("Fig3.brexit.did.descr.jpeg", width=12, height=6, units="in", res=200)
p1b
dev.off()

#did
attgt <- att_gt(yname = "total",
                        tname = "year",
                        idname = "country",
                        gname = "treatment",
                        data = d2)
summary(attgt)
p2 <- ggdid(attgt, theming = TRUE)+
  theme_minimal()+
  ylab("")+
  xlab("ATT\n")+
  scale_x_continuous(breaks=seq(2010,2021,1))+
  scale_y_continuous(breaks=seq(0,30000,5000))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  labs(title = "Figure 4. Effect of Brexit Referendum on Citizenship Acquisitions by UK nationals in rest of Europe",
       subtitle = "Average Treatment Effect on the Treated (ATT) | Control group: nationals from 30 other European countries",
       caption = "Data source: Eurostat [migr_acq], 01-03-2023 | Analysis with did package by Callaway & Sant'Anna (2021) | @maartenpvink")
p2
#Save as jpeg file
jpeg("Fig4. brexit.did.att.jpeg", width=10, height=6, units="in", res=200)
p2
dev.off()


agg.gs <- aggte(attgt, type = "group")
summary(agg.gs)
#Overall summary of ATT's based on group/cohort aggregation:  
#      ATT    Std. Error     [ 95%  Conf. Int.]  
# 12984.19       180.649   12630.12    13338.25 *
??ggdid
p3 <- ggdid(agg.gs, theming = TRUE)+
  theme_minimal()+
  ylab("")+
  xlab("ATT")+
  scale_x_continuous(breaks=seq(0,20000,5000))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  labs(title = "Effect of Brexit Referendum on Citizenship Acquisitions by UK nationals in rest of Europe",
       subtitle = "Average Treatment Effect on the Treated (ATT) | Control group: nationals from 30 other European countries",
       caption = "Data source: Eurostat [migr_acq], 01-03-2023 | Analysis with did package by Callaway & Sant'Anna (2021) | @maartenpvink")
             
p3
#Save as jpeg file
jpeg("brexit.did.p3.jpeg", width=12, height=6, units="in", res=200)
p3
dev.off()
#End