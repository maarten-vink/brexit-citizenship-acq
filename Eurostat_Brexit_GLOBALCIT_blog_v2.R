# 2025 update with 2023 Eurostat data: Brexit DID analysis
# Maarten Vink, EUI | @maartenpvink
# This script contains code for a blog post on https://globalcit.eu titled 
# 'The Brexit Naturalisation Effect Eight Years After the Referendum'
# https://globalcit.eu/the-brexit-naturalization-effect-eight-years-after-the-referendum/
# Original published March 6, 2023, update in March 2025
# This script partially builds on adjusted code from https://david-reichel.github.io/posts/citizenship-acquisition-of-brits-in-the-eu/

#start with clean workspace
rm(list=ls(all=TRUE))

library(tidyverse)
library(eurostat) # see also: https://ropengov.github.io/eurostat/articles/eurostat_tutorial.html
library(did)
library(lubridate)

#make variable for 27 EU member states + 3 associated countries 
EU27plus <- c("AT","BE","BG", "CY", "CZ","DK","EE", "FI","FR","DE","EL","HU", "HR", "IE","IT",
              "LV","LU", "LT","MT","NL","PL","PT","RO", "SK", "SI","ES","SE",
              "CH", "IS","NO")
EU27plus

# download data on citizenship acquisitions from Eurostat
# dat <- get_eurostat("migr_acq")
# Export dataset
# write.csv(dat, file = paste0("Eurostat_cit_data_", Sys.Date(), ".csv"), row.names = FALSE)
dat <- read.csv("Eurostat_cit_data_2025-02-28.csv", stringsAsFactors = FALSE) 

# select data
d1 <- dat |>
  filter(geo %in% EU27plus) |>
  filter(age == "TOTAL") |>
  filter(agedef == "COMPLET") |>
  filter(unit == "NR") |>
  filter(sex == "T") |>
  mutate(year = as.numeric(format(as.Date(TIME_PERIOD), format="%Y"))) |>
  filter(year > 2007) |> # 8 years before 2016, 8 years from 2016 (2023)
  mutate(brexit = ifelse(year > 2015, 1, 
                         ifelse(year < 2016, 0, NA))) |>
  select(geo, year, cit_acq = values, citizen, brexit) 
View(d1)

#plot with totals
d2 <- d1 |>
  filter(citizen == "UK") |> 
  group_by(year, brexit) |> 
  summarise(total = sum(cit_acq))
View(d2)

#calculate total
total_pre_post <- d2 |>
  group_by(brexit) |> 
  summarise(total_pre = sum(total)) 
total_pre_post 

#brexit total_pre
#   0     20468
#   1    123819

#plot
fig1 <- d2 |> 
  ggplot(aes(x=year, y=total, fill = brexit)) + 
  geom_bar(stat = "identity")+
  theme_minimal()+
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022))+
  geom_vline(xintercept = 2016, linetype="dotted", size = 0.8) +  
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  theme(text = element_text(size = 15))+
  geom_text(aes(label = total), vjust = -0.2, colour = "black")+
  geom_vline(aes(xintercept = 2016)) +
  geom_text(aes(x = 2016.5, y = 32000, label = "Brexit referendum"), hjust = 0.2) +
  labs(title = "Over a 120 thousand British nationals acquired citizenship elsewhere in Europe since 2016",
       subtitle = "But we're well over the peak of 2019",
       caption = "Data source: Eurostat [migr_acq], 28-02-2025 | @maartenpvink.bsky.social")
fig1

#Save as jpeg file
jpeg("Fig1_citaq_UK_total.v2.jpeg", width=13, height=8, units="in", res=200)
fig1
dev.off()

#make plot for selected countries

#select data on UK nationals cit acq in European countries 
#calculate top3 post-brexit citizenship granting countries by citizenship granting country
d2a <- d1 |>
  filter(citizen == "UK") |> 
  group_by(geo, brexit) |> 
  summarise(total = sum(cit_acq))
View(d2a)

top3 <- d2a |>
  filter(brexit==1) |>
  group_by(brexit) |>
  mutate(total_uk_postref = sum(total)) |>
  ungroup() |>
  group_by(geo) |>
  mutate(total_country = sum(total)) |>
  mutate(share_country = (total_country / total_uk_postref) *100) |>
  ungroup() |>
  arrange(desc(total)) 
top3 
#geo brexit total
#DE          1 36888 (32%)
#FR          1 20966 (17%)
#SE          1 11384 (10%)
# 55 percent of all cit acq in 8 years from 2016 are in these 3 countries

#plot
fig2 <- d1 |> 
  filter(citizen == "UK") |> 
  ggplot(aes(x=year, y=cit_acq, fill = brexit)) + 
  geom_bar(stat = "identity")+
  facet_wrap(~geo, scales = "free")+
  theme_minimal()+
  geom_vline(xintercept = 2016, linetype="dotted", size = 0.8) +  
  ylab("")+
  xlab("")+
  theme(legend.position = "none")+
  theme(text = element_text(size = 15))+
  theme(plot.title = element_text(face = "bold"))+
  labs(title = "55 percent of 120k post-Brexit referendum naturalisations by British in 3 countries: Germany, France, Sweden",
       subtitle = "(37 thousand in Germany, 19 thousand in France, 11 thousand in Sweden)",
       caption = "Data source: Eurostat [migr_acq], 28-02-2025 | @maartenpvink.bsky.social")
fig2

#Save as jpeg file
jpeg("Fig2_citaq_UK_iso_d.v2.jpeg", width=14, height=8, units="in", res=500)
fig2
dev.off()


# Compare trend of naturalisations by British v Europeans from other countries
# Figure 3

#plot with totals
d3 <- d1 |>
  filter(citizen %in% EU27plus | citizen == 'UK') |>
  transform(country = factor(citizen, labels = c(1:31))) |>
  mutate(treatment = ifelse(citizen == 'UK', 2016, 0)) |> 
  group_by(citizen, year, brexit, treatment, country) |> 
  summarise(total = as.numeric(sum(cit_acq)))
View(d3)

# save all descriptive data 
require(openxlsx)
list_of_datasets <- list("Country-year-origin-full-data" = d1, "Year-totals-UK-cits-pre-post" = d2,
                         "Country-year-European-origin" = d3)
write.xlsx(list_of_datasets, file = "Eurostat_Brexit_Effect_Citizenship.xlsx")



d3$country <- as.numeric(as.character(d3$country))
class(d3$country)

#descriptive plot
d3a <- d3 |>
  group_by(year, treatment) |>
  summarise(citacq = sum(total)) # calculate cit acq by UK and rest of Europe
View(d3a)

d3a <- d3a |>
  mutate(citacq_c = ifelse(treatment == 0, citacq/30,
                           citacq)) #calculate mean cit acq for rest of Europe
View(d3a)

p1 <- d3a |> 
  ggplot(aes(x=year, y=citacq_c, group=treatment, colour=treatment))+
  geom_line()+
  theme_minimal()+
  geom_vline(xintercept = 2016, linetype="dotted", size = 0.8) +  
  geom_vline(xintercept = 2020, linetype="dotted", size = 0.8) +  
  ylab("")+
  xlab("")+
  scale_x_continuous(breaks=seq(2008,2023,1))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  geom_label(label="UK nationals", 
    x=2021,
    y=17500,
    label.size = 0.35,
    color = "black",
    fill="#69b3a2")+
  geom_label(label="Nationals from rest of Europe", 
             x=2017.5,
             y=5000,
             label.size = 0.35,
             color = "black",
             fill="#69b3a2")+
  geom_label(label="Brexit referendum", 
             x=2016.8,
             y=30000,
             label.size = 0.25,
             color = "black")+
  geom_label(label="Britain leaves EU", 
             x=2020.8,
             y=30000,
             label.size = 0.25,
             color = "black")+
  labs(title = "Annual citizenship acquisitions in European countries by UK nationals and nationals from rest of Europe*",
       subtitle = "*27 EU member states + Iceland, Norway, Switzerland, 2008-2023",
       caption = "Data source: Eurostat [migr_acq], 28-02-2025 | @maartenpvink.bsky.social")
p1

#Save as jpeg file
jpeg("Fig3a.brexit.v2.jpeg", width=12, height=6, units="in", res=200)
p1
dev.off()

#alternative plot with polynomial
p1b <- d3 |> 
  ggplot(aes(x = year, y = total, color = treatment)) +
  geom_point(size = 1, color = 'lightgrey')+
  theme_minimal()+
  geom_smooth(aes(group=treatment), method = "lm", formula = y ~ poly(x, 4))+
  geom_vline(xintercept = 2016, linetype="dotted", size = 0.8) + 
  geom_vline(xintercept = 2020, linetype="dotted", size = 0.8) +  
  ylab("")+
  xlab("")+
  scale_x_continuous(breaks=seq(2008,2023,1))+
  theme(legend.position = "none")+
  theme(text = element_text(size = 15))+
  theme(plot.title = element_text(face = "bold"))+
  geom_label(label="UK nationals", 
             x=2021,
             y=17500,
             label.size = 0.35,
             color = "black",
             fill="#69b3a2")+
  geom_label(label="Nationals from rest of Europe", 
             x=2018.5,
             y=7000,
             label.size = 0.35,
             color = "black",
             fill="#69b3a2")+
  geom_label(label="Brexit referendum", 
             x=2016.8,
             y=30000,
             label.size = 0.25,
             color = "black")+
  geom_label(label="Britain leaves EU", 
             x=2020.8,
             y=30000,
             label.size = 0.25,
             color = "black")+
  labs(title = "Citizenship acquisitions in other European countries by UK nationals and nationals from rest of Europe*",
       subtitle = "*27 EU member states + Iceland, Norway, Switzerland, 2008-2023",
       caption = "Data source: Eurostat [migr_acq], 28-02-2025 | @maartenpvink.bsk.social")
p1b

#Save as jpeg file
jpeg("Fig3.brexit.v2.descr.jpeg", width=14, height=6, units="in", res=200)
p1b
dev.off()

#did
attgt <- att_gt(yname = "total",
                        tname = "year",
                        idname = "country",
                        gname = "treatment",
                        data = d3)
# Warning message:
#   In pre_process_did(yname = yname, tname = tname, idname = idname,  :
#                        Be aware that there are some small groups in your dataset.
#                      Check groups: 2016.
               
summary(attgt)

p2 <- ggdid(attgt, theming = TRUE, grtitle ="Brexit referendum")+
  theme_minimal()+
  geom_vline(xintercept = 7.5, linetype="dotted", size = 0.8) +  
  ylab("Average Treatment Effect on the Treated (ATT)\n")+
  xlab("")+
  scale_y_continuous(breaks=seq(0,30000,5000))+
  theme(legend.position = "none")+
  theme(text = element_text(size = 15))+
  theme(plot.title = element_text(face = "bold"))+
  labs(title = "Effect of Brexit Referendum on Citizenship Acquisitions by UK nationals in Europe",
       subtitle = "Estimated difference in naturalisations after the Brexit referendum | Comparison group: nationals from 30 other European countries",
       caption = "Data source: Eurostat [migr_acq], 26-02-2025 | Analysis with did package by Callaway & Sant'Anna (2021) | @maartenpvink.bsky.social")
p2
#Save as jpeg file
jpeg("Fig4. brexit.did.att.v2.jpeg", width=14, height=6, units="in", res=200)
p2
dev.off()


agg.gs <- aggte(attgt, type = "group")
summary(agg.gs)
# Overall summary of ATT's based on group/cohort aggregation:  
#       ATT    Std. Error     [ 95%  Conf. Int.]  
#  11643.22      174.4343   11301.34    11985.11 *


# Since we have 8 post-treatment years, this means that 8 * 11.643 =  93.144 more British nationals have acquired European citizenship than would have done without Brexit 

# plot overall effect
p3 <- ggdid(agg.gs, theming = TRUE)+
  theme_minimal()+
  ylab("")+
  xlab("ATT")+
  scale_x_continuous(breaks=seq(0,20000,5000))+
  theme(legend.position = "none")+
  theme(plot.title = element_text(face = "bold"))+
  labs(title = "Effect of Brexit Referendum on Citizenship Acquisitions by UK nationals in rest of Europe",
       subtitle = "Estimated additional number of post-referendum naturalisations compared to no-Brexit counterfactual | Comparison group: nationals from 30 other European countries",
       caption = "Data source: Eurostat [migr_acq], 28-02-2025 | Analysis with did package by Callaway & Sant'Anna (2021) | @maartenpvink.bsky.social")
             
p3
#Save as jpeg file
jpeg("Fig5.brexit.did.v3.jpeg", width=12, height=6, units="in", res=200)
p3
dev.off()
#End
