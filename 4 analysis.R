library(tidyverse)
library(openxlsx)
library(DescTools)

rm(list=ls())

#------------------------------------------------------------------------------
dataq_raw <- read_csv("results/quarterly.csv")
datay_raw <- read_csv("results/yearly.csv")
firms <- read_csv("data/firms.csv")

# Function to copy paste dataframe (e.g. into excel without writing an excel)
copyto <- function(x, row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#-------------------------------------------------------------------------------
##### Data wrangling anew after calculations are done
# Select observations with correct listing periods and observations after 1989
datay <- left_join(datay_raw, firms %>% select(gvkey, first, last), by='gvkey') %>% 
  filter(datadate >= first & datadate <= last,
         fyear >=1990) %>%
  select(-first,-last)

# Get average employment in last 10 years per firm and add it to firms. Also filter for firms present in yearly dataset (difference due to above time filtering, firms existing only before 1990 or less than 1 year)
emp <- datay %>%
  group_by(gvkey) %>% 
  slice(tail(row_number(), 10)) %>%
  summarise(avgemp = round(mean(emp, na.rm = T),0))

# Add average employment to firm dataset and split firms into two groups
firms <- inner_join(firms, emp, by ='gvkey') %>%
  mutate(group = if_else(avgemp >= mean(avgemp, na.rm =T), 'big', 'small'),
         group = if_else(is.nan(avgemp), 'small', group))

datay <- left_join(datay, firms %>% select(gvkey, group, gsector), by='gvkey') # add group to firms

# Select observations with correct listing periods and observations after 1989 and firms existing in yearly data
dataq <- left_join(dataq_raw, firms %>% select(gvkey, first, last, group, gsector), by='gvkey') %>%
  filter(gvkey %in% firms$gvkey) %>% 
  filter(datadate >= first & datadate <= last,
         fyearq >= 1990) %>%
  select(-first,-last)

rm(list=setdiff(ls(), c('datay', 'dataq','firms','copyto')))

#-------------------------------------------------------------------------------
##### WORKBENCH #####
















#### make stats with plots with and without group ####
statsy <- datay %>%
  filter(!is.na(icr) & !is.na(oscore) & !is.na(profit)) %>% 
  group_by(fyear, gsector) %>% # ! group gsector!
  summarise(n = n(),
            icr_z = sum(icr < 1)/n,
            oscore_z = sum(oscore > 0.5)/n,
            profit_z = sum(profit < 0)/n)

# conditions hold in two years in a row ####
stats2y <- datay %>% 
  select(fyear, gvkey, group, icr, oscore, profit) %>% 
  group_by(gvkey) %>% 
  mutate(icrz =if_else(is.na(icr) | is.na(lag(icr)), NA,   if_else(icr <1 & lag(icr)<1, T ,F)),
         oscorez =if_else(is.na(oscore) | is.na(lag(oscore)), NA,   if_else(oscore > 0.5 & lag(oscore) > 0.5, T ,F)),
         profitz =if_else(is.na(profit) | is.na(lag(profit)), NA,   if_else(profit < 0 & lag(profit)< 0, T ,F))) %>% 
  filter(!is.na(icrz) & !is.na(oscorez) & !is.na(profitz) & fyear != 1990) %>% 
  group_by(fyear) %>% 
  summarise(n=n(),
            icr_z = sum(icrz)/n(),
            oscore_z = sum(oscorez)/n(),
            profit_z = sum(profitz)/n())
#####

statsy %>% 
  ggplot(aes(x=fyear)) +  #! linetype = group !
  geom_line(aes(y = icr_z, color='icr < 1'), size=1)+
  geom_line(aes(y = oscore_z, color='oscore > 0.5'), size=1)+
  geom_line(aes(y = profit_z, color='econ profit < 0'), size=1)+
  geom_point(aes(y = profit_z), color='#70AD47')+
  geom_point(aes(y = icr_z), color='#4F81BD')+
  geom_point(aes(y = oscore_z), color='#C0504D')+
  scale_color_manual(values = c('#70AD47', '#4F81BD','#C0504D'))+
  scale_x_continuous(breaks = seq(1991,2021,2), expand = c(0,0))+ # adjust breaks for 1990-2020 etc.
  labs(y='Share of Zombies', x='Year',
       title = 'Yearly zombie variables over time', subtitle = 'Conditions must hold for two consecutive years')+
  theme_bw()+theme(
    legend.title = element_blank(),
    legend.position = 'bottom'
  )

ggsave('results/yearly_2y.png', width = 12, height=8)

# chart yearly data per sector ####

statsy$gsector <- as.factor(statsy$gsector)
sectornames <- c("Energy", "Materials", "Industrials","Consumer Discretionary",
                 "Consumer Staples", "Health Care","Information Technology", "Communication Services", "Utilities")
names(sectornames) <- levels(statsy$gsector)

statsy %>% 
  ggplot(aes(x=fyear)) +  
  geom_line(aes(y = icr_z, color='ICR < 1'), size=1)+
  geom_line(aes(y = oscore_z, color='Oscore > 0.5'), size=1)+
  geom_line(aes(y = profit_z, color='Profit < 0'), size=1)+
  geom_point(aes(y = icr_z), color='#4F81BD')+
  geom_point(aes(y = oscore_z), color='#C0504D')+
  geom_point(aes(y = profit_z), color='#70AD47')+
  scale_color_manual(values = c('#4F81BD','#C0504D','#70AD47'))+
  scale_x_continuous(breaks = seq(1990,2020,2), expand = c(0,0))+ 
  labs(y='Share of Zombies', x='Year',
       title = 'Yearly zombie variables over time')+
  theme_bw()+theme(
    legend.title = element_blank(),
    legend.position = 'bottom')+
  facet_wrap(~gsector, scales="free",  ncol=3, labeller = labeller(gsector=sectornames))

ggsave('results/yearly_sectors.png', width = 20, height=12)

#####

statsq <- dataq %>%
  filter(!is.na(icr) & !is.na(oscore) & !is.na(profit) & !is.na(chs)) %>% 
  group_by(fyearq, fqtr) %>% # ! group !
  summarise(n = n(),
            icr_z = sum(icr < 1)/n,
            oscore_z = sum(oscore > 0.5)/n,
            chs_z = sum(prob >0.05)/n,
            profit_z = sum(profit < 0)/n) %>% 
  mutate(date = fyearq + fqtr*0.25)


statsq %>% 
  ggplot(aes(x=date)) + # ! linetype = group !
  geom_line(aes(y = icr_z, color='icr < 1'), size=1)+
  geom_line(aes(y = oscore_z, color='oscore > 0.5'), size=1)+
  geom_line(aes(y = chs_z, color='chs > 0.05'), size=1)+
  geom_line(aes(y = profit_z, color='econ profit < 0'), size=1)+
  geom_point(aes(y = icr_z), color='#4F81BD')+
  geom_point(aes(y = oscore_z), color='#C0504D')+
  geom_point(aes(y = profit_z), color='#70AD47')+
  geom_point(aes(y = chs_z), color='#FFC000')+
  scale_color_manual(values = c('#FFC000','#70AD47','#4F81BD','#C0504D'))+
  scale_x_continuous(breaks = seq(1990,2020,2), expand = c(0,0))+
  labs(y='Share of Zombies', x='Year',
       title = 'Quarterly zombie variables over time')+
  theme_bw()+theme(
    legend.title = element_blank(),
    legend.position = 'bottom'
  )

ggsave('results/quarterly.png', width = 12, height=8)
#-------------------------------------------------------------------------------
##### with stronger restrictions #####

stats2q <- dataq %>% 
  select(fyearq, fqtr, gvkey, group, icr, oscore, prob, profit) %>% 
  mutate(icrz =if_else(is.na(icr) | is.na(lag(icr)), NA,   if_else(icr <1 & lag(icr)<1, T ,F)),
         oscorez =if_else(is.na(oscore) | is.na(lag(oscore)), NA,   if_else(oscore > 0.5 & lag(oscore) > 0.5, T ,F)),
         chsz =if_else(is.na(prob) | is.na(lag(prob)), NA,   if_else(prob > 0.05 & lag(prob) > 0.05, T ,F)),
         profitz =if_else(is.na(profit) | is.na(lag(profit)), NA,   if_else(profit < 0 & lag(profit)< 0, T ,F))) %>% 
  filter(!is.na(icrz) & !is.na(oscorez) & !is.na(profitz) & !is.na(chsz)) %>% 
  group_by(fyearq, fqtr) %>% 
  summarise(icr_z = sum(icrz)/n(),
            oscore_z = sum(oscorez)/n(),
            chs_z = sum(chsz)/n(),
            profit_z = sum(profitz)/n()) %>% 
  mutate(date = fyearq + fqtr*0.25)

stats3q <- dataq %>% 
  select(fyearq, fqtr, gvkey, group, icr, oscore, prob, profit) %>% 
  mutate(icrz =if_else(is.na(icr) | is.na(lag(icr)) | is.na(lag(icr, 2)), NA,
                       if_else(icr <1 & lag(icr)<1 & lag(icr,2)<1, T ,F)),
         oscorez =if_else(is.na(oscore) | is.na(lag(oscore)) | is.na(lag(oscore, 2)), NA,
                          if_else(oscore > 0.5 & lag(oscore) > 0.5 & lag(oscore,2) > 0.5, T ,F)),
         chsz =if_else(is.na(prob) | is.na(lag(prob)) | is.na(lag(prob,2)), NA,
                       if_else(prob > 0.05 & lag(prob) > 0.05 & lag(prob,2) > 0.05, T ,F)),
         profitz =if_else(is.na(profit) | is.na(lag(profit)) | is.na(lag(profit,2)), NA,
                          if_else(profit < 0 & lag(profit)< 0 & lag(profit,2)< 0, T ,F))) %>% 
  filter(!is.na(icrz) & !is.na(oscorez) & !is.na(profitz) & !is.na(chsz)) %>% 
  group_by(fyearq, fqtr) %>% 
  summarise(n=n(),
            icr_z = sum(icrz)/n(),
            oscore_z = sum(oscorez)/n(),
            chs_z = sum(chsz)/n(),
            profit_z = sum(profitz)/n()) %>% 
  mutate(date = fyearq + fqtr*0.25)

stats2q %>% 
  ggplot(aes(x=date)) +
  geom_line(aes(y = icr_z, color='icr < 1'), size=1)+
  geom_line(aes(y = oscore_z, color='oscore > 0.5'), size=1)+
  geom_line(aes(y = chs_z, color='chs > 0.05'), size=1)+
  geom_line(aes(y = profit_z, color='econ profit < 0'), size=1)+
  geom_point(aes(y = icr_z), color='#4F81BD')+
  geom_point(aes(y = oscore_z), color='#C0504D')+
  geom_point(aes(y = profit_z), color='#70AD47')+
  geom_point(aes(y = chs_z), color='#FFC000')+
  scale_color_manual(values = c('#FFC000','#70AD47','#4F81BD','#C0504D'))+
  scale_x_continuous(breaks = seq(1990,2020,2), expand = c(0,0))+
  labs(y='Share of Zombies', x='Year')+
  theme_bw()+theme(
    legend.title = element_blank(),
    legend.position = 'bottom'
  )

ggsave('results/quarterly_2Q.png', width = 12, height=8)

### 

test <- datay %>%
  filter(oibdp > 0)


statsy <- datay %>%
  filter(!is.na(icr) & !is.na(oscore) & !is.na(profit)) %>% 
  group_by(fyear) %>% # ! group gsector!
  summarise(n = n(),
            oibdp = sum(oibdp < 0)/n,
            icr_z = sum(icr < 1)/n,
            oscore_z = sum(oscore > 0.5)/n,
            profit_z = sum(profit < 0)/n)

statsy %>% 
  ggplot(aes(x=fyear)) +  #! linetype = group !
  geom_line(aes(y = oibdp, color='oibdp < 0'), size=1)+
  geom_line(aes(y = icr_z, color='icr < 1'), size=1)+
  geom_line(aes(y = oscore_z, color='oscore > 0.5'), size=1)+
  geom_line(aes(y = profit_z, color='econ profit < 0'), size=1)+
  scale_color_manual(values = c('#70AD47', '#4F81BD','black','#C0504D'))+
  scale_x_continuous(breaks = seq(1990,2020,2), expand = c(0,0))+ # adjust breaks for 1990-2020 etc.
  labs(y='Share of Zombies', x='Year',
       title = 'Yearly zombie variables over time', subtitle = 'Only with operating profit > 0')+
  theme_bw()+theme(
    legend.title = element_blank(),
    legend.position = 'bottom'
  )

ggsave('results/yearly_test.png', width = 12, height=8)

















############# Archiv 02.08
### Quarterly evolution of Zombies with different limits
statsq <- dataq %>%
  select(fyearq:gvkey, icr, oscore, chs, prob) %>%
  group_by(gvkey) %>%
  mutate(icrz =if_else(is.na(icr) | is.na(lag(icr)),NA, if_else(icr <1 & lag(icr)<1, T ,F))) %>%
  filter(!is.na(icrz)&!is.na(oscore)&!is.na(chs)) %>%
  group_by(fyearq, fqtr) %>%
  summarise(firms = n(),
            icr_zombie = sum(icrz, na.rm = T)/firms,
            o_zombie1 = sum(oscore > 0.5, na.rm = T)/firms,
            o_zombie2 = sum(oscore > 0.25, na.rm = T)/firms,
            o_zombie3 = sum(oscore > 0.1, na.rm = T)/firms,
            chs_zombie1 = sum(prob > 0.25, na.rm = T)/firms,
            chs_zombie2 = sum(prob > 0.1, na.rm = T)/firms,
            chs_zombie3 = sum(prob > 0.05, na.rm = T)/firms) %>% ungroup()
  

### Yearly evolution of Zombies with different limits (Oscore)
statsa <- dataa %>%
  group_by(gvkey) %>%
  mutate(icrz =if_else(is.na(icr) | is.na(lag(icr)),NA, if_else(icr <1 & lag(icr)<1, T ,F))) %>%
  filter(!is.na(icrz)&!is.na(oscore)) %>%
  group_by(fyear) %>%
  summarise(firms = n(),
            icr_zombie = sum(icrz, na.rm = T)/firms,
            o_zombie1 = sum(oscore > 0.5, na.rm = T)/firms,
            o_zombie2 = sum(oscore > 0.25, na.rm = T)/firms,
            o_zombie3 = sum(oscore > 0.1, na.rm = T)/firms) %>% ungroup()
copyto(statsa)

### Number of firms per sector over time
statsf1 <- left_join(dataa, firms, by='gvkey') %>%
  count(fyear, gsector) %>%
  spread(gsector,n) %>%
  rowwise() %>% 
  mutate(total = sum(`10`,`15`,`20`,`25`,`30`,`35`,`45`,`50`,`55`)) 


### Evolution of Zombies per sector (Q4 of each year)
statsf2 <- left_join(dataq, firms[,c(1,7)], by='gvkey') %>%
  select(fyearq:gvkey, icr, oscore, prob, gsector) %>%
  group_by(gvkey) %>%
  mutate(icrz =if_else(is.na(icr) | is.na(lag(icr)),NA, if_else(icr <1 & lag(icr)<1, T ,F))) %>%
  filter(!is.na(icrz) & !is.na(prob)) %>%
  group_by(fyearq, fqtr, gsector) %>%
  summarise(firms = n(),
            icr_zombie = sum(icrz, na.rm = T)/firms,
            o_zombie = sum(oscore > 0.5, na.rm = T)/firms,
            chs_zombie = sum(prob > 0.05, na.rm = T)/firms) %>% ungroup() %>%
  filter(fqtr==4)

ggplot(statsf2, aes(x=fyearq, color=as_factor(gsector)))+
  geom_line(aes(y=icr_zombie, linetype='icr'), size=1)+
  geom_line(aes(y=chs_zombie,linetype = "chs",), size=1)+
  scale_linetype_manual(values = c('longdash','solid'))+
  scale_x_continuous(breaks = seq(1990,2020,2), expand = c(0,0))+
  scale_y_continuous(breaks = seq(0,0.7,0.1))+
  scale_color_manual(values = c('#4472C4','#ED7D31','#A5A5A5','#FFC000','#5B9BD5','#70AD47','#264478','#9E480E','#636363'))+
  guides(color = guide_legend(override.aes = list(size = 3)))+
  labs(y='Share of Zombies', x=element_blank(), title = 'Share of Zombies per sector', subtitle = 'Q4 of each year')+
  theme_bw()+theme(
    legend.title = element_blank(),
    legend.key.width = unit(1,"cm")
  )
  

ggsave('results/zombies per sector.png', width = 12, height=8)



############ archiv ############

dataa1 <- dataa_raw %>% 
  filter(!is.na(icr)) %>%
  group_by(gvkey) %>%
  mutate(icrz = if_else(icr <1 & lag(icr<1), T ,F)) %>% 
  group_by(fyear) %>% 
  summarise(icr_firms = n(),
            icr_zombie = sum(icrz, na.rm = T))

dataa2 <- dataa_raw %>% 
  filter(!is.na(oscore)) %>%
  group_by(fyear) %>% 
  summarise(o_firms = n(),
            o_zombie1 = sum(oscore > 0.5, na.rm = T),
            o_zombie2 = sum(oscore > 0.25, na.rm = T),
            o_zombie3 = sum(oscore > 0.1, na.rm = T))

statsa <- left_join(dataa1, dataa2, by='fyear')%>% 
  filter(fyear >= 1990)

write.xlsx(statsa, 'test.xlsx')

###

dataq1 <- dataq_raw %>% 
  filter(!is.na(icr)) %>%
  group_by(gvkey) %>%
  mutate(icrz = if_else(icr <1 & lag(icr<1), T ,F)) %>% 
  group_by(fyearq, fqtr) %>% 
  summarise(icr_firms = n(),
            icr_zombie = sum(icrz, na.rm = T)) %>% ungroup()

dataq2 <- dataq_raw %>% 
  filter(!is.na(oscore)) %>%
  group_by(fyearq, fqtr) %>%
  summarise(o_firms = n(),
              o_zombie1 = sum(oscore > 0.5, na.rm = T),
              o_zombie2 = sum(oscore > 0.25, na.rm = T),
              o_zombie3 = sum(oscore > 0.1, na.rm = T)) %>% ungroup()

dataq3 <- dataq_raw %>% 
  filter(!is.na(chs)) %>%
  group_by(fyearq, fqtr) %>%
  summarise(chs_firms = n(),
            chs_zombie1 = sum(prob > 0.25, na.rm = T),
            chs_zombie2 = sum(prob > 0.1, na.rm = T),
            chs_zombie3 = sum(prob > 0.05, na.rm = T)) %>% ungroup()



statsq <- left_join(dataq1, dataq2, by=c('fyearq', 'fqtr')) %>%
  left_join(dataq3, by=c('fyearq', 'fqtr')) %>% 
  filter(fyearq >= 1990)


rm(list=setdiff(ls(), c('statsa', 'statsq', 'dataa_raw', 'dataq_raw')))

write.xlsx(statsa, 'results/zombie stats annual.xlsx')
write.xlsx(statsq, 'results/zombie stats quarterly.xlsx')




###############
dens <- dataq_raw %>%
  filter(fyearq >= 2006 & fqtr ==4)

dens$fyearq <- as_factor(dens$fyearq)
dens$oscore <- Winsorize(dens$oscore, na.rm = T, probs = c(0.01,0.99))

ggplot(dens, aes(x=oscore, color=fyearq)) + 
  geom_density()+
  scale_x_continuous(limits = c(-10,25))
 # geom_vline(xintercept = 1, linetype="dashed")


ggsave('dens oscore Q4 all.png', width=6, height=6)
############


xint <- dataa_raw %>%
  filter(fyear %in% c(2007,2008)) %>%
  group_by(gvkey) %>%
  mutate(xintdiff = xint/lag(xint)) %>%
  filter(!is.na(xintdiff))

ggplot(xint, aes(x=xintdiff)) +
  geom_density()+
  scale_x_continuous(limits = c(-1,5))

ggsave('results/charts/xintdiff.png')




##################

ggplot(dataa, aes(fyear))+
  geom_line(aes(y=icr_perc,color='ICR'), size=1)+
  geom_line(aes(y=o_perc,color='Oscore'), size=1)+
  geom_point(aes(y=icr_perc), color='#4F81BD')+
  geom_point(aes(y=o_perc), color='#C0504D')+
  scale_color_manual(values = c('#4F81BD','#C0504D'))+
  scale_x_continuous(breaks = seq(1990,2020,2), expand = c(0,0))+
  labs(y='Share of Zombies', x='Year')+
  theme_bw()+theme(
    legend.title = element_blank(),
    legend.position = 'bottom'
  )

ggsave('results/plot1.png', width = 10, height=6)




###############

test <- dataq_raw %>%
  filter(!is.na(oscore) & fqtr==1)%>%
  group_by(fyearq) %>%
  summarise(total = n(),
            above1 = sum(oscore >0.5),
            above2 = sum(oscore >0)) %>%
  mutate(perc1 = above1/total,
         perc2 = above2/total)

ggplot(test, aes(fyearq))+
  geom_line(aes(y=perc1,color='>0.5'), size=1)+
  geom_line(aes(y=perc2,color='>0'), size=1)+
  scale_color_manual(values = c('#4F81BD','#C0504D'))


ggsave('oscore above perc2.png', width = 10, height=6)
##################



test <- dataa_raw%>%
  mutate(across(c(size:clca,nita,chin,futl), ~ Winsorize(.x, na.rm = T, probs = c(0.05,0.95)))) %>%
  group_by(fyear)%>%
  summarise(size = sd(size, na.rm = T)/mean(size, na.rm = T),
            tlta = sd(tlta, na.rm = T)/mean(tlta, na.rm = T),
            wcta = sd(wcta, na.rm = T)/mean(wcta, na.rm = T),
            clca = sd(clca, na.rm = T)/mean(clca, na.rm = T),
            nita = sd(nita, na.rm = T)/mean(nita, na.rm = T),
            chin = sd(chin, na.rm = T)/mean(chin, na.rm = T),
            futl = sd(futl, na.rm = T)/mean(futl, na.rm = T)
            )



