library(tidyverse)
library(zoo)

rm(list=ls())
#-------------------------------------------------------------------------------
funda_raw <- read_csv("raw_data/funda.csv")
fundq_raw <- read_csv("raw_data/fundq.csv")

monthlysf_raw <- read_csv("raw_data/monthlysf.csv")
shrcds_raw <- read_csv("raw_data/shrcds.csv")
gnp_raw <- read_csv("raw_data/gnpdef.csv")

#-------------------------------------------------------------------------------
### Make monthly values from QUARTERLY GNP data with interpolation
gnp <- tibble(date = seq(as.Date("1968/1/1"), as.Date("2021/1/1"), "month"))
gnp <- left_join(gnp, gnp_raw, by=c('date'='DATE')) %>%
  mutate(GNPDEF = na.approx(GNPDEF, na.rm=F),
         gnp = GNPDEF/19.274*100,
         datadate = date-1)

#-------------------------------------------------------------------------------
###### Financial statements YEARLY: 
# Consolidate restatements
funda <- funda_raw %>%
  mutate(across(ebit:ncol(funda_raw), ~ .*1000000),
         emp = emp * 1000) %>% # scale all financial accounting variables
  group_by(gvkey, datadate) %>% 
  fill(emp:ncol(funda_raw)) %>%
  ungroup()
funda <- funda[!duplicated(funda[c('gvkey','datadate')], fromLast = T),]

# Complement with GNP data
funda <- left_join(funda, gnp[,3:4], by='datadate') #gnp for most recent months in 2021 missing
  nrow(funda %>% distinct(gvkey, datadate)) == nrow(funda)
  rm(funda_raw, gnp_raw)
  
#-------------------------------------------------------------------------------
##### Financial statements QUARTERLY: 
# Adjust scale from millions, colsolidate restatements (that are not yet)
fundq <- fundq_raw %>%
  mutate(across(oibdpq:ncol(fundq_raw), ~ .*1000000)) %>%
  group_by(gvkey, datadate) %>% 
  fill(oibdpq:ncol(fundq_raw)) %>%
  ungroup() 
fundq <- fundq[!duplicated(fundq[c('gvkey','datadate')], fromLast = T),]

# Drop missing fiscal quarters, split cumulative variables into quarterly values (!! revise missing quarterly values, possibly due to change in fiscal year end)
fundq <- fundq %>%
  filter(!is.na(fqtr)) %>%
  group_by(gvkey, fyearq) %>% 
  mutate(across(ibcy:fopoy, ~ if_else(fqtr==1, .x,.x-lag(.x)))) %>%  # split cumulative variables into quarterly values
  ungroup()

# Complement with GNP data
fundq <- left_join(fundq, gnp[,3:4], by='datadate')
  nrow(fundq %>% distinct(gvkey,datadate)) == nrow(fundq)
  rm(fundq_raw, gnp)

# Fill missing quarterly data
fundq <- fundq %>%
  mutate(teqq = if_else(!is.na(teqq),teqq, ceqq),
         teqq = if_else(!is.na(teqq), teqq, atq-ltq),
         mibq = if_else(!is.na(mibq), mibq, 0)) %>%
  select(-ceqq)

# Fill missing quarterly data with yearly values either by uniform distribution (income statements) or interpolation (balance sheet items)
fundq <- left_join(fundq, funda %>% select(datadate, gvkey, txdb), by = c('datadate','gvkey')) 
fundq <- left_join(fundq, funda %>% select(fyear, gvkey, xint, oibdp), by = c('gvkey', 'fyearq'='fyear')) %>%
  group_by(gvkey) %>%
  mutate(txdb = if_else(datadate == min(datadate) & datadate >= '1990-01-01', lead(txdb,3)/4, txdb),
         txdb = round(na.approx(txdb, na.rm=F),0),
         txdbq = if_else(!is.na(txdbq), txdbq, txdb),
         xintq = if_else(!is.na(xintq), xintq, xint/4),
         oibdpq = if_else(!is.na(oibdpq), oibdpq, oibdp/4)) %>%
  select(-xint, -txdb, -oibdp)


### Create dataset containing all unique firms existing in Compustat data (yearly and quarterly financial statements)
comp_firms <- inner_join(distinct(funda %>% arrange(desc(fyear)), gvkey, .keep_all = T),
                         distinct(fundq, gvkey, .keep_all = T),
                         by=c('gvkey','tic','cusip')) %>%
  select(gvkey:naics,fyr=fyr.x)

#-------------------------------------------------------------------------------
##### MONTHLY market data
# Create time spans when firms share had code 10 or 11
shrcds <- shrcds_raw %>% 
  group_by(permno) %>% 
  mutate(first = min(namedt), last = max(nameenddt)) %>% 
  select(permno, first, last) %>% 
  distinct(permno, .keep_all = T) %>% 
  mutate(last = if_else(last =='2020-12-31', as.Date('2021-12-31'), last))
  
# Clean monthly stock data
monthlysf_raw <- left_join(monthlysf_raw, shrcds, by='permno') %>% 
  filter(date >= first & date <= last)

doubles <- monthlysf_raw %>% distinct(permno, gvkey)
doubles <- doubles[duplicated(doubles$permno) | duplicated(doubles$gvkey),]
monthlysf_raw <- filter(monthlysf_raw, !(permno %in% doubles$permno | gvkey %in% doubles$gvkey))

  nrow(monthlysf_raw %>% distinct(date, permno)) == nrow(monthlysf_raw)

# Adjust scale from thousands
monthlysf_raw <- monthlysf_raw %>%
  mutate(shrout = shrout * 1000,
         totshrout = totshrout * 1000,
         totval = totval * 1000)

# Create dataset containing all unique firms existing in CRSP data (the market data)
crsp_firms <- distinct(monthlysf_raw, gvkey, .keep_all = T) %>% 
  select(permco, permno, gvkey, cusip) # 12234 firms

#-------------------------------------------------------------------------------
### Create master firm dataset with unique firms existing in all datasets and complement with additional information
firms <- inner_join(comp_firms, crsp_firms, by='gvkey') %>%  
  mutate(cusip.x = substr(cusip.x,1,nchar(cusip.x)-1),
         cusip = if_else(cusip.x==cusip.y,0,1)) 

firms <- inner_join(firms, shrcds, by='permno')

  rm(doubles, comp_firms, crsp_firms, shrcds_raw, shrcds)

#-------------------------------------------------------------------------------
### Filter observations from firms existing in all dataset
funda <- filter(funda, gvkey %in% firms$gvkey) %>% select(-cusip,-(loc:naics))
fundq <- filter(fundq, gvkey %in% firms$gvkey) %>% select(-cusip)
monthlysf <- filter(monthlysf_raw, gvkey %in% firms$gvkey) %>% select(-permco,-(cusip:linkprim), -first, -last)

### Write datasets to local
write_csv(funda, 'data/funda.csv')
write_csv(fundq, 'data/fundq.csv')
write_csv(monthlysf, 'data/monthlysf.csv')
write_csv(firms, 'data/firms.csv')

