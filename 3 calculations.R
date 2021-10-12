library(tidyverse)
library(openxlsx)
library(DescTools)
library(zoo)

rm(list=ls())
#-------------------------------------------------------------------------------
funda_raw <- read_csv("data/funda.csv")
fundq_raw <- read_csv("data/fundq.csv")

monthlysf_raw <- read_csv("data/monthlysf.csv")
monthlysp500 <- read_csv("raw_data/monthlysp500.csv")
yield10t_raw <- read_csv("raw_data/fred10year.csv")

#-------------------------------------------------------------------------------
##### Prepare market data 
  # Fix date for later join with financial statements data (Last trading day in month can differ from last day of month (as in datadate))
dates <- tibble(datadate = seq(as.Date("1989/2/1"), as.Date("2021/1/1"), "month") - 1) %>%
  mutate(ym = as.yearmon(datadate))

monthlysf <- monthlysf_raw %>%
  mutate(ym = as.yearmon(date))

monthlysf <- left_join(monthlysf, dates, by='ym') %>%
  select(datadate, date, everything(), -ym)

  # Calculate YEARLY rolling averages for MONTHLY market data
monthlysf <- monthlysf %>%
  group_by(gvkey) %>%
  mutate(totprcy = rollsum(totprc, 12, align = 'right', fill = NA),
         totshrouty = rollsum(totshrout, 12, align = 'right', fill = NA),
         totny = rollsum(n, 12, align = 'right', fill = NA),
         avgprcy = totprcy/totny,
         avgshrouty = totshrouty/totny,
         totprcm = rollsum(totprc, 3, align = 'right', fill = NA),
         totshroutm = rollsum(totshrout, 3, align = 'right', fill = NA),
         totnm = rollsum(n, 3, align = 'right', fill = NA),
         avgprcm = totprcm/totnm,
         avgshroutm = totshroutm/totnm)

rm(monthlysf_raw, dates)

### Calculate annual return of Sp500 index and then rolling 30 years average (with geometric mean)
avgsp500ret <- monthlysp500 %>%
  select(caldt, vwretd)%>%
  mutate(var = vwretd + 1,
         vwretdy = rollapply(var, 12, prod, fill=NA, align='right'),
         avgret = (rollapply(vwretdy, 360, prod, fill=NA, align='right'))^(1/360)-1) %>%
  filter(caldt >= '1990-01-01') %>%
  add_column(datadate = seq(as.Date("1990/2/1"), as.Date("2021/1/1"), "month") - 1) # Complement with end of month dates

### Clean risk free rate (10 year US treasury return), select last of the month value and complement with end of month dates
yield10t <- yield10t_raw %>%
  mutate(rfrate = as.numeric(DGS10)*0.01,
         ym = as.yearmon(DATE)) %>%
  filter(!is.na(rfrate)) %>%
  group_by(ym) %>%
  filter(DATE <'2021-07-01', DATE == max(DATE)) %>%
  add_column(datadate = seq(as.Date("1989/2/1"), as.Date("2021/7/1"), "month") - 1)

### Combine average market rate and risk free rate into one clean dataset
rates <- left_join(avgsp500ret, yield10t, by='datadate') %>%
  select(datadate, avgret, rfrate)
  rm(yield10t, avgsp500ret, yield10t_raw,monthlysp500)

#-------------------------------------------------------------------------------
##### Calculations in YEARLY dataset:
### Compute O-score variables
funda <- funda_raw %>% 
  group_by(gvkey) %>%
  mutate(icr = oibdp/xint,
         size = log(at/gnp), 
         tlta = lt/at, 
         wcta = wcap/at, 
         clca = lct/act, 
         oeneg = if_else(lt>at,1,0),
         nita = ni/at, 
         intwo = if_else(ni < 0 & lag(ni)<0,1,0), 
         chin = (ni-lag(ni))/(abs(ni)+abs(lag(ni)))) %>%
  rowwise() %>% 
  mutate(futl = sum(ibc,dpc,xidoc,txdc,esubc,sppiv,fopo, na.rm = T)/lt) %>%
  select(-(ibc:fopo)) %>%
  ungroup()
  
  # Compute O-score (perhaps winsorize variables first)
funda <- funda %>%
  mutate(#across(c(size:clca,nita,chin,futl), ~ Winsorize(.x, na.rm = T, probs = c(0.05,0.95))),
      oscore = -1.32-0.407*size+6.03*tlta-1.43*wcta+0.0757*clca-1.72*oeneg-2.37*nita-1.83*futl+0.285*intwo-0.521*chin,
      oprob = exp(oscore)/(1+exp(oscore)))

### Economic Profit:
  # Combine dataset with MONTHLY market data and rates
yearly <- left_join(funda, monthlysf %>% select(datadate, gvkey, beta, avgprcy, avgshrouty), by=c('gvkey','datadate'))
yearly <- left_join(yearly, rates, by='datadate')

  # Compute Economic Profit
yearly <- yearly %>%
  mutate(dbtocap = (dltt+dlc)/(at-seq+avgprcy*avgshrouty-txdb),
         costofeqt = rfrate + (avgret-rfrate)*beta,
         costofdb = if_else(xint==0 | (dltt == 0 & dlc == 0), 0, xint/(dltt + dlc) * (1-(txt/pi))),
         costofdb = if_else(pi == 0, xint/(dltt + dlc), costofdb),
         wacc = costofeqt*(1-dbtocap) + costofdb*dbtocap,
         costofcap = wacc*icapt,
         profit = if_else(is.na(xrd)==F, oibdp + xrd - costofcap, oibdp - costofcap))

write_csv(yearly, 'results/yearly.csv')

#-------------------------------------------------------------------------------
##### Calculations in QUARTERLY dataset:
### Compute ICR and O-score variables
fundq <- fundq_raw %>% 
  group_by(gvkey) %>%
  mutate(icr = oibdpq/xintq, 
         size = log(atq/gnp), 
         tlta = ltq/atq, 
         wcta = wcapq/atq, 
         clca = lctq/actq, 
         oeneg = if_else(ltq>atq,1,0),
         nita = niq*4/atq, 
         intwo = if_else(niq < 0 & lag(niq)<0,1,0), 
         chin = (niq-lag(niq))/(abs(niq)+abs(lag(niq)))) %>%
  rowwise() %>% 
  mutate(futl = sum(ibcy,dpcy,xidocy,txdcy,esubcy,sppivy,fopoy, na.rm = T)/ltq) %>%
  select(-(ibcy:fopoy)) %>%
  ungroup()

  # Compute O-score (perhaps winsorize variables first)
fundq <- fundq %>%
  mutate(#across(c(size:clca,nita,chin,futl), ~ Winsorize(.x, na.rm = T, probs = c(0.05,0.95))),
         oscore = -1.32-0.407*size+6.03*tlta-1.43*wcta+0.0757*clca-1.72*oeneg-2.37*nita-1.83*futl+0.285*intwo-0.521*chin,
         oprob = exp(oscore)/(1+exp(oscore)))

### CHS score calculations:
    # Compute CHS variables based on market data
monthlysf <- monthlysf %>%
  group_by(gvkey) %>%
  mutate(exret = log(1+ret)-log(1+vwretd),
         meq = abs(prc)*shrout,
         prccap = if_else(abs(prc)<=15,log(abs(prc)),log(15)),
         rsize = log(meq/totval),
         exretavg = exret*0.220053+lag(exret,1)*0.174656+lag(exret,2)*0.138625+lag(exret,3)*0.110026+lag(exret,4)*0.087328
         +lag(exret,5)*0.069312+lag(exret,6)*0.055013+lag(exret,7)*0.043664+lag(exret,8)*0.034656+lag(exret,9)*0.027507
         +lag(exret,10)*0.021832+lag(exret,11)*0.017328,
         sigma = sqrt((totretsqr + lag(totretsqr,1)+ lag(totretsqr, 2)) *252/((n+lag(n,1)+lag(n,2))-1)))

  # Complement QUARTERLY financial statements with CHS variables
quarterly <- left_join(fundq, monthlysf %>% select(datadate, gvkey, meq:sigma), by=c('gvkey','datadate'))

  # Compute final CHS variables
quarterly <- quarterly %>% 
  group_by(gvkey) %>%
  mutate(nimta = niq*4/(meq+ltq+mibq),
         nimtaavg = nimta*0.5333+lag(nimta,1)*0.2666+lag(nimta,2)*0.1333+lag(nimta,3)*0.0666,
         tlmta = (ltq+mibq)/(meq+ltq+mibq),
         cashmta = cheq/(meq+ltq+mibq),
         beq = if_else(teqq+txditcq>0, teqq+txditcq, 1),                                          #upstkq to be included
         mb = meq/((beq)+0.1*(meq-(beq)))) %>%
  ungroup() %>%
  select(-beq)

  # Winsorize variables, then compute CHSscore/default probability
quarterly <- quarterly %>%
  mutate(chs = 8.87+20.12*nimtaavg-1.6*tlmta+7.88*exretavg-1.55*sigma+0.005*rsize+2.27*cashmta-0.07*mb+0.09*prccap,
         across(rsize:mb, ~ Winsorize(.x, na.rm = T, probs = c(0.05,0.95))),
         prob = 1/(1+exp(chs)))

### Economic profit calculations:
  # Complement dataset with market variables based on daily data and rates
quarterly <- left_join(quarterly, monthlysf %>% select(datadate, gvkey, beta, avgprcm, avgshroutm), by=c('gvkey','datadate'))
quarterly <- left_join(quarterly, rates, by=c('datadate'))

  # Calculate Economic profit
quarterly <- quarterly %>%
  mutate(dbtocap = if_else(!is.na(txdbq), (dlttq + dlcq) / (atq - seqq + avgprcm * avgshroutm - txdbq), 
                           (dlttq + dlcq) / (atq - seqq + avgprcm * avgshroutm)),
         costofeqt = rfrate + (avgret - rfrate) * beta,
         costofdb = if_else(xintq ==0 | (dlttq==0 & dlcq==0), 0, xintq *4/ (dlttq+dlcq) * (1 - (txtq/piq))),
         costofdb = if_else(piq == 0, xintq/(dlttq + dlcq), costofdb),
         wacc = costofeqt * (1-dbtocap) + costofdb*dbtocap,
         costofcap = wacc * icaptq,
         profit = if_else(!is.na(xrdq), oibdpq*4 + xrdq*4 - costofcap, oibdpq*4 - costofcap))

write_csv(quarterly, 'results/quarterly.csv')
rm(list=setdiff(ls(), c("quarterly",'yearly')))

