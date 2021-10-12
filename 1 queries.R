library(RPostgres)
library(tidyverse)
library(data.table)
library(zoo)
library(tictoc)

#-------------------------------------------------------------------------------
### Establish connection to WRDS database
wrds <- dbConnect(Postgres(),
                  host='wrds-pgdata.wharton.upenn.edu',
                  port=9737,
                  dbname='wrds',
                  sslmode='require',
                  user='usernameexample',
                  password='passwordexample') # use your own wharton login data

rm(list=setdiff(ls(), "wrds"))

#-------------------------------------------------------------------------------
### Download Compustat YEARLY financial statement variables
res <- dbSendQuery(wrds, "select a.datadate, a.fyr, a.fyear, a.gvkey, a.cusip, a.tic, a.conm, b.loc, b.fic, b.gsector, b.ggroup, b.gind, b.gsubind, b.naics, a.emp,
                   a.ebit, a.oibdp, a.oiadp, a.xint, a.at, a.act, a.lt, a.dltt, a.dlc, a.lct, a.wcap, a.ni, a.ibc, a.dpc, a.xidoc, a.txdc, a.esubc, 
                   a.sppiv, a.fopo,  a.seq, a.txdb, a.txt, a.pi, a.icapt, a.xrd, a.xsga
                   from comp.funda a join comp.company b
                   on a.gvkey = b.gvkey
                   where b.gsector in ('10','15','20','25','30','35','45','50','55')
                   and a.datadate >= '1989-01-01'
                   and indfmt = 'INDL'
                   and popsrc ='D'
                   and consol ='C'
                   and curcd ='USD'
                   order by a.gvkey, a.datadate
                   ")
funda <- dbFetch(res, n=-1)
dbClearResult(res)

write_csv(funda, 'raw_data/funda.csv')

#-------------------------------------------------------------------------------
### Download Compustat QUARTERLY financial statement variables
res <- dbSendQuery(wrds, "select a.datadate, a.fyr, a.fyearq, a.fqtr, a.gvkey, a.cusip, a.tic, a.oibdpq, a.oiadpq, a.xintq, a.atq, a.actq,
                   a.ltq, a.dlttq, a.dlcq, a.lctq, a.mibq, a.wcapq, a.niq, a.cheq, a.ibcy, a.dpcy, a.xidocy, a.txdcy, a.esubcy, a.sppivy,
                   a.fopoy, a.teqq, a.ceqq, a.txditcq, a.seqq, a.txdbq, a.txtq, a.piq, a.icaptq, a.xrdq, a.xsgaq
                   from comp.fundq a join comp.company b
                   on a.gvkey = b.gvkey
                   where b.gsector in ('10','15','20','25','30','35','45','50','55')
                   and a.datadate >= '1989-01-01'
                   and indfmt = 'INDL'
                   and popsrc ='D'
                   and consol ='C'
                   order by a.gvkey, a.datadate
                   ")
fundq <- dbFetch(res, n=-1)
dbClearResult(res)

write_csv(fundq, 'raw_data/fundq.csv')

#-------------------------------------------------------------------------------
### Download DAILY SP500 index market data
res <- dbSendQuery(wrds, "select *
                   from crsp.dsp500
                   where caldt between '1989-01-01'
                   and '2021-12-31'
                   order by caldt")
daily_sp500 <- setDT(dbFetch(res, n=-1))
dbClearResult(res)

### Download DAILY market data
res <- dbSendQuery(wrds, "select c.date, c.permno, c.prc, c.shrout, c.ret
                   from crsp.dsf c join crsp.ccmxpf_linktable d
                   on c.permno = d.lpermno
                   where d.gvkey in 
                                   (select distinct a.gvkey
                                   from comp.funda a join comp.company b
                                   on a.gvkey = b.gvkey
                                   where b.gsector in ('10','15','20')
                                   and a.datadate >= '1990-01-01'
                                   and indfmt = 'INDL'
                                   and popsrc ='D'
                                   and consol ='C'
                                   and curcd ='USD')
                   and c.date between '1989-01-01'
                   and '2021-12-31'
                   and linktype in ('LU','LC')
                   and linkprim in ('P','C')
                   order by c.permno, c.date")
dailysf <- setDT(dbFetch(res, n=-1))
dbClearResult(res)

### Combine daily market and index data, wrangle and then calculate beta
dailysf <- unique(dailysf, by = c('permno', 'date'))
dailysf <- dailysf[daily_sp500, on=c(date = "caldt")][order(permno, date)]
dailysf <- na.omit(dailysf, cols=c('ret','vwretd','prc','shrout'))
dailysf <- dailysf[, c("date","permno", "prc","shrout","ret","vwretd")]

dailysf[, ym := as.yearmon(date)]
dailysf[, widthy := seq_along(ym) - findInterval(ym- 1, ym)-1, by = 'permno']
dailysf[, applyto := fifelse(date == max(date), TRUE, FALSE), by = c('permno','ym')]

z <- which(dailysf$applyto == T)
dailysf$beta <- NA

for (i in z) {
  tic(paste('Index is',i))
  y <- dailysf[[i,8]]
  dailysf[[i,10]] <- cov(dailysf[(i-y):i, 5], dailysf[(i-y):i, 6]) / var(dailysf[(i-y):i,6])
  toc()
}

### Compute monthly sums of daily values for later averages (hence also include n)
dailyres <- dailysf %>%
  group_by(permno, ym) %>%
  mutate(totprc = sum(abs(prc)),
         totshrout = sum(shrout),
         totretsqr = sum(ret^2),
         n = n()) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  select(date, permno, beta, totprc, totshrout, totretsqr, n)
  
write_csv(dailyres, 'raw_data/dailyres.csv')


### Download MONTHLY market data
res <- dbSendQuery(wrds, "select c.date, c.permno, c.permco, d.gvkey, c.cusip, d.linktype, d.linkprim, c.prc, c.shrout, c.ret
                   from crsp.msf c join crsp.ccmxpf_linktable d on c.permno = d.lpermno
                   where d.gvkey in 
                                   (select distinct a.gvkey
                                   from comp.funda a join comp.company b
                                   on a.gvkey = b.gvkey
                                   where b.gsector in ('10','15','20','25','30','35','45','50','55')
                                   and a.datadate >= '1990-01-01'
                                   and indfmt = 'INDL'
                                   and popsrc ='D'
                                   and consol ='C'
                                   and curcd ='USD')
                   and c.date between '1989-01-01'
                   and '2021-12-31'
                   and linktype in ('LU','LC')
                   and linkprim in ('P','C')
                   order by c.permno, c.date")
monthlysf_raw <- dbFetch(res, n=-1)
dbClearResult(res)

### Wrangle MONTHLY data, combine with monthly sums of daily data
monthlysf_raw <- monthlysf_raw %>%                                  #filter out duplicated due to doubled comp-crsp link,
  distinct(permno, date, .keep_all = T)                             # see test <- monthlysf_raw %>% group_by(permno, date) %>% filter(n()>1)

monthlysf <- left_join(monthlysf_raw, dailyres, by=c('permno','date'))

### Combine MONTHLY market data withh MONTHLY SP500 index data
res <- dbSendQuery(wrds, "select *
                   from crsp.msp500
                   where caldt between '1959-01-01'
                   and '2021-12-31'
                   order by caldt")
monthly_sp500 <- dbFetch(res, n=-1)
dbClearResult(res)

monthlysf <- left_join(monthlysf, monthly_sp500 %>% select(caldt,vwretd,totval,usdval), by=c('date'='caldt'))

write_csv(monthlysf, 'raw_data/monthlysf.csv')
write_csv(monthly_sp500, 'raw_data/monthlysp500.csv')

#-------------------------------------------------------------------------------
### Download share code dataset and filter for firms with share code 10 or 11
res <- dbSendQuery(wrds, "select c.*
                   from crsp.stocknames c join crsp.ccmxpf_linktable d 
                   on c.permno = d.lpermno
                   where d.gvkey in 
                                   (select distinct a.gvkey
                                   from comp.funda a join comp.company b
                                   on a.gvkey = b.gvkey
                                   where b.gsector in ('10','15','20','25','30','35','45','50','55')
                                   and a.datadate >= '1990-01-01'
                                   and indfmt = 'INDL'
                                   and popsrc ='D'
                                   and consol ='C'
                                   and curcd ='USD')
                   and shrcd in (10,11)
                   and linktype in ('LU','LC')
                   and linkprim in ('P','C')
                   order by c.permno, c.nameenddt")
shrcds <- dbFetch(res, n=-1)
dbClearResult(res)

write_csv(shrcds, 'raw_data/shrcds.csv')
#-------------------------------------------------------------------------------




