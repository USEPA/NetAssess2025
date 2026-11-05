#################################################################
## Update SQL database for the monitoring network assessment tool
## Author: Ben Wells, Statistician, US EPA/OAQPS
## Updated: August 6, 2025
#################################################################
if (dir.exists("C:/")) {
  source("C:/Users/bwells01/Documents/R/get_aqs_data.r")
  setwd("C:/Users/bwells01/Documents/NetAssess/NetAssess-test/")
}
if (dir.exists("/home/")) {
  source("/home/bwells01/R/get_aqs_data.r")
  setwd("/work/YODA/users/bwells01/NetAssess/NetAssess-test/")
}
options(stringsAsFactors=FALSE,warn=-1)
library(data.table); library(DBI); library(plyr); library(RSQLite); library(sf);
db <- dbConnect(SQLite(),dbname="netassess.sqlite") ## Connect to SQLite database
if (!dir.exists("data")) { dir.create("data") } ## Store intermediate data files

###########################################################################
## Step 1: Update Geography Tables using latest available Census data
## type = one of "nation", "state", "county", "cbsa", or "csa"
## year = year of shapefile publication - for latest year available, check
##   https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html
###########################################################################
update.geo <- function(type,year) {
  base.url <- paste("https://www2.census.gov/geo/tiger/GENZ",year,"/shp",sep="")
  file.name <- paste("cb_",year,"_us_",type,"_5m.zip",sep="")
  if (!file.exists(paste("data",file.name,sep="/"))) {
    download.file(url=paste(base.url,file.name,sep="/"),
      destfile=paste("data",file.name,sep="/"),quiet=TRUE)
    unzip(zipfile=paste("data",file.name,sep="/"),exdir="data")
  }
  shp.data <- read.shapefile(paste("data",gsub(".zip","",file.name),sep="/"))
  dbf <- as.data.frame(shp.data$dbf$dbf)
  if (type == "nation") { out <- data.frame(code="US",name="United States") }
  if (type == "state") { out <- data.frame(code=dbf$GEOID,name=dbf$NAME,abbr=dbf$STUSPS) }
  if (type == "county") { out <- data.frame(code=dbf$GEOID,name=dbf$NAME) }
  if (type == "cbsa") { out <- data.frame(code=dbf$GEOID,name=dbf$NAME) }
  if (type == "csa") { out <- data.frame(code=dbf$GEOID,name=dbf$NAME) }
  out$geometry <- NA
  shp <- subset(change.id(convert.to.simple(shp.data$shp),dbf$GEOID),Id %in% out$code)
  shp.ind <- lapply(out$code,function(x) grep(x,shp$Id))
  nparts <- unlist(lapply(shp.data$shp$shp,function(x) x$num.parts))
  parts <- lapply(shp.data$shp$shp,function(x) x$parts+1)
  part.ind <- match(out$code,dbf$GEOID)
  for (i in 1:nrow(out)) {
    t <- shp[shp.ind[[i]],]
    N <- nparts[part.ind[i]]
    p <- parts[[part.ind[i]]]
    temp <- vector("list",N)
    for (j in 1:N) {
      k <- c(p[j]:ifelse(j < N,p[j+1]-1,nrow(t)))
      temp[[j]] <- paste("structure(c(",paste(unlist(c(t$Y[k],t$X[k])),collapse=","),
        "),.Dim=c(",length(k),"L,2L),.Dimnames=list(NULL,c('lat','lng')))",sep="")
    }
    out$geometry[i] <- paste("list(",paste(temp,collapse=","),")",sep="")
  }
  out <- out[order(out$code),]
  table.name <- switch(type,nation="usborder",state="states",
    cbsa="cbsas",csa="csas",county="counties")
  dbWriteTable(db,name=table.name,value=out,row.names=FALSE,overwrite=TRUE)
  file.rm <- list.files("data")[grep(gsub(".zip","",file.name),list.files("data"))]
  unlink(paste("data",file.rm,sep="/"))
  invisible(NULL)
}
update.geo(type="nation",year=2021)
update.geo(type="state",year=2021)
update.geo(type="county",year=2021)
update.geo(type="cbsa",year=2021)
update.geo(type="csa",year=2021)

## Add EPA regions to states table
states <- dbGetQuery(db,"SELECT * FROM states")
temp <- get.aqs.data("SELECT state_code AS code, 
                             TO_NUMBER(epar_epa_region_code,'99') AS epa_region 
                        FROM states
                       WHERE state_code NOT IN ('80','CC')")
states <- merge(states,temp,by="code")[,c(1:3,5,4)]
dbWriteTable(db,name="states",value=states,row.names=FALSE,overwrite=TRUE)

## Add CBSA/CSA assignments to counties table
counties <- dbGetQuery(db,"SELECT * FROM counties")
poly <- sapply(counties$geometry,function(x) eval(parse(text=x)))
cty.shp <- SpatialPolygonsDataFrame(Sr=SpatialPolygons(Srl=mapply(function(poly,id) {
      Polygons(srl=lapply(poly,function(x) Polygon(x[,c("lng","lat")])),ID=id) },
      poly=poly,id=counties$code)),data=as.data.frame(counties[,1:3]),match.ID="code")
cbsas <- dbGetQuery(db,"SELECT * FROM cbsas")
poly <- sapply(cbsas$geometry,function(x) eval(parse(text=x)))
cbsa.shp <- SpatialPolygonsDataFrame(Sr=SpatialPolygons(Srl=mapply(function(poly,id) {
      Polygons(srl=lapply(poly,function(x) Polygon(x[,c("lng","lat")])),ID=id) },
      poly=poly,id=cbsas$code)),data=as.data.frame(cbsas[,1:3]),match.ID="code")
temp <- gWithin(cty.shp,cbsa.shp,byid=TRUE)
cbsa.ind <- apply(temp,2,function(x) ifelse(length(which(x)) == 0,NA,which(x)))
counties$cbsa_name <- sapply(cbsa.ind,function(x) ifelse(is.na(x),"",cbsas$name[x]))
csas <- dbGetQuery(db,"SELECT * FROM csas")
poly <- sapply(csas$geometry,function(x) eval(parse(text=x)))
csa.shp <- SpatialPolygonsDataFrame(Sr=SpatialPolygons(Srl=mapply(function(poly,id) {
      Polygons(srl=lapply(poly,function(x) Polygon(x[,c("lng","lat")])),ID=id) },
      poly=poly,id=csas$code)),data=as.data.frame(csas[,1:3]),match.ID="code")
temp <- gWithin(cty.shp,csa.shp,byid=TRUE)
csa.ind <- apply(temp,2,function(x) ifelse(length(which(x)) == 0,NA,which(x)))
counties$csa_name <- sapply(csa.ind,function(x) ifelse(is.na(x),"",csas$name[x]))
counties <- counties[,c(1,2,4,5,3)]
dbWriteTable(db,name="counties",value=counties,row.names=FALSE,overwrite=TRUE)

#######################################################################################
## Step 2: Update census tract population information - MANUAL
## a) Download latest census tract geographic information from the Census Bureau
##    https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2021_Gazetteer/
##    Extract the .zip file to the current working 'data' directory
## b) Download latest census tract level population data from https://data.census.gov/table/
##    Select "Census Tract" from Geography and "Decennial Census" from Surveys, then table DP1
##    Extract the .zip file to the current working 'data' directory
## c) Run below code to read, merge, and write the tract-level data to the database
########################################################################################
update.population <- function(geo.file,pop.file,prob.file,no2.file) {
  t <- read.delim(paste("data",geo.file,sep="/"),header=TRUE,
    colClasses=c(rep("character",2),rep("numeric",6)))
  geo <- data.frame(CTFIPS=t$GEOID,latitude=round(t$INTPTLAT,6),
    longitude=round(t$INTPTLONG,6),area_km=t$ALAND/1e6)
  h <- unlist(read.csv(paste("data",pop.file,sep="/"),nrow=1))
  t <- read.csv(paste("data",pop.file,sep="/"),header=FALSE,skip=2,colClasses="character")
  col.rm <- union(grep("Percent",h),grep("Annotation",h))
  h <- h[-col.rm]; t <- t[,-col.rm]; n <- nchar(t[1,1]);
  pop <- data.frame(CTFIPS=substr(t[,1],n-10,n),total=as.integer(t[,3]),male=as.integer(t[,27]),
    female=as.integer(t[,51]),white=as.integer(t[,80]),black=as.integer(t[,81]),
    native=as.integer(t[,82]),asian=as.integer(t[,83]),islander=as.integer(t[,84]),
    other=as.integer(t[,85]),multiple=as.integer(t[,86]),hispanic=as.integer(t[,95]),
    age_0_4=as.integer(t[,4]),age_5_9=as.integer(t[,5]),age_10_14=as.integer(t[,6]),
    age_15_19=as.integer(t[,7]),age_20_24=as.integer(t[,8]),age_25_29=as.integer(t[,9]),
    age_30_34=as.integer(t[,10]),age_35_39=as.integer(t[,11]),age_40_44=as.integer(t[,12]),
    age_45_49=as.integer(t[,13]),age_50_54=as.integer(t[,14]),age_55_59=as.integer(t[,15]),
    age_60_64=as.integer(t[,16]),age_65_69=as.integer(t[,17]),age_70_74=as.integer(t[,18]),
    age_75_79=as.integer(t[,19]),age_80_84=as.integer(t[,20]),age_85_up=as.integer(t[,21]))
  load(paste("data",prob.file,sep="/"))
  no2 <- subset(read.csv(paste("data",no2.file,sep="/"),colClasses=c("integer","character",
    "numeric")),year == 2022,c("Ct_Fips","spmod_pred"))
  colnames(no2) <- c("CTFIPS","no2_sat")
  no2$no2_sat <- round(no2$no2_sat,2)
  tracts <- merge(geo,merge(pop,merge(prob.out,no2,by="CTFIPS",all=TRUE),
     by="CTFIPS",all=TRUE),by="CTFIPS",all=TRUE)
  dbWriteTable(db,name="tracts",value=tracts,row.names=FALSE,overwrite=TRUE)
}
update.population(geo.file="2021_Gaz_tracts_national.txt",
  pop.file="DECENNIALDP2020.DP1-Data.csv",
  prob.file="excprob_2019_2021.Rdata",
  no2.file="ct2020avg_spmod_preds.csv")

#########################################################################
## Step 3: Retrieve data from AQS to update the remaining database tables
## Includes tables 'annual','daily','dvs','monitors','sites','standards'
#########################################################################

## Table 'standards' - AQS parameter code/pollutant standards info
download.file(url="https://aqs.epa.gov/aqsweb/documents/codetables/pollutant_standards.csv",
  destfile=paste("data","pollutant_standards.csv",sep="/"),quiet=TRUE)
t <- read.csv("data/pollutant_standards.csv")
t <- t[order(t$Pollutant.Standard.ID),]
naaqs <- data.frame(ps_id=t$Pollutant.Standard.ID,
  naaqs=t$Pollutant.Standard.Short.Name,
  pollutant=sapply(t$Pollutant.Standard.Short.Name,
    function(x) tolower(unlist(strsplit(x," "))[1])),
  poll_name=sapply(t$Pollutant.Standard.Short.Name,
    function(x) gsub("25","2.5",unlist(strsplit(x," "))[1])),
  avg_time=sapply(t$Pollutant.Standard.Short.Name,
    function(x) tolower(unlist(strsplit(x," "))[2])),
  year=sapply(t$Pollutant.Standard.Short.Name,
    function(x) as.numeric(substr(x,nchar(x)-3,nchar(x)))),
  parameter_code=t$Parameter.Code,parameter_desc=t$Parameter,
  daily_statistic=NA,annual_statistic=t$NAAQS.Statistic,
  level=sapply(t$Primary.Standard.Level,function(x) ifelse(is.na(x),10,x)),
  units=sapply(t$Standard.Units,function(x) ifelse(grepl("Micrograms",x),
    "ug/m3",ifelse(grepl("billion",x),"ppb","ppm"))),row.names=1:nrow(t))
  naaqs$daily_statistic <- sapply(naaqs$avg_time,function(x) 
    ifelse(grepl("hour",x) & !grepl("24",x),"max","mean"))
dbWriteTable(db,name="standards",value=naaqs,row.names=FALSE,overwrite=TRUE)

## Table 'monitors' - get all active AQS monitors for most recent data year
par <- unlist(dbGetQuery(db,"SELECT DISTINCT parameter_code FROM standards"))
year <- as.numeric(substr(Sys.Date(),1,4)) - ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)
monitors <- get.aqs.data(paste("
SELECT monitors.mo_id AS mo_id,
       sites.si_id AS key,
       monitors.pa_parameter_code AS parameter_code,
       sites.state_code || sites.county_code || sites.site_id AS site_id,
       monitors.poc AS poc,
       TO_CHAR(sp.sampling_begin_date+1/24,'YYYY-MM-DD') AS begin_date
  FROM monitors monitors,
       sample_periods sp,
       site_basic sites
 WHERE monitors.si_si_id = sites.si_id
   AND sp.mo_mo_id = monitors.mo_id
   AND monitors.status_ind = 'P'
   AND sites.status_ind = 'P'
   AND sp.status_ind = 'P'
   AND EXTRACT(YEAR FROM monitors.last_sampling_date) >= ",year,"
   AND monitors.pa_parameter_code IN (",
       paste(sapply(par,function(x) paste("\'",x,"\'",sep="")),collapse=","),")
   AND sites.state_code NOT IN ('80','CC')
   AND sp.sampling_end_date IS NULL
 ORDER BY 3,4,5",sep=""))
monitors$mo_id <- as.integer(monitors$mo_id)
monitors$key <- as.integer(monitors$key)
monitors$poc <- as.integer(monitors$poc)
dbWriteTable(db,name="monitors",value=monitors,row.names=FALSE,overwrite=TRUE)

## Table 'sites' - Site location information and pollutants monitored
naaqs <- dbGetQuery(db,"SELECT * FROM standards")
sites <- subset(get.aqs.data("
SELECT sites.si_id AS key,
       sites.state_code || sites.county_code || sites.site_id AS site_id,
       COALESCE(sites.local_site_name,' ') AS site_name,
       sites.street_address AS address,
       sites.standard_latitude AS latitude,
       sites.standard_longitude AS longitude,
       regions.epa_region_code AS epa_region,
       states.state_name AS state_name,
       counties.county_name AS county_name,
       COALESCE(cbsas.cbsa_name,' ') AS cbsa_name,
       COALESCE(cbsas.csa_title,' ') AS csa_title
  FROM cbsa_mapping cbsas,
       counties counties,
       epa_regions regions,
       site_basic sites,
       states states
 WHERE regions.epa_region_code = states.epar_epa_region_code
   AND sites.county_code = cbsas.county_code(+)
   AND sites.county_code = counties.county_code
   AND sites.state_code = cbsas.state_code(+)
   AND sites.state_code = counties.stt_state_code
   AND sites.state_code = states.state_code
   AND sites.status_ind = 'P'
 ORDER BY 2"),key %in% unique(monitors$key))
sites$key <- as.integer(sites$key)
sites$latitude <- as.numeric(sites$latitude)
sites$longitude <- as.numeric(sites$longitude)
sites$epa_region <- as.integer(sites$epa_region)
sites$monitor_count <- sapply(sites$key,function(x) sum(monitors$key == x))
site.par <- sapply(sites$key,function(x) monitors$parameter[which(monitors$key == x)])
site.poll <- lapply(site.par,function(x) naaqs$poll_name[match(x,naaqs$parameter_code)])
sites$pollutants <- unlist(lapply(site.poll,function(x) paste(unique(x),collapse=", ")))
dbWriteTable(db,name="sites",value=sites,row.names=FALSE,overwrite=TRUE)

## Table 'daily' - daily summary statistics for most recent 3 years
naaqs <- dbGetQuery(db,"SELECT * FROM standards")
sites <- dbGetQuery(db,"SELECT * FROM sites")
years <- as.numeric(substr(Sys.Date(),1,4)) - c(3:1) - 
  ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,0,1)
begin.date <- paste(years[1],"01-01",sep="-")
end.date <- paste(years[3],"12-31",sep="-")
curr.psid <- c(2,4,12,19,20,23,26)
temp <- vector("list",length(curr.psid))
for (p in curr.psid) {
  i <- match(p,curr.psid)
  std <- subset(naaqs,ps_id == p)
  poll <- std$pollutant[1]
  ds.table <- switch(poll,co="daily_summaries",lead="site_daily_values",
    no2="site_daily_values",ozone="site_daily_values",pm10="site_daily_values",
    pm25="site_daily_values",so2="daily_summaries")
  date.col <- switch(ds.table,daily_summaries="ds.daily_coll_date",
    site_daily_values="ds.sample_day")
  value.col <- switch(ds.table,daily_summaries="ds.daily_max_sample_value",
    site_daily_values="ds.daily_value")
  temp[[i]] <- get.aqs.data(paste("
  SELECT '",poll,"' AS pollutant, 
         si.si_id AS key,",
         ifelse(ds.table == "daily_summaries","ds.mo_mo_id AS mo_id,",""),"
         TO_CHAR(",date.col,",'YYYY-MM-DD') AS sample_date,
         GREATEST(",value.col,",0) AS value ",
   "FROM ",ds.table," ds,",
         ifelse(ds.table == "daily_summaries","monitors mo,",""),"
         site_basic si
   WHERE ",date.col," >= TO_DATE('",begin.date,"','YYYY-MM-DD')
     AND ",date.col," <= TO_DATE('",end.date,"','YYYY-MM-DD')
     AND ds.edt_",ifelse(ds.table == "daily_summaries","edt_",""),"id IN (0,2)",
     ifelse(ds.table == "daily_summaries","AND ds.mo_mo_id = mo.mo_id",""),"
     AND ",switch(poll,co="ds.daily_criteria_ind = 'Y'",
           lead="ds.creditable_day_indicator IN ('M','S')",
           no2="ds.valid_day_indicator = 'Y'",
           ozone="ds.valid_day_indicator = 'Y'",
           pm10="ds.creditable_day_indicator IN ('M','S')",
           pm25="ds.creditable_day_indicator IN ('M','S')",
           so2="ds.daily_criteria_ind = 'Y'"),"
     AND ds.parameter_code = '",std$parameter_code[1],"'
     AND ds.pollutant_standard_id = ",std$ps_id[1],
     ifelse(ds.table == "daily_summaries",paste(
       " AND mo.pa_parameter_code = '",std$parameter_code[1],"'
         AND mo.si_si_id = si.si_id",sep=""),
       " AND ds.si_id = si.si_id"),"
     AND si.state_code NOT IN ('80','CC')
   ORDER BY 2,3,4",sep=""),dbname="aqsprod")
  temp[[i]]$key <- as.integer(temp[[i]]$key)
  temp[[i]]$value <- as.numeric(temp[[i]]$value)
  if (ds.table == "daily_summaries") {
    temp[[i]] <- ddply(temp[[i]],c("pollutant","key","sample_date"),summarize,value=max(value))
  }
  cat(std$naaqs[1],"\n")
}
daily <- subset(as.data.frame(rbindlist(temp)),key %in% sites$key)
dbWriteTable(db,name="daily",value=daily,row.names=FALSE,overwrite=TRUE)

## Table 'annual' - annual summary statistics for trends plots
naaqs <- dbGetQuery(db,"SELECT * FROM standards")
sites <- dbGetQuery(db,"SELECT * FROM sites")
years <- c(2000:(as.numeric(substr(Sys.Date(),1,4)) -  
  ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)))
curr.psid <- c(2,3,4,8,12,19,20,23,26,27,28)
temp <- vector("list",length(curr.psid))
for (p in curr.psid) {
  i <- match(p,curr.psid)
  poll <- subset(naaqs,ps_id == p)$pollutant
  std <- ""
  if (poll == "co") { std <- ifelse(p == 3,"1-hour","8-hour") }
  if (poll == "no2") { std <- ifelse(p == 20,"1-hour","annual") }
  if (poll == "pm25") { std <- ifelse(p == 26,"daily","annual") }
  if (poll == "so2") { std <- ifelse(p == 19,"1-hour","annual") }
  ans.table <- switch(poll,co="annual_summaries",lead="lead_site_annual_summaries",
    no2="annual_summaries",ozone="ozone_linked_site_annual",pm10="site_annual_summaries",
    pm25="site_annual_summaries",so2="annual_summaries")
  temp[[i]] <- get.aqs.data(paste("
  SELECT '",poll,"' AS pollutant,
         ans.pollutant_standard_id AS ps_id, 
         si.si_id AS key, ",
         ifelse(ans.table == "annual_summaries","ans.mo_mo_id AS mo_id,",""),"
         ans.",ifelse(ans.table == "annual_summaries","annual_",""),"summary_year AS year, ",
         ifelse(ans.table == "annual_summaries","ans.annual_obs_cnt AS obs, ",""),
         switch(poll,co="sm.max_sample_value",
                   lead="ans.annual_max",
                    no2=switch(std,'1-hour'="sp.percentile_sample_value",annual="ans.annual_arith_mean"),
                  ozone="ans.max_4",
                   pm10="ans.second_maximum",
                   pm25=switch(std,annual="ans.arith_mean",daily="ans.percentile_98th"),
                    so2=switch(std,'1-hour'="sp.percentile_sample_value",annual="ans.annual_arith_mean"))," AS value, ",
         switch(ans.table,annual_summaries="ans.annual_criteria_ind",
           lead_site_annual_summaries="CASE WHEN ans.valid_months >= 9 THEN 'Y' ELSE 'N' END",
           ozone_linked_site_annual="CASE WHEN ans.annual_percent_complete >= 75 THEN 'Y' ELSE 'N' END",
           site_annual_summaries=switch(std,annual="ans.arith_mean_completeness_ind",
             daily="ans.percentile_98th_complete_ind",
             "CASE WHEN ans.pct_obs >= 75 THEN 'Y' ELSE 'N' END"))," AS valid
    FROM ",ans.table," ans, ",
         ifelse(ans.table == "annual_summaries","monitors mo, ",""),
         "site_basic si",
         switch(poll,co=", summary_maximums sm ",
                    no2=switch(std,'1-hour'=", summary_percentiles sp ",annual=" "),
                    so2=switch(std,'1-hour'=", summary_percentiles sp ",annual=" ")," "),"
   WHERE ",ifelse(ans.table == "annual_summaries","ans.mo_mo_id = mo.mo_id AND mo.si_si_id = si.si_id","ans.si_id = si.si_id"),"
     AND ans.",ifelse(ans.table == "annual_summaries","annual_",""),"summary_year IN (",paste(years,collapse=","),")
     AND ans.edt_",ifelse(ans.table == "annual_summaries","edt_",""),"id IN (0,5)
     AND ans.pollutant_standard_id = ",p,"
     AND si.state_code NOT IN ('80','CC')",switch(poll,
      co=paste(" AND sm.ans_ans_id = ans.ans_id AND sm.max_level = 2 AND ",switch(std,
        '1-hour'="ans.sd_duration_code = '1' AND sm.max_ind = 'REG' ",
        '8-hour'="ans.sd_duration_code = 'Z' AND sm.max_ind = 'OVR' "),sep=""),
    lead="AND ans.annual_parameter_code = '14129' AND ans.summary_year = ans.dv_year",
     no2=paste(" AND ans.sd_duration_code = '1' ",switch(std,annual="",
        '1-hour'="AND sp.ans_ans_id = ans.ans_id AND sp.percentile_num = 98 "),sep=""),
     so2=paste(" AND ans.sd_duration_code = '1' ",switch(std,annual="",
        '1-hour'="AND sp.ans_ans_id = ans.ans_id AND sp.percentile_num = 99 "),sep="")," "),"
   ORDER BY key, year",sep=""),dbname="aqsprod")
  temp[[i]]$pollutant <- paste(poll,substr(std,1,1),sep="")
  temp[[i]]$ps_id <- as.integer(temp[[i]]$ps_id)
  temp[[i]]$key <- as.integer(temp[[i]]$key)
  temp[[i]]$year <- as.integer(temp[[i]]$year)
  temp[[i]]$value <- as.numeric(temp[[i]]$value)
  if (ans.table == "annual_summaries") {
    temp[[i]]$obs <- as.integer(temp[[i]]$obs)
    temp[[i]] <- ddply(temp[[i]],c("pollutant","key","year"),summarize,
      value=value %*% obs/sum(obs),valid=max(valid))
  }
  if (ans.table != "annual_summaries") { 
    temp[[i]] <- temp[[i]][,c("pollutant","key","year","value","valid")] 
  }
  cat(p,poll,std,"\n")
}
annual <- subset(as.data.frame(rbindlist(temp)),key %in% sites$key)
dbWriteTable(db,name="annual",value=annual,row.names=FALSE,overwrite=TRUE)

## Table 'dvs' - design values for trends plots
max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
annual <- dbGetQuery(db,"SELECT * FROM annual")
naaqs <- dbGetQuery(db,"SELECT * FROM standards")
sites <- dbGetQuery(db,"SELECT * FROM sites")
curr.psid <- c(2,3,4,8,12,19,20,23,26,27,28)
years <- c(2000:(as.numeric(substr(Sys.Date(),1,4)) -  
  ifelse(as.numeric(substr(Sys.Date(),6,7)) > 4,1,2)))
naaqs.ind <- match(curr.psid,naaqs$ps_id)
view.name <- paste("EUV_",toupper(naaqs$pollutant[naaqs.ind]),c(rep("",3),"_ANNUAL",
  rep("",2),"_1HOUR","","_24HR","_ANNUAL",""),"_DVS",c(rep("",10),"_SECONDARY"),sep="")
polls <- unique(annual$pollutant)
dv.col <- sapply(polls,function(x) switch(x,co1="co_1hr_2nd_max_value",
  co8="co_8hr_2nd_max_value",pm25d="daily_design_value","design_value"))
dv.valid <- sapply(polls,function(x) switch(x,co1="'Y'",co8="'Y'",lead="dv_validity_ind",
  no21="dv_validity_indicator",no2a="dv_validity_indicator",ozone="dv_validity_indicator",
  pm10="dv_validity_indicator",pm25a="dv_validity_ind",pm25d="dv_validity_ind",
  so21="dv_validity_indicator",so2a="three_year_design_validity"))
np <- length(polls); ny <- length(years);
dvs <- vector("list",np); names(dvs) <- polls;
## Queries can take a long time, load previously completed queries
if (file.exists("temp.Rdata")) { load("temp.Rdata") }

for (p in curr.psid) {
  if (p == 12) { next } ## Skip PM10 for now
  i <- match(p,curr.psid)
  if (length(dvs[[i]]) > 0) { next } ## Skip if data already loaded above
  dvs[[i]] <- get.aqs.data(paste("
    SELECT '",polls[i],"' AS pollutant,
           si_id AS key,",
           ifelse(grepl("co",polls[i])," poc AS poc,",""),"
           dv_year AS year, ",
           dv.col[i]," AS value, ",
           dv.valid[i]," AS valid
      FROM ",view.name[i],"
     WHERE dv_year >= ",years[1],"
       AND dv_year <= ",years[ny],"
       AND edt_id IN (0,5)
       AND latitude IS NOT NULL
       AND parameter_code = '",naaqs$parameter_code[naaqs.ind][i],"'",
       ifelse(grepl("co",polls[i]),"",paste(" AND ",ifelse(!(p %in% c(23,28)),
         "pollutant_",""),"standard_id = ",p,sep="")),"
       AND state_code NOT IN ('80','CC') ",
  switch(polls[i],co1="",co8="",lead="AND dv_year_max_value IS NOT NULL",
    no21="AND year_0_98th_percentile IS NOT NULL",no2a="",ozone="AND dv_year_4th_max IS NOT NULL",
    pm25a="AND dv_year_arith_mean IS NOT NULL",pm25d="AND dv_year_98th_percentile IS NOT NULL",
    so21="AND year_0_99th_percentile IS NOT NULL",so2a="AND year0_annual_mean IS NOT NULL"),"
  ORDER BY key, year",sep=""),dbname="aqsprod.world")
  dvs[[i]]$key <- as.integer(dvs[[i]]$key)
  dvs[[i]]$year <- as.integer(dvs[[i]]$year)
  dvs[[i]]$value <- as.numeric(dvs[[i]]$value)
  ## Save queries that have completed
  save(dvs,file="temp.Rdata")
  cat(p,view.name[i],as.character(round(Sys.time())),"\n")
}
dvs$co1 <- ddply(dvs$co1,c("pollutant","key","year"),summarize,value=max.na(value),valid=valid[1])
dvs$co8 <- ddply(dvs$co8,c("pollutant","key","year"),summarize,value=max.na(value),valid=valid[1])

## PM10 design concentrations
dvs$pm10 <- get.aqs.data(paste("
  SELECT 'pm10' AS pollutant,
         dc.si_id AS key,
         dc.poc AS poc,
         dc.dv_year AS year,
         dc.design_concentration AS value,
         dv.dv_validity_indicator AS valid
    FROM EUV_PM10_DVS dv,
         PM10_MON_DESIGN_CONCENTRATIONS dc
   WHERE dv.dv_year = dc.dv_year
     AND dv.edt_id = dc.edt_id
     AND dv.mo_id = dc.mo_id
     AND dv.si_id = dc.si_id
     AND dc.dv_year >=",years[1],"
     AND dc.dv_year <=",years[ny],"
     AND dc.edt_id IN (0,5)
     AND dc.design_concentration IS NOT NULL
     AND dc.parameter_code = '81102'
     AND dc.pollutant_standard_id = 12
     AND dc.state_code NOT IN ('80','CC')
ORDER BY key, year"),dbname="aqsprod.world")
dvs$pm10$key <- as.integer(dvs$pm10$key)
dvs$pm10$year <- as.integer(dvs$pm10$year)
dvs$pm10$value <- as.numeric(dvs$pm10$value)
dvs$pm10 <- ddply(dvs$pm10,c("pollutant","key","year"),summarize,
  value=max.na(value),valid=max(valid))

dvs <- subset(as.data.frame(rbindlist(dvs)),key %in% sites$key)
dbWriteTable(db,name="dvs",value=dvs,row.names=FALSE,overwrite=TRUE)

## Create JSON object for existing sites
createSites <- function() {
  jsonArray <- function(x,quote=FALSE) {
    return(ifelse(quote,paste('["',paste(x,collapse='", "'),'"]',sep=""), 
      paste("[",paste(x,collapse=", "),"]",sep=""))) 
  }
  jsonObject <- function(x) { 
    return(paste("{",paste(paste('"',names(x),'"',sep=""),
      sapply(x,function(y) ifelse(substr(y,1,1) %in% c("[","{") & 
      substr(y,nchar(y),nchar(y)) %in% c("}","]"),y,paste('"',y,'"',sep=""))),
      sep = ": ",collapse=", "),"}",sep=""))
  }
  m <- dbGetQuery(db,"SELECT * FROM sites")
  coord <- paste(round(m$latitude,6),round(m$longitude,6))
  s <- m[!duplicated(coord),]
  d <- m[duplicated(coord),]
  sites <- sapply(c(1:nrow(s)),function(r) {
    alt <- d$latitude == s$latitude[r] & d$longitude == s$longitude[r]
    key <- s$key[r]
    site_id <- s$site_id[r]
    if(sum(alt) > 0) {
      key <- c(key,d$key[alt])
      site_id <- c(site_id,d$site_id[alt])
      s$monitor_count[r] <- s$monitor_count[r]+sum(d$monitor_count[alt])
    }
    key <- jsonArray(key)
    site_id <- jsonArray(site_id,TRUE)
    p <- c(key=key,site_id=site_id,as.list(s[r,c("site_name","address","epa_region",
      "state_name","county_name","cbsa_name","csa_title","monitor_count","pollutants")]))
    p$site_name <- gsub('"',"&quot;",gsub("'","&#039;",p$site_name,fixed=TRUE),fixed=TRUE)
    p$address <- gsub('"',"&quot;",gsub("'","&#039;",p$address,fixed=TRUE),fixed=TRUE)
    p <- jsonObject(p)
    g <- jsonArray(c(round(s$longitude[r],6),round(s$latitude[r],6)))
    g <- jsonObject(list(type="Point",coordinates=g))
    return(jsonObject(list(type="Feature",geometry=g,properties=p)))
  })
  write(jsonObject(list(type="FeatureCollection",features=jsonArray(sites))),
    file="www/data/sites.geojson")
}
createSites()

## Remove the temporary data directory and its contents
unlink("data",recursive=TRUE,force=TRUE)
dbDisconnect(db)
