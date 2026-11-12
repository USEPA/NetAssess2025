##############################################################################
## Calculate ozone and PM2.5 exceedance probabilities using Downscaler outputs
## Note: Script run on NCC/HPCC cluster (atmos), some files too large for PC
##############################################################################
setwd("/work/YODA/users/bwells01/Modeling/Boot/DS/")
options(stringsAsFactors=FALSE,warn=-1)
library(data.table); library(RSQLite); library(sf);

## Get year of most recent Downscaler run from EPA website
download.file(url="https://www.epa.gov/hesc/rsig-related-downloadable-data-files",destfile="temp.txt")
html.txt <- c(read.table(file="temp.txt",sep="\n")[,1])
years <- substr(gsub("<ul>","",gsub("\t","",html.txt[grep("(CONUS)",html.txt)])),5,8)
curr.year <- as.numeric(years[length(years)])
unlink("temp.txt"); rm(html.txt,years);

## Download and process daily ozone and PM2.5 outputs from RSIG website
get.ds.output <- function(year) {
  base.url <- "https://ofmpub.epa.gov/rsig/rsigserver?data/FAQSD/outputs/"
  download.file(paste(base.url,year,"_ozone_daily_8hour_maximum.txt.gz",sep=""),
    destfile=paste("DSoutput_ozone_",year,".txt.gz",sep=""))
  ozone <- read.csv(gzfile(paste("DSoutput_ozone_",year,".txt.gz",sep="")),
    colClasses=c(rep("character",2),rep("numeric",4)))
  colnames(ozone) <- c("date","fips","lon","lat","pred","stderr")
  dates <- gsub("/","-",unique(ozone$date)); tracts <- unique(ozone$fips);
  ozone.pred <- matrix(ozone$pred,nrow=length(tracts),ncol=length(dates),dimnames=list(tracts,dates))
  ozone.stderr <- matrix(ozone$stderr,nrow=length(tracts),ncol=length(dates),dimnames=list(tracts,dates))
  ozone.pred <- ozone.pred[order(rownames(ozone.pred)),]
  ozone.stderr <- ozone.stderr[order(rownames(ozone.stderr)),]
  colnames(ozone.pred) <- colnames(ozone.stderr) <- as.character(seq(as.Date(paste(year,"01-01",sep="-")),
    as.Date(paste(year,"12-31",sep="-")),1))
  save(list=c("ozone.pred","ozone.stderr"),file=paste("DSoutput_ozone_",year,"_v2.Rdata",sep=""))
  unlink(paste("DSoutput_ozone_",year,".txt.gz",sep=""))
  download.file(paste(base.url,year,"_pm25_daily_average.txt.gz",sep=""),
    destfile=paste("DSoutput_PM25_",year,".txt.gz",sep=""))
  pm25 <- read.csv(gzfile(paste("DSoutput_PM25_",year,".txt.gz",sep="")),
    colClasses=c(rep("character",2),rep("numeric",4)))
  colnames(pm25) <- c("date","fips","lon","lat","pred","stderr")
  dates <- gsub("/","-",unique(pm25$date)); tracts <- unique(pm25$fips);
  pm25.pred <- matrix(pm25$pred,nrow=length(tracts),ncol=length(dates),dimnames=list(tracts,dates))
  pm25.stderr <- matrix(pm25$stderr,nrow=length(tracts),ncol=length(dates),dimnames=list(tracts,dates))
  pm25.pred <- pm25.pred[order(rownames(pm25.pred)),]
  pm25.stderr <- pm25.stderr[order(rownames(pm25.stderr)),]
  colnames(pm25.pred) <- colnames(pm25.stderr) <- as.character(seq(as.Date(paste(year,"01-01",sep="-")),
    as.Date(paste(year,"12-31",sep="-")),1))
  save(list=c("pm25.pred","pm25.stderr"),file=paste("DSoutput_PM25_",year,"_v2.Rdata",sep=""))
  unlink(paste("DSoutput_PM25_",year,".txt.gz",sep=""))
}
get.ds.output(curr.year)

## Use bootstrapping to estimate ozone and PM2.5 exceedance probabilities
## Note: this part of the script takes several hours to run
for (y in (curr.year-2):(curr.year)) {
  load(paste("DSoutput_ozone_",y,"_v2.Rdata",sep=""))
  load(paste("DSoutput_PM25_",y,"_v2.Rdata",sep=""))
  assign(paste("ozone.pred",y,sep="."),ozone.pred)
  assign(paste("ozone.stderr",y,sep="."),ozone.stderr)
  assign(paste("pm25.pred",y,sep="."),pm25.pred)
  assign(paste("pm25.stderr",y,sep="."),pm25.stderr)
  rm(ozone.pred,pm25.pred,ozone.stderr,pm25.stderr)
  cat(y,"\n"); gc(); 
}
ctfips <- eval(parse(text=paste("rownames(ozone.pred.",curr.year,")",sep="")))
prob.out <- data.frame(CTFIPS=ctfips,ozone_prob=NA,pm25_prob=NA)
set.seed(8675309)
for (i in 1:nrow(prob.out)) {
  assign(paste("ozone.boot",(curr.year-2),sep="."),
    mapply(function(pred,stderr) pmax(rnorm(1000,pred,stderr),0),
    pred=eval(parse(text=paste("ozone.pred.",(curr.year-2),"[i,]",sep=""))),
    stderr=eval(parse(text=paste("ozone.stderr.",(curr.year-2),"[i,]",sep="")))))
  assign(paste("ozone.boot",(curr.year-1),sep="."),
    mapply(function(pred,stderr) pmax(rnorm(1000,pred,stderr),0),
    pred=eval(parse(text=paste("ozone.pred.",(curr.year-1),"[i,]",sep=""))),
    stderr=eval(parse(text=paste("ozone.stderr.",(curr.year-1),"[i,]",sep="")))))
  assign(paste("ozone.boot",curr.year,sep="."),
    mapply(function(pred,stderr) pmax(rnorm(1000,pred,stderr),0),
    pred=eval(parse(text=paste("ozone.pred.",curr.year,"[i,]",sep=""))),
    stderr=eval(parse(text=paste("ozone.stderr.",curr.year,"[i,]",sep="")))))
  assign(paste("ozone.max4",(curr.year-2),sep="."),
    apply(eval(parse(text=paste("ozone.boot",(curr.year-2),sep="."))),1,
    function(x) x[order(x,decreasing=TRUE)][4]))
  assign(paste("ozone.max4",(curr.year-1),sep="."),
    apply(eval(parse(text=paste("ozone.boot",(curr.year-1),sep="."))),1,
    function(x) x[order(x,decreasing=TRUE)][4]))
  assign(paste("ozone.max4",curr.year,sep="."),
    apply(eval(parse(text=paste("ozone.boot",curr.year,sep="."))),1,
    function(x) x[order(x,decreasing=TRUE)][4]))
  ozone.dv <- (eval(parse(text=paste("ozone.max4",(curr.year-2),sep="."))) + 
    eval(parse(text=paste("ozone.max4",(curr.year-1),sep="."))) + 
    eval(parse(text=paste("ozone.max4",curr.year,sep="."))))/3
  prob.out$ozone_prob[i] <- sum(ozone.dv >= 71)/1000
  assign(paste("pm25.boot",(curr.year-2),sep="."),
    mapply(function(pred,stderr) pmax(rnorm(1000,pred,stderr),0),
    pred=eval(parse(text=paste("pm25.pred.",(curr.year-2),"[i,]",sep=""))),
    stderr=eval(parse(text=paste("pm25.stderr.",(curr.year-2),"[i,]",sep="")))))
  assign(paste("pm25.boot",(curr.year-1),sep="."),
    mapply(function(pred,stderr) pmax(rnorm(1000,pred,stderr),0),
    pred=eval(parse(text=paste("pm25.pred.",(curr.year-1),"[i,]",sep=""))),
    stderr=eval(parse(text=paste("pm25.stderr.",(curr.year-1),"[i,]",sep="")))))
  assign(paste("pm25.boot",curr.year,sep="."),
    mapply(function(pred,stderr) pmax(rnorm(1000,pred,stderr),0),
    pred=eval(parse(text=paste("pm25.pred.",curr.year,"[i,]",sep=""))),
    stderr=eval(parse(text=paste("pm25.stderr.",curr.year,"[i,]",sep="")))))
  ly <- as.numeric(c((curr.year-2):curr.year) %% 4 == 0)
  q <- data.frame(b=rep(c(1,91,182,274),times=3)+rep(ly,each=4),
    e=rep(c(90,181,273,365),times=3)+rep(ly,each=4)); q$b[c(1,5,9)] <- 1;
  assign(paste("pm25.mean",(curr.year-2),sep="."),
    apply(eval(parse(text=paste("pm25.boot",(curr.year-2),sep="."))),1,
    function(x) (mean(x[q$b[1]:q$e[1]]) + mean(x[q$b[2]:q$e[2]]) + 
      mean(x[q$b[3]:q$e[3]]) + mean(x[q$b[4]:q$e[4]]))/4))
  assign(paste("pm25.mean",(curr.year-1),sep="."),
    apply(eval(parse(text=paste("pm25.boot",(curr.year-1),sep="."))),1,
    function(x) (mean(x[q$b[5]:q$e[5]]) + mean(x[q$b[6]:q$e[6]]) + 
      mean(x[q$b[7]:q$e[7]]) + mean(x[q$b[8]:q$e[8]]))/4))
  assign(paste("pm25.mean",curr.year,sep="."),
    apply(eval(parse(text=paste("pm25.boot",curr.year,sep="."))),1,
    function(x) (mean(x[q$b[9]:q$e[9]]) + mean(x[q$b[10]:q$e[10]]) + 
      mean(x[q$b[11]:q$e[11]]) + mean(x[q$b[12]:q$e[12]]))/4))
  pm25.annual <- (eval(parse(text=paste("pm25.mean",(curr.year-2),sep="."))) + 
    eval(parse(text=paste("pm25.mean",(curr.year-1),sep="."))) + 
    eval(parse(text=paste("pm25.mean",curr.year,sep="."))))/3
  assign(paste("pm25.p98",(curr.year-2),sep="."),
    apply(eval(parse(text=paste("pm25.boot",(curr.year-2),sep="."))),1,
    function(x) x[order(x,decreasing=TRUE)][8]))
  assign(paste("pm25.p98",(curr.year-1),sep="."),
    apply(eval(parse(text=paste("pm25.boot",(curr.year-1),sep="."))),1,
    function(x) x[order(x,decreasing=TRUE)][8]))
  assign(paste("pm25.p98",curr.year,sep="."),
    apply(eval(parse(text=paste("pm25.boot",curr.year,sep="."))),1,
    function(x) x[order(x,decreasing=TRUE)][8]))
  pm25.daily <- (eval(parse(text=paste("pm25.p98",(curr.year-2),sep="."))) + 
    eval(parse(text=paste("pm25.p98",(curr.year-1),sep="."))) + 
    eval(parse(text=paste("pm25.p98",curr.year,sep="."))))/3
  prob.out$pm25_prob[i] <- sum(pm25.annual >= 9.05 | pm25.daily >= 35.5)/1000
  if (i %% 1000 == 0) {
    save(prob.out,file="temp.Rdata")
    cat(i,as.character(round(Sys.time())),"\n") 
  }
}
save(prob.out,file=paste("excprob_",(curr.year-2),"_",curr.year,".Rdata",sep=""))
unlink("temp.Rdata")

## Download and extract 2020 census tract shapefiles from Census Bureau website - Run on PC
setwd("C:/Users/bwells01/Documents/NetAssess/NetAssess-test/")
download.file(url="https://www2.census.gov/geo/tiger/cb_2021_us_tract_500k.zip"
  destfile="data/cb_2021_us_tract_500k.zip")
unzip(zipfile="data/cb_2021_us_tract_500k.zip")
temp <- st_read(dsn="data",layer="cb_2021_us_tract_500k")
coord <- subset(as.data.frame(st_coordinates(temp)),L1 == 1)
tract.shape <- data.frame(fips=temp$GEOID[coord$L3],id=coord$L2,lon=coord$X,lat=coord$Y)
tract.shape <- tract.shape[order(tract.shape$fips,tract.shape$id),]
coord <- sf_project(pts=tract.shape[,c("lon","lat")],from=st_crs(4269),
  to="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0")
tract.shape$X <- coord[,1]; tract.shape$Y <- coord[,2];
save(tract.shape,file="data/census_tract_shape_2020.Rdata")

## Generate ozone and PM2.5 exceedance probability maps for the network assessment app overlays
## Download exceedance probabilities to the data subfolder on PC before running this section
source("C:/Users/bwells01/Documents/Census/shape2020/rmapfuns.r")
load("data/census_tract_shape_2020.Rdata")
load(paste("data/excprob_",(curr.year-2),"_",curr.year,".Rdata",sep=""))
conus.tracts <- subset(tract.shape,fips %in% prob.out$CTFIPS)
tracts <- split(conus.tracts,conus.tracts$fips)
ozone.col <- assign.colors(prob.out$ozone_prob,range=c(0,1),palette=tim.colors(16)[4:13])
pm25.col <- assign.colors(prob.out$pm25_prob,range=c(0,1),palette=tim.colors(16)[4:13])
ctfips <- names(tracts)

## Ozone exceedance probability map
png(file="www/images/ozone_prob.png",width=10000,height=5000,bg="transparent")
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",xlim=range(conus.tracts$X),
  yaxs='i',ylab="",ylim=range(conus.tracts$Y),main="")
for (i in 1:length(tracts)) {
  k <- match(ctfips[i],prob.out$CTFIPS)
  add.layer(type="polygon",x=tracts[[i]]$X,y=tracts[[i]]$Y,
    id=tracts[[i]]$id,rescale=FALSE,border=NA,col=ozone.col[k])
}
dev.off()

## PM2.5 exceedance probability map
png(file="www/images/pm25_prob.png",width=10000,height=5000,bg="transparent")
par(mar=c(0,0,0,0))
plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",xlim=range(conus.tracts$X),
  yaxs='i',ylab="",ylim=range(conus.tracts$Y),main="")
for (i in 1:length(tracts)) {
  k <- match(ctfips[i],prob.out$CTFIPS)
  add.layer(type="polygon",x=tracts[[i]]$X,y=tracts[[i]]$Y,
    id=tracts[[i]]$id,border=NA,col=pm25.col[k],rescale=FALSE)
}
dev.off()

## Create the probability bar for the app legend
png(file="www/images/probLegend.png",width=350,height=70)
par(mar=c(2.5,1.5,0.5,1.5),cex.axis=1.5,cex.lab=1.5)
image(x=c(1:10)-0.5,y=1,z=matrix(c(1:10),10,1),xlim=c(0,10),ylim=c(0,1),zlim=c(0,10),
  axes=FALSE,col=tim.colors(16)[4:13],legend.only=TRUE)
axis(side=1,at=seq(0,10,2),labels=paste(seq(0,100,20),"%",sep=""))
box(); dev.off();
