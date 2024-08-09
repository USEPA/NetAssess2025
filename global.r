## Load packages
options(stringsAsFactors=FALSE,warn=-1)
require(data.table,quietly=TRUE)
require(deldir,quietly=TRUE)
require(fields,quietly=TRUE)
require(plyr,quietly=TRUE)
require(reshape2,quietly=TRUE)
require(rgdal,quietly=TRUE)
require(rgeos,quietly=TRUE)
require(RSQLite,quietly=TRUE)
require(sp,quietly=TRUE)

## Reset trend chart directory
if (!dir.exists("www/images/temp")) { dir.create("www/images/temp") }
unlink(paste("www/images/temp",list.files("www/images/temp/"),sep="/"))

## Connect to database, create initial geometries
db <- dbConnect(SQLite(),dbname="netassess.sqlite")
usborder <- SpatialPolygons(list(Polygons(lapply(eval(parse(text=dbGetQuery(db,"SELECT geometry FROM usborder"))),
        function(p) Polygon(p[,c("lng","lat")])),ID="US")))
tracts <- SpatialPointsDataFrame(coords=dbGetQuery(db,"SELECT longitude, latitude FROM tracts"),
  data=dbGetQuery(db,"SELECT * FROM tracts"))
temp <- dbGetQuery(db,"SELECT * FROM counties")
poly <- sapply(temp$geometry,function(x) eval(parse(text=x)))
counties <- SpatialPolygonsDataFrame(Sr=SpatialPolygons(Srl=mapply(function(poly,id) {
      Polygons(srl=lapply(poly,function(x) Polygon(x[,c("lng","lat")])),ID=id) },
      poly=poly,id=temp$code)),data=as.data.frame(temp[,1:5]),match.ID="code")
rm(temp,poly)
