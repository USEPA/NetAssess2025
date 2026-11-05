## Load packages
options(stringsAsFactors=FALSE,warn=-1)
require(data.table,quietly=TRUE)
require(deldir,quietly=TRUE)
require(plyr,quietly=TRUE)
require(reshape2,quietly=TRUE)
require(RSQLite,quietly=TRUE)
require(sf,quietly=TRUE)
require(stringr,quietly=TRUE)

## Reset trend chart directory
if (!dir.exists("www/images/temp")) { dir.create("www/images/temp") }
unlink(paste("www/images/temp",list.files("www/images/temp/"),sep="/"))

## Connect to database, create initial geometries
db <- dbConnect(SQLite(),dbname="netassess.sqlite")
usborder <- st_sf(geometry=st_sfc(st_multipolygon(lapply(eval(parse(
  text=dbGetQuery(db,"SELECT geometry from usborder"))),
  function(x) st_polygon(list(x[,c("lng","lat")]))))),crs=4326)
tracts <- st_as_sf(dbGetQuery(db,"SELECT * FROM tracts"),coords=c("longitude","latitude"),crs=4326)
counties <- st_sf(dbGetQuery(db,"SELECT code, name, cbsa, csa FROM counties"),
  geometry=st_sfc(lapply(sapply(unlist(dbGetQuery(db,"SELECT geometry FROM counties")),
  function(x) eval(parse(text=x))),function(y) st_multipolygon(lapply(y,function(z) 
  st_polygon(list(z[,c("lng","lat")])))))),crs=4326)
