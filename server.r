require(shiny,quietly=TRUE)

shinyServer(function(input,output,session) {
  showLoading <- function() { session$sendCustomMessage("loading","show") }
  hideLoading <- function() { session$sendCustomMessage("loading","hide") }
  
  ## Existing sites, new sites, and site selection
  pollutantSites <- reactive({
    if (is.null(input$pollutantSelect)) { return() }
    if (input$pollutantSelect == "none") { return() }
    return(dbGetQuery(db,paste(
      "SELECT DISTINCT sites.* 
         FROM monitors, sites, standards
        WHERE monitors.key = sites.key
          AND monitors.parameter_code = standards.parameter_code
          AND standards.pollutant = '",input$pollutantSelect,"'",sep="")))
  })
  
  newSites <- reactive({
    if (length(input$newSites) == 0) { return() }
    ns <- data.frame(key=unlist(lapply(input$newSites,function(x) x$key)),
      site_id="",site_name=unlist(lapply(input$newSites,function(x) x$name)),
      address="New Site",latitude=unlist(lapply(input$newSites,function(x) x$lat)),
      longitude=unlist(lapply(input$newSites,function(x) x$lng)))
    coord <- st_sfc(st_multipoint(as.matrix(ns[,c("longitude","latitude")])),crs=4326)
    cty <- counties[unlist(st_within(coord,counties)),]
    st <- dbGetQuery(db,"SELECT * FROM states")
    st.ind <- match(substr(cty$code,1,2),st$code)
    ns$epa_region <- st$epa_region[st.ind]
    ns$state_name <- st$name[st.ind]
    ns$county_name <- cty$name
    ns$cbsa_name <- cty$cbsa
    ns$csa_title <- cty$csa
    ns$site_id <- paste(cty$code,"NS",sprintf("%02d",c(1:length(input$newSites))),sep="")
    names <- dbGetQuery(db,"SELECT DISTINCT pollutant, poll_name FROM standards")
    poll <- lapply(input$newSites,function(x) names$poll_name[match(x$poll,names$pollutant)])
    ns$monitor_count <- unlist(lapply(poll,length))
    ns$pollutants <- unlist(lapply(poll,function(x) paste(x,collapse=", ")))
    return(ns)
  })

  observe({
    if (is.null(input$pollutantSelect)) { return() }
    if (is.null(newSites())) { return() }
    ns <- newSites()
    ns$visible <- unlist(lapply(input$newSites,function(x) (input$pollutantSelect %in% x$poll)))
    session$sendCustomMessage(type="updateNewSites",ns)
  })
  
  visibleSites <- reactive({
    ps <- pollutantSites()
    if (is.null(ps)) { return() }
    vs <- ps[ps$key %in% input$visibleSites,]
    ns <- newSites()
    if(!is.null(ns)) { 
      vs <- rbind(vs,ns[ns$key %in% input$visibleNewSites,])
    }
    return(vs)
  })
  
  activeSites <- reactive({
    ss <- input$selectedSites
    vs <- input$visibleSites
    return(intersect(ss,vs))
  })
  
  activeNewSites <- reactive({
    ss <- input$selectedNewSites
    vs <- input$visibleNewSites
    return(intersect(ss,vs))
  })
  
  selectedSites <- reactive({
    ps <- pollutantSites()
    if (is.null(ps)) { return() }
    ss <- ps[ps$key %in% activeSites(),]
    ns <- newSites()
    if(!is.null(ns)) { 
      ss <- rbind(ss,ns[ns$key %in% activeNewSites(),])
    }
    return(ss)
  })
  
  observe({
    if (is.null(pollutantSites())) { keys <- list() 
    } else { keys <- unique(pollutantSites()$key) }
    session$sendCustomMessage(type="updateVisibleMonitors",keys)
  })
  
  ## Area of Interest
  areaOfInterest <- reactive({
    aoi <- lapply(input$areaOfInterest,function(x) t(matrix(unlist(x),nrow=2)))
    poly <- lapply(aoi,function(x) st_polygon(list(rbind(x,x[1,])[,c(2,1)])))
    polygons <- st_sf(geometry=st_sfc(st_multipolygon(poly),crs=4326))
    return(polygons)
  })
  
  observe({
    if(is.null(input$areaSelect)) { return() }
    if(input$areaSelect %in% c("State","CBSA","CSA")) {
      type <- paste(tolower(isolate(input$areaSelect)),"s",sep="")
      vals <- dbGetQuery(db,paste("SELECT code, name FROM",type))
      choices <- vals$code; names(choices) <- vals$name;
    } else { choices <- c("") }
    updateSelectInput(session,"areaSelectSelect",choices=choices)
  })
  
  observe({
    if (is.null(input$areaSelectSelect)) { return() }
    if (input$areaSelectSelect == "") { return() }
    type <- paste(tolower(isolate(input$areaSelect)),"s",sep="")
    q <- paste("SELECT geometry FROM ",type," WHERE code = '",input$areaSelectSelect,"'",sep="")
    coords <- eval(parse(text=dbGetQuery(db,q)[1,1]))
    session$sendCustomMessage(type="displayPredefinedArea",
      list(properties=list(name="test",type=type,id=input$areaSelectSelect),coords=coords))
  })
  
  ## Area Served
  selectedNeighbors <- reactive({
    vs <- visibleSites()
    ss <- selectedSites()
    if (is.null(ss)) { return() }
    if (nrow(ss) == 0) { return() }
    us.lat <- c(24.4,49.4); us.lon <- c(-124.8,-66.9);
    lat <- range(ss$latitude); lon <- range(ss$longitude);
    lat.rng <- max(abs(lat[2]-lat[1]),1); lon.rng <- max(abs(lon[2]-lon[1]),1);
    gtg <- FALSE
    while(!gtg) {
      lat.test <- c(lat[1]-lat.rng,lat[2]+lat.rng)
      lon.test <- c(lon[1]-lon.rng,lon[2]+lon.rng)
      bounds <- list(north=lat.test[2] >= us.lat[2],south=lat.test[1] <= us.lat[1],
        east=lon.test[2] >= us.lon[2],west=lon.test[1] <= us.lon[1])
      neighbors <- unique(vs[vs$latitude >= lat.test[1] & vs$latitude <= lat.test[2] &
        vs$longitude >= lon.test[1] & vs$longitude <= lon.test[2],])
      if(!bounds$north) {
        n <- neighbors[neighbors$latitude > lat[2],]
        bounds$north <- (sum(n$longitude > lon[2]) > 0 & sum(n$longitude < lon[1]) > 0 &
          sum(n$longitude < lon[2] & n$longitude > lon[1]) > 0)
      }
      if(!bounds$south) {
        n <- neighbors[neighbors$latitude < lat[1],]
        bounds$south <- (sum(n$longitude > lon[2]) > 0 & sum(n$longitude < lon[1]) > 0 &
          sum(n$longitude < lon[2] & n$longitude > lon[1]) > 0)
      }
      if(!bounds$east) {
        n <- neighbors[neighbors$longitude > lon[2],]
        bounds$east <- (sum(n$latitude > lat[2]) > 0 & sum(n$latitude < lat[1]) > 0 &
          sum(n$latitude < lat[2] & n$latitude > lat[1]) > 0)
      }
      if(!bounds$west) {
        n <- neighbors[neighbors$longitude < lon[1],]
        bounds$west <- (sum(n$latitude > lat[2]) > 0 & sum(n$latitude < lat[1]) > 0 &
          sum(n$latitude < lat[2] & n$latitude > lat[1]) > 0)
      }
      gtg <- bounds$north & bounds$south & bounds$east & bounds$west
      lat.rng <- lat.rng*2; lon.rng <- lon.rng*2;
    }
    neighbors <- neighbors[!duplicated(neighbors[,c("latitude","longitude")]),]
    v <- deldir(neighbors$longitude,neighbors$latitude)$delsgs
    v$ind1 <- neighbors$key[v$ind1]; v$ind2 <- neighbors$key[v$ind2];
    v <- v[v$ind1 %in% ss$key | v$ind2 %in% ss$key,]
    v <- unique(c(v$ind1,v$ind2))
    return(neighbors[neighbors$key %in% v,])
  })
  
  polygons <- eventReactive(input$areaServedButton,{
    ss <- isolate(selectedSites())
    if (is.null(ss)) { return() }
    if (nrow(ss) == 0) { return() }
    sn <- isolate(selectedNeighbors())
    if (is.null(sn)) { return() }
    if (nrow(sn) > 400 | nrow(sn) < 2) { return() }
    if (is.null(input$areaServedClipping)) { return() }
    if (input$areaServedClipping == "border") { b <- usborder }
    if (input$areaServedClipping == "aoi") { b <- areaOfInterest() }
    points <- st_as_sf(sn,coords=c("longitude","latitude"),crs=4326)
    bb <- st_bbox(st_union(st_as_sfc(st_bbox(b)),st_as_sfc(st_bbox(points))))
    tiles <- tile.list(deldir(x=sn$longitude,y=sn$latitude,rw=bb[c(1,3,2,4)]))
    polys <- st_intersection(st_make_valid(subset(st_sf(sn,geometry=st_sfc(lapply(tiles,function(t)
      st_polygon(list(matrix(c(t$x[c(1:length(t$x),1)],t$y[c(1:length(t$y),1)]),ncol=2)))),
      crs=4326)),key %in% ss$key)),st_make_valid(b))
    ind <- st_within(tracts,polys)
    t <- tracts[which(sapply(ind,length) > 0),]
    sum.cols <- c(grep("area",colnames(t)),grep("integer",lapply(t,class)))
    avg.cols <- c("ozone_prob","pm25_prob","no2_sat")
    t$key <- st_drop_geometry(polys)$key[unlist(ind)]
    d <- cbind(apply(st_drop_geometry(t)[,sum.cols],2,function(x) 
      tapply(x,list(t$key),function(y) round(sum(y,na.rm=TRUE)))),
      apply(st_drop_geometry(t)[,avg.cols],2,function(x)
      tapply(x,list(t$key),function(y) round(mean(y),1))))
    v <- merge(polys,d,by.x="key",by.y="row.names",all.x=TRUE,all.y=FALSE)
    return(v)
  })
  
  observeEvent(input$areaServedButton,{
    if(is.null(polygons())) { return() }
    poly <- polygons()
    coord <- st_coordinates(poly)
    coord <- coord[which(coord[,"L1"] == 1),]
    if (ncol(coord) == 4) {
      p <- lapply(seq(nrow(poly)),function(i) {
        list(id=poly$key[i],coords=apply(coord[which(coord[,"L2"] == i),],1,
          function(r) list(lat=r[[2]],lng=r[[1]])))
      })
    }
    if (ncol(coord) == 5) {
      N <- tapply(coord[,"L2"],list(coord[,"L3"]),max)
      p <- lapply(seq(nrow(poly)),function(i) {
        list(id=poly$key[i],coords=lapply(seq(N[i]),function(j) {
          apply(coord[which(coord[,"L3"] == i & coord[,"L2"] == j),],1,
          function(r) list(lat=r[[2]],lng=r[[1]]))
      }))})
    }
    session$sendCustomMessage(type="updateAreaServed",p)
  })
  
  output$areaServedPollutant <- renderText({
    if (is.null(input$clickedAreaServed)) { return() }
    if (is.null(input$pollutantSelect)) { return() }
    if (input$pollutantSelect == "none") { return() }
    return(as.character(dbGetQuery(db,paste("SELECT DISTINCT poll_name FROM standards
      WHERE pollutant = '",input$pollutantSelect,"'",sep=""))))
  })
  
  output$areaServedMonitor <- renderText({
    if (is.null(input$clickedAreaServed)) { return() }
    poly <- as.numeric(input$clickedAreaServed)
    if (is.null(selectedSites())) { return() }
    if (!(poly %in% selectedSites()$key)) { return() }
    ss <- selectedSites()
    id <- ss$site_id[ss$key %in% poly][1]
    return(paste(substr(id,1,2),substr(id,3,5),substr(id,6,9),sep="-"))
  })
  
  output$areaServedArea <- renderText({
    if (is.null(input$clickedAreaServed)) { return() }
    poly <- polygons()
    if (is.null(poly)) { return() }
    area.km <- as.numeric(poly$area_km[poly$key == input$clickedAreaServed])
    return(paste(format(round(area.km*0.3861,0),big.mark=",")," mi<sup>2</sup> (",
      format(area.km,big.mark=",")," km<sup>2</sup>)",sep=""))
  })
  
  output$areaServedPopulation <- renderText({
    if (is.null(input$clickedAreaServed)) { return() }
    poly <- polygons()
    if(is.null(poly)) { return() }
    return(format(poly$total[poly$key == input$clickedAreaServed],big.mark=","))
  })
  
  output$areaServedDemographics <- renderPlot({
    if (is.null(input$clickedAreaServed)) { return() }
    poly <- as.integer(input$clickedAreaServed)
    if (is.null(selectedSites())) { return() }
    if (!(poly %in% selectedSites()$key)) { return() }
    df <- subset(st_drop_geometry(polygons()),key == poly)
    if (nrow(df) == 0) { return() }
    site.id <- paste("Site",paste(substr(df$site_id,1,2),substr(df$site_id,3,5),substr(df$site_id,6,9),sep="-"))
    pop.cols <- c("male","female","white","black","native","asian","islander","other","multiple","hispanic",
      "age_0_4","age_5_9","age_10_14","age_15_19","age_20_24","age_25_29","age_30_34","age_35_39","age_40_44",
      "age_45_49","age_50_54","age_55_59","age_60_64","age_65_69","age_70_74","age_75_79","age_80_84","age_85_up")
    colors <- c(rep("cyan",2),rep("magenta",8),rep("yellow",18))
    bar.hgts <- unlist(df[,pop.cols])
    ymax <- 10000*ceiling(max(c(bar.hgts,10000),na.rm=TRUE)/10000)
    yspace <- 10^floor(log(ymax,base=10))/ifelse(substr(ymax,1,1) > 5,1,ifelse(substr(ymax,1,1) > 2,2,5))
    ymax <- ymax + yspace/2
    par(mar=c(10,8,2,0),las=2,cex.main=2,cex.axis=2)
    plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(0,30),xlab="",yaxs='i',ylim=c(0,ymax),ylab="",
      main=paste("Demographics for Area Served by AQS Site ID =",site.id))
    axis(side=1,at=c(1,2,seq(3.5,10.5,1),seq(12,29,1)),labels=pop.cols)
    axis(side=2,at=seq(0,ymax,yspace),labels=prettyNum(sprintf("%8d",seq(0,ymax,yspace)),big.mark=","))
    rect(xleft=0,ybottom=0,xright=30,ytop=ymax,col="gray85")
    abline(h=seq(from=yspace,to=ymax,by=yspace),col="white")
    for (i in 1:length(pop.cols)) {
      xp <- i + 0.5*(i > 2) + 0.5*(i > 10)
      rect(xleft=xp-0.5,ybottom=0,xright=xp+0.5,ytop=bar.hgts[i],col=colors[i])
    }
    box()
  },width=1200,height=900)
  
  ## Trend chart
  output$trendChart <- renderImage({
    if (is.null(input$pollutantSelect)) { return(list(src="images/notrend.png")) }
    if (input$pollutantSelect == "none") { return(list(src="images/notrend.png")) }
    if (is.null(input$popupID)) { return(list(src="images/notrend.png")) }
    site <- input$popupID[1]
    poll <- input$pollutantSelect
    annual.data <- dbGetQuery(db,paste(
      "SELECT annual.pollutant AS poll,
              sites.site_id AS site_id,
              annual.year AS year,
              annual.value AS annual_value,
              annual.valid AS annual_valid
         FROM annual, sites
        WHERE annual.key = ",site,"
          AND annual.pollutant LIKE '",poll,"%'
          AND sites.key = ",site,sep=""))
    dv.data <- dbGetQuery(db,paste(
      "SELECT dvs.pollutant AS poll,
              sites.site_id AS site_id,
              dvs.year AS year,
              dvs.value AS design_value,
              dvs.valid AS design_valid
         FROM dvs, sites
        WHERE dvs.key = ",site,"
          AND dvs.pollutant LIKE '",poll,"%'
          AND sites.key = ",site,sep=""))
    curr.psids <- c(2,3,4,8,12,19,20,23,26,27,28)
    site.id <- as.character(dbGetQuery(db,paste("SELECT site_id FROM sites WHERE key =",site,sep="")))
    aqs.site.id <- paste(substr(site.id,1,2),substr(site.id,3,5),substr(site.id,6,9),sep="-")
    naaqs <- dbGetQuery(db,paste(
      "SELECT poll_name, avg_time, level, units 
         FROM standards
        WHERE pollutant = '",poll,"'
          AND ps_id IN (",paste(curr.psids,collapse=","),")
     ORDER BY avg_time",sep=""))
    naaqs$std_lab <- naaqs$poll_name
    if (nrow(naaqs) > 1) { naaqs$std_lab <- paste(str_to_title(naaqs$avg_time),naaqs$std_lab) }
    if (poll == "pm25") { naaqs <- naaqs[c(2,1),] }
    curr.year <- max(dbGetQuery(db,"SELECT DISTINCT year FROM annual"))
    max.na <- function(x) { return(ifelse(all(is.na(x)),NA,max(x,na.rm=TRUE))) }
    ymax <- pmax(1.2*max.na(annual.data$annual_value),1.2*max.na(dv.data$design_value),
      1.2*ifelse(poll == "co",9,max(naaqs$level)),na.rm=TRUE)
    annual.data <- split(annual.data,annual.data$poll)
    dv.data <- split(dv.data,dv.data$poll)
    file.name <- paste("images/temp/trend_",poll,"_",site.id,".png",sep="")
    png(filename=paste("www",file.name,sep="/"),width=1200,height=900)
    par(mar=c(4,5,2,1),mgp=c(3.75,1,0),cex.axis=1.5,cex.lab=1.5,cex.main=2,las=2)
    plot(x=NULL,y=NULL,type='n',xaxs='i',xaxt='n',xlim=c(1999.5,(curr.year+0.5)),xlab="",
      yaxs='i',ylim=c(0,ymax),ylab=paste("Concentration (",naaqs$units[1],")",sep=""),
      main=paste(naaqs$poll_name[1],"Trends for AQS Site ID =",aqs.site.id))
    axis(side=1,at=c(2000:curr.year),labels=c(2000:curr.year))
    rect(xleft=1999.5,xright=(curr.year+0.5),ybottom=0,ytop=ymax,col="gray80")
    abline(h=seq(par("yaxp")[1],par("yaxp")[2],length.out=(par("yaxp")[3]*2+1)),
      v=c(2000:curr.year),col="white")
    abline(h=naaqs$level[1],lty=1,lwd=2,col="black"); box();
    if (nrow(naaqs) > 1) { abline(h=naaqs$level[2],lty=2,lwd=2,col="black") }
    legend.args <- data.frame(col="black",lty=1,pch=NA,
      legend=paste(naaqs$std_lab[1],"NAAQS Level"))
    if (length(annual.data) == 1) { 
      legend.args <- rbind(legend.args,data.frame(col=NA,lty=NA,pch=NA,legend="")) 
    }
    if (length(annual.data) > 1) { 
      legend.args <- rbind(legend.args,data.frame(col=c("black",NA,NA),lty=c(2,NA,NA),pch=NA,
        legend=c(paste(naaqs$std_lab[2],"NAAQS Level"),"","")))
    }   
    ann <- annual.data[[1]]; dv <- dv.data[[1]];
    if (nrow(ann) > 0) {
      pt.pch <- sapply(ann$annual_valid,switch,N=22,Y=15)
      lines(x=ann$year,y=ann$annual_value,lty=2,lwd=2,col="blue")
      points(x=ann$year,y=ann$annual_value,cex=2,col="blue",pch=pt.pch)
      legend.args <- rbind(legend.args,data.frame(col=rep("blue",2),lty=rep(2,2),pch=c(15,22),
        legend=paste(c("Valid","Invalid"),"Annual",switch(poll,co="2nd Max 1-hour",lead="Max 3-month",
          no2="98th Percentile",ozone="4th Maximum",pm10="2nd Maximum",pm25="Mean",so2="99th Percentile"))))
    }
    if (nrow(dv) > 0) {
      pt.pch <- sapply(dv$design_valid,switch,N=21,Y=16)
      lines(x=dv$year,y=dv$design_value,lty=1,lwd=2,col="blue")
      points(x=dv$year,y=dv$design_value,cex=2,col="blue",pch=pt.pch)
      legend.args <- rbind(legend.args,data.frame(col=rep("blue",2),lty=rep(1,2),pch=c(16,21),
        legend=paste(c("Valid","Invalid"),naaqs$std_lab[1],"Design",ifelse(poll == "pm10",
          "Concentration","Value"))))
    }
    if (length(annual.data) > 1) {
      ann <- annual.data[[2]]; dv <- dv.data[[2]];
      if (nrow(ann) > 0 & poll != "no2") {
        pt.pch <- sapply(ann$annual_valid,switch,N=22,Y=15)
        lines(x=ann$year,y=ann$annual_value,lty=2,lwd=2,col="red")
        points(x=ann$year,y=ann$annual_value,cex=2,col="red",pch=pt.pch)
        legend.args <- rbind(legend.args,data.frame(col=rep("red",2),lty=rep(2,2),pch=c(15,22),
          legend=paste(c("Valid","Invalid"),"Annual",switch(poll,co="2nd Max 8-hour",
            no2="Mean",pm25="98th Percentile",so2="Mean"))))
      }
      if (nrow(dv) > 0) {
        pt.pch <- sapply(dv$design_valid,switch,N=21,Y=16)
        lines(x=dv$year,y=dv$design_value,lty=1,lwd=2,col="red")
        points(x=dv$year,y=dv$design_value,cex=2,col="red",pch=pt.pch)
        legend.args <- rbind(legend.args,data.frame(col=rep("red",2),lty=rep(1,2),pch=c(16,21),
          legend=paste(c("Valid","Invalid"),naaqs$std_lab[2],"Design Value")))
      }
    }
    if (poll == "co") { 
      legend.args <- legend.args[-c(3,4,6,8,10,12),]
      legend.args$legend <- gsub("Valid","",legend.args$legend)
      if (ymax < 35) { legend.args$col[1] <- NA; legend.args$lty[1] <- NA; legend.args$legend[1] <- ""; }
    }
    legend(x="top",legend=legend.args$legend,col=legend.args$col,lty=legend.args$lty,
      lwd=2,pch=legend.args$pch,cex=1.5,pt.cex=2,ncol=3,bty='n')
    dev.off()
    session$sendCustomMessage(type="updateTrendChart",file.name)
    return(list(src=file.name))
  },deleteFile=FALSE)
  
  ## Correlation matrices
  cormatTable <- eventReactive(input$cormatButton,{
    sites <- subset(pollutantSites(),key %in% activeSites())
    if (is.null(sites)) { return() }
    if (nrow(sites) < 2) { return() }
    showLoading()
    conc <- dbGetQuery(db,paste("
     SELECT sites.site_id AS site,
            daily.sample_date AS date,
            daily.value AS conc
       FROM daily, sites
      WHERE daily.key = sites.key
        AND daily.pollutant = '",input$pollutantSelect,"'
        AND sites.site_id IN ('",paste(sites$site_id,collapse="','"),"')
      ORDER BY 1,2",sep=""))
    if (nrow(conc) == 0) { hideLoading(); return(); }
    sites <- st_as_sf(subset(sites,site_id %in% unique(conc$site)),
      coords=c("longitude","latitude"),crs=4326)
    mean.na <- function(x) { return(ifelse(all(is.na(x)),NA,mean(x,na.rm=TRUE))) }
    dist.km <- round(st_distance(sites,sites)/1000); units(dist.km) <- NULL;
    values <- dcast(conc,date ~ site,value.var="conc")
    cor.val <- round(cor(values[,-1],use="pairwise.complete.obs",method="pearson"),4)
    cor.table <- vector("list",(nrow(sites)*(nrow(sites)-1)/2)); k <- 1;
    for (i in 1:(nrow(sites)-1)) {
      for (j in (i+1):nrow(sites)) {
        cor.table[[k]] <- data.frame(site1=sites$site_id[i],site2=sites$site_id[j],
          dist=dist.km[i,j],obs=sum(!is.na(values[,(i+1)]) & !is.na(values[,(j+1)])),
          cor=cor.val[i,j],diff=round(mean.na(abs(values[,(i+1)] - values[,(j+1)])),4))
        k <- k + 1
      }
    }
    hideLoading()
    return(as.data.frame(rbindlist(cor.table)))
  })
  
  output$cormatChart <- renderPlot({
    input$cormatButton
    if (input$cormatButton == 0) { return() }
    isolate({
      cor.table <- cormatTable()
      if (is.null(cor.table)) { return() }
      if (nrow(cor.table) > 800) { return() }
      poll <- as.character(dbGetQuery(db,paste("SELECT DISTINCT poll_name FROM standards
        WHERE pollutant = '",input$pollutantSelect,"'",sep="")))
      aoi <- "Custom"
      if (!is.null(input$areaSelect) & !is.null(input$areaSelectSelect)) {
        if (input$areaSelect != "none" & input$areaSelectSelect != "") { 
          aoi <- as.character(dbGetQuery(db,paste("SELECT name FROM ",
            switch(tolower(input$areaSelect),state="states",cbsa="cbsas",csa="csas"),"
            WHERE code = '",input$areaSelectSelect,"'",sep="")))
        }
      }
      unit <- as.character(dbGetQuery(db,paste("SELECT DISTINCT units FROM standards
        WHERE pollutant = '",input$pollutantSelect,"'",sep="")))
      curr.year <- max(dbGetQuery(db,"SELECT DISTINCT year FROM annual"))
      ids <- unique(c(cor.table$site1,cor.table$site2)); N <- length(ids);
      dvs <- dbGetQuery(db,paste(
        "SELECT sites.site_id, dvs.pollutant, dvs.value
           FROM dvs, sites
          WHERE dvs.key = sites.key
            AND dvs.pollutant ",switch(input$pollutantSelect,pm25="IN ('pm25a','pm25d')",
                paste("= '",input$pollutantSelect,"'",sep="")),"
            AND sites.site_id IN ('",paste(ids,collapse="','"),"')
            AND dvs.year = ",curr.year,"
          ORDER BY 1",sep=""))
      if (poll == "PM2.5") { dvs <- dcast(dvs,site_id ~ pollutant) }
      cor.diff <- obs.dist <- matrix(NA,nrow=N,ncol=N,dimnames=list(c(ids),c(ids)))
      cor.diff[lower.tri(cor.diff)] <- cor.table$cor[order(cor.table$site1)]
      cor.diff[upper.tri(cor.diff)] <- cor.table$diff[order(cor.table$site2)]
      obs.dist[lower.tri(obs.dist)] <- cor.table$obs[order(cor.table$site1)]
      obs.dist[upper.tri(obs.dist)] <- cor.table$dist[order(cor.table$site2)]
      if (poll != "PM2.5") { 
        diag(cor.diff) <- diag(obs.dist) <- dvs$value[match(ids,dvs$site_id)]
      }
      if (poll == "PM2.5") { 
        diag(cor.diff) <- dvs$pm25a[match(ids,dvs$site_id)]
        diag(obs.dist) <- dvs$pm25d[match(ids,dvs$site_id)]
      }
      cor.colors <- colorRampPalette(c("#0000FF","#FFFFFF"))
      cor.col <- c("#0000CC",cor.colors(11)[1:10])
      cor.seq <- c(-1,seq(0,1,0.1))
      diff.colors <- colorRampPalette(c("#FFFFFF","#FF0000"))
      diff.col <- c(diff.colors(11)[2:11],"#CC0000")
      diff.max <- switch(input$pollutantSelect,co=1,lead=0.1,no2=20,ozone=0.01,
        pm10=50,pm25=10,so2=50,1)
      diff.seq <- c(seq(0,diff.max,diff.max/10),Inf)
      mar.axis <- ifelse(N > 25,7,ifelse(N > 15,10,13))
      txt.cex <- ifelse(N > 25,1.5,ifelse(N > 15,2,2.5))
      col.vals <- matrix("#FFFFFF",nrow=N,ncol=N,dimnames=list(c(ids),c(ids)))
      col.vals[lower.tri(col.vals)] <- sapply(cor.diff[lower.tri(cor.diff)],
        function(x) cor.col[as.integer(cut(x,breaks=cor.seq,right=FALSE))])
      col.vals[upper.tri(col.vals)] <- sapply(cor.diff[upper.tri(cor.diff)],
        function(x) diff.col[as.integer(cut(x,breaks=diff.seq,right=FALSE))])
      layout(matrix(c(1,1,2,3,4,4),3,2),widths=c(0.9,0.1),heights=c(0.5,0.42,0.08))
      par(mar=c(1,mar.axis,mar.axis,1),mgp=c(2,0.25,0),las=2,tcl=0,cex.axis=txt.cex)
      plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",xlim=c(0,N)+0.5,
        yaxs='i',ylab="",ylim=c(0,N)+0.5,main="")
      axis(side=2,at=N:1,labels=ids); axis(side=3,at=1:N,labels=ids);
      rect(xleft=rep(1:N,each=N)-0.5,xright=rep(1:N,each=N)+0.5,ybottom=rep(N:1,times=N)-0.5,
        ytop=rep(N:1,times=N)+0.5,col=col.vals)
      text(x=rep(1:N,each=N),y=rep(N:1,times=N),cex=txt.cex,labels=obs.dist)
      if (poll == "PM2.5") {
        rect(xleft=c(1:N)-0.5,xright=c(1:N)+0.5,ybottom=c(N:1)-0.5,ytop=c(N:1)+0.5,col="#FFFFFF")
        text(x=c(1:N),y=c(N:1),labels=paste(diag(cor.diff),diag(obs.dist),sep="/"),cex=txt.cex)
      }
      par(mar=c(0,mar.axis,0,0))
      plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlim=c(0,1),xlab="",
        yaxs='i',ylim=c(0,1),ylab="")
      text(x=0,y=0.8,pos=4,cex=2.5,labels="Values in lower triangle = # of obs used in correlation")
      text(x=0,y=0.5,pos=4,cex=2.5,labels="Values in upper triangle = Distance in km between sites")
      text(x=0,y=0.2,pos=4,cex=2.5,labels="Values along the diagonal = Most recent design values")
      text(x=0.5,y=0.8,pos=4,cex=2.5,labels=paste("Pollutant =",poll))
      text(x=0.5,y=0.5,pos=4,cex=2.5,labels=paste("Area of Interest =",aoi))
      text(x=0.5,y=0.2,pos=4,cex=2.5,labels="To save chart, right-click and select 'Save image as...'")
      par(mar=c(2,4,1,7),mgp=c(1,1,0),cex.axis=2.5,cex.lab=2.5)
      plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",xlim=c(0,1),yaxs='i',
        ylab=paste("Mean Absolute Difference (",unit,")",sep=""),ylim=c(0,11),main="")
      rect(xleft=rep(0,11),xright=rep(1,11),ybottom=seq(0,10,1),ytop=seq(1,11,1),
        border=NA,col=diff.col)
      axis(side=4,at=seq(0,11,1),labels=diff.seq); box();
      par(mar=c(1,4,2,7),mgp=c(1,1,0),cex.axis=2.5,cex.lab=2.5)
      plot(x=NULL,y=NULL,type='n',axes=FALSE,xaxs='i',xlab="",xlim=c(0,1),
        yaxs='i',ylab="Pearson Correlation (R)",ylim=c(0,11),main="")
      rect(xleft=rep(0,11),xright=rep(1,11),ybottom=seq(0,10,1),ytop=seq(1,11,1),
        border=NA,col=cor.col)
      axis(side=4,at=seq(0,11,1),labels=cor.seq); box();
      session$sendCustomMessage("showCormat",TRUE)
    })
  },width=1600,height=1200)
  
  ## Removal bias
  rembiasTable <- reactive({
    sites <- subset(pollutantSites(),key %in% activeSites())
    sn <- selectedNeighbors()
    if (is.null(sites) | is.null(sn)) { 
      session$sendCustomMessage("showAlert",list(header="Insufficient Data",
      body="No daily data could be found for sites within your area of interest.
        Please expand your area of interest, or select a different pollutant.")) 
      return()
    }
    showLoading()
    conc <- dbGetQuery(db,paste("
      SELECT key, sample_date, value FROM daily
       WHERE key IN (",paste(sn$key,collapse=","),")
         AND pollutant = '",input$pollutantSelect,"'
       ORDER BY 2,3",sep=""))
    sites <- subset(sites,key %in% unique(conc$key))
    sn <- subset(sn,key %in% unique(conc$key))
    if (nrow(sites) == 0 | nrow(sn) == 0) { 
      session$sendCustomMessage("showAlert",list(header="Insufficient Data",
      body="No daily data could be found for sites within your area of interest.
        Please expand your area of interest, or select a different pollutant."))
      hideLoading()
      return()
    }
    combos <- deldir(sn$longitude,sn$latitude)$delsgs
    combos$dist <- st_distance(st_as_sf(combos[,1:2],coords=c("x1","y1"),crs=4326),
      st_as_sf(combos[,3:4],coords=c("x2","y2"),crs=4326),by_element=TRUE)/1000
    combos$ind1 <- sn$key[combos$ind1]; combos$ind2 <- sn$key[combos$ind2];
    units(combos$dist) <- NULL
    bias.table <- lapply(sites$key,function(site) {
      site.id <- sites$site_id[which(sites$key == site)]
      site.data <- subset(conc,key == site,c("sample_date","value"))
      neighbors <- subset(combos,ind1 == site | ind2 == site)
      neighbors$key <- apply(neighbors,1,function(r) {
        if(r['ind1'] == site) { return(r['ind2']) } else { return(r['ind1'])}})
      neigh.data <- subset(conc,key %in% neighbors$key,c("key","sample_date","value"))
      neigh.data$dist <- neighbors$dist[match(neigh.data$key,neighbors$key)]
      neigh.data <- subset(neigh.data,sample_date %in% site.data$sample_date) 
      values <- dcast(neigh.data,sample_date ~ key,value.var="value")
      weights <- dcast(neigh.data,sample_date ~ key,value.var="dist")
      weights[is.na(values)] <- NA
      num <- apply(values[,-1]*(1/(weights[,-1]^2)),1,sum,na.rm=TRUE)
      den <- apply(1/(weights[,-1]^2),1,sum,na.rm=TRUE)
      avg <- data.frame(sample_date=values[den > 0,1],estimate=num[den > 0]/den[den > 0])
      bias <- merge(site.data,avg,by="sample_date")
      d <- switch(input$pollutantSelect,co=1,lead=2,no2=0,ozone=3,pm10=0,pm25=1,so2=0,0)
      bias$diff <- bias$estimate - bias$value
      rdiff <- 100*(bias$diff[bias$value > 0]/bias$value[bias$value > 0])
      data.frame(key=site,site_id=site.id,bias_n=nrow(neighbors),bias_obs=nrow(bias),
        bias_mean=round(mean(bias$diff),d+1),bias_sd=round(sd(bias$diff),d+1),
        bias_min=round(min(bias$diff),d),bias_max=round(max(bias$diff),d),
        rel_mean=round(mean(rdiff),1),rel_min=round(min(rdiff)),rel_max=round(max(rdiff)))
    })
    hideLoading()
    return(as.data.frame(rbindlist(bias.table)))
  })
  
  observeEvent(input$rembiasButton,{
    validate(need(rembiasTable(),FALSE))
    isolate({ if(!is.null(rembiasTable())) {
      session$sendCustomMessage("rembiasUpdate",list(data=rembiasTable()))  
    }})
  })
  
  ## Download buttons
  output$trendDataDownload <- downloadHandler(filename=function() {
    paste("trenddata_",input$pollutantSelect,"_",gsub("-","",gsub(":","",
      gsub(" ","_",round(Sys.time())))),".csv",sep="")},content=function(file) {
    poll <- input$pollutantSelect
    sites <- selectedSites()
    if (poll != "lead") {
      annual.data <- dbGetQuery(db,paste(
        "SELECT pollutant as poll, key, year, value AS ann FROM annual
          WHERE annual.key IN ('",paste(sites$key,collapse="','"),"')
            AND annual.pollutant ",switch(poll,pm25="IN ('pm25a','pm25d')",
                paste("= '",poll,"'",sep="")),sep=""))
      if (poll == "pm25") { annual.data <- dcast(annual.data,key + poll ~ year,value.var="ann") }
      if (poll != "pm25") { annual.data <- dcast(annual.data,key ~ year,value.var="ann") }
      cols <- c(ifelse(poll == "pm25",3,2):ncol(annual.data))
      colnames(annual.data)[cols] <- paste("ann",colnames(annual.data)[cols],sep=".")
    }
    if (poll != "co") {
      dv.data <- dbGetQuery(db,paste(
        "SELECT pollutant as poll, key, year, value AS dv FROM dvs
          WHERE dvs.key IN ('",paste(sites$key,collapse="','"),"')
            AND dvs.pollutant ",switch(poll,pm25="IN ('pm25a','pm25d')",
                paste("= '",poll,"'",sep="")),sep=""))
      if (poll == "pm25") { dv.data <- dcast(dv.data,key + poll ~ year,value.var="dv") }
      if (poll != "pm25") { dv.data <- dcast(dv.data,key ~ year,value.var="dv") }
      cols <- c(ifelse(poll == "pm25",3,2):ncol(dv.data))
      colnames(dv.data)[cols] <- paste("dv",as.numeric(colnames(dv.data)[cols])-2,
        colnames(dv.data)[cols],sep=".")
    }
    if (poll == "co") { 
      colnames(annual.data) <- gsub("ann","dv",colnames(annual.data))
      conc.data <- annual.data
    }
    if (poll == "lead") { conc.data <- dv.data }
    if (poll == "pm25") { 
      conc.data <- merge(annual.data,dv.data,by=c("key","poll"),all=TRUE)
      conc.data$poll <- sapply(conc.data$poll,function(x) ifelse(x == "pm25a","Annual","Daily"))
      colnames(conc.data)[2] <- "standard"
    }
    if (!poll %in% c("co","lead","pm25")) { 
      conc.data <- merge(annual.data,dv.data,by="key",all=TRUE)
    }
    out <- merge(sites,conc.data,by="key",all=TRUE)[,-1]
    write.csv(out,file,row.names=FALSE,na="")
  })
  
  output$areaServedDownload <- downloadHandler(filename=function() {
    paste("areaserved_",input$pollutantSelect,"_",gsub("-","",gsub(":","",
      gsub(" ","_",round(Sys.time())))),".csv",sep="")},content=function(file) {
    t <- st_drop_geometry(polygons())
    out <- t[,c("site_id","site_name","address","latitude","longitude",
      "epa_region","state_name","county_name","cbsa_name","csa_title","monitor_count",
      "pollutants","area_km","ozone_prob","pm25_prob","no2_sat","total","male","female",
      "white","black","native","asian","islander","other","multiple","hispanic",
      "age_0_4","age_5_9","age_10_14","age_15_19","age_20_24","age_25_29",
      "age_30_34","age_35_39","age_40_44","age_45_49","age_50_54","age_55_59",
      "age_60_64","age_65_69","age_70_74","age_75_79","age_80_84","age_85_up")]
    colnames(out) <- c("AQS Site ID","Site Name","Address","Latitude","Longitude","EPA Region",
      "State Name","County Name","CBSA Name","CSA Name","Monitor Count","Pollutants","Area (km^2)",
      "Ozone Exceedance Probability (%)","PM2.5 Exceedance Probability (%)","Annual Mean NO2 Concentration (ppb)",
      "Total Population","Male","Female","Caucasian/White","African/Black","Native American",
      "Asian","Pacific Islander","Other Race","Multiple Races","Hispanic/Latino",
      "Age 0 to 4","Age 5 to 9","Age 10 to 14","Age 15 to 19","Age 20 to 24","Age 25 to 29",
      "Age 30 to 34","Age 35 to 39","Age 40 to 44","Age 45 to 49","Age 50 to 54","Age 55 to 59",
      "Age 60 to 64","Age 65 to 69","Age 70 to 74","Age 75 to 79","Age 80 to 84","Age 85 and Over")
      write.csv(out,file=file,row.names=FALSE,na="")                             
  })
  
  output$correlationDownload <- downloadHandler(filename=function() {
    paste("correlation_",input$pollutantSelect,"_",gsub("-","",gsub(":","",
      gsub(" ","_",round(Sys.time())))),".csv",sep="")},content=function(file) {
      out <- cormatTable()
      colnames(out) <- c("AQS Site ID 1","AQS Site ID 2","Distance (km)",
        "# Observations","Correlation","Mean Difference")
      write.csv(out,file,row.names=FALSE,na="")
  })
  
  output$removalBiasDownload <- downloadHandler(filename=function() {
    paste("rembias_",input$pollutantSelect,"_",gsub("-","",gsub(":","",
      gsub(" ","_",round(Sys.time())))),".csv",sep="")},content=function(file) {
      out <- rembiasTable()
      out <- out[,c("site_id","bias_n","bias_obs","bias_mean","bias_sd",
         "bias_min","bias_max","rel_mean","rel_min","rel_max")]
      colnames(out) <- c("AQS Site ID","Neighbors Included","Daily Obs Count",
        "Mean Removal Bias","Removal Bias Standard Deviation","Min Removal Bias",
        "Max Removal Bias","Mean Relative Bias (%)","Min Relative Bias (%)",
        "Max Relative Bias (%)")
      write.csv(out,file,row.names=FALSE,na="")
  })
  
  observe({input$mPTCPO*2})
})