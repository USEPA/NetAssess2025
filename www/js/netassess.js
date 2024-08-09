$(document).ready(function() {
  var ua = window.navigator.userAgent;
  var msie = ua.indexOf("MSIE ");
  if (msie > 0 || !!navigator.userAgent.match(/Trident.*rv\:11\./)) { 
    netAssess.showAlert("Internet Explorer Detected", "Unfortunately, NetAssess2025 does not work with Internet Explorer. For best performance, please use a modern version of <a href = 'https://www.google.com/chrome/'>Chrome</a> or <a href = 'https://www.mozilla.org/en-US/firefox/new/'>Firefox</a>.")
  } else {
    setTimeout(netAssess.iCH, 1*60*1000)
  }
  return false;
})

netAssess.resizeMap = function() {
  document.getElementById("map").style.width = window.innerWidth + "px";
  document.getElementById("map").style.height = (window.innerHeight - 40) + "px";
}

$(window).resize(function() {
  netAssess.resizeMap();
})
netAssess.resizeMap();

netAssess.data = {
  us_bounds: L.latLngBounds([24.4,-124.8],[49.4,-66.9]),
  newSiteCounter: 200001,
  newSites: {},
  biasLayers: []
}

netAssess.loading = {
  show: function() {
    $("div.loading").removeClass("hidden");
  },
  hide: function() {
    $("div.loading").addClass("hidden");
  }
}
netAssess.loading.show();

netAssess.basemaps = {
  "Gray": L.layerGroup([L.esri.basemapLayer("Gray"), L.esri.basemapLayer("GrayLabels")]),
  "Street": L.esri.basemapLayer("Streets"),
  "Satellite": L.layerGroup([L.esri.basemapLayer("Imagery"), L.esri.basemapLayer("ImageryLabels")])
}

netAssess.overlays = {
    o3: L.imageOverlay("images/ozone_prob.png",[[24.51748,-124.76255],[49.38436,-66.92599]],{opacity: 0.65}),
  pm25: L.imageOverlay("images/pm25_prob.png",[[24.51748,-124.76255],[49.38436,-66.92599]],{opacity: 0.65}),
  o3ej: L.imageOverlay("images/ozone_EJindex.png",[[24.51748,-124.76255],[49.38436,-66.92599]],{opacity: 0.65}),
  pmej: L.imageOverlay("images/pm25_EJindex.png",[[24.51748,-124.76255],[49.38436,-66.92599]],{opacity: 0.65})
}

netAssess.layerGroups = {}
netAssess.layerGroups.aoi = L.featureGroup(null);
netAssess.layerGroups.rembias = L.featureGroup(null);
netAssess.layerGroups.newSiteSelection = L.layerGroup();
netAssess.layerGroups.areaServed = L.featureGroup(null);
netAssess.layerGroups.sites = L.siteGroup({
  aoiLayer: netAssess.layerGroups.aoi,
  onEachSite: function(site) {
    po = "<span class = 'popup-text'><h4 class = 'header'>Site Information</h4>"
    po = po + "<center><table class = 'popup-table'>"
    po = po + "<tr><td>Site ID</td><td>" + site.properties.site_id + "</td></tr>"
    po = po + "<tr><td>Site Name</td><td>" + site.properties.site_name + "</td></tr>"
    po = po + "<tr><td>Address</td><td>" + site.properties.address + "</td></tr>"
    po = po + "<tr><td>EPA Region</td><td>" + site.properties.epa_region + "</td></tr>"
    po = po + "<tr><td>State</td><td>" + site.properties.state_name + "</td></tr>"
    po = po + "<tr><td>County</td><td>" + site.properties.county_name + "</td></tr>"
    po = po + "<tr><td>CBSA</td><td>" + site.properties.cbsa_name + "</td></tr>"
    po = po + "<tr><td>CSA</td><td>" + site.properties.csa_title + "</td></tr>"
    po = po + "<tr><td>Monitor Count</td><td>" + site.properties.monitor_count + "</td></tr>"
    po = po + "<tr><td>Pollutants</td><td>" + site.properties.pollutants + "</td></tr>"
    po = po + "<tr><td colspan = 2 style = 'text-align: center; padding-top: 10px; border-right: none;'>Trends (click to enlarge)</td></tr>"
    po = po + "<tr><td colspan = 2 style = 'text-align: center; border-right: none;'><div class = 'popup-trend'><img src = 'images/notrend.png' /></div></center></td></tr>"
    po = po + "</table>"
    po = po + "</span>"
    site.bindPopup(po, {minWidth: 300});
  }
})

netAssess.layerGroups.newSites = L.siteGroup({
  contextmenu: true,
  aoiLayer: netAssess.layerGroups.aoi,
  visibilityTest: function(site) { return (site.properties.visible) },
  onEachSite: function(site) {
    po = "<span class = 'popup-text'><h4 class = 'header'>New Site Information</h4>"
    po = po + "<center><table class = 'popup-table'>"
    po = po + "<tr><td>Site Name</td><td>" + site.properties.site_name + "</td></tr>"
    po = po + "<tr><td>EPA Region</td><td>" + site.properties.epa_region + "</td></tr>"
    po = po + "<tr><td>State</td><td>" + site.properties.state_name + "</td></tr>"
    po = po + "<tr><td>County</td><td>" + site.properties.county_name + "</td></tr>"
    po = po + "<tr><td>CBSA</td><td>" + site.properties.cbsa_name + "</td></tr>"
    po = po + "<tr><td>CSA</td><td>" + site.properties.csa_title + "</td></tr>"
    po = po + "<tr><td>Pollutants</td><td>" + site.properties.pollutants + "</td></tr>"
    po = po + "</table></span>"
    site.bindPopup(po,{minWidth: 300});
  },
  styles: {
    selected: {radius: 5, opacity: 0.67, fillOpacity: 0.67, fillColor: "#40ff40", color: "#000", weight: 1},
    unselected: {radius: 4, opacity: 0.67, fillOpacity: 0.67, fillColor: "#008000", color: "#000", weight: 1}
  }
})

netAssess.floaters = {
  aoi: $.floater("#aoiFloater", {title: "Area of Interest", close: true, minimize: false, resize: false, width: "400px", top: "50px", left: "50px"}),
  newSite: $.floater("#newSiteFloater", {title: "New Site", close: false, minimize: false, resize: false, width: "400px", top: "50px", left: "50px"}),
  areaServed: $.floater("#areaServedFloater", {title: "Area Served", close: true, minimize: false, resize: false, width: "350px", top: "50px", right: "50px"}),
  cormat: $.floater("#cormatFloater", {title: "Correlation Matrix", close: true, minimize: false, resize: true, width: "800px", height: "640px", top: "50px", left: "50px"}),
  popup: $.floater("#popupFloater", {title: "Chart", close: true, minimize: false, resize: true, width: "800px", height: "640px", top: "50px", left: "50px"}),
  legend: $.floater("#legendFloater", {title: "Legend", close: false, minimize: true, resize: false, width: "400px", bottom: "10px", left: "10px"}),
  download: $.floater("#downloadFloater", {title: "Download Data", close: true, minimize: false, resize: false, width: "300px", top: "50px", right: "50px"})
}

$("#aoiButton").on("click", netAssess.floaters.aoi.open)
$("#downloadDataButton").on("click", netAssess.floaters.download.open)

$("#newSiteButton").on("click", function(e) {
  netAssess.layerGroups.newSiteSelection.clearLayers();
  netAssess.draw.newSite.enable();
})

netAssess.map = L.map("map", {
  contextmenu: true,
  contextmenuWidth: 140,
  contextmenuItems: [
    {text: "Full Extent", iconCls: "fa fa-search-minus", callback: netAssess.zoomOut}  
  ],
  zoomControl: true,
  maxZoom: 18,
  minZoom: 3
})

netAssess.draw = {
  polygon: new L.Draw.Polygon(netAssess.map, {allowInterSection: false, showArea: false, 
    drawError: {color: '#b00b00', timeout: 1000}, shapeOptions: {color: '#0033ff', fill: false}}),
  rectangle: new L.Draw.Rectangle(netAssess.map, {shapeOptions: {color: '#0033ff', fill: false}}),
  disable: function() {
    netAssess.draw.polygon.disable();
    netAssess.draw.rectangle.disable();
  },
  newSite: new L.Draw.Marker(netAssess.map, {icon: L.divIcon({className: 'fa fa-crosshairs new-site-selector'})})
}

$("#drawPolygonButton").on('click', function() {
  netAssess.draw.disable();
  netAssess.draw.polygon.enable()
});
	  
$("#drawRectangleButton").on('click', function() {
  netAssess.draw.disable();
  netAssess.draw.rectangle.enable()
});
  
$("#cancelDrawButton").on('click', netAssess.draw.disable);

netAssess.map.on('draw:created', function(e) {
  if(e.layerType == "marker") {
    netAssess.getNewSite(e.layer)
  } else {
    netAssess.setAOI(e.layer)
    netAssess.layerGroups.areaServed.clearLayers();
    $("#areaSelect0").click();
  }
})

netAssess.setAOI = function(aoi) {
  netAssess.layerGroups.aoi.clearLayers();
  aoi.addTo(netAssess.layerGroups.aoi);
  var aoiPolygons = {};
  netAssess.layerGroups.aoi.eachLayer(function(layer) {
    if(layer.hasOwnProperty("_layers")) {
      layer.eachLayer(function(sublayer) {
        var ll = sublayer.getLatLngs();
        aoiPolygons[sublayer._leaflet_id] = ll;
      })
    } else {
      var ll = layer.getLatLngs();
      aoiPolygons[layer._leaflet_id] = ll;
    }
  })
  document.getElementById("areaOfInterest").updateAnchor(aoiPolygons);
  netAssess.map.addLayer(netAssess.layerGroups.aoi);
}

netAssess.getNewSite = function(newSite) {
  newSite.addTo(netAssess.layerGroups.newSiteSelection)
  var lat = newSite._latlng.lat;
  var lng = newSite._latlng.lng;
  $("#nsLat").val(Math.round(lat * 1000000) / 1000000);
  $("#nsLng").val(Math.round(lng * 1000000) / 1000000);
  netAssess.floaters.newSite.open();
}

netAssess.setNewSite = function(e) {
  var latlng = L.latLng({lat: $("#nsLat").val(), lng: $("#nsLng").val()});
  var props = {
    name: $("#nsName").val(), poll: $("#nsPollutants").val(),
    key: netAssess.data.newSiteCounter
  }
  var opts = {contextmenu: true}          
  netAssess.data.newSiteCounter++
  netAssess.layerGroups.newSites.addSite(latlng, props, opts)
  netAssess.layerGroups.newSiteSelection.clearLayers();
  netAssess.floaters.newSite.close();
  netAssess.data.newSites[props.key] = {
    key: props.key, name: props.name, 
    lat: latlng.lat, lng: latlng.lng, 
   poll: props.poll, properties: { }
  }
  document.getElementById("newSites").updateAnchor(netAssess.data.newSites)
}

$("#cancelSiteAddButton").on("click", function(e) {
  netAssess.layerGroups.newSiteSelection.clearLayers();
  netAssess.floaters.newSite.close();
});

$("#newSiteAddButton").on("click", netAssess.setNewSite);

netAssess.basemaps.Gray.addTo(netAssess.map)

netAssess.zoomOut = function() {
  netAssess.map.fitBounds(netAssess.data.us_bounds);
}
netAssess.zoomOut()

$("#fullExtentButton").on("click", netAssess.zoomOut)

L.control.layers(netAssess.basemaps, 
  {"Area of Interest": netAssess.layerGroups.aoi,
   "Area Served": netAssess.layerGroups.areaServed,
   "Ozone Probability": netAssess.overlays.o3,
   "PM<sub>2.5</sub> Probability": netAssess.overlays.pm25,
   "Ozone EJ Index": netAssess.overlays.o3ej,
   "PM<sub>2.5</sub> EJ Index": netAssess.overlays.pmej,
   "Removal Bias": netAssess.layerGroups.rembias,
  }, {position: 'topleft'}).addTo(netAssess.map);

$.ajax({
  dataType: "json",
  url: "data/sites.geojson",
  success: function(data) {
    var d = netAssess.layerGroups.sites.addGeoJSON(data)
    window.setTimeout(function() {
      for(var i = 0; i < netAssess.data.biasLayers.length; i++) {
        netAssess.addBiasLayer(netAssess.data.biasLayers[i])
      }
      netAssess.loading.hide();
    },1000)
  }
});

netAssess.layerGroups.areaServed.addTo(netAssess.map);
netAssess.layerGroups.sites.addTo(netAssess.map).bringToFront();
netAssess.layerGroups.newSites.addTo(netAssess.map).bringToFront();
netAssess.layerGroups.newSiteSelection.addTo(netAssess.map);
$("#pollutantSelect").select2({width: "300px", height: "24px;"});
$("#areaSelectSelect").select2({width: "80%"});
$("#nsPollutants").select2({width: "100%"});

$("#pollutantSelect").on("change", function(e) {
  netAssess.layerGroups.newSites.testVisibility();
  netAssess.layerGroups.rembias.clearLayers();
  netAssess.layerGroups.areaServed.clearLayers();
  $("#areaServedDownload").parents("tr").addClass("disabled");
  $("#correlationDownload").parents("tr").addClass("disabled");
  $("#removalBiasDownload").parents("tr").addClass("disabled");
});

netAssess.layerGroups.aoi.on("layerremove", function() {
  $("#areaServedDownload").parents("tr").addClass("disabled");
  $("#correlationDownload").parents("tr").addClass("disabled");
  $("#removalBiasDownload").parents("tr").addClass("disabled");
  $("#trendDataDownload").parents("tr").addClass("disabled");
})

$("#areaServedDemographics").on("click", function(event) {
  $("#bigChart").attr("src", $(this).find("img").attr("src"))
  netAssess.floaters.popup.open();
})

netAssess.layerGroups.aoi.on("layeradd", function() {
  $("#trendDataDownload").parents("tr").removeClass("disabled");
})

$("#areaServedButton").on("click", function(event) {
  $("#areaServedDownload").parents("tr").removeClass("disabled");
  netAssess.errorChecking.areaServed(event);
})

$("#cormatButton").on("click", function(event) {
  $("#correlationDownload").parents("tr").removeClass("disabled");
  netAssess.errorChecking.cormat(event);
})

$("#rembiasButton").on("click", function(event) {
  $("#removalBiasDownload").parents("tr").removeClass("disabled");
  netAssess.errorChecking.rembias(event);
})

netAssess.errorChecking = {};

netAssess.errorChecking.basics = function(siteMax, siteMin) {
  var active = true;
  var body = "Please correct the following problems:<ul>";
  if($("#pollutantSelect").select2("val") == "none") {
    active = false;
    body = body + "<li>No pollutant selected</li>"
  }
  var activeSites = 0;
  var vs = netAssess.layerGroups.sites.options.visibleSites.concat(netAssess.layerGroups.newSites.options.visibleSites);
  var ss = netAssess.layerGroups.sites.options.selectedSites.concat(netAssess.layerGroups.newSites.options.selectedSites);
  for(var i = 0; i < ss.length; i++) {
    if(vs.indexOf(ss[i]) != -1) {
      activeSites++
    }
  }
  if(activeSites == 0) {
    active = false;
    body = body + "<li>No sites selected</li>";
  } else if(activeSites < siteMin && siteMin != 1) {
    active = false;
    body = body + "<li>Too few sites selected. Please select at least " + siteMin + " sites.</li>";
  } else if(activeSites > siteMax) {
    active = false;
    body = body + "<li>Too many sites selected. Please select no more than " + siteMax + " sites.</li>";
  }
  return {active: active, body: body};
}

netAssess.errorChecking.areaServed = function(event) {
  var bc = netAssess.errorChecking.basics(100, 2);
  if(bc.active) {
    netAssess.loading.show();
    netAssess.floaters.areaServed.close();
    netAssess.floaters.popup.close();
    netAssess.map.addLayer(netAssess.layerGroups.areaServed)
  } else {
    event.stopImmediatePropagation();
    bc.body = bc.body + "</ul>";
    netAssess.showAlert("Area Served Error", bc.body)
  }
}

netAssess.errorChecking.rembias = function(event) {
  var bc = netAssess.errorChecking.basics(100, 2);
  if(bc.active) {
    netAssess.loading.show();
    netAssess.layerGroups.rembias.addTo(netAssess.map);
  } else {
    event.stopImmediatePropagation();
    bc.body = bc.body + "</ul>";
    netAssess.showAlert("Removal Bias Error", bc.body)
  }
}

netAssess.errorChecking.cormat = function(event) {
  var bc = netAssess.errorChecking.basics(40, 2);
  if(bc.active) {
    netAssess.loading.show();
    netAssess.floaters.cormat.open()
  } else {
    event.stopImmediatePropagation();
    bc.body = bc.body + "</ul>";
    netAssess.showAlert("Correlation Matrix Error", bc.body);
  }
}

netAssess.showAlert = function(heading, body) {
  $("#alert-heading").html(heading);
  $("#alert-body").html(body);
  $("#alert").addClass("alert-open");
}

$("#areaSelectZoomButton, #aoiZoomButton").on("click", function() {
  netAssess.map.fitBounds(netAssess.layerGroups.aoi.getBounds());
})

$("#alert-close").on("click", function() {$("#alert").removeClass("alert-open")})

function updateAnchor(id,data) {
  var el = document.getElementById(id);
  if (el && el.updateAnchor) el.updateAnchor(data);
}
netAssess.layerGroups.sites.on("visibilityupdate", function(event) {
//  document.getElementById("visibleSites").updateAnchor(event.keys);
  updateAnchor("visibleSites",event.keys);
})
netAssess.layerGroups.sites.on("selectionupdate", function(event) {
//  document.getElementById("selectedSites").updateAnchor(event.keys);
  updateAnchor("selectedSites",event.keys);
})
netAssess.layerGroups.newSites.on("visibilityupdate", function(event) {
//  document.getElementById("visibleNewSites").updateAnchor(event.keys);
  updateAnchor("visibleNewSites",event.keys);
})
netAssess.layerGroups.newSites.on("selectionupdate", function(event) {
//  document.getElementById("selectedNewSites").updateAnchor(event.keys);
  updateAnchor("selectedNewSites",event.keys);
})

netAssess.map.on("popupopen", function(e) {
  if(e.popup._source.hasOwnProperty("properties")) {
    var key = e.popup._source.properties.key;
    document.getElementById("popupID").updateAnchor(key);
    $(".popup-trend").on("click", function(event) {
      $("#bigChart").attr("src", $(this).find("img").attr("src"))
      netAssess.floaters.popup.open();
    })
  }
})

/* Scripts to setup and handle the Sidebars */
netAssess.sidebars = {
  settings: L.control.sidebar('settings-sb', {position: 'right', autoPan: false}),
  help: L.control.sidebar('help-sb', {position: 'right', autoPan: false}),
  about: L.control.sidebar("about-sb", {position: 'right', autoPan: false})
}
L.easyButton("fa-cogs", function() {netAssess.toggleSidebars("settings");}, "Settings", netAssess.map);
L.easyButton("fa-question", function() {netAssess.toggleSidebars("help");}, "Help", netAssess.map);
L.easyButton("fa-info", function() {netAssess.toggleSidebars("about");}, "About", netAssess.map);

for(var sb in netAssess.sidebars) {
  if(netAssess.sidebars.hasOwnProperty(sb)) {
    netAssess.map.addControl(netAssess.sidebars[sb]);
  }
};

netAssess.toggleSidebars = function(sb) {
  var sidebars = netAssess.sidebars;
  for(var x in sidebars) {
    if(sidebars.hasOwnProperty(x)) {
      if(x == sb) {
        sidebars[x].toggle();
      } else {
        sidebars[x].hide();
      }
    }
  }
}

netAssess.map.on("overlayadd",function(e) {
  if(e.name == "Ozone Probability" || e.name == "PM<sub>2.5</sub> Probability") {
    $("#probLegend").css("display","table-row");
  } else if(e.name == "Ozone EJ Index" || e.name == "PM<sub>2.5</sub> EJ Index") {
    $("#EJLegend").css("display","table-row");
  } else if(e.name == "Removal Bias") {
    $("#biasLegend").css("display","table-row");
  } else {
    e.layer.bringToBack();
  }
})

netAssess.map.on("overlayremove",function(e) {
  if(e.name == "Ozone Probability" || e.name == "PM<sub>2.5</sub> Probability") {
    $("#probLegend").css("display","none");
  } else if(e.name == "Ozone EJ Index" || e.name == "PM<sub>2.5</sub> EJ Index") {
    $("#EJLegend").css("display","none");
  } else if(e.name == "Removal Bias") {
    $("#biasLegend").css("display","none");
  }
})

netAssess.updateBiasLayer = function(data) {
  var layer = netAssess.layerGroups.rembias, 
  style = {radius: 8, stroke: true, weight: 1, opacity: 1, color: "#000", fill: true, fillOpacity: 1}, 
  poll = $("#pollutantSelect").select2("val");
  if(poll == "co" || poll == "ozone") {
    unit = " ppm"
  } else if(poll == "no2" || poll == "so2") {
    unit = " ppb"
  } else {
    unit = " &mu;g/m<sup>3</sup>"
  }
  layer.clearLayers()
  var min = Math.min.apply(Math, data.data.bias_mean)
  var max = Math.max.apply(Math, data.data.bias_mean)
  max = Math.max(Math.abs(min), Math.abs(max));  
  function colorCalc(bias) {
    var b = Math.abs(bias);
    var col = "#FFF";
    var c = 256 - parseInt(b/max*256, 10);
    c = c.toString(16);
    if(c.length == 1) {c = "0" + c}
    if(bias < 0) {
      col = "#" + c + c + "FF";
    } else if(bias > 0) {
      col = "#FF" + c + c;
    }
    return col
  }
  netAssess.layerGroups.sites.eachLayer(function(site) {
    if(site.keyCheck(data.data.key)) {
      for(var i = 0; i < site.properties.key.length; i++) {
        var n = data.data.key.indexOf(site.properties.key[i])
        if(n != -1) { break; }
      }
      style.fillColor = colorCalc(data.data.bias_mean[n])
      var mark = L.circleMarker(site._latlng, style), po, id = ""
      po = "<span class = 'popup-text'><h4 class = 'header'>Removal Bias Information</h4>"
      po = po + "<center><table class = 'popup-table bias'>"
      po = po + "<tr><td>AQS Site ID</td><td>" + data.data.site_id[n] + "</td></tr>"
      po = po + "<tr><td>Neighbors Included</td><td>" + data.data.bias_n[n] + "</td></tr>"
      po = po + "<tr><td>Daily Obs Count</td><td>" + data.data.bias_obs[n] + "</td></tr>"
      po = po + "<tr><td>Mean Bias</td><td>" + data.data.bias_mean[n] + unit + "</td></tr>"
      po = po + "<tr><td>Bias Std Deviation</td><td>" + data.data.bias_sd[n] + unit + "</td></tr>"
      po = po + "<tr><td>Minimum Bias</td><td>" + data.data.bias_min[n] + unit + "</td></tr>"
      po = po + "<tr><td>Maximum Bias</td><td>" + data.data.bias_max[n] + unit + "</td></tr>"
      po = po + "<tr><td>Mean Relative Bias</td><td>" + data.data.rel_mean[n] + " %</td></tr>"
      po = po + "<tr><td>Min Relative Bias</td><td>" + data.data.rel_min[n] + " %</td></tr>"
      po = po + "<tr><td>Max Relative Bias</td><td>" + data.data.rel_max[n] + " %</td></tr>"
      po = po + "</table></span>"
      mark.bindPopup(po, {minWidth: 300})
      layer.addLayer(mark)
    }
  })
  netAssess.loading.hide();
}

$("#resetAppButton").on("click", function() {
  var r = confirm("Are you sure you want to reset the app?")
  if(r == true) { netAssess.resetApp(); }
})

netAssess.resetApp = function() {
  $("#pollutantSelect").select2("val","none");
  $("#pollutantSelect").trigger("change");
  $("#areaSelect0").click();    
  netAssess.layerGroups.aoi.clearLayers();
  netAssess.layerGroups.areaServed.clearLayers();
  netAssess.layerGroups.rembias.clearLayers();
  netAssess.map.removeLayer(netAssess.overlays.o3);
  netAssess.map.removeLayer(netAssess.overlays.pm25);
  netAssess.map.removeLayer(netAssess.overlays.o3ej);
  netAssess.map.removeLayer(netAssess.overlays.pmej);
  netAssess.map.removeLayer(netAssess.layerGroups.rembias);
  netAssess.map.removeLayer(netAssess.layerGroups.aoi);
  netAssess.map.addLayer(netAssess.layerGroups.areaServed);
  $("#alert").removeClass("alert-open")
  var k = Object.keys(netAssess.floaters);
  for(var i = 0; i < k.length; i++) {
    if(k[i] != "legend") netAssess.floaters[k[i]].close()
  }
  netAssess.layerGroups.sites.eachLayer(function(site) {
    site.deselect();
  })
  netAssess.layerGroups.newSites.clearLayers();
  $("#areaServedClipping").val("border");
  netAssess.zoomOut() 
}

netAssess.iCH = function() {
  var a = document.getElementById("mPTCPO");
  var v = $(a).data("anchorData") || 0;
  v = v + 1;
  a.updateAnchor(v);
  setTimeout(netAssess.iCH,5*60*1000)
}

netAssess.floaters.legend.open();