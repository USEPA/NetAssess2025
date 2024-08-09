netAssess.shinyBindings = {};

netAssess.shinyBindings.customAnchor = new Shiny.InputBinding();
  $.extend(netAssess.shinyBindings.customAnchor, {
    find: function(scope) {
      return $(scope).find(".shiny-custom-anchor");
    },
    getValue: function(el) {
      return $(el).data("anchorData");
    },
    setValue: function(el, value) {
      $(el).data("anchorData", value)
      $(el).trigger("anchorupdate");
    },
    subscribe: function(el, callback) {
      $(el).on("anchorupdate", callback)
    },
    unsubscribe: function(el) {
      $(el).off("anchorupdate")
    },
    initialize: function(el) {
      el.updateAnchor = function(data) {
        $(this).data("shinyInputBinding").setValue(this, data);
      }    
    }
  })
  Shiny.inputBindings.register(netAssess.shinyBindings.customAnchor);
  Shiny.addCustomMessageHandler("updateVisibleMonitors", function(data) {
    netAssess.layerGroups.sites.setVisibleSites(data);
})

Shiny.addCustomMessageHandler("updateNewSites", function(data) {
  netAssess.layerGroups.newSites.clearLayers();
  for(var i = 0; i < data.key.length; i++) {
    var latlng = L.latLng({lat: data.latitude[i], lng: data.longitude[i]});
    var props = {
      key: data.key[i], site_id: data.site_id[i], 
      site_name: data.site_name[i], address: data.address[i],
      epa_region: data.epa_region[i], state_name: data.state_name[i], 
      county_name: data.county_name[i], cbsa_name: data.cbsa_name[i],
      csa_title: data.csa_title[i], monitor_count: data.monitor_count[i],
      pollutants: data.pollutants[i], visible: data.visible[i]
    }
    var opts = {contextmenu: true}
    netAssess.layerGroups.newSites.addSite(latlng, props, opts)   
  }
})

Shiny.addCustomMessageHandler("displayPredefinedArea", function(data) {
  var x = L.featureGroup(null);
  for(var i = 0; i < data.coords.length; i++) {
    L.polygon(data.coords[i], {fill: false}).addTo(x)
  }
  netAssess.draw.disable();
  netAssess.setAOI(x);
});

Shiny.addCustomMessageHandler("updateAreaServed", function(data) {
  var areaServed = netAssess.layerGroups.areaServed;
  areaServed.clearLayers();
  var areaSelectStyle = {fillColor: '#666', weight: 2, opacity: 0.75, color: 'white', dashArray: '3', fillOpacity: 0.4}
  for(var i = 0; i < data.length; i++) {
    if(data[i].coords.length == 1) {
      var a = L.polygon(data[i].coords[0], {id: data[i].id}).addTo(areaServed)
    } else {
      var a = L.multiPolygon([data[i].coords], {id: data[i].id}).addTo(areaServed)
    }
    a.setStyle(areaSelectStyle)
    .on("mouseover", function(e) {
      var layer = e.target;
      layer.setStyle({
      weight: 5,
      color: '#666',
      dashArray: '',
      fillOpacity: 0.7
    });
    if(!L.Browser.id && !L.Browser.opera) {
      layer.bringToFront();
        netAssess.layerGroups.sites.bringToFront();
        netAssess.layerGroups.newSites.bringToFront();
        netAssess.layerGroups.rembias.bringToFront();
	}
    })
    .on("mouseout", function(e) {
    e.target.setStyle(areaSelectStyle);
  })
  .on("click", function(e) {
    var layer = e.target;
    if(layer.hasOwnProperty("options")) {
      document.getElementById("clickedAreaServed").updateAnchor(layer.options.id);
    } else if(layer.hasOwnProperty("_options")) {
      document.getElementById("clickedAreaServed").updateAnchor(layer._options.id);
    }
    netAssess.floaters.areaServed.open();
    $("#map").trigger("areaClick")
  })
  netAssess.layerGroups.sites.bringToFront();
  netAssess.layerGroups.newSites.bringToFront();
  netAssess.layerGroups.rembias.bringToFront();
  netAssess.loading.hide();
  }
})

Shiny.addCustomMessageHandler("updateTrendChart", function(data) {
  $(".popup-trend").find("img").attr("src", data)
})

Shiny.addCustomMessageHandler("rembiasUpdate", function(data) {
  netAssess.updateBiasLayer(data);
})

Shiny.addCustomMessageHandler("showCormat", function(data) {
  netAssess.loading.hide();
  netAssess.floaters.cormat.open();
})

Shiny.addCustomMessageHandler("loading", function(data) {
  if(data == "show") {
    netAssess.loading.show();
  } else {
    netAssess.loading.hide();
  }
})

Shiny.addCustomMessageHandler("showAlert", function(data) {
  netAssess.showAlert(data.header,data.body)
})