netAssess.tour = {slides: [], slideCount: 0, width: 400, height: 300, active: true}

netAssess.tour.makeSlide = function(options) {
  options = $.extend({
    position: "center",
    title: " ",
    text: " ",
    target: "#map",
    runbefore: function() {},
    runafter: function() {}
  }, options)
  netAssess.tour.slides.push(options)
}

netAssess.tour.makeSlide({ title: "Welcome to NetAssess2025!",
  text: "The NetAssess2025 app is meant to assist with the 5-year Ambient Air Monitoring Network Assessments as required by 40 CFR &#0167;58.10(d).<br><br>You can start with this quick tour, or close this window using the 'X' in the top right corner and try it out on your own."
})

netAssess.tour.makeSlide({title: "Menu Bar",
  text: "The menu bar above is how you will access most of the functionality of the NetAssess2025 app. <br><br>Each icon above is a button that gives you access to a tool or function in the app.",
  target: "#menuControls",
  position: "below"
})

netAssess.tour.makeSlide({title: "Zoom Buttons",
  text: "These buttons allow you to zoom in and zoom out on the map. You can also zoom in and zoom out using the scroll wheel on your mouse.",
  target: ".leaflet-control-zoom",
  position: "right"
})

netAssess.tour.makeSlide({title: "Layers",
  text: "The Layers control to the left allows you to switch between different base maps and to turn different app layers on and off.",
  target: ".leaflet-control-layers-toggle",
  position: "right"
})

netAssess.tour.makeSlide({title: "Sidebars",
  text: "These buttons open side panels that offer additional settings and information.<br><br>The top button opens the 'Settings' sidebar, the middle button opens the 'Help' sidebar, and the bottom button opens the 'About' sidebar.",
  target: ".leaflet-top.leaflet-right",
  position: "left"
})

netAssess.tour.makeSlide({title: "Legend",
  text: "The legend panel below explains what the different symbols displayed on the map mean. <br><br>The legend will change depending on which tools your are using, and which layers you have displayed.",
  target: "#legendFloater",
  position: "above"
})

netAssess.tour.makeSlide({title: "Using NetAssess2025",
  text: "Now we will cover how to get started using the tools.<br><br>The following steps do not require you to make any selections. The app will make appropriate selections to illustrate the functionality."
})

netAssess.tour.makeSlide({title: "Pollutant Selection",
  text: "The first step is to select a pollutant using the menu above.<br><br>When you select a pollutant, the map will display red circles representing the current monitoring site locations for that pollutant. We have selected <b>O3 - Ozone (44201)</b> for this example.",
  target: "#pollSelect",
  position: "below", 
  runbefore: function() {
    $("#pollutantSelect").select2("val", "ozone");
    $("#pollutantSelect").trigger("change");
  }
})
                             
netAssess.tour.makeSlide({title: "Monitoring Sites",
  text: "When a pollutant is selected, all sites that monitor for that pollutant will appear as red circles on the map.<br><br>You can click on any site to open a window with basic information about that site and a graphic displaying the air quality trend for your selected pollutant at that site.",
  position: "right"
})

netAssess.tour.makeSlide({title: "Area of Interest",
  text: "The next step is to select an Area of Interest.<br><br>Clicking on the 'Area of Interest' button above opens the Area of Interest selection window, which will allow you to choose an area of the U.S. to focus your analysis.", 
  target: "#aoiButton", 
  position: "below",
  runafter: netAssess.floaters.aoi.open
})

netAssess.tour.makeSlide({title: "Area of Interest",
  text: "You can select an Area of Interest in several ways.<br><br>You can draw your own area using the 'Draw an Area of Interest' controls.<br><br>Or, you can select a pre-defined area such as a State, CBSA, or CSA. We have selected the State of Arkansas for this example.", 
  target: "#aoiFloater", 
  position: "right",
  runbefore: function() {
    $("[name='areaSelect'][value='State']").trigger("click");
    setTimeout(function() {
      $("#areaSelectSelect").select2("val", "05");
      $("#areaSelectSelect").trigger("change");
    }, 500)
  },
  runafter: netAssess.floaters.aoi.close
})

netAssess.tour.makeSlide({title: "Zoom to Area of Interest",
  text: "You can use this button to zoom in to your Area of Interest, once you have defined one.",
  target: "#aoiZoomButton",
  position: "below",
  runafter: function() { $("#aoiZoomButton").trigger("click"); }
})

netAssess.tour.makeSlide({title: "Site Selection",
  text: "Notice that the sites within the Area of Interest have gotten brighter and larger to indicate that they are selected.<br><br>You can right-click on individual sites to open a menu that allows you to select, deselect, or hide those sites.",
  position: "right"
})

netAssess.tour.makeSlide({title: "Add New Sites",
  text: "At this point, you may want to add new sites to your monitoring network.<br><br>To place a new site on the map, click on the 'Add New Site' button above, then click the location on the map where you want the new monitor to be placed.<br><br>New sites are displayed as green circles on the map and are treated just like existing sites for the purposes of Area Served calculations.",
  target: "#newSiteButton",
  position: "below"
})

netAssess.tour.makeSlide({title: "Area Served",
  text: "The Area Served tool estimates the area served by each monitoring site in your Area of Interest.<br><br>Clicking the 'Area Served' button above calculates the Area Served by each selected site in your Area of Interest.",
  target: "#areaServedButton",
  position: "below",
  runafter: function() { $("#areaServedButton").trigger("click"); }
})

netAssess.tour.makeSlide({title: "Area Served",
  text: "In a moment, grey polygons will appear around each selected site.<br><br>Each polygon represents the area that is closer to the monitor within it than any other monitor in the network.<br><br>You can click on any polygon to open a new window with detailed information about that area.",
  position: "right",
  runafter: function() {
    var key = Object.keys(netAssess.layerGroups.areaServed._layers)[1];
      netAssess.layerGroups.areaServed._layers[key].fire("click");
  }
})

netAssess.tour.makeSlide({title: "Area Served",
  text: "When you click on a polygon, a new window will appear to the right. This window gives you geographic and demographic information about the area you clicked.<br><br>You can click on the demographic chart to make it larger, and use right-click/Save As... to save the chart image.", 
  target: "#areaServedFloater",
  position: "left",
  runafter: netAssess.floaters.areaServed.close
})

netAssess.tour.makeSlide({title: "Correlation Matrix",
  text: "Clicking on the 'Correlation Matrix' button will open a new window displaying a correlation matrix graphic. This may take a moment, especially if you have a large number of sites selected.<br><br>This graphic gives you information about how concentrations at monitors within your Area of Interest compare to one another.", 
  target: "#cormatButton", 
  position: "below",
  runbefore: function() { $("#cormatButton").trigger("click"); }
})

netAssess.tour.makeSlide({title: "Correlation Matrix",
  text: "Each monitor comparison is represented by a square in the chart.<br><br>The blue squares in the bottom-left corner show the correlation between each pair of monitors, with text indicating the number of days used in the calculation.",
  position: "right"
})

netAssess.tour.makeSlide({title: "Correlation Matrix",
  text: "The red squares in the top-right corner show the mean absolute difference in concentrations between each pair of monitors, with text indicating the distance in kilometers between each pair of monitors.<br><br>The numbers along the diagonal indicate the most recent design value for each monitor.",
  position: "right",
  runafter: netAssess.floaters.cormat.close
})

netAssess.tour.makeSlide({title: "Removal Bias",
  text: "When you click on the 'Removal Bias' button, all selected sites with data available will become larger and change color.<br><br>The color represents the average Removal Bias calculated for that site, as indicated by the Legend.<br><br>Clicking on a site will open a window that gives more information about the Removal Bias at that location.",
  target: "#rembiasButton",
  position: "below",
  runbefore: function() { $("#rembiasButton").trigger("click"); }
})

netAssess.tour.makeSlide({title: "Removal Bias",
  text: "The Removal Bias tool finds the nearest neighbors to each selected site and then uses the data from the neighboring sites to estimate concentrations at the site. It then compares the estimates to the actual concentrations measured at the selected site to determine the Removal Bias.<br><br>If the bias is small, that may indicate that the monitor is redundant and could be removed.",
  position: "right"
})

netAssess.tour.makeSlide({title: "Download Data",
  text: "All data calculated by the NetAssess2025 app is available for download.<br><br>Clicking on the 'Download Data' button above will open a new window where you can choose the type of data you want to download.",
  target: "#downloadDataButton",
  position: "below",
  runafter: netAssess.floaters.download.open
})

netAssess.tour.makeSlide({title: "Download Data",
  text: "Click the download icon next to a data type to download the data in a CSV file.<br><br>If a selection is greyed out, that means that you haven't made sufficient selections needed to generate that data type yet.",
  target: "#downloadFloater",
  position: "below",
  runafter: netAssess.floaters.download.close
})

netAssess.tour.makeSlide({title: "Zoom to Continental U.S.",
  text: "You can use this button to zoom out and view the entire continental United States.",
  target: "#fullExtentButton",
  position: "below",
  runafter: function() {
    netAssess.map.removeLayer(netAssess.layerGroups.aoi);
    netAssess.map.removeLayer(netAssess.layerGroups.areaServed);
    netAssess.map.removeLayer(netAssess.layerGroups.rembias);
    $("#pollutantSelect").select2("val","none");
    $("#pollutantSelect").trigger("change");
    $("#fullExtentButton").trigger("click");
  }
})

netAssess.tour.makeSlide({title: "Exceedance Probabilities",
  text: "Exceedance Probabilities for each Census Tract in the Continental U.S. have been calculated for Ozone and PM<sub>2.5</sub>. These values represent the probability of a NAAQS exceedance based on Downscaler fused air quality surfaces for 2019-2021.<br><br>You can view this information on the map by turning on the 'Ozone Probability' or 'PM<sub>2.5</sub> Probability' layers in the Layers Control. The PM<sub>2.5</sub> probability layer is currently displayed.",
  target: ".leaflet-control-layers-toggle",
  position: "right",
  runbefore: function() { netAssess.map.addLayer(netAssess.overlays.pm25) },
  runafter: function() { netAssess.map.removeLayer(netAssess.overlays.pm25) }
})

netAssess.tour.makeSlide({title: "Reset App",
  text: "Clicking on this button will reset the NetAssess2025 app to its initial state.<br><br>All layers will be erased, all selections will be removed, and all settings will be restored to their defaults.",
  target: "#resetAppButton",
  position: "below"
})

netAssess.tour.makeSlide({title: "Conclusion",
  text: "This concludes the tour.<br><br>You can check the 'Don't show again' box below to prevent this tour from opening automatically the next time you visit the app. You can always reopen it using the 'Help' sidebar to the right.",
  runbefore: function() { netAssess.resetApp() }
})

$(document).ready(function() {
  var name = "showtour"
  var showTour = "true";
  var ca = document.cookie.split(';');
  for(var i=0; i < ca.length; i++) {
    var c = ca[i];
    while (c.charAt(0)==' ') c = c.substring(1);
    if (c.indexOf(name) != -1) showTour = c.substring(name.length+1,c.length);
  }
  if(showTour == "true") {
    netAssess.tour.active = true;
    document.getElementById("tourGone").checked = false;
  } else {
    netAssess.tour.active = false;
    document.getElementById("tourGone").checked = true;
  }
  if(netAssess.tour.active) {
    netAssess.tour.advance()
  } 
})

netAssess.tour.setPosition = function(target, position) {
  var rect = $(target)[0].getBoundingClientRect();
  var rect_center = {x: (rect.width / 2) + rect.left,
                     y: (rect.height / 2) + rect.top
  }
  var arrowPos = {
    "left": "",
    "top": "",
    "display": "none",
    "border-left-color": "transparent",
    "border-top-color": "transparent",
    "border-right-color": "transparent",
    "border-bottom-color": "transparent"
  }
  switch(position) {
    case "center":
      var position = {
        top: rect_center.y - (netAssess.tour.height / 2),
        left: rect_center.x - (netAssess.tour.width / 2),
        display: "block"
      }
      arrowPos.top = "";
      arrowPos.left = "";
      arrowPos.display = "none";
      var arrowBorderPos = {};
      $.extend(arrowBorderPos, arrowPos);
      break;
    case "above":
      var position = {
        top: rect.top - (netAssess.tour.height + 15),
        left: rect_center.x - (netAssess.tour.width / 2),
        display: "block"
      };
      arrowPos.top = netAssess.tour.height - 5;
      arrowPos.left = netAssess.tour.width / 2;
      arrowPos.display = "block";
      arrowPos["border-top-color"] = "#EFEFEF";
      var arrowBorderPos = {};
      $.extend(arrowBorderPos, arrowPos);
      arrowBorderPos.top = arrowBorderPos.top + 2.5
      arrowBorderPos["border-top-color"] = "black";
      break;
    case "below":
      var position = {
        top: rect.bottom + 15,
        left: rect_center.x - (netAssess.tour.width / 2),
        display: "block"
      };
      arrowPos.top = -20;
      arrowPos.left = (netAssess.tour.width / 2) - 10;
      arrowPos.display = "block";
      arrowPos["border-bottom-color"] = "#EFEFEF";
      var arrowBorderPos = {};
      $.extend(arrowBorderPos, arrowPos);
      arrowBorderPos.top = arrowBorderPos.top - 2.5
      arrowBorderPos["border-bottom-color"] = "black";
      break;
    case "left":
      var position = {
        top: rect_center.y - (netAssess.tour.height / 2),
        left: rect.left - (netAssess.tour.width + 15),
        display: "block"
      }
      arrowPos.top = (netAssess.tour.height / 2) - 10;
      arrowPos.left = netAssess.tour.width - 5;
      arrowPos.display = "block";
      arrowPos["border-left-color"] = "#EFEFEF";
      var arrowBorderPos = {};
      $.extend(arrowBorderPos, arrowPos);
      arrowBorderPos.left = arrowBorderPos.left + 2.5;
      arrowBorderPos["border-left-color"] = "black";
      break;
    case "right":
      var position = {
        top: rect_center.y - (netAssess.tour.height / 2),
        left: rect.right + 15,
        display: "block"
      }
      arrowPos.top = (netAssess.tour.height / 2) - 10;
      arrowPos.left = -20;
      arrowPos.display = "block";
      arrowPos["border-right-color"] = "#EFEFEF";
      var arrowBorderPos = {};
      $.extend(arrowBorderPos, arrowPos);
      arrowBorderPos.left = arrowBorderPos.left - 2.5;
      arrowBorderPos["border-right-color"] = "black";
      break;
    default:
      console.log("Unrecognized 'position' to setPosition function.")
  }
  var w = window.innerWidth;
  var h = window.innerHeight;
  if(position.left < 0) {
    var offset_x = 5 + (position.left + netAssess.tour.width);
  } else if((position.left + netAssess.tour.width) > w) {
    var offset_x = 5 + ((position.left + netAssess.tour.width) - w);
  } else {
    var offset_x = 0;
  }
  position.left = parseInt(position.left - offset_x, 10) + "px";
  arrowPos.left = parseInt(arrowPos.left + offset_x, 10) + "px";
  arrowBorderPos.left = parseInt(arrowBorderPos.left + offset_x, 10) + "px";
  if(position.top < 0) {
    var offset_y = 5 - position.top; 
  } else if((position.top + netAssess.tour.height) > h) {
    var offset_y = (position.top + netAssess.tour.height) - h;
  } else {
    var offset_y = 0;
  }
  position.top = parseInt(position.top + offset_y, 10) + "px";
  arrowPos.top = parseInt(arrowPos.top - offset_y, 10) + "px";
  arrowBorderPos.top = parseInt(arrowBorderPos.top - offset_y, 10) + "px";
  var $tour = $("#tour");
  $tour.css(position);
  $tour.find(".tour-arrow").css(arrowPos);
  $tour.find(".tour-arrow-border").css(arrowBorderPos);
}

netAssess.tour.advance = function() {
  var tour = netAssess.tour;
  var $tour = $("#tour");
  var cnt = tour.slideCount;
  if(cnt > 0) tour.slides[cnt - 1].runafter();
  tour.slides[cnt].runbefore();
  $tour.find(".header").html(tour.slides[cnt].title);
  $tour.find(".content")[0].scrollTop = 0;
  $tour.find(".content").html(tour.slides[cnt].text);
  tour.setPosition(tour.slides[cnt].target, tour.slides[cnt].position);
  tour.slideCount++
}

netAssess.tour.close = function() {
  netAssess.tour.active = false;
  $("#tour").css("display", "none")
  $("*").off(".tour");
}

$("#tourNext").on("click", function() {
  if(netAssess.tour.slideCount == netAssess.tour.slides.length - 1) {
    $("#tourNext").text("Close")
  } else {
    $("#tourNext").text("Next")
  }
  if(netAssess.tour.slideCount == netAssess.tour.slides.length) {
    netAssess.tour.close()
  } else {
    netAssess.tour.advance()
  }
})

$("#tour .close").on("click", netAssess.tour.close);

$("#tour #tourGone").on("click", function(e) {
  var d = new Date();
  d.setTime(d.getTime() + (60*24*60*60*1000));
  var expires = "expires="+d.toUTCString();
  if(this.checked) {
    document.cookie = "showtour=false; " + expires
  } else {
    document.cookie = "showtour=true; " + expires
  }
})

$("#openTour").on("click", function() {
  if(netAssess.tour.active == false) {
    netAssess.tour.slideCount = 0;
    netAssess.tour.active = true;
    netAssess.tour.advance();
    netAssess.sidebars.help.hide();
  }
})

// Disabled the Next button until the page has a chance to load
$("#tourNext").attr("disabled", true);
$(document).ready(function() {
  setTimeout(function() {$("#tourNext").attr("disabled", false)}, 1500)
})