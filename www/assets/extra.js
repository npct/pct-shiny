$( window ).load(function() {
  var togglePanel = function(panelId, link){
    var panel = $(panelId);
    // Commment toggling as the toggle function is being handled by shinyjs
    //panel.toggle();
    if(panel.is(":visible")){
      $(link).html('<span class="glyphicon glyphicon-circle-arrow-down">Show</span>');
    }else{
      $(link).html('<span class="glyphicon glyphicon-circle-arrow-up">Hide</span>');
    }
  };

  $('#toggle_panel').click(function(){ togglePanel('#input_panel', this); });
  $('#toggle_legend').click(function(){ togglePanel('#zone_legend', this); });
  $('#toggle_map_legend').click(function(){ togglePanel('#map_legend', this); });

  $('select').addClass("form-control");
  var urlUpdater = function(lMap, oldRegion){
    var grp = lMap.layerManager.getLayerGroup("region_name");
    var currentRegion;
    if(grp && grp.getLayers()[0]) {
      currentRegion = grp.getLayers()[0].options.layerId;
    }
    if(!!oldRegion && !!currentRegion && oldRegion != currentRegion){
      var newUrl = (window.history.state == "changed") ? currentRegion : "../" + currentRegion;
      window.history.pushState("changed", currentRegion, newUrl);
      setTimeout(urlUpdater, 500, lMap, currentRegion);
    } else {
      setTimeout(urlUpdater, 500, lMap, currentRegion || oldRegion);
    }
  };

  var initMap = function(){
    if($(map).data('leaflet-map')){
      // lMap is the leaflet map object see http://leafletjs.com/reference.html
      var lMap = $(map).data('leaflet-map');
      L.control.scale().addTo(lMap);
      urlUpdater(lMap, undefined);
    }
    else {
      setTimeout(initMap, 100);
    }
  };
  initMap();

  $("#printBtn").click(function(){
    $('#map').print();
  });
});
