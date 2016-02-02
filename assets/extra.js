$( window ).load(function() {
  var togglePanel = function(panelId, link){
    var panel = $(panelId);
    panel.toggle();
    if(panel.is(":visible")){
      $(link).html('<span class="glyphicon glyphicon-circle-arrow-up">Hide</span>');
    }else{
      $(link).html('<span class="glyphicon glyphicon-circle-arrow-down">Show</span>');
    }
  };
  $('#togglePanel').click(function(){ togglePanel('#input_panel', this); });
  $('#toggleLegend').click(function(){ togglePanel('#zone_legend', this); });
  $('#toggleMapLegend').click(function(){ togglePanel('#map_legend', this); });

  var imd = $("[id^='map_base'] [value='IMD']");
  imd.popover({
      placement: "bottom",
      content: 'From the <a href="http://maps.cdrc.ac.uk/#/geodemographics/imde2015/" target="_blank">Index of Multiple Deprivation 2015</a>',
      html: true,
      trigger: "click"
  });
  var initMap = function(){
    if($(map).data('leaflet-map')){
      // lMap is the leaflet map object see http://leafletjs.com/reference.html
      var lMap = $(map).data('leaflet-map');
      L.control.scale().addTo(lMap);
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
