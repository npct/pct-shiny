$( window ).load(function() {
  var toggle_panel = function(panelId, link){
    var panel = $(panelId);
    // Commment toggling as the toggle function is being handled by shinyjs
    //panel.toggle();
    if(panel.is(":visible")){
      $(link).html('<span class="glyphicon glyphicon-circle-arrow-down">Show</span>');
    }else{
      $(link).html('<span class="glyphicon glyphicon-circle-arrow-up">Hide</span>');
    }
  };

  $('#toggle_panel').click(function(){ toggle_panel('#input_panel', this); });
  $('#toggle_legend').click(function(){ toggle_panel('#zone_legend', this); });
  $('#toggle_map_legend').click(function(){ toggle_panel('#map_legend', this); });

  $('select').addClass("form-control");
  var url_updater = function(l_map, old_region){
    var grp = l_map.layerManager.getLayerGroup("region_name");
    var current_region;
    if(grp && grp.getLayers()[0]) {
      current_region = grp.getLayers()[0].options.layerId;
    }
    if(!!old_region && !!current_region && old_region != current_region){
      var new_url = (window.history.state == "changed") ? current_region : "../" + current_region;
      window.history.pushState("changed", current_region, new_url);
      setTimeout(url_updater, 500, l_map, current_region);
    } else {
      setTimeout(url_updater, 500, l_map, current_region || old_region);
    }
  };

  var initMap = function(){
    if($(map).data('leaflet-map')){
      // l_map is the leaflet map object see http://leafletjs.com/reference.html
      var l_map = $(map).data('leaflet-map');
      L.control.scale().addTo(l_map);
      url_updater(l_map, undefined);

      $('select, input').each(function() {
        send_ga = function(e){
          ga('send', 'event', 'controls', e.target.id , e.target.value);
        };
        $(this).change(send_ga);
      });
    }
    else {
      setTimeout(initMap, 100);
    }
  };
  initMap();

  $('a[data-toggle=tab]').each(function() {
    send_ga = function(){
      ga('send', 'event', 'nav', $(this).data('value'));
    };
    $(this).click(send_ga);
  });
});
