$(document).ready(function() {
  if(window.location.hostname !== "www.pct.bike") {
    $(".test-banner").show();
  }
  if(window.location.href.indexOf("r=wales") !== -1) {
    $(".test-banner").show();
    $(".test-banner-message").html('This is a trial version of the PCT site that includes a region for Wales with some features missing (LSOA route network and national data downloads are not there for Wales). This trial version will be available until October 2017. Tell us what you think on <a href="mailto:pct@pct.bike">pct@pct.bike</a>.');
  }

  if(typeof ga === "undefined"){
    ga = function(){};
  }

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
  $('#toggle_trip_menu').click(function(){ toggle_panel('#trip_menu', this); });
  $('#toggle_map_legend').click(function(){ toggle_panel('#map_legend', this); });

  $('select').addClass("form-control");

  Shiny.addCustomMessageHandler("regionchange", function(newRegion) {
    var newUrl = window.location.origin + window.location.pathname + "?r=" + newRegion;
    history.pushState(newRegion, newRegion, newUrl);
  });

  var initMap = function(){
    if($(map).data('leaflet-map')){
      // l_map is the leaflet map object see https://leafletjs.com/reference.html
      var l_map = $(map).data('leaflet-map');
      L.control.scale().addTo(l_map);
      $('select, input').each(function() {
        $(this).change(function(e){
          ga('send', 'event', 'controls', e.target.id , e.target.value);
        });
      });
    }
    else {
      setTimeout(initMap, 100);
    }
  };
  initMap();

  $('a[data-toggle=tab]').each(function() {
    $(this).click(function(){
      ga('send', 'event', 'nav', $(this).data('value'));
    });
  });
});
