$(document).ready(function() {
  var testBannerDisplay = function() {
    if(window.location.hostname.indexOf("pct.bike") === -1)  {
      $(".test-banner").show();
      $(".test-banner-message").html('This is the PCT test site. Please do not circulate links to this site. Findings from this site should not be assumed to be reliable without specific agreement from the PCT team. Please use <a href="//www.pct.bike">www.pct.bike</a> instead.');
    } else {
      $(".test-banner").hide();
    }
  };

  testBannerDisplay();

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

  var capitalize = function (s){
    s = s.replace(/-/g, ' ');
    s = s.toLowerCase().replace( /\b./g, function(a){ return a.toUpperCase(); } );
    s = s.replace('And', 'and');
    s = s.replace('Of', 'of');
    s = s.replace('\'S', '\'s');
    return s;
  };

  Shiny.addCustomMessageHandler("regionchange", function(newRegion) {
    var newUrl = window.location.href.split("?")[0] + "?r=" + newRegion;
    history.pushState(newRegion, newRegion, newUrl);
    document.title = "Propensity to Cycle Tool - " + capitalize(newRegion);
    testBannerDisplay();
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
