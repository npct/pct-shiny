$( window ).load(function() {
  togglePanel = function(panelId, link){
    panel = $(panelId)
    panel.toggle();
    if(panel.is(":visible")){
      $(link).html('<span class="glyphicon glyphicon-circle-arrow-up">Hide</span>');
    }else{
      $(link).html('<span class="glyphicon glyphicon-circle-arrow-down">Show</span>');
    }
  };
  $('#togglePanel').click(function(){ togglePanel('#input_panel', this) });
  $('#toggleLegend').click(function(){ togglePanel('#zone_legend', this) });
});