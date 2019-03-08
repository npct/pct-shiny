$(document).ready(function(){
  var selectedLayerName = "commute";
  var selectedVariable = "bicycle_perc";
  var featureGroup = null;

  var baseLayer = L.tileLayer('https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png', {
    maxZoom: 18,
    attribution: '<a href="//leafletjs.com" target = "_blank">Leaflet</a> | ' +
    'Map data &copy; <a href="//openstreetmap.org" target = "_blank">OpenStreetMap</a> contributors | ' +
    '<a href="//creativecommons.org/licenses/by-sa/2.0/" target = "_blank">CC-BY-SA</a>'
  });

  var defaultCommuteLayer = L.tileLayer('')

  var commuteLayers = {
    "Commute": defaultCommuteLayer,
    "School" : L.tileLayer('')
  };

  var defaultScenario = L.tileLayer('');

  var scenarioLayers = {
    "Census 2011": defaultScenario,
    "Government Target": L.tileLayer(''),
    "Gender Equality": L.tileLayer(''),
    "Go Dutch": L.tileLayer(''),
    "Ebikes": L.tileLayer('')
  };

  var map = L.map('map');

  L.control.layers(commuteLayers, null, {collapsed: false}, {position: 'topleft'}).addTo(map);

  L.control.layers(scenarioLayers, null, {collapsed: false}, {position: 'topleft'}).addTo(map);

  baseLayer.addTo(map);
  defaultCommuteLayer.addTo(map);
  defaultScenario.addTo(map);


  // initialize the map
  /*var map = L.map('map', {
        center: [53, -0.4],
        zoom: 6
    });*/

  var selectedVariableMap = {}
  selectedVariableMap["Census 2011"] = "bicycle_perc"
  selectedVariableMap["Government Target"] = "govtarget_slc_perc"
  selectedVariableMap["Gender Equality"] = "gendereq_slc_perc"
  selectedVariableMap["Go Dutch"] = "dutch_slc_perc"
  selectedVariableMap["Ebikes"] = "ebike_slc_perc"

  var selectedLayerMap = {}
  selectedLayerMap["Commute"] = "Commute"
  selectedLayerMap["School"] = "School"

  map.on('baselayerchange', layerUpdate);

  map.setView([53, -0.4], 6);

  // control that shows region info on hover
  var info = L.control();

  info.onAdd = function (map) {
    this._div = L.DomUtil.create('div', 'info-box cycle-to-work');
    this.update();
    return this._div;
  };

  // Armin's solution to a fixed decimal places
  // taken from a stackoverflow thread:
  // http://stackoverflow.com/questions/1726630/formatting-a-number-with-exactly-two-decimals-in-javascript
  // source: https://gist.github.com/ArminVieweg/28647e735aa6efaba401
  function sign(num) {
    // IE does not support method sign here
    if (typeof Math.sign === 'undefined') {
      if (num > 0) {
        return 1;
      }
      if (num < 0) {
        return -1;
      }
      return 0;
    }
    return Math.sign(num);
  }

  function precise_round(num, decimals) {
    var t=Math.pow(10, decimals);
    return (Math.round((num * t) + (decimals>0?1:0)*(sign(num) * (10 / Math.pow(100, decimals)))) / t).toFixed(decimals);
  }

  info.scenarioMap = {
    bicycle_perc: "2011 Census",
    govtarget_slc_perc: "Government Target",
    gendereq_slc_perc: "Gender Equality",
    dutch_slc_perc: "Go Dutch",
    ebike_slc_perc: "Ebike"
  };

  info.update = function (props, scenario) {

    var regionText;
    if(scenario !== undefined && props) {
      regionText = '<b>' + capitalize(props.region_name) + '</b><br />' + precise_round(props[scenario], 1) + '  % in ' + info.scenarioMap[scenario];
    } else {
      regionText = 'Hover over a region';
    }

    if (selectedLayerName == "Commute") {
      this._div.innerHTML = '<h4>Cycling to work</h4>' + regionText;
    }else if (selectedLayerName == "School") {
      this._div.innerHTML = '<h4>Cycling to school</h4>' + regionText;
    }
  };

  // Capitalize all words in a region except 'and' and 'of'
  function capitalize(s){
    s = s.replace(/-/g, ' ');
    s = s.toLowerCase().replace( /\b./g, function(a){ return a.toUpperCase(); } );
    s = s.replace('And', 'and');
    s = s.replace('Of', 'of');
    return s;
  }

  info.addTo(map);

  var polyColor = 'black';
  var polyWeight = 0.5;
  var polyFillOpacity = polyOpacity = 0.8;
  var polyDashArry = '';

  function highlightFeature(property) {
    return function(e) {
      var layer = e.target;

      layer.setStyle({
        weight: 5,
        color: '#666',
        dashArray: '',
        fillOpacity: polyFillOpacity
      });

      if (!L.Browser.ie && !L.Browser.opera && !L.Browser.edge) {
        layer.bringToFront();
      }

      info.update(layer.feature.properties, property);
    };
  }

  function resetHighlight(property, feature) {
    return function(e) {
      e.target.setStyle(mapStyle(property)(feature));
      info.update();
    };
  }

  function openRegion(e) {
    window.open(".\/m/?r=" + e.target.feature.properties.region_name, '_top');
  }

  function eachFeature(property) {
    //console.log("onEachFeature " + property)

    return function(feature, layer) {
      layer.on({
        mouseover: highlightFeature(property),
        mouseout: resetHighlight(property, feature),
        click: openRegion
      });
    };
  }

  function mapStyle(property) {
    return function (feature) {
      //console.log("mapStyle " + feature.properties[property])

      return {
        weight: polyWeight,
        //opacity: poly_opacity,
        color: polyColor,
        dashArray: polyDashArry,
        fillOpacity: polyFillOpacity,
        fillColor: getColor(feature.properties[property])
      };
    };
  }


  function layerUpdate (e) {
    //console.log(e)
    selectedVariable = selectedVariableMap[e.name] || selectedVariable

    selectedLayerName = selectedLayerMap[e.name] || selectedLayerName
    setControls();

    featureGroup ? featureGroup.clearLayers() : "";

    if (selectedLayerName == "School") {
      featureGroup = L.geoJson(school, {
        style: mapStyle(selectedVariable),
        onEachFeature: eachFeature(selectedVariable)
      }).addTo(map);
    }else if (selectedLayerName == "Commute") {
      featureGroup = L.geoJson(commute, {
        style: mapStyle(selectedVariable),
        onEachFeature: eachFeature(selectedVariable)
      }).addTo(map);

    }

  };

  function selectableControl(controlEl, disabled) {
    $(controlEl).attr('disabled', disabled)
    if(disabled) {
      $(controlEl).parent().addClass("text-muted")
    } else {
      $(controlEl).parent().removeClass("text-muted")
    }
  }

  function setControls(){
    // Disable Gender Equality and Ebikes for School Layer
    if (selectedLayerName == "School") {
      // Disable Health and CO2 radio buttons
      $('input:radio[name="leaflet-base-layers"]:not(:checked)').each(function () {
        if ($(this).parent().text().trim() == "Gender Equality" || $(this).parent().text().trim() == "Ebikes"){
          selectableControl(this, true)
        }
      });
    }else{
      $('input:radio[name="leaflet-base-layers"]:not(:checked)').each(function () {
        selectableControl(this, false)
      });
    }

    // Disable school layer for Ebikes and Gender Equality scenarios
    if (selectedLayerName == "Commute" && (selectedVariable == "ebike_slc_perc" || selectedVariable == "gendereq_slc_perc")) {
      $('input:radio[name="leaflet-base-layers"]:not(:checked)').each(function () {
        if ($(this).parent().text().trim() == "School"){
          selectableControl(this, true)
        }
      });
    }

  }

  var legend = L.control({position: 'topleft'});

  function getColor(d) {
    return d >= 40  ? '#4575B4' :
      d >= 30  ? '#74ADD1' :
      d >= 25  ? '#ABD9E9' :
      d >= 20  ? '#C6DBEF' :
      d >= 15  ? '#ffffbf' :
      d >= 10  ? '#FEE090' :
      d >= 7   ? '#FDAE61' :
      d >= 4   ? '#F46D43' :
      d >= 2   ? '#D73027' :
      d >= 0   ? '#A50026' :
      '#313695';

  }

  legend.onAdd = function (map) {

    var div = L.DomUtil.create('div', 'info-box legend'),
      grades = [0, 2, 4, 7, 10, 15, 20, 25, 30, 40],
      bin_labels = ["0-1%",
        "2-3%",
        "4-6%",
        "7-9%",
        "10-14%",
        "15-19%",
        "20-24%",
        "25-29%",
        "30-39%",
        "40%+"],

      labels = [],
      from;
    for (var i = 0; i < grades.length; i++) {
      from = grades[i];
      labels.push(
        '<i style="background:' + getColor(from + 1) + '"></i> ' +
        bin_labels[i]);
    }
    div.innerHTML = labels.join("<br class=\"clear\">");
    return div;
  };

  legend.addTo(map);
  map.whenReady(layerUpdate);
});
