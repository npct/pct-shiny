// Inspired from Choropleth example on leaflet's website:
//
// http://leafletjs.com/examples/choropleth-example.html

$(document).ready(function(){
  $.getJSON("/regions.geojson").done(function(statesData) {
    var map = L.map('map').setView([ 53, -0.4], 6);

    L.tileLayer('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', {
      maxZoom: 18,
      attribution: '<a href="http://leafletjs.com" target = "_blank">Leaflet</a> | ' +
      'Map data &copy; <a href="http://openstreetmap.org" target = "_blank">OpenStreetMap</a> contributors | ' +
      '<a href="http://creativecommons.org/licenses/by-sa/2.0/" target = "_blank">CC-BY-SA</a> | ' +
      'Imagery © <a href="http://mapbox.com" target = "_blank">Mapbox</a>',
      id: 'mapbox.light'
    }).addTo(map);

    // Don't show the 'Leaflet' text. Attribution overload
    map.attributionControl.setPrefix('');


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
      pcycle: "2011 Census",
      govtarget_slc: "Government Target",
      gendereq_slc: "Gender Equality",
      dutch_slc: "Go Dutch",
      ebike_slc: "Ebike"
    };

    info.update = function (props, scenario) {
      var regionText;
      if(scenario !== undefined && props) {
        regionText = '<b>' + capitalize(props.Region) + '</b><br />' + precise_round(props[scenario], 1) + '  % in ' + info.scenarioMap[scenario];
      } else {
        regionText = 'Hover over a region';
      }
      this._div.innerHTML = '<h4>Cycling to work</h4>' + regionText;
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

      // return d > 40  ? '#313695' :
      //   d > 30  ? '#4575B4' :
      //   d > 25  ? '#74ADD1' :
      //   d > 20  ? '#ABD9E9' :
      //   d > 15   ? '#C6DBEF' :
      //   d > 10   ? '#ffffbf' :
      //   d > 7   ? '#FEE090' :
      //   d > 4   ? '#FDAE61' :
      //   d > 2   ? '#F46D43' :
      //   d > 0   ? '#D73027' :
      //   '#A50026';
      // grades = [0, 2, 4, 7, 10, 15, 20, 25, 30, 40]
      // zone_fill_breaks = c(0, 1, 2, 4, 6, 8, 10, 14, 20, 60) / 100 # The bins used for the scale
      // [0, 3, 6, 12, 20, 40],
    }

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

    function zoomToFeature(e) {
      window.open(e.target.feature.properties.url, '_top');
    }

    function onEachFeature(property) {
      return function(feature, layer) {
        layer.on({
          mouseover: highlightFeature(property),
          mouseout: resetHighlight(property, feature),
          click: zoomToFeature
        });
      };
    }

    function mapStyle(property) {
      return function (feature) {
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

    var olc = L.geoJson(statesData, {
      style: mapStyle("pcycle"),
      onEachFeature: onEachFeature("pcycle")
    });

    var govtarget = L.geoJson(statesData, {
      style: mapStyle("govtarget_slc"),
      onEachFeature: onEachFeature("govtarget_slc")
    });

    var gendereq = L.geoJson(statesData, {
      style: mapStyle("gendereq_slc"),
      onEachFeature: onEachFeature("gendereq_slc")
    });

    var dutch = L.geoJson(statesData, {
      style: mapStyle("dutch_slc"),
      onEachFeature: onEachFeature("dutch_slc")
    });

    var ebike = L.geoJson(statesData, {
      style: mapStyle("ebike_slc"),
      onEachFeature: onEachFeature("ebike_slc")
    });

    var map_layers = {
      "Census 2011": olc,
      "Government Target": govtarget,
      "Gender equality": gendereq,
      "Go Dutch": dutch,
      "Ebikes": ebike
    };

    L.control.layers(map_layers, null, {collapsed: false}, {position: 'topleft'}).addTo(map);

    // Add census as the default layer
    olc.addTo(map);

    var legend = L.control({position: 'topleft'});

    legend.onAdd = function (map) {

      var div = L.DomUtil.create('div', 'info-box legend'),
        // zone_fill_breaks = c(0, 1.5, 3.5, 6.5, 9.5, 14.5, 19.5, 24.5, 29.5, 100) / 100  # The bins used for the scale
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
    div.innerHTML = labels.join('<br>');
    return div;
  };

  legend.addTo(map);
  });
});
