// Inspired from Choropleth example on leaflet's website:
//
// http://leafletjs.com/examples/choropleth-example.html

    // Currently pointing always to 'commute' map, ? in future introduce ability to switch to school map.
    // NB that will require also restricting the scenarios only to those available...

$(document).ready(function(){
  $.getJSON("/www/front_page/commute/pct_regions_lowres_scenario.geojson").done(function(statesData) {
    var map = L.map('map').setView([ 53, -0.4], 6);

    L.tileLayer('https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}.png', {
      maxZoom: 18,
      attribution: '<a href="//leafletjs.com" target = "_blank">Leaflet</a> | ' +
      'Map data &copy; <a href="//openstreetmap.org" target = "_blank">OpenStreetMap</a> contributors | ' +
      '<a href="//creativecommons.org/licenses/by-sa/2.0/" target = "_blank">CC-BY-SA</a>'
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

    function openRegion(e) {
      window.open(".\/m/?r=" + e.target.feature.properties.region_name, '_top');
    }

    function onEachFeature(property) {
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
      style: mapStyle("bicycle_perc"),
      onEachFeature: onEachFeature("bicycle_perc")
    });

    var govtarget = L.geoJson(statesData, {
      style: mapStyle("govtarget_slc_perc"),
      onEachFeature: onEachFeature("govtarget_slc_perc")
    });

    var gendereq = L.geoJson(statesData, {
      style: mapStyle("gendereq_slc_perc"),
      onEachFeature: onEachFeature("gendereq_slc_perc")
    });

    var dutch = L.geoJson(statesData, {
      style: mapStyle("dutch_slc_perc"),
      onEachFeature: onEachFeature("dutch_slc_perc")
    });

    var ebike = L.geoJson(statesData, {
      style: mapStyle("ebike_slc_perc"),
      onEachFeature: onEachFeature("ebike_slc_perc")
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
    div.innerHTML = labels.join("<br clear='both'>");
    return div;
  };

  legend.addTo(map);
  });
});
