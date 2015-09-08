var animationSpeed = 50;

var availableRegions = ['Leeds', 'Manchester', 'Norfolk', 'Coventry'];

var hoverStyle = {
  fill: "#A8BED5",
  "stroke-width": 1.5,
};

var unavailableStyle = {
  fill: "#ddd",
  stroke: "#aaa",
  "stroke-width": 0.75,
  "stroke-linejoin": "round"
};

var availableStyle = {
  fill: "#8080CC",
  stroke: "#aaa",
  "stroke-width": 0.75,
  "stroke-linejoin": "round",
  cursor: "pointer"
};

var coastStyle = {
  "stroke-linecap": "round",
  "stroke-linejoin": "round",
  "stroke-miterlimit": 100,
  fill: "#ADADFF",
  stroke: "#aaa",
  "stroke-width": 0.5,
};

var styleRegion = function (region, name) {
  if(availableRegions.indexOf(name) > -1){
    region[0].addEventListener("mouseover", function() {
      region.animate(hoverStyle, animationSpeed);
    }, true);

    region[0].addEventListener("mouseout", function() {
      region.animate(availableStyle, animationSpeed);
    }, true);
    region.attr(availableStyle);
    region.attr({href: name.replace(/\s+/g, '_').toLowerCase(), title: name});
  } else {
    region.attr(unavailableStyle);
    region.attr({title: name});
  }
};

coast.attr(coastStyle);
coast[0].setAttribute("fill-rule", "evenodd");

map.canvas.style.backgroundColor = '#FAF8F6';
for(var regionName in regions) {
  styleRegion(regions[regionName], regionName);
}
