L.controlCredits = function(t) {
    return new L.CreditsControl(t)
}, L.CreditsControl = L.Control.extend({
    options: {
        position: "bottomright"
    },
    initialize: function(t) {
        if (!t.text) throw "L.CreditsControl missing required option: text";
        if (!t.image) throw "L.CreditsControl missing required option: image";
        if (!t.link) throw "L.CreditsControl missing required option: link";
        L.setOptions(this, t)
    },
    onAdd: function(t) {
        this._map = t;
        var i = L.DomUtil.create("div", "leaflet-credits-control", i);
        i.style.backgroundImage = "url(" + this.options.image + ")", this.options.width && (i.style.paddingRight = this.options.width + "px"), this.options.height && (i.style.height = this.options.height + "px");
        var o = L.DomUtil.create("a", "", i);
        L.DomUtil.addClass(o,'leaflet-credits-showlink');
        i.onclick = function(){
          window.open(o.href, "_blank");
        }
        return o.target = "_blank", o.href = this.options.link, o.innerHTML = this.options.text, i.link = o,
        this._container = i, this._link = o, i
    },
    setText: function(t) {
        this._link.innerHTML = t
    }
});