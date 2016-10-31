map = L.map('mapid').setView([-24.0, -49.1], 8);
map.scrollWheelZoom.disable();
var OpenTopoMap = L.tileLayer('http://.tile.opentopomap.org/{z}/{x}/{y}.png', {
    maxZoom: 17,
    attribution: 'Map data: &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>, <a href="http://viewfinderpanoramas.org">SRTM</a> | Map style: &copy; <a href="https://opentopomap.org">OpenTopoMap</a> (<a href="https://creativecommons.org/licenses/by-sa/3.0/">CC-BY-SA</a>)'
});

var osmUrl='/tiles/{z}/{x}/{y}.png';
var osmAttrib='Map data © <a href="http://openstreetmap.org">OpenStreetMap</a> contributors';
var tiles = L.tileLayer(osmUrl, {
    attribution: osmAttrib,
    subdomains: 'abcd',
    minZoom: 5,
    maxZoom: 12
});

var stamenUrl    = 'http://stamen-tiles-{s}.a.ssl.fastly.net/terrain/{z}/{x}/{y}.{ext}'
var stamenAttrib = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
var stamenTiles = L.tileLayer(stamenUrl, {
        attribution: stamenAttrib,
        subdomains: 'abcd',
	minZoom: 0,
	maxZoom: 18,
	ext: 'png'
});
stamenTiles.addTo(map);

var photoLayer = L.photo.cluster({ spiderfyDistanceMultiplier: 1.2 });
var onClickFunction = function (evt) {
    evt.layer.bindPopup(L.Util.template('<img src="{url}" style="max-width: 100%; max-height: 100%px"/>', evt.layer.photo),
                        {className: 'leaflet-popup-photo',
                         minWidth:  300,
                         maxHeight: 500
                        }).openPopup();
};
photoLayer.on('click', onClickFunction);

var options = {
    color: "black",
    weight: 5,
    opacity: 0.5
};
var photos = [];
var points = [];
var polylines = [];

for (i = 0; i < images.length; i++) {
    photos.push([]);
    points.push([]);
    console.log(images[i][1].length)
    for (j = 0; j < images[i][1].length; j++) {
        img = images[i][1][j];
        photos[i].push({
            lat: img[1],
            lng: img[2],
            post_url: images[i][0],
            url: img[0],
            caption: "",
            thumbnail: img[0].concat(".jpg")
        });
        points[i].push(new L.LatLng(img[1], img[2]));
    }
    photoLayer.add(photos[i]).addTo(map);
    polylines.push(L.polyline(points[i], options));
    map.addLayer(polylines[i]);
}
map.fitBounds(photoLayer.getBounds());
