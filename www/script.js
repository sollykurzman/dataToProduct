// L.Marker.include({
//   _adjustLabelPosition: function() {
//     if (!this._label || !this._map) return;

//     const label = this._label._container;
//     const map = this._map;
//     const pos = map.latLngToContainerPoint(this._latlng);

//     const labelWidth = label.offsetWidth;
//     const labelHeight = label.offsetHeight;

//     const padding = 10; // margin from edges

//     let direction = "top"; // default

//     // If too close to top → show below
//     if (pos.y - labelHeight < padding) {
//       direction = "bottom";
//     }

//     // If too close to bottom → show above
//     if (pos.y + labelHeight > map.getSize().y - padding) {
//       direction = "top";
//     }

//     // Apply direction
//     label.classList.remove("leaflet-label-top", "leaflet-label-bottom");
//     label.classList.add(direction === "top" ? "leaflet-label-top" : "leaflet-label-bottom");
//   }
// });

// // Wait for map to load and add animation enhancements
// $(document).on('shiny:connected', function() {
//   setTimeout(function() {

//     function animateFlightPaths() {
//       var paths = document.querySelectorAll('.leaflet-overlay-pane svg path');
//       var flightPaths = [];

//       paths.forEach(function(path) {
//         if (
//           path.getAttribute('stroke') &&
//           path.getAttribute('stroke-dasharray') &&
//           path.getAttribute('stroke-dasharray').includes('10')
//         ) {
//           flightPaths.push(path);
//         }
//       });

//       flightPaths.forEach(function(path, index) {
//         var delay = index * 0;
//         path.style.animation = 'flowingGlow 1s linear infinite';
//         path.style.animationDelay = delay + 's';
//         path.style.filter = 'drop-shadow(0 0 6px rgba(102, 126, 234, 0.5))';
//       });
//     }

//     animateFlightPaths();
//     setTimeout(animateFlightPaths, 1000);
//     setTimeout(animateFlightPaths, 2000);

//   }, 500);
// });

// // Re-run animations after search
// Shiny.addCustomMessageHandler('animate_paths', function(message) {
//   setTimeout(function() {
//     var paths = document.querySelectorAll('.leaflet-overlay-pane svg path');
//     var flightPaths = [];

//     paths.forEach(function(path) {
//       if (
//         path.getAttribute('stroke') &&
//         path.getAttribute('stroke-dasharray') &&
//         path.getAttribute('stroke-dasharray').includes('10')
//       ) {
//         flightPaths.push(path);
//       }
//     });

//     flightPaths.forEach(function(path, index) {
//       var delay = index * 0;
//       path.style.animation = 'none';

//       setTimeout(function() {
//         path.style.animation = 'flowingGlow 1s linear infinite';
//         path.style.animationDelay = delay + 's';
//         path.style.filter = 'drop-shadow(0 0 6px rgba(102, 126, 234, 0.5))';
//       }, 50);
//     });
//   }, 800);
// });
