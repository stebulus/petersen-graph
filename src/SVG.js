"use strict";

// module SVG

exports.toScreenImpl = function(svg,evt) {
  var pt = svg.createSVGPoint();
  pt.x = evt.clientX;
  pt.y = evt.clientY;
  var q = pt.matrixTransform(svg.getScreenCTM().inverse());
  return {u: q.x, v: q.y};
};
