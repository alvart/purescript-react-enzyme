/*
  @license MIT
  Enzyme.js
*/

"use strict";

var Enzyme = require("enzyme");
var Adapter = require("enzyme-adapter-react-16");

exports.configure = function () {
  Enzyme.configure({ adapter: new Adapter() });
};
