/*
  @license MIT
  Enzyme.js
*/

"use strict";

var Enzyme = require("enzyme");
var Adapter = require("enzyme-adapter-react-16");
var GlobalDOM = require("jsdom-global");

exports.configure = function () {
  Enzyme.configure({ adapter: new Adapter() });
};

exports.withGlobalDOM = GlobalDOM;
