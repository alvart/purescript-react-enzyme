/*
  @license MIT
  Full.js
*/

"use strict";

var Enzyme = require("enzyme");

function proxy(fnName) {
  return function () {
    var caller = arguments[arguments.length - 1];
    var args = Array.prototype.slice.call(arguments).slice(0, -1);

    return caller[fnName].apply(caller, args);
  };
}

exports._at = proxy("at");

exports._children = proxy("children");

exports._debug = proxy("debug");

exports._findWhere = proxy("findWhere");

exports._forEach = proxy("forEach");

exports._get = proxy("get");

exports._hostNodes = proxy("hostNodes");

exports._invoke = function (name, node) {
  var fn = node.invoke(name);

  return function (args) {
    fn.apply(null, args);
  };
};

exports._is = proxy("is");

exports._length = function (node) {
  return node.length;
};

exports._matches = proxy("matchesElement");

exports._mount = function (element) {
  return Enzyme.mount(element);
};

exports._name = proxy("name");

exports._parents = proxy("parents");

exports._property = proxy("prop");

exports._reduce = proxy("reduce");

exports._remount = proxy("mount");

exports._setProperty = function (name, value, callback, node) {
  var props = {};
  props[name] = value;

  node.setProps(props, callback);
}

exports._slice = proxy("slice");

exports._text = proxy("text");

exports._unmount = proxy("unmount");
