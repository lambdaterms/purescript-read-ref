/* globals exports */
"use strict";

exports.newRef = function (val) {
  return function () {
    const ref = { value: val };
    return ref;
  };
};

exports.write = function (ref) {
  // precache unit value
  return function(val) {
    return function () {
      ref.value = val;
      return {};
    };
  };
};

exports.modify = function(ref) {
  return function(f) {
    return function() {
      ref.value = f(ref.value);
      return ref.value;
    };
  };
};

exports["modify'"] = function(ref) {
  return function(f) {
    return function() {
      var r = f(ref.value);
      ref.value = r.state;
      return r.value;
    };
  };
};

exports.read = function (ref) {
  return function () {
    return ref.value;
  };
};
