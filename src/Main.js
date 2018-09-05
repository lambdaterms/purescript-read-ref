"use strict";

exports.new = function (s){
  return function (val) {
    return function () {
      const ref = { value: val };
      const mutation = function(f){
        return function(){
          return ref.value = f(ref.value);
        };
      };
      return {ref: ref, mutation: mutation };
    };
  };
};


exports.read = function (ref) {
  return function () {
    return ref.value;
  };
};
