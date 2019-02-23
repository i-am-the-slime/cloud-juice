'use strict'

var writable = require('writable2');
var throughMap = require('through2-map');
var split2 = require('split2');

exports.writableImpl = function(fn) {
    return function() {
        return writable.obj(function(data, enc, done) {
            fn(data)(enc)(done)();
        });
    }
}

exports.throughMap = function(fn) {
    return function() {
        return throughMap.obj(fn);
    }
}

exports.split2 = function(mapper) {
    return function () { return split2(mapper); };
};
