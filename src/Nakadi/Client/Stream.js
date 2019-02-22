'use strict'

var split2 = require('split2');

exports.split2 = function () { return split2({ readableHighWaterMark: 1024 * 1024, writableHighWaterMark: 1024 * 1024 }); };
