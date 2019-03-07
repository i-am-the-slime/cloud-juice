'use strict'
const threads = require('worker_threads');

exports.workerImpl = function () {
    new threads.Worker(__filename, {workerData: i});
}
