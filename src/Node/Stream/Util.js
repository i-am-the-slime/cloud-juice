'use strict'

exports.splitAtNewlineImpl = function(callback) {
    return function () {
        const bufsize = 1024*1024*50;
        const buffer = Buffer.allocUnsafe(bufsize);
        var pos = 0;
        return function (chunk) {
            return function() {
                if(chunk.length + pos > bufsize) {
                    throw new Error("Exceeded maximum buffer size of " + bufsize / 1024 / 1024 + " MiB")
                } else {
                    for (var i = 0; i < chunk.length; i++) {
                        if (chunk[i] == 10) { // newline
                            const toPush = buffer.toString('utf-8', 0, pos)
                            callback(JSON.parse(toPush))();
                            pos = 0;
                        } else {
                            buffer[pos] = chunk[i];
                            pos++;
                        }
                    }
                }
            }
        }
    }
}

exports.newHttpsKeepAliveAgent =
  function() { return new require('https').Agent({ keepAlive: true }) };

exports.newHttpKeepAliveAgent =
  function() { return new require('http').Agent({ keepAlive: true }) };