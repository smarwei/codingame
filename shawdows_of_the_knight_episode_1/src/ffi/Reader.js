"use strict";

exports.readline = readline

exports.parseInput = function() {
    var inputs = readline().split(' ');
    var W = parseInt(inputs[0]); // width of the building.
    var H = parseInt(inputs[1]); // height of the building.
    var N = parseInt(readline()); // maximum number of turns before game over.
    var inputs = readline().split(' ');
    var X0 = parseInt(inputs[0]);
    var Y0 = parseInt(inputs[1]);
    return {
        width: W,
        height: H,
        turns: N,
        x: X0,
        y: Y0,
    }
};
