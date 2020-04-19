"use strict";

exports.readline = readline

exports.parseInput = function() {
    var inputs = readline().split(' ');
    var x = parseInt(inputs[0]);
    var y = parseInt(inputs[1]);
    var humanCount = parseInt(readline());
    var humans = []
    for (let i = 0; i < humanCount; i++) {
        var inputs = readline().split(' ');
        var humanId = parseInt(inputs[0]);
        var humanX = parseInt(inputs[1]);
        var humanY = parseInt(inputs[2]);
        humans.push({
            id: humanId,
            x: humanX,
            y: humanY,
        })
    }
    var zombieCount = parseInt(readline());
    var zombies = []
    for (let i = 0; i < zombieCount; i++) {
        var inputs = readline().split(' ');
        var zombieId = parseInt(inputs[0]);
        var zombieX = parseInt(inputs[1]);
        var zombieY = parseInt(inputs[2]);
        var zombieXNext = parseInt(inputs[3]);
        var zombieYNext = parseInt(inputs[4]);
        zombies.push({
            id: zombieId,
            x: zombieX,
            y: zombieY,
            nextX: zombieXNext,
            nextY: zombieYNext,
        })
    }

    return {
        player: { x, y },
        humanCount,
        zombieCount,
        humans,
        zombies,
    }
};
