"use strict";

exports.readline = readline

exports.parseInitInput = function() {
    var size = parseInt(readline());
    var board = []
    for (var i = 0; i < size; i++) {
        var inner = []
        var line = readline();
        for (var l=0; l < line.length; l++) {
            inner.push(line[l])
        }
        board.push(inner)
    }
    var myId = parseInt(readline()); // ID of your hero

    return {
        boardSize: size,
        heroId: myId,
        board: board
    }
};

exports.parseInput = function(numSites) {
    return function() {
        var entityCount = parseInt(readline()); // the number of entities
        var entities = []
        for (var i = 0; i < entityCount; i++) {
            var inputs = readline().split(' ');
            var entityType = inputs[0]; // HERO or MINE
            var id = parseInt(inputs[1]); // the ID of a hero or the owner of a mine
            var x = parseInt(inputs[2]); // the x position of the entity
            var y = parseInt(inputs[3]); // the y position of the entity
            var life = parseInt(inputs[4]); // the life of a hero (-1 for mines)
            var gold = parseInt(inputs[5]); // the gold of a hero (-1 for mines)
            entities.push({
                entityType: entityType,
                id: id,
                x: x,
                y: y,
                life: life,
                gold: gold,
            })
        }

        return {
            entityCount: entityCount,
            entities: entities
        }
    }
};
