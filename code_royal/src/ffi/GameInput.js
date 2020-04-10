"use strict";

exports.readline = readline

exports.parseInitInput = function() {
    var sites = []
    var numSites = parseInt(readline());
    for (let i = 0; i < numSites; i++) {
        var inputs = readline().split(' ');
        var siteId = parseInt(inputs[0]);
        var x = parseInt(inputs[1]);
        var y = parseInt(inputs[2]);
        var radius = parseInt(inputs[3]);
        sites.push({
            id: siteId,
            x, y,
            radius,
        })
    }
    return {
        numSites,
        sites,
    }
};

exports.parseInput = function(numSites) {
    return function() {
        var inputs = readline().split(' ');
        var gold = parseInt(inputs[0]);
        var touchedSite = parseInt(inputs[1]); // -1 if none
        var sites = []
        for (let i = 0; i < numSites; i++) {
            var inputs = readline().split(' ');
            var siteId = parseInt(inputs[0]);
            var mineGold = parseInt(inputs[1]); // The total number of gold remaining to be mined from this site (-1 if unknown)
            var maxMineSize = parseInt(inputs[2]); // The maximum rate that a mine can extract gold from this site (-1 if unknown)
            var structureType = parseInt(inputs[3]); // -1 = No structure, 2 = Barracks
            var owner = parseInt(inputs[4]); // -1 = No structure, 0 = Friendly, 1 = Enemy
            var param1 = parseInt(inputs[5]);
            var param2 = parseInt(inputs[6]);
            sites.push({
                id: siteId,
                gold: mineGold,
                maxMineSize,
                structureType,
                owner,
                param1,
                param2,
            })
        }
        var numUnits = parseInt(readline());
        var units = []
        for (let i = 0; i < numUnits; i++) {
            var inputs = readline().split(' ');
            var x = parseInt(inputs[0]);
            var y = parseInt(inputs[1]);
            var owner = parseInt(inputs[2]);
            var unitType = parseInt(inputs[3]); // -1 = QUEEN, 0 = KNIGHT, 1 = ARCHER
            var health = parseInt(inputs[4]);
            units.push({
                x, y,
                owner,
                unitType,
                health
            })
        }

        return {
            gold,
            touchedSite,
            sites,
            units,
        }
    }
};
