// Generated by purs bundle 0.13.6
var PS = {};
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Control.Semigroupoid"] = $PS["Control.Semigroupoid"] || {};
  var exports = $PS["Control.Semigroupoid"];
  var Semigroupoid = function (compose) {
      this.compose = compose;
  };
  var semigroupoidFn = new Semigroupoid(function (f) {
      return function (g) {
          return function (x) {
              return f(g(x));
          };
      };
  });
  exports["semigroupoidFn"] = semigroupoidFn;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Control.Category"] = $PS["Control.Category"] || {};
  var exports = $PS["Control.Category"];
  var Control_Semigroupoid = $PS["Control.Semigroupoid"];                
  var Category = function (Semigroupoid0, identity) {
      this.Semigroupoid0 = Semigroupoid0;
      this.identity = identity;
  };
  var identity = function (dict) {
      return dict.identity;
  };
  var categoryFn = new Category(function () {
      return Control_Semigroupoid.semigroupoidFn;
  }, function (x) {
      return x;
  });
  exports["identity"] = identity;
  exports["categoryFn"] = categoryFn;
})(PS);
(function(exports) {
  "use strict";

  exports.intSub = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x - y | 0;
    };
  };
})(PS["Data.Ring"] = PS["Data.Ring"] || {});
(function(exports) {
  "use strict";

  exports.intAdd = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x + y | 0;
    };
  };

  exports.intMul = function (x) {
    return function (y) {
      /* jshint bitwise: false */
      return x * y | 0;
    };
  };
})(PS["Data.Semiring"] = PS["Data.Semiring"] || {});
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Data.Semiring"] = $PS["Data.Semiring"] || {};
  var exports = $PS["Data.Semiring"];
  var $foreign = $PS["Data.Semiring"];
  var Semiring = function (add, mul, one, zero) {
      this.add = add;
      this.mul = mul;
      this.one = one;
      this.zero = zero;
  };                                                                            
  var semiringInt = new Semiring($foreign.intAdd, $foreign.intMul, 1, 0);
  exports["semiringInt"] = semiringInt;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Data.Ring"] = $PS["Data.Ring"] || {};
  var exports = $PS["Data.Ring"];
  var $foreign = $PS["Data.Ring"];
  var Data_Semiring = $PS["Data.Semiring"];
  var Ring = function (Semiring0, sub) {
      this.Semiring0 = Semiring0;
      this.sub = sub;
  };                  
  var ringInt = new Ring(function () {
      return Data_Semiring.semiringInt;
  }, $foreign.intSub);
  exports["ringInt"] = ringInt;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Data.CommutativeRing"] = $PS["Data.CommutativeRing"] || {};
  var exports = $PS["Data.CommutativeRing"];
  var Data_Ring = $PS["Data.Ring"];
  var CommutativeRing = function (Ring0) {
      this.Ring0 = Ring0;
  }; 
  var commutativeRingInt = new CommutativeRing(function () {
      return Data_Ring.ringInt;
  });
  exports["commutativeRingInt"] = commutativeRingInt;
})(PS);
(function(exports) {
  "use strict";

  exports.intDegree = function (x) {
    return Math.min(Math.abs(x), 2147483647);
  };

  // See the Euclidean definition in
  // https://en.m.wikipedia.org/wiki/Modulo_operation.
  exports.intDiv = function (x) {
    return function (y) {
      if (y === 0) return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };

  exports.intMod = function (x) {
    return function (y) {
      if (y === 0) return 0;
      var yy = Math.abs(y);
      return ((x % yy) + yy) % yy;
    };
  };
})(PS["Data.EuclideanRing"] = PS["Data.EuclideanRing"] || {});
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Data.EuclideanRing"] = $PS["Data.EuclideanRing"] || {};
  var exports = $PS["Data.EuclideanRing"];
  var $foreign = $PS["Data.EuclideanRing"];
  var Data_CommutativeRing = $PS["Data.CommutativeRing"];  
  var EuclideanRing = function (CommutativeRing0, degree, div, mod) {
      this.CommutativeRing0 = CommutativeRing0;
      this.degree = degree;
      this.div = div;
      this.mod = mod;
  }; 
  var euclideanRingInt = new EuclideanRing(function () {
      return Data_CommutativeRing.commutativeRingInt;
  }, $foreign.intDegree, $foreign.intDiv, $foreign.intMod);
  var div = function (dict) {
      return dict.div;
  };
  exports["div"] = div;
  exports["euclideanRingInt"] = euclideanRingInt;
})(PS);
(function(exports) {
  "use strict";

  exports.fromNumberImpl = function (just) {
    return function (nothing) {
      return function (n) {
        /* jshint bitwise: false */
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };

  exports.toNumber = function (n) {
    return n;
  };
})(PS["Data.Int"] = PS["Data.Int"] || {});
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];
  var Control_Category = $PS["Control.Category"];  
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var maybe = function (v) {
      return function (v1) {
          return function (v2) {
              if (v2 instanceof Nothing) {
                  return v;
              };
              if (v2 instanceof Just) {
                  return v1(v2.value0);
              };
              throw new Error("Failed pattern match at Data.Maybe (line 217, column 1 - line 217, column 51): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name ]);
          };
      };
  };
  var fromMaybe = function (a) {
      return maybe(a)(Control_Category.identity(Control_Category.categoryFn));
  };
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["fromMaybe"] = fromMaybe;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Data.Int"] = $PS["Data.Int"] || {};
  var exports = $PS["Data.Int"];
  var $foreign = $PS["Data.Int"];
  var Data_Maybe = $PS["Data.Maybe"];
  var fromNumber = $foreign.fromNumberImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
  exports["fromNumber"] = fromNumber;
  exports["toNumber"] = $foreign.toNumber;
})(PS);
(function(exports) {
  "use strict";

  exports.showIntImpl = function (n) {
    return n.toString();
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Data.Show"] = $PS["Data.Show"] || {};
  var exports = $PS["Data.Show"];
  var $foreign = $PS["Data.Show"];
  var Show = function (show) {
      this.show = show;
  };                                                 
  var showInt = new Show($foreign.showIntImpl);
  var show = function (dict) {
      return dict.show;
  };
  exports["Show"] = Show;
  exports["show"] = show;
  exports["showInt"] = showInt;
})(PS);
(function(exports) {
  "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
      return {};
    };
  };
})(PS["Effect.Console"] = PS["Effect.Console"] || {});
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Effect.Console"] = $PS["Effect.Console"] || {};
  var exports = $PS["Effect.Console"];
  var $foreign = $PS["Effect.Console"];
  exports["log"] = $foreign.log;
})(PS);
(function(exports) {
  "use strict";

  // module Math

  exports.abs = Math.abs;
})(PS["Math"] = PS["Math"] || {});
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Math"] = $PS["Math"] || {};
  var exports = $PS["Math"];
  var $foreign = $PS["Math"];
  exports["abs"] = $foreign.abs;
})(PS);
(function(exports) {
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
})(PS["Reader"] = PS["Reader"] || {});
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Reader"] = $PS["Reader"] || {};
  var exports = $PS["Reader"];
  var $foreign = $PS["Reader"];
  exports["parseInput"] = $foreign.parseInput;
  exports["readline"] = $foreign.readline;
})(PS);
(function($PS) {
  // Generated by purs version 0.13.6
  "use strict";
  $PS["Main"] = $PS["Main"] || {};
  var exports = $PS["Main"];
  var Data_EuclideanRing = $PS["Data.EuclideanRing"];
  var Data_Int = $PS["Data.Int"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Show = $PS["Data.Show"];
  var Effect_Console = $PS["Effect.Console"];
  var $$Math = $PS["Math"];
  var Reader = $PS["Reader"];                
  var Pos = (function () {
      function Pos(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Pos.create = function (value0) {
          return function (value1) {
              return new Pos(value0, value1);
          };
      };
      return Pos;
  })();
  var Area = (function () {
      function Area(value0, value1) {
          this.value0 = value0;
          this.value1 = value1;
      };
      Area.create = function (value0) {
          return function (value1) {
              return new Area(value0, value1);
          };
      };
      return Area;
  })();
  var showPos = new Data_Show.Show(function (v) {
      return Data_Show.show(Data_Show.showInt)(v.value0) + (" " + Data_Show.show(Data_Show.showInt)(v.value1));
  });
  var calcArea = function (input) {
      var rY = Pos.create(0)(input.height - 1 | 0);
      var rX = Pos.create(0)(input.width - 1 | 0);
      return new Area(rX, rY);
  };
  var abs = function (x) {
      return Data_Maybe.fromMaybe(-1 | 0)(Data_Int.fromNumber($$Math.abs(Data_Int.toNumber(x))));
  };
  var getMiddlePos = function (v) {
      var y = Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt)(abs(v.value1.value0 + v.value1.value1 | 0))(2);
      var x = Data_EuclideanRing.div(Data_EuclideanRing.euclideanRingInt)(abs(v.value0.value0 + v.value0.value1 | 0))(2);
      return new Pos(x, y);
  };
  var loop = function (input) {
      return function (area) {
          var yUp = function (y) {
              return new Pos(y - 1 | 0, 0);
          };
          var yDown = function (y) {
              return new Pos(y + 1 | 0, input.height - 1 | 0);
          };
          var xRight = function (x) {
              return new Pos(x + 1 | 0, input.width - 1 | 0);
          };
          var xLeft = function (x) {
              return new Pos(x - 1 | 0, 0);
          };
          var shrinkArea = function (v) {
              return function (v1) {
                  if (v === "U") {
                      return new Area(new Pos(v1.value0, v1.value0), yUp(v1.value1));
                  };
                  if (v === "D") {
                      return new Area(new Pos(v1.value0, v1.value0), yDown(v1.value1));
                  };
                  if (v === "R") {
                      return new Area(xRight(v1.value0), new Pos(v1.value1, v1.value1));
                  };
                  if (v === "L") {
                      return new Area(xLeft(v1.value0), new Pos(v1.value1, v1.value1));
                  };
                  if (v === "UR") {
                      return new Area(xRight(v1.value0), yUp(v1.value1));
                  };
                  if (v === "DR") {
                      return new Area(xRight(v1.value0), yDown(v1.value1));
                  };
                  if (v === "DL") {
                      return new Area(xLeft(v1.value0), yDown(v1.value1));
                  };
                  if (v === "UL") {
                      return new Area(xLeft(v1.value0), yUp(v1.value1));
                  };
                  return new Area(new Pos(9999, 9999), new Pos(9999, 9999));
              };
          };
          return function __do() {
              var dir = Reader.readline();
              var pos = new Pos(input.x, input.y);
              var area$prime = shrinkArea(dir)(pos);
              var v = getMiddlePos(area$prime);
              var input$prime = {
                  x: v.value0,
                  y: v.value1,
                  height: input.height,
                  turns: input.turns,
                  width: input.width
              };
              Effect_Console.log(Data_Show.show(Data_Show.showInt)(v.value0) + (" " + Data_Show.show(Data_Show.showInt)(v.value1)))();
              return loop(input$prime)(area$prime)();
          };
      };
  };
  var main = function __do() {
      var input = Reader.parseInput();
      return loop(input)(calcArea(input))();
  };
  exports["Pos"] = Pos;
  exports["Area"] = Area;
  exports["main"] = main;
  exports["calcArea"] = calcArea;
  exports["loop"] = loop;
  exports["getMiddlePos"] = getMiddlePos;
  exports["abs"] = abs;
  exports["showPos"] = showPos;
})(PS);
PS["Main"].main();