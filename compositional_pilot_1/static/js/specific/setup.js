var setupTrials = function(items) {
  var info = {};

  var reducer = function(base, spec, count) {
    var item = _.clone(spec);
    var rev = {'pos': 'neg', 'neg': 'pos'};
    var type = _.random(0, 1) === 0 ? 'pos' : 'neg';

    var adjective = info[item.antonymPair];

    if (adjective !== undefined) {
      item.type = rev[adjective];
      item.word = item[rev[adjective]];
    } else {
      info[item.antonymPair] = type;
      item.word = item[type];
      item.type = type;
    }

    base.push(item);
    return base;
  };

  return _.reduce(_.shuffle(items), reducer, []);
};
