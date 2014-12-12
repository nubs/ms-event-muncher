;(function() {
  'use strict';

  var _ = require('underscore');
      _.str = require('underscore.string');

  module.exports = function(bookshelf) {
    var Event = bookshelf.Model.extend({
      idAttribute: 'id',
      tableName: 'event',
      format: function(attrs) {
        return _.reduce(attrs, function(memo, val, key) {
          memo[_.str.underscored(key)] = val;
          return memo;
        }, {});
      },
      parse: function(attrs) {
        return _.reduce(attrs, function(memo, val, key) {
          memo[_.str.camelize(key)] = val;
          return memo;
        }, {});
      }
    });

    return Event;
  };
})();
