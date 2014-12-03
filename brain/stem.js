require('assert-dotenv')({}, function() {
  'use strict';

  // Utility Modules
  var _ = require('underscore');
  var uuid = require('uuid');

  // Main configuration settings.
  var HOX = require('../gonads/hox-genes/brain.hox');

  // Configure Bookshelf.
  var bookshelf = HOX.bookshelf.configure({
    client: 'pg',
    connection: process.env.DB
  });

  // Configure the DB models.
  var Event = require('./cerebrum/event.js')(bookshelf);

  // Instantiate the ESB Client.
  var HelpEsb = require('help-esb');
  var esbClient = new HelpEsb.Client(process.env.ESB);

  esbClient.login('cache-nibbler');

  function addCommand(event, command) {
    esbClient.send('socketIOGroup', {
      uri: 'addValidCommand',
      event: 'client ' + event + ' event',
      command: command
    });
  }

  addCommand('chat', 'start');
  addCommand('chat', 'end');

  esbClient.rpcReceive('socketIOGroup', function(payload, meta) {
    var message = payload.message;

    if (typeof message === 'object' && message.hasOwnProperty('data')) {
      var _event = {
        id: message.meta.id || uuid.v4(),
        createdAt: message.meta.createdAt || new Date(),
        content: JSON.stringify(message.data),
        type: message.meta.type,
        senderType: message.meta.access || message.data.access || 'FAKE',
        senderId: message.meta.senderId || message.data.senderId || uuid.v4(),
        customerId: message.meta.customerId || message.data.customerId || uuid.v4(),
        eventGroupId: message.meta.eventGroupId || message.data.eventGroupId || uuid.v4(),
        organizationId: message.meta.organizationId || message.data.organizationId || uuid.v4()
      };

      _.extend(_event, message.data, message.meta);

      var event = {
        id: _event.id,
        createdAt: _event.createdAt,
        content: _event.content,
        type: _event.type,
        senderType: _event.senderType,
        senderId: _event.senderId,
        customerId: _event.customerId,
        eventGroupId: _event.eventGroupId
      };

      console.log('socketIOGroup: ' + JSON.stringify(event));
      console.log('');

      new Event(event).save({}, { method: 'insert' });
    }
  });

  esbClient.rpcReceive('allChatGroup', function(payload) {
    var message = payload.message;

    if (typeof message === 'object' && message.hasOwnProperty('data')) {
      var _event = {
        id: message.meta.id || uuid.v4(),
        createdAt: message.meta.createdAt || new Date(),
        content: JSON.stringify(message.data),
        type: message.meta.type,
        senderType: message.meta.access || message.data.access || 'FAKE',
        senderId: message.meta.senderId || message.data.senderId || uuid.v4(),
        customerId: message.meta.customerId || message.data.customerId || uuid.v4(),
        eventGroupId: message.meta.eventGroupId || message.data.eventGroupId || uuid.v4(),
        organizationId: message.meta.organizationId || message.data.organizationId || uuid.v4()
      };

      _.extend(_event, message.data, message.meta);

      var event = {
        id: _event.id,
        createdAt: _event.createdAt,
        content: _event.content,
        type: _event.type,
        senderType: _event.senderType,
        senderId: _event.senderId,
        customerId: _event.customerId,
        eventGroupId: _event.eventGroupId
      };

      console.log('allChatGroup: ' + JSON.stringify(event));
      console.log('');

      new Event(event).save({}, { method: 'insert' });
    }
  });
});
