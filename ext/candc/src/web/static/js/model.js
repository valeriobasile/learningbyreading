// generic model class
var Model = Class.create(KVC, {
  initialize: function($super) {
    $super();
    this.plural_path = "";
    this.instances = {};
  },
  
  // -----------------------------------------------
  // Model Collection Functions
  // -----------------------------------------------
  find: function(conditions, callback) {
    context = this;
    
    new Ajax.Request(this.plural_path, {method: 'get', parameters: {conditions: JSON.stringify(conditions)}, onSuccess: function() {
      return function(response) {context.respondWithResults(response, callback);}
    }() });
  },
  
  create: function() {
    alert("Uninmplemented virtual function: create. Intended to allow model collections to create new model instances.");
  },
  
  
  // -----------------------------------------------
  // Model Instance Functions
  // -----------------------------------------------
  path: function() {
    alert("Unimplemnted virtual function: path. Intended to allow model instances to set their URL path.");
  },
  
  save: function() {
    if(this.get('id')) {
      method = 'put';
      url = this.path();
    } else {
      method = 'post';
      url = this.plural_path;
    }
    
    context = this;
    new Ajax.Request(url, {method: method, parameters: this.values, onSuccess: function() {
      return function(response) {context.parseJSONResponse($H(response.responseJSON), context)}
    }() });
  },
  
  destroy: function() {
    if(!this.get('id'))
      alert("Error: attempt to delete a model collection")
    else
      new Ajax.Request(this.path(), {method: 'delete'});
  },
  
  
  // -----------------------------------------------
  // Private Functions
  // -----------------------------------------------
  parseJSONResponse: function(values, target) {
    values.each(function(pair) {
      target.set(pair.key, pair.value);
    });
  },
  
  respondWithResults: function(response, callback) {
    response_values = response.responseJSON;
    response_instances = [];
    
    response_values.each(function(model) {
      if(context.instances[model.id]) {
        context.instances[model.id].parseJSONResponse($H(model));
        response_instances.push(context.instances[model.id]);
      } else {
        new_model = context.create();
        new_model.parseJSONResponse($H(model), new_model);
        context.instances[model.id] = new_model;
        response_instances.push(new_model);
      }
    });
    
    callback(response_instances);
  }
});