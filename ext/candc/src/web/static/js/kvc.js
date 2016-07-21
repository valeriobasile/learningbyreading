// generic KVC class. allows observation of values using key-value-coding.
var KVC = Class.create({
  initialize: function() {
    this.observers = {};
    this.values = {};
  },
  
  addObserver: function(key, observer) {
    if(!this.observers[key])
      this.observers[key] = [];
    this.observers[key].push(observer);
  },
  
  set: function(key, value) {
    // send the will update event to all observers of the key
    if(this.observers[key]) {
      currentValue = this[key];
      this.observers[key].each(function(observer) {
        observer.keyWillUpdate(this, key, currentValue, value);
      });
    }
    
    this.values[key] = value;
    
    // send the did update event to all observers of the key
    if(this.observers[key]) {
      this.observers[key].each(function(observer) {
        observer.keyDidUpdate(this, key, value);
      });
    }
  },
  
  get: function(key) {
    return this.values[key];
  }
});
