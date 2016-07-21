// TODO: move to prototype classes? some shared functionality between ParseTree and ParseNode should be abstracted to a parent class

function ParseTree() {
  this.rootNode = new ParseNode(null, {tree: this});
  
  // -----------------------------------------------
  // tree functions
  // -----------------------------------------------
  // determine the maximum depth of the parse tree
  this.depth = function() {
    var current_depth = 0;
    var max_depth = 0;
    var nodes = [[this.rootNode, 1]];

    while(nodes.length > 0) {
      state = nodes.shift();
      node = state[0];
      current_depth = state[1];
      
      if(max_depth < current_depth)
        max_depth = current_depth;
      for(var i = 0; i < node.children.length; i++)
        nodes.push([node.children[i], current_depth + 1]);
    }
    return max_depth
  }
  
  
  // -----------------------------------------------
  // view helpers
  // -----------------------------------------------
  this.views = [];
  this.addNodeView = function(view) {
    this.views.push(view);
  }
  
  this.resetViews = function() {
    this.views = [];
  }
}


function ParseNode(parent, attributes) {
  // -----------------------------------------------
  // key value coding (observer) functions
  // -----------------------------------------------
  // register observers of an attribute of this node
  this.observers = {};
  this.registerObserver = function(observer, key) {
    if(this.observers[key] == null)
      this.observers[key] = [observer];
    else
      this.observers[key].push(observer);
  };
  
  // set an attribute of this node, and inform observers of the assignment
  this.set = function(key, value) {
    // will update
    if(this.observers[key] != null) {
      currentValue = this[key];
      for(var i = 0; i < this.observers[key].length; i++)
        this.observers[key][i].keyWillUpdate(this, key, currentValue, value);
    }
    
    this[key] = value;
    
    // did update
    if(this.observers[key] != null) {
      currentValue = this[key];
      for(var i = 0; i < this.observers[key].length; i++)
        this.observers[key][i].keyDidUpdate(this, key, value);
    }
  }
  
  // simple get attribute value function to help with consistency
  this.get = function(key) {
    return this[key];
  }
  
  
  // -----------------------------------------------
  // tree functions
  // -----------------------------------------------
  this.insertChild = function(node, position) {
    newChildren = [];
    for(var i = 0; i < this.children.length; i++) {
      if(i == position)
        newChildren.push(node);
      newChildren.push(this.children[i]);
    }
    
    this.set('children', newChildren);
  }
  
  this.appendChild = function(node) {
    // handle a special case where the child being added has a this node as
    // a parent and this node knows which tree it belongs to. the parser JS
    // output format (for simplicity) will trigger these conditions, so swap
    // outthe tree's current rootNode for this new node.
    if(this.tree && this.parent == null) {
      this.tree.rootNode = node;
      return;
    }
    
    // the keyWillUpdate callback on observers send the 'current' and new
    // values of an attribute. to support this, we need to create a copy
    // of the children list and set that, rather than simply appending
    // the child to the existing children's list
    newChildren = [];
    for(var i = 0; i < this.children.length; i++)
      newChildren.push(this.children[i]);
    newChildren.push(node);
    
    this.set('children', newChildren);
  }
  
  this.leaf = function() {
    return this.children.length == 0;
  }
  
  
  // -----------------------------------------------
  // view helpers
  // -----------------------------------------------
  // determine the maximum width of a node. effectively the number of
  // tokens (words) that appear underneath this branch of the tree
  this.width = function() {
    if(this.children.length == 0)
      return 1;
    
    var width = 0;
    for(var i = 0; i < this.children.length; i++)
      width += this.children[i].width();
    return width;
  }
  
  this.views = [];
  this.addNodeView = function(view) {
    this.views.push(view);
  }
  
  this.resetViews = function() {
    this.views = [];
  }
  
  this.registerEvent = function(event, handler) {
    for(var i = 0; i < this.views.length; i++)
      this.views[i].addEventListener(event, handler);
  }
  
  
  // -----------------------------------------------
  // initialisation
  // -----------------------------------------------
  // add the initial attributes
  for(var key in attributes) {
    this.set(key, attributes[key]);
  }
  
  this.children = [];
  this.parent = parent;
  if(parent)
    parent.appendChild(this);
  
  // -----------------------------------------------
  // attribute documentation
  // -----------------------------------------------
  // attributes that will always be set externally:
  // - type
  // - cat
  //
  // other attributes that may be set (on leaf nodes):
  // - word
  // - lemma
  // - pos
  // - chunk
  // - entity
  // - start
  // - span
}
