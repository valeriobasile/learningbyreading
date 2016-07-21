// TODO: rp = ?
// Mapping between parser transition types, and the html to represent them
var NODE_TYPE_MAP = {};
NODE_TYPE_MAP['fa'] = '&gt;';
NODE_TYPE_MAP['ba'] = '&lt;';
NODE_TYPE_MAP['fc'] = '&gt;<sub>B</sub>';
NODE_TYPE_MAP['bc'] = '&lt;<sub>B</sub>';
NODE_TYPE_MAP['bx'] = '&lt;<sub>B<sub>X</sub></sub>';
NODE_TYPE_MAP['fx'] = '&gt;<sub>B<sub>X</sub></sub>';
NODE_TYPE_MAP['lex'] = 'PSG';
NODE_TYPE_MAP['tr'] = 'T';
NODE_TYPE_MAP['ft'] = '&gt;<sub>T</sub>';
NODE_TYPE_MAP['bt'] = '&lt;<sub>T</sub>';
NODE_TYPE_MAP['conj'] = '&gt;ɸ';
NODE_TYPE_MAP['rp'] = '&gt;'


// leaf (word) nodes have two view nodes associated with them. a table header
// tag (this class), representing the word, and a NodeView object with a div
// representing the category of the word
function WordView(node) {
  this.parseNode = node;
  this.parseNode.addNodeView(this);
  
  // draw the node
  this.domNode = document.createElement('th');
  this.domNode.innerHTML = this.parseNode.get('word');
  
  // register observer
  this.parseNode.registerObserver(this, 'word');
  this.keyWillUpdate = function(node, key, currentValue, newValue) {;}
  this.keyDidUpdate = function(node, key, value) {
    this.domNode.innerHTML = value;
  }
  
  // allow attaching events to this view
  this.addEventListener = function(event, handler) {
    this.domNode.addEventListener(event, handler, false);
  }
}



function NodeView(node) {
  this.parseNode = node;
  this.parseNode.addNodeView(this);
  
  // create the enclosing table data element
  this.domNode = document.createElement('td');
  this.domNode.setAttribute('colspan', this.parseNode.width());
  
  // create the category div
  this.categoryTag = document.createElement('p');
  this.categoryTag.innerHTML = this.parseNode.get('cat');
  this.categoryDivTag = document.createElement('div');
  this.categoryDivTag.appendChild(this.categoryTag);
  this.domNode.appendChild(this.categoryDivTag);
  
  // if this is not a leaf node, add the transition type div element
  if(!this.parseNode.leaf()) {
    this.typeTag = document.createElement('div')
    this.typeTag.className = 'extra';
    this.typeTag.innerHTML = NODE_TYPE_MAP[this.parseNode.get('type')];
    this.categoryDivTag.appendChild(this.typeTag);
  }
  
  // register observers
  this.parseNode.registerObserver(this, 'cat');
  this.parseNode.registerObserver(this, 'type');
  this.keyWillUpdate = function(node, key, currentValue, newValue) {;}
  this.keyDidUpdate = function(node, key, value) {
    if(key == 'cat')
      this.categoryTag.innerHTML = value;
    if(key == 'type')
      this.typeTag.innerHTML = NODE_TYPE_MAP[value];
  }
  
  // allow attaching events to this view
  this.addEventListener = function(event, handler) {
    this.domNode.addEventListener(event, handler, false);
  }
}



function TreeView(tree) {
  this.rows = [];
  this.domNode = null;
  this.parseTree = tree;
  this.treeDepth = 1;
  this.parentDomNode = null;
  
  // draw or redraw the tree
  this.draw = function() {
    // if the tree has been drawn before, start from scratch
    if(this.domNode != null) {
      this.parentDomNode = this.domNode.parentNode;
      this.domNode.parentNode.removeChild(this.domNode);
      this.cleanViewsFromModels(this.parseTree.rootNode);
    }
    
    // the parse tree is drawn inside a table
    this.domNode = document.createElement('table');
    this.domNode.className = 'parseTree';
    this.treeDepth = this.parseTree.depth();
    
    // intialise the rows; we add 1 to the number of rows because leaves
    // have two rows associated with them (the category row, and the word)
    this.rows = [];
    for(var i = 0; i <= this.treeDepth; i++)
      this.rows.push(document.createElement('tr'));
    
    // recursively add the various views to the tree
    this.addNodeAndChildren(this.parseTree.rootNode, 0, this.parentDomNode == null);
    
    // add the rows to the table in reverse order (so the words are on top)
    for(var i = this.treeDepth; i >= 0; i--)
      this.domNode.appendChild(this.rows[i]);
    
    // if the table has previously been inserted into an element,
    // re-insert the newly created tree
    if(this.parentDomNode)
      this.parentDomNode.appendChild(this.domNode);
  }
  
  this.addNodeAndChildren = function(node, currentDepth, addObserver) {
    nodeView = new NodeView(node);
    
    if(node.leaf()) {
      // add the word at the top of the table
      this.rows[this.treeDepth - 1].appendChild(nodeView.domNode);
      
      // and the word header
      wordView = new WordView(node);
      this.rows[this.treeDepth].appendChild(wordView.domNode);
      
      // add any spacer nodes necessary to push the word to the top of the table
      if(currentDepth != this.treeDepth)
        for(var i = currentDepth; i < this.treeDepth - 1; i++)
          this.rows[i].appendChild(document.createElement('td'));
    } else {
      // add the node at the current level of the tree
      this.rows[currentDepth].appendChild(nodeView.domNode);
      
      // add an observer on this nodes children
      if(addObserver)
        node.registerObserver(this, 'children');
      
      // add children of this node
      for(var i = 0; i < node.children.length; i++)
        this.addNodeAndChildren(node.children[i], currentDepth + 1, addObserver)
    }    
  }
  
  this.cleanViewsFromModels = function(node) {
    node.resetViews();
    if(!node.leaf())
      for(var i = 0; i < node.children.length; i++)
        this.cleanViewsFromModels(node.children[i]);
  }
  
  
  // for now, whenever the list of children updates, simply redraw the
  // entire tree. in the future we will want to insert an element where
  // necessary, preserving the existing NodeView's.
  this.keyDidUpdate = function(node, key, value) {
    this.draw();
  }
  this.keyWillUpdate = function(node, key, currentValue, newValue) {;}
  
  // start to construct the table
  this.draw();
}

