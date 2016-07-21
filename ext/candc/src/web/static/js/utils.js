
function element(tag, klass, b1, b2, b3, b4, b5){
  var res = new Element(tag, {'class': klass}).update(b1);
  return res.insert(b2).insert(b3).insert(b4).insert(b5);
}

function element_id(tag, klass, id, b1, b2, b3, b4, b5){
  var undef;
  if(id === undef)
    id = null;
  var res = new Element(tag, {'class': klass, 'id': id}).update(b1);
  return res.insert(b2).insert(b3).insert(b4).insert(b5);
}

function A(klass, href, b1){
  return new Element('a', {'class': klass, 'href': href}).update(b1);
}

function P(klass, b1, b2, b3, b4, b5){
  return element('p', klass, b1, b2, b3, b4, b5);
}

function S(klass, b1, b2, b3, b4, b5){
  return element('span', klass, b1, b2, b3, b4, b5);
}

function D(klass, b1, b2, b3, b4, b5){
  return element('div', klass, b1, b2, b3, b4, b5);
}

function SUB(klass, b1, b2, b3, b4, b5){
  return element('sub', klass, b1, b2, b3, b4, b5);
}

function TABLE(klass, b1, b2, b3, b4, b5){
  return element('table', klass, b1, b2, b3, b4, b5);
}

function TR(klass, b1, b2, b3, b4, b5){
  return element('tr', klass, b1, b2, b3, b4, b5);
}

function TD(klass, b1, b2, b3, b4, b5){
  return element('td', klass, b1, b2, b3, b4, b5);
}

function TH(klass, b1, b2, b3, b4, b5){
  return element('th', klass, b1, b2, b3, b4, b5);
}

function UL(klass, id, b1, b2, b3, b4, b5){
  return element_id('ul', klass, id, b1, b2, b3, b4, b5);
}

function LI(klass, id, b1, b2, b3, b4, b5){
  return element_id('li', klass, id, b1, b2, b3, b4, b5);
}
