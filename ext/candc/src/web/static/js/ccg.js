/* convert JSON category hashes to HTMLElements and Strings
 * 
 * Simple categories have atom a, feature f and variable v
 * Complex categories have result res, argument arg, slash s and variable v
 * res and arg are nested JSON category hashes
 */

function feature2html(cat){
  if('f' in cat && cat.f != 'X')
    return S('feature', cat.f);
  return null;
}

function simple2html(cat){
  return S('cat', S('atom', cat.a), feature2html(cat));
}

function complex2html(cat, nested){
	var open = nested ? S('paren', '(') : '(';
  var close = nested ? S('paren', ')') : ')';
	return S('cat', open, cat2html(cat.res, false), cat.s, cat2html(cat.arg, false), close);
}

function cat2html(cat, nested){
  if(cat === null)
    return 'none';

  var res = null;
  if('a' in cat)
    res = simple2html(cat);
  else
		res = complex2html(cat, nested);
  return res.insert(SUB('var', cat.v));
}

function cat2str(cat){
  if(cat === null)
    return 'none';

  if('a' in cat){
    var feat = '';
    if('f' in cat && cat.f != 'X')
      feat = '[' + cat.f + ']';
    return cat.a + feat + '{' + cat.v + '}';
  }else{
    var res = cat2str(cat.res);
    if('s' in cat.res)
      res = '(' + res + ')';
    var arg = cat2str(cat.arg);
    if('s' in cat.arg)
      arg = '(' + arg + ')';
    return res + cat.s + arg;
  }
}

function fillers2html(v){
  var res = UL('fillers');
  var nfilled = 0;
  for(var f = 0; f < v.fillers.length; f++){
    var pos = v.fillers[f];
    if(pos != 0){
      res.insert(LI('filler', null, S('word', data.words[pos - 1]), SUB('pos', pos)));
      nfilled++;
    }else if(v.fillers.length > 1){
      res.insert(LI('filler', null, S('word', '?')));
      nfilled++;
    }
  }
  if(nfilled == 0)
    return null;

  res.addClassName((nfilled <= 1) && 'single' || 'set');
  return res;
}

function vars2html(vars, used){
  var res = TABLE('vars ' + used);
  for(var i = 0; i < vars.length; i++){
    var v = vars[i];
    var fillers = fillers2html(v);
    if(fillers)
      res.insert(TR('assignment', TD('var', v.varid), TD(null, ' = '), TD(null, fillers)));
  }
  return res;
}

function unfilled2html(unfilled, used){
  var e_deps = TABLE('deps ' + used);
  for(var j = 0; j < unfilled.length; j++){
    var d = unfilled[j];
    e_deps.insert(TR('dep',
      TD(null,
        S('word', d.word),
	SUB('pos', d.pos)
      ),
      TD('rel',
	S('cat', html(d.cat)),
	S('slot', d.slot)
      ),
      TD('var', d.varid)
    ));
  }
  return e_deps;
}

function alts2html(alts, used){
  if(alts.length == 1)
    return null;

  var res = UL('alts ' + used);
  for(var i = 0; i < alts.length; i++){
    var alt = alts[i];
    if(alt.l == 0 && alt.r == 0)
      continue;

    index['a_' + alt.id] = alt;

    var e_alt = LI('alt', 'a_' + alt.id, S('comb', alt.c));
    e_alt.observe('mouseover', equiv_mouseover);
    e_alt.observe('mouseout', equiv_mouseout);
    e_alt.observe('click', equiv_click);

    res.insert(e_alt);
  }
  return res;
}

function toggleVars(){
	$('vars').toggle();
  $('parens').toggle();
}
