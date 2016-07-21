var data = {};
var index = {};
var cache = [];

function html(str){
  return cache[str].cloneNode(true);
}

function label_tree(id, label){
  if(id == 0)
    return;

  var elem = $(id + '');
  if(elem)
    elem.addClassName(label);

  var equiv = index[id];
  if(!equiv)
    return;
  label_tree(equiv.l, label);
  label_tree(equiv.r, label);
}

function equiv_mouseover(event){
  label_tree(this.id, 'hl');
  event.stop();
}

function equiv_mouseout(event){
  $$('li.hl').invoke('removeClassName', 'hl');
  event.stop();
}

function equiv_click(event){
  $$('li.equiv').invoke('hide');
  label_tree(this.id, 'show');
  $$('li.show').invoke('show');
  event.stop();
}

function equiv_contextmenu(event){
  $$('li.equiv').invoke('show');
  $$('li.show').invoke('removeClassName', 'show');
  event.stop();  
}

function get_detail_click(event){
  event.stop();

  var equiv = index[this.id];
  if('vars' in equiv){
    $('d_' + equiv.id).toggle();
    return;
  }

  var url = '/detail/' + equiv.cell.pos + '/' + equiv.cell.span + '/' + equiv.i + '/';
  new Ajax.Request(url, {
    onSuccess: function(transport){
      var extra_cats = transport.responseJSON.extra_cats;
      cache = cache.concat(extra_cats.map(cat2html));

      var detail = transport.responseJSON.detail;
      equiv.ehash = detail.ehash;
      equiv.vars = detail.vars;
      equiv.unfilled = detail.unfilled;
      equiv.alts = detail.alts;

      var e_detail = $('d_' + equiv.id);
      var used = equiv.m == 1 && 'used' || 'unused';
      e_detail.insert(alts2html(equiv.alts, used));
      e_detail.insert(vars2html(equiv.vars, used));
      e_detail.insert(unfilled2html(equiv.unfilled, used));
      e_detail.show();
    }
  });
}

function get_unused_click(event){
  event.stop();

  var cell = index[this.parentNode.id];
  if('unused' in cell){
    cell.elem.select('.unused').invoke('toggle');
    return;
  }

  var url = '/unused/' + cell.pos + '/' + cell.span + '/';
  new Ajax.Request(url, {
    onSuccess: function(transport){
      var extra_cats = transport.responseJSON.extra_cats;
      cache = cache.concat(extra_cats.map(cat2html));

      cell.unused = transport.responseJSON.unused;
      var elem = $('e_' + cell.pos + '_' + cell.span);
      var children = $A(elem.childNodes);
      for(var j = cell.unused.length - 1; j >= 0; j--){
	var eq = cell.unused[j];
	cell.eqs.push(eq);
	eq.cell = cell;
	index[eq.id] = eq;
	children.push(equiv2html(eq));
      }
      elem.update();
      // reverse sort by cell index to interleave unused equivalence classes
      children = children.sortBy(function(e){ e = index[e.id]; return e && -e.i || 1; });
      children.each(function(e){ elem.insert(e); });
    }
  });
}

UNUSED_LINK = A('get_unused', '#', '+');
DETAIL_LINK = A('get_detail', '#', '?');
DETAIL = D('detail');

function equiv2html(eq){
  var used = eq.m == 1 && 'equiv used' || 'equiv unused';
  var res = LI(used, eq.id, html(eq.cat));
  var a_detail = DETAIL_LINK.cloneNode(true);
  a_detail.id = 'gd_' + eq.id;
  index[a_detail.id] = eq;
  a_detail.observe('click', get_detail_click);
  a_detail.observe('mouseover', Event.stop);
  res.insert(a_detail);
  var e_detail = DETAIL.cloneNode(true);
  e_detail.id = 'd_' + eq.id;
  res.insert(e_detail);
  res.observe('mouseover', equiv_mouseover);
  res.observe('mouseout', equiv_mouseout);
  res.observe('click', equiv_click);
  res.observe('contextmenu', equiv_contextmenu);
  return res;
}

function chart2html(transport){
  data = transport.responseJSON.chart;
  var nwords = data.stats.nwords;
  var ncells = data.cells.length;
  var chart = $('chart_body');

  if(window.console)
    console.time('cleared chart');
  chart.update();
  if(window.console)
    console.timeEnd('cleared chart');

  if(window.console)
    console.time('created table');
  for(var span = nwords; span >= 1; span--){
    var tr = TR('span');
    for(var pos = 0; pos < nwords - span + 1; pos++) {
      var td = TD('cell', UL('equivs', 'e_' + pos + '_' + span));
      td.id = 'cell_' + pos + '_' + span;
      tr.insert(td);
    }
    chart.insert(tr);
  }
  if(window.console)
    console.timeEnd('created table');

  if(window.console)
    console.time('cached cats');
  cache = data.cats.map(cat2html);
  if(window.console)
    console.timeEnd('cached cats');

  if(window.console)
    console.time('created cells');
  for(var i = 0; i < ncells; i++){
    var cell = data.cells[i];
    var elem = $('e_' + cell.pos + '_' + cell.span);
    $('cell_' + cell.pos + '_' + cell.span).observe('dblclick', cellSpanHandler.curry(cell));

    if(cell.neqs == 0)
      elem.parentNode.addClassName('empty');

    if(cell.neqs - cell.nused > 0){
      var a_unused = UNUSED_LINK.cloneNode(true);
      a_unused.observe('click', get_unused_click);
      elem.insert(a_unused);
    }

    cell.elem = elem;
    index[elem.id] = cell;
    for(var j = cell.eqs.length - 1; j >= 0; j--){
      var eq = cell.eqs[j];
      eq.cell = cell;
      index[eq.id] = eq;
      elem.insert(equiv2html(eq));
    }
  }
  if(window.console)
    console.timeEnd('created cells');

  if(window.console)
    console.time('registering handlers');
  $('unhide').observe('click', function(event) {
    $$('li.equiv').invoke('show');
    $$('li.show').invoke('removeClassName', 'show');
    event.stop();
  });

  $('toggle_detail').observe('click', function(event) {
    $$('div.detail').invoke('toggle');
    event.stop();
  });

  $('toggle_unused').observe('click', function(event) {
    $$('li.unused').invoke('toggle');
    event.stop();
  });
  
  $('pop_shell').observe('click', function(event) {
      window.open('/shell/window/', 'Python Shell', 'location=no,menubar=no,scrollbars=no,status=no,width=600,height=400');
  });

  document.observe('contextmenu', Event.stop);
  if(window.console)
    console.timeEnd('registering handlers');
}


function cellSpanHandler(cell, event) {
  var str = 'cell(' + cell.pos + ',' + cell.span + '): ';
  str += cell.tokens.join(' ');
  $('tokens').update(str);
  event.stop();
}

