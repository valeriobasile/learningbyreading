
var NODE_TYPE_MAP = {};
NODE_TYPE_MAP['fa'] = '&gt;';
NODE_TYPE_MAP['ba'] = '&lt;';
NODE_TYPE_MAP['fc'] = '&gt;<sub>B</sub>';
NODE_TYPE_MAP['gfc'] = '&gt;<sub>B<sup>*</sup></sub>';
NODE_TYPE_MAP['bc'] = '&lt;<sub>B</sub>';
NODE_TYPE_MAP['gbc'] = '&lt;<sub>B<sup>*</sup></sub>';
NODE_TYPE_MAP['bx'] = '&lt;<sub>B<sub>X</sub></sub>';
NODE_TYPE_MAP['gbx'] = '&lt;<sub>B<sup>*</sup><sub>X</sub></sub>';
NODE_TYPE_MAP['fx'] = '&gt;<sub>B<sub>X</sub></sub>';
NODE_TYPE_MAP['lex'] = 'L';
NODE_TYPE_MAP['tr'] = 'T';
NODE_TYPE_MAP['ft'] = '&gt;<sub>T</sub>';
NODE_TYPE_MAP['bt'] = '&lt;<sub>T</sub>';
NODE_TYPE_MAP['conj'] = '&gt;<sub>ɸ</sub>';

NODE_TYPE_MAP['rp'] = '&gt;';
NODE_TYPE_MAP['lp'] = '&lt;';

NODE_TYPE_MAP['funny'] = 'F';
NODE_TYPE_MAP['ltc'] = '&gt;<sub>TC</sub>';
NODE_TYPE_MAP['rtc'] = '&lt;<sub>TC</sub>';

NODE_TYPE_MAP['funny'] = 'A';

function getRow(node, table){
  var id = 'spans' + node.depth;
  var row = $(id);
	if(row == null){
		row = TR('spans');
		row.id = id;
		table.insert(row);
	}
  return row;
}

function rejectSpan(span){
	constraints.push('reject ' + span.colSpan + ' ' + span.start);
}

function changeSpan(span){
	constraints.push('expect ' + span.colSpan + ' '  + span.start);
}

function requireSpan(span){
	constraints.push('require ' + span.colSpan + ' ' + span.start);
}

function span2html(node, table, cache){
	node.depth = 0;
	if(node.l != null && !node.collapsed){
	 	span2html(node.l, table, cache);
    node.depth = node.l.depth + 1;
		if(node.r != null){
			span2html(node.r, table, cache);
      node.depth = Math.max(node.depth, node.r.depth + 1);
    }
	}

	var row = getRow(node, table);

  var start = 0;
	if(row.lastChild != null)
		start = row.lastChild.end;

	if(start != node.pos){
		var gap = TD('gap');
		gap.start = start;
		gap.end = node.pos;
		gap.colSpan = gap.end - gap.start;
		row.insert(gap);
	}

	var marker = null;
	if(node.l)
   	marker = D('extra', NODE_TYPE_MAP[node.c]);

  var cat = html(node.cat);
  var change = A('change', '#', '?');
  var require = A('require', '#', '&#x2713;');
  var reject = null;
  if(node.span > 1)
    reject = A('reject', '#', '&#x2715;');
  var controls = S('controls', require, change, reject);
	var span = TD('rule', D(null, P(null, cat, controls), marker));
	span.start = node.pos;
	span.end = node.pos + node.span;
	span.colSpan = node.span;
	span.node = node;
	row.insert(span);

  $(change).observe('click', function(event){
		changeSpan(span);
    event.stop();
	});

  $(require).observe('click', function(event){
		requireSpan(span);
    event.stop();
	});

  if(node.span > 1){
    $(reject).observe('click', function(event){
		  rejectSpan(span);
      event.stop();
	  });
  }
}

function grsRow(words, table){
	var row = TR('grs');
  table.insert(row);
  var col = TD();
  col.colSpan = words.length;
  col.id = 'raphael';
  row.insert(col);
}

function words2html(words, table){
	var row = TR('words');
	table.insert(row);
	for(var i = 0; i < words.length; ++i){
		var word = TH('word', words[i]);
    word.id = 'word' + i;
		row.insert(word);
	}
}

function word2x(word){
  var word_box = word.getBoundingClientRect();
  var raphael_box = $('raphael').getBoundingClientRect();
  return word_box.left + (word_box.width - 5)/2.0 - raphael_box.left;
}

function createEdgePath(link){
	var path = 'M ' + link.from_x + ' ' + link.bottom;
  path += 'L ' + link.from_x + ' ' + link.top;
  path += 'L ' + link.to_x + ' ' + link.top;
  path += 'L ' + link.to_x + ' ' + link.bottom;
  return path;
}

function createArrowPath(link){
	var path = 'M ' + link.to_x + ' ' + (link.bottom + 3);
  path += 'L ' + (link.to_x - 3) + ' ' + (link.bottom - 3);
  path += 'L ' + (link.to_x + 3) + ' ' + (link.bottom - 3);
  path += 'Z';
  return path;
}

function getLinkBox(link){
  var bbox = link.text.getBBox();
  return {'x': bbox.x - 2, 'y': bbox.y - 2, 'width': bbox.width + 4, 'height': bbox.height + 4};
}

function drawLink(paper, link){
  var attr = {'stroke': link.color, 'stroke-width': 2, 'stroke-linejoin': 'round'};

  link.line = paper.path(createEdgePath(link)).attr(attr);

	link.text = paper.text((link.from_x + link.to_x)/2.0, link.top, link.type);
  link.text.attr({'font-size': 12, 'fill': link.color});

  attr.fill = 'white';

  var bbox = getLinkBox(link);
	link.box = paper.rect(bbox.x, bbox.y, bbox.width, bbox.height, 3);
  link.box.attr(attr);

  link.label = paper.set();
  link.label.push(link.box, link.text);
  link.label.toFront();

  link.from = paper.circle(link.from_x, link.bottom, 3).attr(attr);
  link.to = paper.path(createArrowPath(link)).attr(attr);
  link.from.link = link.to.link = link;

  return link;
}

function fromToFront(link){
  link.label.toFront();
  link.to.toFront();
  link.from.toFront();
}

function toToFront(link){
  link.label.toFront();
  link.from.toFront();
  link.to.toFront();
}

var links = [];

function is_inside(a, b){
	var min = Math.min(b.from_x, b.to_x);
  var max = Math.max(b.from_x, b.to_x);
	return (a.from_x >= min && a.from_x <= max);
}

function setLinkHeight(paper, link){
	var ninside = 0;
	for(var i = 0; i < links.length; ++i){
		if(link === links[i])
			continue;
		ninside += is_inside(links[i], link);
  }
  link.height = 16 + ninside*24;
	link.bottom = paper.height - 6;
  link.top = link.bottom - link.height;
}

function updateLinkHeights(paper){
	links.sortBy(function(x){ return x.from_x; });
	for(var i = 0; i < links.length; ++i){
		setLinkHeight(paper, links[i]);
    redrawLink(links[i]);
  }
}

function addLink(paper, head, filler, label, color){
  var from_x = word2x($('word' + head));
  var to_x = word2x($('word' + filler));

  var link = {
		'from_word': head, 'to_word': filler,
		'from_x': from_x, 'to_x': to_x,
		'type': label, 'color': color,
	};

  setLinkHeight(paper, link);
  links.push(link);
  drawLink(paper, link);
  updateLinkHeights(paper);

  link.from.drag(
    function(dx, dy){ moveFrom(this.link, this.ox + dx, this.oy + dy); },
    function(){
			this.ox = this.link.from_x;
			this.oy = this.link.bottom;
      fromToFront(this.link);
		},
    function(){}
	);

  link.to.drag(
    function(dx, dy){ moveTo(this.link, this.ox + dx, this.oy + dy); },
    function(){
			this.ox = this.link.to_x;
			this.oy = this.link.bottom;
      toToFront(this.link);
		},
    function(){}
	);

  return link;
}

function redrawLink(link){
  link.line.attr('path', createEdgePath(link));
	link.from.attr('cx', link.from_x);
	link.to.attr('path', createArrowPath(link));
	link.text.attr({'x': (link.from_x + link.to_x)/2.0, 'y': link.top});
  link.box.attr(getLinkBox(link));
}

function moveFrom(link, x, y){
	link.from_x = x;
  updateLinkHeights(link.line.paper);
}

function moveTo(link, x, y){
	link.to_x = x;
  updateLinkHeights(link.line.paper);
}

function renderParse(data){
  var nwords = data.stats.nwords;
  var ncells = data.cells.length;

  deriv = $('chart_body');
  deriv.update();

  cache = data.cats.map(cat2html);
  grsRow(data.words, deriv);
	words2html(data.words, deriv);
	span2html(data.tree, deriv, cache);
  paper = Raphael('raphael');
  paper.setSize($('raphael').getWidth(), 110);

  constraints = [];
}

function parse2html(transport){
  data = transport.responseJSON.chart;
  renderParse(data);
}
