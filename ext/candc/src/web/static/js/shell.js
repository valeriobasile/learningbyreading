var commands = $A();
var backtrace = 0;

function shellKeypress(event) {
  var input = $('input');

  if (event.keyCode == 10 || event.keyCode == 13) {
    submitShell();
    backtrace = commands.size();
  }
  else if (event.keyCode == 38) {
    if (backtrace > 0) {
      --backtrace;
      input.focus();
      input.value = commands[backtrace];
    }
  }
  else if (event.keyCode == 40) {
    if (backtrace < commands.size() - 1) {
      ++backtrace;
      input.focus();
      input.value = commands[backtrace];
    }
    else if (backtrace == commands.size() - 1) {
      input.focus();
      input.value = '';
    }
  }
}


function submitShell() {
  var input = $('input');
  var value = input.value || '';
  commands.push(value);
  new Ajax.Request('/shell/post/', {
    parameters: {cmd: value},
    onSuccess: submitShellResponse,
  });
  input.value = '';
  input.focus();
}


function submitShellResponse(transport) {
  var json = transport.responseJSON;
  
  var line = new Element('div').addClassName('cmd').update('>>> ' + json.cmd);
  $('output').appendChild(line);

  if (json.data) {
    line = new Element('div').addClassName('line').update(json.data);
    if (json.success)
      line.addClassName('stdout')
    else
      line.addClassName('traceback');
    $('output').appendChild(line);
  }
  
  var c = $('out_container');
  c.scrollTop = c.scrollHeight;
}

