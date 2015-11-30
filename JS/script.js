var keyboard = "ansi";

function send(msg){
  socket.send(msg);
  conState.className += " send";
  window.setTimeout(function(){conState.className = conState.className.replace(/ send/g, '');}, 10);
}

function sendKey(event, down){
  if(event.target.id != "textinput") {
    if(event.keyIdentifier == undefined){
      //try to work around firefox not having keyIdentifier
      if(event.key.length == 1){
        if(event.key == " ") event.keyIdentifier = "Space";
        else{
          var char = event.key.toUpperCase().charCodeAt(0).toString(16);
          event.keyIdentifier = ("U+" + "0".repeat(4-char.length) + char).toUpperCase();
        }
      }
      else{
        switch(event.key){
          case "Backspace": event.keyIdentifier = "U+0008"; break;
          case "Delete": event.keyIdentifier = "U+007F"; break;
          case "Escape": event.keyIdentifier = "U+001B"; break;
          default: event.keyIdentifier = event.key.replace("Arrow",""); break;
        }
      }
    }
    //prevent browser interpretation of the keypresses in some cases
    event.preventDefault();
    send((down ? "char: " : "CHAR: ") + getKeyIdentifier(event));
  }
}

function setUp(){
  conState = document.getElementById("conState");

  if(getCookie("keyboard") != "") [].slice.call(document.getElementsByName("source")).filter(function(e){return e.value == getCookie("keyboard")})[0].click();
  socket = new WebSocket("ws://tutorremote.madge.me:9160/", "tutorremote");
  socket.onopen = function (event) {
    send("INPUTINIT");
    conState.className = "ready";
    conState.innerHTML = "Connected! Just start typing";
  };
  socket.onerror = function (error) {
    conState.className = "error";
    conState.innerHTML = "Error... Connection failed";
  };
  socket.onclose = function (event) {
    conState.className = "closed";
    conState.innerHTML = "Connection closed";
  };
  socket.onmessage = function (event) {
    var ctrl = event.data.split(": ")
    switch(ctrl[0]){
      case "TOKN": updateToken(ctrl[1]); break;
      default: break;
    }
    console.log(event.data);
  };
}

function updateKeyboard(event){
  keyboard = event.target.value;
  setCookie("keyboard", keyboard, 365)
}

function close(){
  socket.close();
  console.log("end");
}

function updateToken(token){
  document.getElementById("tokenField").innerHTML = token;
}

function sendText(){
  send("TEXT: " + document.getElementById("textinput").value);
  document.getElementById("textinput").value = "";
}

function sendReset(){
  send("RESETMOD");
}

function getKeyIdentifier(event){
  if(keyboard == "ansi"){
    if (event.shiftKey) {
      switch (event.keyIdentifier) {
        case "U+0021": return "U+0031"; break;
        case "U+0040": return "U+0032"; break;
        case "U+0023": return "U+0033"; break;
        case "U+0024": return "U+0034"; break;
        case "U+0025": return "U+0035"; break;
        case "U+005E": return "U+0036"; break;
        case "U+0026": return "U+0037"; break;
        case "U+002A": return "U+0038"; break;
        case "U+0028": return "U+0039"; break;
        case "U+0029": return "U+0030"; break;
        case "U+005F": return "U+002D"; break;
        case "U+002B": return "U+003D"; break;
        case "U+007B": return "U+005B"; break;
        case "U+007D": return "U+005D"; break;
        case "U+003A": return "U+003B"; break;
        case "U+0022": return "U+0027"; break;
        case "U+007C": return "U+005C"; break;
        case "U+007E": return "U+0060"; break;
        case "U+003C": return "U+002C"; break;
        case "U+003E": return "U+002E"; break;
        case "U+003F": return "U+002F"; break;
        default: return event.keyIdentifier;
      }
    }
    else { return event.keyIdentifier; }
  }
  if(keyboard == "german"){
    if (event.shiftKey) {
      switch (event.keyIdentifier) {
        case "U+0021": return "U+0031"; break;
        case "U+0022": return "U+0032"; break;
        case "U+00A7": return "U+0033"; break;
        case "U+0024": return "U+0034"; break;
        case "U+0025": return "U+0035"; break;
        case "U+0026": return "U+0036"; break;
        case "U+002F": return "U+0037"; break;
        case "U+0028": return "U+0038"; break;
        case "U+0029": return "U+0039"; break;
        case "U+003D": return "U+0030"; break;
        case "U+003F": return "U+002D"; break;
        case "U+002B": return "U+003D"; break;
        case "U+005A": return "U+0059"; break;
        case "U+0059": return "U+005A"; break;
        case "U+00DC": return "U+005B"; break;
        case "U+002A": return "U+005D"; break;
        case "U+00D6": return "U+003B"; break;
        case "U+00C4": return "U+0027"; break;
        case "U+0027": return "U+005C"; break;
        case "U+003E": return "U+0060"; break;
        case "U+003B": return "U+002C"; break;
        case "U+003A": return "U+002E"; break;
        case "U+005F": return "U+002F"; break;
        case "Unidentified": switch (event.which) {
          case 187: return "U+003D";
          default: return event.keyIdentifier;
        } break;
        default: return event.keyIdentifier;
      }
    }
    else{
      switch (event.keyIdentifier) {
        case "U+00DF": return "U+002D"; break;
        case "U+005A": return "U+0059"; break;
        case "U+0059": return "U+005A"; break;
        case "U+003C": return "U+0060"; break;
        case "U+002D": return "U+002F"; break;
        case "U+00FC": return "U+005B"; break;
        case "U+002B": return "U+005D"; break;
        case "U+00F6": return "U+003B"; break;
        case "U+00E4": return "U+0027"; break;
        case "U+0023": return "U+005C"; break;
        case "Unidentified": switch (event.which) {
          case 187: return "U+003D";
          default: return event.keyIdentifier;
        } break;
        default: return event.keyIdentifier;
      }
    }
  }
  else {
    return event.keyIdentifier;
  }
}

function setCookie(cname, cvalue, exdays) {
  var d = new Date();
  d.setTime(d.getTime() + (exdays*24*60*60*1000));
  var expires = "expires="+d.toUTCString();
  document.cookie = cname + "=" + cvalue + "; " + expires;
}

function getCookie(cname) {
  var name = cname + "=";
  var ca = document.cookie.split(';');
  for(var i=0; i<ca.length; i++) {
      var c = ca[i];
      while (c.charAt(0)==' ') c = c.substring(1);
      if (c.indexOf(name) == 0) return c.substring(name.length, c.length);
  }
  return "";
}
