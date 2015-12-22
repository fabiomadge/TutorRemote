var server = "ws://_server_:9160/";

var socket;
var conState;
var dest = "";

var lastReq = 0;

var sCharMap = {};
sCharMap["8"] = "Bksp";
sCharMap["9"] = "Tab";
sCharMap["13"] = "Enter";
sCharMap["27"] = "Esc";
sCharMap["33"] = "PgUp";
sCharMap["34"] = "PgDn";
sCharMap["35"] = "End";
sCharMap["36"] = "Home";
sCharMap["37"] = "Left";
sCharMap["38"] = "Up";
sCharMap["39"] = "Right";
sCharMap["40"] = "Down";
sCharMap["45"] = "Ins";
sCharMap["46"] = "Del";


function isCombo(event){
	return (event.ctrlKey && !event.altKey) || (!event.ctrlKey && event.altKey) ||
		(
			(event.charCode == undefined || event.charCode == 0) &&
			(event.key == undefined || event.key.length > 1) &&
			(""+event.keyCode) in sCharMap
		);
}
function sendKey(event){
	if(event.target.id == "textinput" || event.target.id == "outputT")
		return;
	
	//document.getElementById("textinput").value += "\nKey: "+isCombo(event)+" "+event.keyCode+"  "+event.charCode+"  "+event.key+"  "+event.keyIdentifier;
	
	if(isCombo(event)) // goes to sendCombo anyway
		return;
	
	//prevent browser interpretation of the keypresses in some cases
	event.preventDefault();
	
	var kString = "";
	var tString = "";
	if(event.key != undefined){
		tString = "key";
		if(event.key.length == 1){
			var ch = event.key.charCodeAt(0).toString(16);
			kString = ("U+" + "0000".substring(ch.length) + ch).toUpperCase();
		}
		else{
			return; // yes, that's a single-Key-Combo :(
		}
	}else if(event.charCode != undefined && event.charCode != 0){
		tString = "charCode";
		var ch = event.charCode.toString(16);
		kString = ("U+" + "0000".substring(ch.length) + ch).toUpperCase();
	}else{
		console.log("Could not get Key Type");
		return;
	}
	console.log("Key: "+tString+": "+kString);
	send("CHAR: " + kString);
}
function sendCombo(event){
	if(event.target.id == "textinput" || event.target.id == "outputT")
		return;
	
	//document.getElementById("textinput").value += "\nCmb: "+isCombo(event)+" "+event.keyCode+"  "+event.charCode+"  "+event.key+"  "+event.keyIdentifier;
	
	if(!isCombo(event)) // goes to sendKey anyway
		return;
	
	//prevent browser interpretation of the keypresses in some cases
	event.preventDefault();
	
	var kString = "";
	var tString = "";
	if(""+event.keyCode in sCharMap){
		tString = "map";
		kString = sCharMap[""+event.keyCode];
	}else if(event.ctrlKey || event.metaKey){
		tString = "ctrl";
		switch(String.fromCharCode(event.keyCode)){
			case 'C': kString = 'Copy'; break;
			case 'X': kString = 'Cut'; break;
			case 'V': kString = 'Paste'; break;
			case 'Z': kString = 'Back'; break;
			case 'Y': kString = 'Forward'; break;
			case 'A': kString = 'All'; break;
			case 'N': kString = 'New'; break;
			default: return;
		}
	}else if(event.altKey){
		tString = "alt";
		switch(String.fromCharCode(event.keyCode)){
			default: return;
		}
	}else{
		console.log("Could not get Key Type");
		return;
	}
	var mod = (event.ctrlKey?"[CTRL]":"") + (event.shiftKey?"[SHFT]":"");
	
	console.log("Combo: "+tString+": "+mod+kString);
	send("CHAR: " + mod + " " + kString);
}
function sendFallback(){
	if(document.getElementById("keyboardFetcher").value == "")
		return;
	//document.getElementById("textinput").value += "\nFallback!";
	send("TEXT: " + document.getElementById("keyboardFetcher").value);
	document.getElementById("keyboardFetcher").value = "";
}
function sendCursor(cur){
	send("CHAR: " + sCharMap[""+(37+cur)]);
}
function sendBksp(cur){
	send("CHAR: " + sCharMap["8"]);
}
function setUp(){
	conState = document.getElementById("conState");
	var timeout = null;

	socket = new WebSocket(server, "tutorremote");
	socket.onopen = function (event) {
		socket.send("INPUTINIT");
		conState.className = "searching";
		conState.innerHTML = "Connecting to Output Token";
		testToken();
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
		var ctrl = event.data.split(": ");
		switch(ctrl[1]){
			case "TOKN": updateToken(ctrl[2]); break;
			case "TICK":
				window.clearTimeout(timeout);
				addClass(document.getElementById('keyboardFetcher'), 'active');
				timeout = window.setTimeout(function(){
					conState.className = "ready";
					conState.innerHTML = "Token OK!";
					rmClass(document.getElementById('keyboardFetcher'), 'active');
				}, 2000)
				conState.className = "active";
				conState.innerHTML = "Active! You can type now!";
				rmClass(document.getElementById('outputT'), 'needed');
				break;
			case "DING":
				conState.className = "ready";
				conState.innerHTML = "Token OK";
				rmClass(document.getElementById('outputT'), 'needed');
				console.log(ctrl[2]);
				break;
			case "NOPE":
				conState.className = "searching";
				conState.innerHTML = "Connecting to Output Token";
				addClass(document.getElementById('outputT'), 'needed');
				console.log("NOPE: "+ctrl[2]);
				break;
			default: break;
		}
		//console.log(" ---------  " + event.data);
	};
}
function close(){
	socket.close();
	console.log("end");
}
function updateToken(token){
	document.getElementById("tokenField").innerHTML = token;
}

function testToken(){
	dest = document.getElementById('outputT').value.toUpperCase();
	send("TEST: Blablub");
	console.log("TEST");
	conState.className = "searching";
	conState.innerHTML = "Connecting to Output Token";
	addClass(document.getElementById('outputT'), 'needed');
}
function requestActivation(){
	var time = new Date().getTime();
	if(time - lastReq < 5000) return;
	lastReq = time;
	send("SNMA: Senpai Notice Me Already!");
	console.log("Senpai Notice Me Already!");
	conState.innerHTML = "Requesting Activation...";
}
function sendText(){
	send("TEXT: " + document.getElementById("textinput").value);
	console.log("TEXT");
	document.getElementById("textinput").value = "";
}
function sendReset(){
	send("RESETMOD");
	console.log("RESETMOD");
}
function send(msg){
	socket.send(dest+": "+msg);
	addClass(conState, "send");
	window.setTimeout(function(){rmClass(conState, 'send');}, 10);
}

function addClass(el, cl){
	if(el.className.indexOf(cl) >= 0) return;
	el.className += " "+cl;
}
function rmClass(el, cl){
	el.className = el.className.replace(cl, '').trim();
}

window.onload = setUp;
window.onunload = close;
