import './main.css';
import { Main } from './Main.elm';


var app = Main.embed(document.getElementById('root'));
var CLIENT_ID = '787419036517-pqu3ga58d833sr5c81jgebkdre0q9t76.apps.googleusercontent.com';
var DISCOVERY_DOCS = ["https://www.googleapis.com/discovery/v1/apis/calendar/v3/rest"];
var SCOPES = "https://www.googleapis.com/auth/calendar";


/**
 * In Elm's Port world, message is a Tuple
 * In JS it will be [payload, cmd]
 **/
app.ports.fromElm.subscribe(function (msg) {
  var payload = msg[0];
  var cmd = msg[1];
  var debug = false;
  console.log("google", gapi);
  console.log(">>>",msg,"payload",payload,"cmd",cmd); 
  
  if (!gapi) return app.ports.fromJs.send(["Warning - request ignored. GAPI not present.", "noGapi"])

  if (debug) return ([["noop"], "noop"]);

  switch (cmd) {
		case "init":
      gapi.load('client:auth2', initClient);
			break;

    case "auth":
      gapi.auth2.getAuthInstance().signIn().then(function (res) {
        app.ports.fromJs.send([JSON.stringify(res), "auth"])
        console.log("signin res", res);
      });
      break;

    case "isSignedIn":
      var status = getSigninStatus();
      app.ports.fromJs.send(["" + status, "isSignedIn"]);
      break;
    
    case "signout":
      gapi.auth2.getAuthInstance().signOut();
      app.ports.fromJs.send(["Wait for status update", "signout"]);
      break;

    case "getCalendars":
        console.log("gc1")
        gapi.client.calendar.calendarList.list().then(function (res) {
          var items = res.result.items;
          console.log("calendarList", items)
          app.ports.fromJs.send([JSON.stringify(items), "getCalendars"]);   
        })
      break;
    
    case "eventsFromPrimaryCalendar":
      break;

    case "addCalendar":
      console.log('addCalendar', payload);
      addCalendar(payload[0])
      break;
    
    case 'addEvents':
      var calId = payload[0];
      var events = JSON.parse(payload[1]);
      addEvents(calId, events);
      break;
  }
})

/**
 * API 
 */
function initClient() {
    gapi.client.init({
			discoveryDocs: DISCOVERY_DOCS,
			clientId: CLIENT_ID,
			scope: SCOPES
		}).then(function () {
      var status = getSigninStatus();
      gapi.auth2.getAuthInstance().isSignedIn.listen(updateSigninStatus);
      app.ports.fromJs.send(["" + status, "updateSigninStatus"]);
		});
  }

function getSigninStatus() {
  return gapi.auth2.getAuthInstance().isSignedIn.get()
}

function updateSigninStatus(isSignedIn) {
  app.ports.fromJs.send(["" + isSignedIn, "updateSigninStatus"])
}

function addCalendar (calName) {
    console.log("gapi addCalendar", calName);
    gapi.client.calendar.calendars.insert({
      summary: calName
    }).then(function (res) {
		  console.log("gapi addCalendar res", res);	
			app.ports.fromJs.send([res.result.id, "addCalendar"]);
    })
}

function addEvents (calId, events) {
  console.log("addEvents", arguments);
  var batch = gapi.client.newBatch();
  for (var i = 0; i < events.length; i++) {
    var x = gapi.client.calendar.events.quickAdd({
      calendarId: calId,
      text: events[i].description
    });
    batch.add(x);
  }
  batch.execute(function (res) {
    console.log("addEvents res", res);
    // return id of inserted calendar
    app.ports.fromJs.send([JSON.stringify(res), "addEvents"]);
  })
}
