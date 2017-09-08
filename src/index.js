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
  console.log("google", gapi);
  console.log(">>>",msg,payload,cmd); 

  switch (cmd) {
		case "init":
      console.log("i")
      gapi.load('client:auth2', initClient);
			break;

    case "auth":
      gapi.auth2.getAuthInstance().signIn().then(function (res) {

        console.log("signin res", res);
      });
      break;

    case "isSignedIn":
      gapi.auth2.getAuthInstance().isSignedIn.get()

    case "getCalendars":
      break;
    
    case "addCalendar":
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
      var status = gapi.auth2.getAuthInstance().isSignedIn.get()
      // update signin status
      app.ports.fromJs.send(["signinStatus- " + status, "init"]);
		});
  }

// Next step is API call / res via port
// figure out how to load the api here
// port quickstart.html to elm / js wraps
