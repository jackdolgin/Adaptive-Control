let screenSizePromptCount = 0;

// function screenSizeIsOk(){
//   // attempts to check window width and height, using first base JS then jquery.
//   // if both fail, returns TRUE
//   let w, h, minWidth = 800, midHeight = 600;
//   try {
//     // base javascript
//     w = window.innerWidth;
//     h = window.innerHeight;
//     if (w == null | h == null) {throw "window.innerWidth/innerHeight failed.";}
//   } catch (err) {
//     try {
//       // jquery
//       w = $(window).width();
//       h = $(window).height();
//       if (w == null | h == null) {throw "$(window).width/height failed.";}
//     } catch (err2) {
//       // failure mode, returns true if both screen checks failed
//       return true;
//     }
//   }
//   // return dimension check if values are defined
//   return w >= minWidth && h >= midHeight;
// }

function checkFullScreen(){
    if (screenfull.isFullscreen){
        checkAudio("Connection");
    } else {
        
        instrOnOff("On", "warning");
    
        const startingText = "You are not in full screen mode. ";
        
        // allows a few warnings before terminating experiment
        if (screenSizePromptCount < numScreenSizeWarnings) {
            screenSizePromptCount++;
            
            // display screen size prompt
            document.querySelector("#warning").innerHTML =
                "<p>" + startingText +
                "If this issue persists, you will not be able to complete the study and will not be paid for your previous time. Please click the button below to return to full screen and continue.</p>" +
                "<button id='removeWarning' class='button-beige' >Full Screen</button>";
            
            document.querySelector("#removeWarning").addEventListener("click", function() {
                if (screenfull.isEnabled) screenfull.request();
                instrOnOff("Off", "warning");
                checkAudio("Connection");
            });
        } else {
            document.querySelector("#warning").innerHTML =
                startingText + "Since this has now happened " + (screenSizePromptCount + 1) + " times, you will not be able to complete the rest of the study. You can reach out to XXXX@gmail.com for any questions or concerns.";
        }
      
        // end_trial("early"); reinstantiate this
    }
}


// async function checkAudioConnection() {
//     if (! await (audioConnected())) { // note to self- need to load DetectRTC.hasMicrophone in task.html

//         if (screenfull.isEnabled) screenfull.exit();

//         // display screen size prompt
//         document.querySelector("#warning").innerHTML =
//             "<p>It appears that your computer is not currently connected to an audio input (you may have disconnected a headphone or microphone cord). When you reconnect the audio input, click the button below to proceed with the experiment.</p><button id='removeWarning' class='button-beige' >Continue</button>";
                
//         instrOnOff("On", "warning");
        
//         document.querySelector("#removeWarning").addEventListener("click", function() {
//             if (await (audioConnected())) {
//                 if (screenfull.isEnabled) screenfull.request();
//                 instrOnOff("Off", "warning");
//                 pre_fixate();
//             }
//         });
//     }
// }


// function audioConnected() {
//     return new Promise((resolve) => {
//         DetectRTC.load(function() {
//             resolve(DetectRTC.hasMicrophone);
//         });
//     });
// }


function helperfunc(abool) {
    return new Promise((resolve) => {
        DetectRTC.load(function() {
            resolve(eval(abool));
        });
    });
}

function proceed(prevfunc) {
    if (prevfunc === 'Connection') {
        checkAudio("Permission", "");
    } else {
        pre_fixate();
    }
}

async function checkAudio(x) {
    let checks = {
        'Connection': ["DetectRTC.hasMicrophone", "<p>It appears that your computer is not currently connected to an audio input (you may have disconnected a headphone or microphone cord). When you reconnect the audio input, click the button below to proceed with the experiment.</p><button id='removeWarning' class='button-beige' >Continue Experiment</button>"],
        'Permission': ["DetectRTC.isWebsiteHasMicrophonePermissions", "<p>It appears microphone access has been blocked for this website. Please go to <a href='chrome://settings/content/siteDetails?site=https%3A%2F%2Fcdmlab.a2hosted.com%2F' target='_blank' >your browser's settings</a>, allow microphone permissions, and then click this button below to proceed with the experiment.</p><button id='removeWarning' class='button-beige' >Continue Experiment</button>"]
    }
    let check = checks[x][0];
    
    if (await (helperfunc(check))){
        proceed(x);
    } else {
        
        document.querySelector("#warning").innerHTML = checks[x][1];
        
        if (screenfull.isEnabled) screenfull.exit();
            
        instrOnOff("On", "warning");
        
        document.querySelector("#removeWarning").addEventListener("click", async function() {
            if (await (helperfunc(check))) {
                if (screenfull.isEnabled) screenfull.request();
                instrOnOff("Off", "warning");
                proceed(x)
            }
        });
    }
}


// "<button id='exitWarning' class='button-beige' >Full Screen</button>"
// <button id='yes-consent-btn' class='consent-btn' style="display: inline; visibility: visible"><b>I agree to participate</b></button>;
        