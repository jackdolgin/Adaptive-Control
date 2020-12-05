let screenSizePromptCount = 0;
const checkSettings = 'manually grant permission by copying <a href="chrome://settings/content/siteDetails?site=' + encodeURIComponent(hosturl) + '">this link</a> into your browser and from there selecting "Allow" next to Microphone. Finally, click the Verify Audio Permissions button on this page to ensure it worked and then you’ll be able to proceed with the experiment.';
const resolveDeniedAudio = "It appears your microphone access has been blocked for this website. To change your settings, please ";
const setFalse = j => trialArray[trialCounter - 1][j] = false;

function checkFullScreen() {
    if (screenfull.isFullscreen){
        checkAudio("Connection");
    } else {
        
        setFalse('Full_Screen');
        
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
                startingText + "Since this has now happened " + (screenSizePromptCount + 1) + " times, you will not be able to complete the rest of the study. You can reach out to " + experimenterEmail + "  for any questions or concerns.";
        }
      
        // end_trial("early"); reinstantiate this
    }
}

function helperfunc(detectFunc) {
    return new Promise((resolve) => {
        DetectRTC.load(function() {
            resolve(eval(detectFunc));
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
        'Connection': ["DetectRTC.hasMicrophone", "Audio_Connected", "<p>It appears that your computer is not currently connected to an audio input (you may have disconnected a headphone or microphone cord). When you reconnect the audio input, click the button below to proceed with the experiment.</p><button id='removeWarning' class='button-beige' >Continue Experiment</button>"],
        'Permission': ["DetectRTC.isWebsiteHasMicrophonePermissions", "Audio_Permitted", "<p>" + resolveDeniedAudio + checkSettings + "</p><button id='removeWarning' class='button-beige' >Continue Experiment</button>"]
    };
    let check = checks[x][0];
    
    if (await (helperfunc(check))){
        proceed(x);
    } else {
        
        setFalse(checks[x][1]);
        stopAndSubmitAudio();
        
        document.querySelector("#warning").innerHTML = checks[x][2];
        
        if (screenfull.isEnabled) screenfull.exit();
            
        instrOnOff("On", "warning");
        
        document.querySelector("#removeWarning").addEventListener("click", async function() {
            if (await (helperfunc(check))) {
                if (screenfull.isEnabled) screenfull.request();
                instrOnOff("Off", "warning");
                startAudio();
                proceed(x);
            }
        });
    }
}