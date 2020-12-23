(async () => {
    
    const localSettings = await new Promise(
    (resolve) => {
        DetectRTC.load(function() {
            resolve(
                [DetectRTC.isMobileDevice,
                DetectRTC.browser.isChrome,
                DetectRTC.browser.version >= 64,
                DetectRTC.hasMicrophone]);
        });
    });
     
    if (localSettings[0]) {
        rejectPreConsent("<p>A computer is required for this experiment (as opposed to a smartphone or tablet).</p>");
    } else if (!localSettings[1]) {
        rejectPreConsent("<p>This experiment can only be run in <a href='https://www.google.com/chrome'>Google Chrome</a>. Please switch browsers to participate.</p>");
    } else if (!localSettings[2]) {
        rejectPreConsent("<p>This experiment can only be run in Chrome versions 64 or greater. Please <a href='https://support.google.com/chrome/answer/95414'>update your browser</a> to participate.</p>");
    } else if (!localSettings[3]) {
        rejectPreConsent("<p>It appears that your computer is not currently connected to an audio input. We’ll need to record your voice during the task to pick up your answers, so it is required that you have a high-quality audio input into your computer (native to some laptops, for others may require an external microphone or headphones with microphone capabilities).</p>");
    } else {
        
        document.getElementById("workerID").style.cssText = "display: block; visibility: visible;";
        document.getElementById("workerID-btn").style.cssText = "display: block; visibility: visible;";
        
        document.getElementById("workerID-btn").addEventListener("click", function() {
            let workerID = document.getElementById("fname").value;
            if (workerID.length > 3){
                sessionStorage.setItem("IDofwrkr", workerID);
                
                document.getElementById("workerID").style.cssText = "display: none; visibility: invisible;";
                
                document.getElementById("info-consent-letter").style.cssText = "display: inline-block; visibility: visible;";
        
                document.getElementById("no-consent-btn").addEventListener("click",
                    function() {
                        rejectPreConsent("<p>You have declined to participate at this time.</p><p>If you change your mind, simply reload this webpage.</p>")
                });
        
                document.getElementById("yes-consent-btn").addEventListener("click", function() {
                    localStorage.setItem("consented", "yes"); // so that one needs to go through the consent form page to access the experiment (as opposed to going directly to the experiment url, which is diff than the consent form's url)
                    window.open("online-task/task.html?" + window.location.search.slice(1) , "_self");
                });                
                
            } else {
                document.getElementById("plz-answer").style.cssText += "display: inline-block; visibility: visible;";
            }
        })
    }
    
})();

    
function rejectPreConsent(text){
    document.querySelector("#cancel").innerHTML = text;
    
    const els = ["info-consent-letter", "no-consent-btn", "yes-consent-btn"];
    els.forEach((element) =>
        document.querySelector("#" + element).style.display = "none");
}