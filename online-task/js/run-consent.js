(async () => {
    
    const localSettings = await new Promise(
    (resolve) => {
        DetectRTC.load(function() {
            resolve(
                [DetectRTC.isMobileDevice,
                DetectRTC.browser.isChrome,
                DetectRTC.hasMicrophone]);
        });
    });
     
    if (localSettings[0]){
        rejectPreConsent("<p>A computer is required for this experiment (as opposed to a smartphone or tablet).</p>");
    } else if (!localSettings[1]) {
        rejectPreConsent("<p>This experiment can only be run in <a href='https://www.google.com/chrome'>Google Chrome</a>. Please switch browsers to participate.</p>");
    } else if (!localSettings[2]) {
        rejectPreConsent("<p>It appears that your computer is not currently connected to an audio input. We’ll need to record your voice during the task to pick up your answers, so it is required that you have a high-quality audio input into your computer (native to some laptops, for others may require an external microphone or headphones with microphone capabilities).</p>")
    } else {
        document.querySelector("#info-consent-letter").style.display = "block";
        document.querySelector("#info-consent-letter").style.visibility = "visible";

        document.querySelector("#no-consent-btn").addEventListener("click",
            function() {
                rejectPreConsent("<p>You have declined to participate at this time.</p><p>If you change your mind, simply reload this webpage.</p>")
        });

        document.querySelector("#yes-consent-btn").addEventListener("click", function() {
            localStorage.setItem("consented", "yes"); // so that one needs to go through the consent form page to access the experiment (as opposed to going directly to the experiment url, which is diff than the consent form's url)
            window.open("online-task/task.html?" + window.location.search.slice(1) , "_self");
        });
    }
    
})();

    
function rejectPreConsent(text){
    document.querySelector("#cancel").innerHTML = text;
    
    document.querySelector("#cancel").style.display = "block";
    document.querySelector("#cancel").style.visibility = "visible";
    document.querySelector("#info-consent-letter").style.display = "none";
    document.querySelector("#no-consent-btn").style.display = "none";
    document.querySelector("#yes-consent-btn").style.display = "none";
}