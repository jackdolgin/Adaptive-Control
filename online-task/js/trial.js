let trialCounter = 0; // tracks current trial


function pre_fixate() {
    // if (screenSizeIsOk()){
    //   fixate();
    // } else {
    //     promptScreenSize
    // }

    eventTimer.setTimeout(fixate, trialDuration["pre_fixate"]);
}

function fixate() {
    // display fixation
    document.querySelector("#stimText").innerHTML = "+";
    document.querySelector("#stimText").style.visibility = "visible";
    
    const recordingDuration = trialDuration["fixate"] + trialDuration["post_fixate"] + trialDuration["stimulus"];
    console.log('heap')
    recordAudio(recordingDuration);
    console.log("tbin")

    eventTimer.setTimeout(post_fixate, trialDuration["fixate"]);
}

function post_fixate() {
    // blank screen after fixation
    document.querySelector("#stimText").innerHTML = "";

    eventTimer.setTimeout(stimulus, trialDuration["post_fixate"]);
}

function stimulus() {
    // save time when the trial started
    trialArray[trialCounter].time_start = window.performance.now();

    document.querySelector("#stimText").innerHTML =
        trialArray[trialCounter].Label;

    let imgdiv = document.querySelector("#stimImage"); // insert trial stimulus image into div
    imgdiv.innerHTML =
        "<img src='" + trialArray[trialCounter].fileName + "'></img>";

    imgdiv.style.visibility = "visible";


    eventTimer.setTimeout(end_trial, trialDuration["stimulus"]);  // this line is temporary
}

function end_trial() {
    
    // blank screen before next trial's fixation
    for (x of ["stimText", "stimImage"]){
        document.querySelector("#" + x).style.visibility = "hidden";
    }
    
    // increase trial counter
    trialCounter++;

    // if there are no more trials end experiment
    // if (trialCounter > trialArray.length - 1) {
    if (trialCounter > 15) { // this line is temporary, reinstate the above line when done testing
        end_exp("on time");
    }

    checkFullScreen();
    
}


function end_exp(x) {
    // typically you would submit the data through php which would automatically trigger the feedback html
    //submit_data();

    if (x === "on time") {
        // but since the php won't post properly without a server I'll just trigger the html
         window.open("feedback-letter.html", "_self");
    }
}

///////////////////////////////////////////////////////////////////////////////////////////
