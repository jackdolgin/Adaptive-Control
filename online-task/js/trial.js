let trialCounter = 0; // tracks current trial
const trialBlock = a => trialArray[trialCounter - a].Block;

function pre_fixate(t=trialDuration["pre_fixate"]) {
    if (trialCounter < pracTrials) {
        $("#countDisplay").html((trialCounter + 1) + " / " + pracTrials + " practice trials");
    } else {
        $("#countDisplay").html((trialCounter + 1 - pracTrials) + " / " + (trialArray.length - pracTrials) + " trials");
    }
    
    eventTimer.setTimeout(fixate, t);
}

function fixate() {
    // display fixation
    document.querySelector("#stimText").innerHTML = "+";
    targetSide(0);
    document.querySelector("#stimText").style.visibility = "visible";
    
    const recordingDuration = trialDuration["fixate"] + trialDuration["post_fixate"] + trialDuration["stimulus"];
    recordAudio(recordingDuration);

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
        "<img src='" + trialArray[trialCounter].fileName + "'/>";
    
    targetSide(trialArray[trialCounter].Xpos);

    // if (task === "Predictive_Locations") {
    //     ["#stimText", "#stimImage"].forEach((element) =>
    //         $(element).css("float",
    //                         (trialArray[trialCounter].xPos > 0) ? "right": "left"));
    // }
    
    

    // if (task === "Predictive_Locations") {
    //     if (trialArray[trialCounter].xPos > 0) {
            
    //     }
    // }

    imgdiv.style.visibility = "visible";


    eventTimer.setTimeout(end_trial, trialDuration["stimulus"]);  // this line is temporary
}

function end_trial() {
    
    // blank screen before next trial's fixation
    for (const x of ["stimText", "stimImage"]){
        document.querySelector("#" + x).style.visibility = "hidden";
    }
    
    // increase trial counter
    trialCounter++;

    // if there are no more trials end experiment
    if (trialCounter + 1 === trialArray.length) {
    // if (trialCounter > 15) { // this line is temporary, reinstate the above line when done testing
        end_exp("on time");
    } else if (trialCounter === pracTrials) {
        console.log("sure");
        instrOnOff("On", "instructionDisplay");
        displayInstructions(9, 10, pre_fixate, [2000,]);
    } else if (trialBlock(0) > trialBlock(1)) {
        console.log("yep");
        displayInstructions(11, 12, failLoop, [displayInstructions, [13, 13, pre_fixate, [2000,]]]);
    } else {
        checkFullScreen();
    }
}

function end_exp(x) {
    // typically you would submit the data through php which would automatically trigger the feedback html
    //submit_data();

    if (x === "on time") {
        // but since the php won't post properly without a server I'll just trigger the html
         window.open("feedback-letter.html", "_self");
    }
}

function targetSide(x){
    ["#stimText", "#stimImage"].forEach((element) => {
        if (x === 0) {
            $(element).removeClass( "target-left target-right" )
        } else if (x > 0) {
            $(element).removeClass( "target-right" ).addClass( "target-left" );
        } else {
            $(element).removeClass( "target-left" ).addClass( "target-right" );   
        }
    });
} 

///////////////////////////////////////////////////////////////////////////////////////////
