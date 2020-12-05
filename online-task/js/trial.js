let trialCounter = 0; // tracks current trial
const logTime = x => trialArray[trialCounter][x] = window.performance.now();
const trialBlock = a => trialArray[trialCounter - a].Block;

function pre_fixate(t=trialDuration["pre_fixate"]) {
    if (trialCounter < pracTrials) {
        $("#countDisplay").html((trialCounter + 1) + " / " + pracTrials + " practice trials");
    } else {
        $("#countDisplay").html((trialCounter + 1 - pracTrials) + " / " + (trialArray.length - pracTrials) + " trials");
    }
    
    logTime('Trial_Start');
    eventTimer.setTimeout(fixate, t);
}

function fixate() {
    // save time when the trial started
    logTime('Fixation_Onset');
    
    // display fixation
    document.querySelector("#stimText").innerHTML = "+";
    targetSide(0);
    document.querySelector("#stimText").style.visibility = "visible";
    
    eventTimer.setTimeout(post_fixate, trialDuration["fixate"]);
}

function post_fixate() {
    logTime('Fixation_Offset');
    
    // blank screen after fixation
    document.querySelector("#stimText").innerHTML = "";

    eventTimer.setTimeout(stimulus, trialDuration["post_fixate"]);
}

function stimulus() {
    logTime('Stimulus_Onset');
    
    document.querySelector("#stimText").innerHTML =
        trialArray[trialCounter].Label;

    let imgdiv = document.querySelector("#stimImage"); // insert trial stimulus image into div
    imgdiv.innerHTML =
        "<img src='" + trialArray[trialCounter].Image_Path + "'/>";
    
    targetSide(trialArray[trialCounter].Xpos);

    imgdiv.style.visibility = "visible";


    eventTimer.setTimeout(end_trial, trialDuration["stimulus"]);  // this line is temporary
}

function end_trial() {
    
    logTime('Stimulus_Offset');
    
    // blank screen before next trial's fixation
    for (const x of ["stimText", "stimImage"]){
        document.querySelector("#" + x).style.visibility = "hidden";
    }
    
    // increase trial counter
    trialCounter++;

    // if there are no more trials end experiment
    if (trialCounter + 1 === trialArray.length) {
        end_exp("on time");
    } else if (trialCounter === pracTrials) {
        instrOnOff("On", "instructionDisplay");
        displayInstructions(9, 10, pre_fixate, [2000,]);
    } else if (trialBlock(0) > trialBlock(1)) {
        displayInstructions(11, 12, failLoop, [displayInstructions, [13, 13, pre_fixate, [2000,]]]);
    } else {
        checkFullScreen();
    }
}

function end_exp(x) {
    
    stopAndSubmitAudio();
    // typically you would submit the data through php which would automatically trigger the feedback html
    submit_data();

    // if (x === "on time") {
    //     // but since the php won't post properly without a server I'll just trigger the html
    //      window.open("feedback-letter.html", "_self");
    // }
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
