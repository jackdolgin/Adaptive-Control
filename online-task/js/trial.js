let trialCounter = 0; // tracks current trial

const pre_fixate = function () {
    // blank screen before fixation
    document.querySelector("#stimText").innerHTML = "";

    timer = eventTimer.setTimeout(fixate, 500);
}

const fixate = function () {
    // display fixation
    document.querySelector("#stimText").innerHTML = "+";

    timer = eventTimer.setTimeout(post_fixate, 1000);
}

const post_fixate = function () {
    // blank screen after fixation
    document.querySelector("#stimText").innerHTML = "";

    timer = eventTimer.setTimeout(stimulus, 500);
}

const stimulus = function () {
    // save time when the trial started
    trialArray[trialCounter].time_start = window.performance.now();

    document.querySelector("#stimText").innerHTML =
        trialArray[trialCounter].Label;

    let imgdiv = document.querySelector("#stimImage"); // insert trial stimulus image into div
    imgdiv.innerHTML =
        "<img src='" + trialArray[trialCounter].fileName + "'></img>";

    imgdiv.style.visibility = "visible";


    timer = eventTimer.setTimeout(end_trial, 2000);  // this line is temporary
}

const end_trial = function () {
    document.querySelector("#stimImage").style.visibility = "hidden";

    // increase trial counter
    trialCounter++

    // if there are no more trials end experiment
    // if (trialCounter > trialArray.length - 1) {
    if (trialCounter > 125) { // this line is temporary, reinstate the above line when done testing
        end_exp();
        return
    }

    // else cue next trial
    pre_fixate();
}


const end_exp = function (){
    // typically you would submit the data through php which would automatically trigger the feedback html
    //submit_data();

    // but since the php won't post properly without a server I'll just trigger the html
     window.open("feedback-letter.html", "_self");
}

///////////////////////////////////////////////////////////////////////////////////////////
