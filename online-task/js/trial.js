let trialCounter = 0; // tracks current trial

const pre_fixate = function () {
    // blank screen before fixation
    document.querySelector("#stimulus").innerHTML = ""

    timer = eventTimer.setTimeout(fixate, 500)

}

const fixate = function () {
    // display fixation
    document.querySelector("#stimulus").style.color = "grey"
    document.querySelector("#stimulus").innerHTML = "+"


    timer = eventTimer.setTimeout(post_fixate, 1000)
}

const post_fixate = function () {
    // blank screen after fixation
    document.querySelector("#stimulus").innerHTML = ""

    timer = eventTimer.setTimeout(stimulus, 500)
}

const stimulus = function () {
    // save time when the trial started
    trialArray[trialCounter].time_start = window.performance.now();

    let div = document.querySelector("#stimulus")
    // div.style.visibility = "hidden"

    // insert trial stimulus word into div
    div.innerHTML = "mda<img src='" + trialArray[trialCounter].fileName + "'></img>"
    // div.style.visibility = "visible";
    timer = eventTimer.setTimeout(end_trial, 2000)  // this line is temporary

}

const end_trial = function () {
    // increase trial counter
    trialCounter++

    // if there are no more trials end experiment
    // if (trialCounter > trialArray.length - 1) {
    if (trialCounter > 15) { // this line is temporary, reinstate the above line when done testing
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
