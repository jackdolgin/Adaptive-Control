const finishedLoading = function() {
    $(".instructionDisplay").hide();
    // $(".countDisplay").html(trialCount + " / " + trialArray.length + " trials");
    $(".top").show();
    $(".targetDisplay").removeClass("initHidden");
    $(".targetDisplay").css("display", "flex");
    $(".targetDisplay").addClass("flexCenter");

    fixate();
}

const displayInstructions = function() {    // create content display within the main-display div

    // create content display within the mainDisplay div
    document.querySelector("#contentDisplay").innerHTML =
        `<div class="top initHidden" style="width: 100%">
            <h3 style="float: left; width: 16.66%" class="countDisplay">1 /27 trials</h3>
            <h3 style="float: left; width: 66.66%; text-align: center" id="reminder"></h3>
            <div style="width: 16.66%; clear: left"></div>
        </div>
        <div class="mainDisplay flexCenter noSelect ">
            <div id="loading" class="initHidden">
                <p> Please wait while the images are loading </p>
                <p id="progress">0%</p>
            </div>
            <div class="instructionDisplay book">
                <div id="pg1" class="page"></div>
                <div id="pg2" class="initHidden page"></div>
                <div id="pg3" class="initHidden page"></div>
                <div class="pgButtons">
                    <button id="pageDown" class="button-blue initHidden" style="float: left;">BACK</button>
                    <button id="pageUp" class="button-blue" style="float: right; margin-right: 4%">NEXT</button>
                    <button id="beginExp" class="button-green initHidden" style="float: right; margin-right: 4%">START</button>
                </div>
            </div>
            <div class="targetDisplay initHidden">
                <p id="stimulus" style="min-height: 82px;"></p>
            </div>
        </div>`

    // Set up initial display & button functions
    pagination.setup();

    // call instruction function
    instructions_pg1();
}

const instructions_pg1 = function () {
    document.querySelector("#pg1").innerHTML =
        `<h2>
            Welcome to the experiment!
        </h2>
        <p>
            Please enter your instructor's last name (if applicable):</p>

        <ol>
            <p>
                Instructor's name:
            </p>
            <input type="text" name="instructor" style="width: 60%">
        </ol>
        <br/>
        <p>
            First, we will review the instructions for the experiment. Feel free to use the "next" and "back" buttons to move through the instructions. Once you hit "start" however, you will not be able to go back.
        </p>
        <h3>1. Instructions:</h3>
        <p>
            There will be two parts to this experiment. The first will be a study phase. You will be shown images one at a time, each presented for 2 seconds. Your task for this phase is to try to memorize each of the images.
        </p>
        <p>`

    instructions_pg2();
}

const instructions_pg2 = function () {
    document.querySelector("#pg2").innerHTML = `kfafds`

    instructions_pg3();
}

const instructions_pg3 = function () {
    document.querySelector("#pg3").innerHTML = `opok`;

    preLoad.loadImages("#loading", "#progress", function() {
        finishedLoading()
        $("#loading").hide();
    });

    $("#beginExp").click(function () {

        if (preLoad.manualCheck()) {
            finishedLoading();
        } else {
            // $(".targetDisplay").css("display", "none");
            $(".instructionDisplay").css("display", "none");
            $("#loading").show();
        }
    });
}

// const startTask = function () {
//     conditionAssignment()
//
//
//     // /* end instructions and begin experiment */
//     // $("#beginExp").click(function () {
//
// }
