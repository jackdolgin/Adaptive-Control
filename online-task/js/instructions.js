function instrOnOff(x, y) {

    let avar = (x === "Off") ? 0 : 1;
    let nonavar = 1 - avar;
    anarray = [["#" + y, "initHidden"], ["#top", "flexCenter"]];

    $(anarray[avar][0]).hide();
    // $(".countDisplay").html(trialCount + " / " + trialArray.length + " trials");
    $(anarray[nonavar][0]).show();
    $("#targetDisplay").removeClass(anarray[avar][1]);
    $("#targetDisplay").addClass(anarray[nonavar][1]);
}

function displayInstructions() {    // create content display within the main-display div

    // let noop = function () {};
    // navigator.getUserMedia = (navigator.getUserMedia || navigator.webkitGetUserMedia || navigator.mozGetUserMedia || navigator.msGetUserMedia);
    // function requestMicrophone() {
    //     navigator.getUserMedia({audio: true}, noop, noop)
    //   }
    // requestMicrophone()

    const numOfInstructions = 5;
    const instrPageArray = [...Array(numOfInstructions)];
    
    for (x of _.range(1, numOfInstructions + 1)) {
        
        instrPageArray[x - 1] = "pg" + x;
        
        $( "#pg" + x ).load( "instructions/pg" + x + ".html" );
        
        if (x === numOfInstructions) proceedToTask();
    }
    
    // Set up initial display & button functions
    pagination.setup(instrPageArray);
    // eventTimer.setTimeout(navigator.mediaDevices.getUserMedia({ audio: true }),
    //                       2000);
    // $("#pageDown").hide();
    $("#pageUp").hide();
    setTimeout(function(){
        navigator.mediaDevices.getUserMedia({ audio: true }); }, 1200);

}

function proceedToTask(){
    preLoad.loadImages("#loading", "#progress", function() {
        finishedLoading()
        $("#loading").hide();
    });

    $("#beginExp").click(function () {

        if (preLoad.manualCheck()) {
            finishedLoading();
        } else {
            $("#instructionDisplay").css("display", "none");
            $("#loading").show();
        }
    });
}

async function finishedLoading() {

    instrOnOff("Off", "instructionDisplay");
    // await fail_loop();
    
    pre_fixate(); // this line is temporary

}

