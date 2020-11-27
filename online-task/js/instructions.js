function displayInstructions(firstPage, lastPage, nextFunc) {    // create content display within the main-display div

    const numOfInstructions = lastPage;
    const instrPageArray = [...Array(numOfInstructions)];
    
    for (x of _.range(firstPage, numOfInstructions + firstPage)) {
        instrPageArray[x - 1] = "pg" + x;
        
        $( "#pg" + x ).load( "instructions/pg" + x + ".html" );
        if (x === numOfInstructions) nextFunc();
    }
    
    pagination.setup(instrPageArray);

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
    fail_loop([4, 8, pre_fixate], displayInstructions);
}

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
