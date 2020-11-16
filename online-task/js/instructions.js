const instrOnOff = function(x) {

    let avar = (x === "On") ? 0 : 1;
    let nonavar = 1 - avar
    anarray = [["#instructionDisplay", "initHidden"], ["#top", "flexCenter"]]

    $(anarray[avar][0]).hide();
    // $(".countDisplay").html(trialCount + " / " + trialArray.length + " trials");
    $(anarray[nonavar][0]).show();
    $("#targetDisplay").removeClass(anarray[avar][1]);
    $("#targetDisplay").addClass(anarray[nonavar][1]);
}

const displayInstructions = function() {    // create content display within the main-display div

    // Set up initial display & button functions
    // pagination.setup();
    pagination.setup(["pg1","pg2","pg3"]);

    // call instruction function
    instructions_pg1();
}

const instructions_pg1 = function () {
    $( "#pg1" ).load( "instructions/pg1.html" );

    instructions_pg2();
}

const instructions_pg2 = function () {
    $( "#pg2" ).load( "instructions/pg2.html" );

    instructions_pg3();
}

const instructions_pg3 = function () {
    $( "#pg3" ).load( "instructions/pg3.html" );

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

finishedLoading = async function() {

    instrOnOff("On")
    // await fail_loop();

    // console.log("failCount = " + failCount)

}

// const instructions_pg4 = function () {
//     $( "#pg4" ).load( "/online-task/instructions/pg4.html" );
// }
