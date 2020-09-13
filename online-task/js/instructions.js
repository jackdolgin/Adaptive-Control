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

    // Set up initial display & button functions
    pagination.setup();

    // call instruction function
    instructions_pg1();
}

const instructions_pg1 = function () {
    $( "#pg1" ).load( "/online-task/instructions/pg1.html" );

    instructions_pg2();
}

const instructions_pg2 = function () {
    $( "#pg2" ).load( "/online-task/instructions/pg2.html" );
    console.log("bleh")
    instructions_pg3();
}

const instructions_pg3 = function () {
    $( "#pg3" ).load( "/online-task/instructions/pg3.html" );

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
