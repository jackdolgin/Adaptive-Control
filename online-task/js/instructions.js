// this function is inspired by https://github.com/nbrosowsky/online-psychology-demos/blob/master/face-inversion/js/pagination.js
function displayInstructions(firstPage, lastPage, nextFunc, nextParams=[]) {    // create content display within the main-display div
    let currentPage = 0;
    instrOnOff("On", "instructionDisplay");
    
    ['pageUp', 'pageDown', 'beginExp'].forEach((button) => $('#' + button).off('click'));
    const nextPages = Array(lastPage - firstPage + 1).fill(firstPage).map((x, y) =>
        'pg' + (x + y));

    nextPages.forEach((element) => {
        $( "#" + element ).load( "instructions/" + element + ".html" );
    })

    for (let [i, value] of nextPages.entries()) {
        value = "#" + value;
        if (i === 0) {
            $(value).show();
            $('#pageDown').hide();
            if (firstPage === lastPage) {
                $('#beginExp').show();
                $('#pageUp').hide();
            } else {
                $('#beginExp').hide();
                $('#pageUp').show();                
            }
        } else {
            $(value).hide();
        }
    }
    
    $("#pageUp").click(function() {
        $("#pageDown").show();
        $("#" + nextPages[currentPage]).hide();
        currentPage++;
        $("#" + nextPages[currentPage]).show();
        if (currentPage + firstPage === lastPage) {
            $("#pageUp").hide();
            $("#beginExp").show();
        }
    });
    
    $("#pageDown").click(function() {
        $("#" + nextPages[currentPage]).hide();
        currentPage--;
        $("#" + nextPages[currentPage]).show();
        $("#pageUp").show();
        $("#beginExp").hide();
        if (currentPage === 0) {
          $("#pageDown").hide();
        }
    });
    
    $("#beginExp").click(function() {
        $("#" + nextPages[currentPage]).hide();
        ['pageDown', 'beginExp'].forEach((button) => $(button).hide());
        instrOnOff("Off", "instructionDisplay");
    });
    
    $("#beginExp").click(function() {
        nextFunc.apply(this, nextParams);
    });


}

function proceedToTask(){
        if (preLoad.manualCheck()) {
            finishedLoading();
        } else {
            $("#instructionDisplay").css("display", "none");
            $("#loading").show();
        }
}

function finishedLoading() {
    failLoop(displayInstructions, [4, 8, pre_fixate, [2000,]]);
}

function instrOnOff(x, y) {

    let avar = (x === "Off") ? 0 : 1;
    let nonavar = 1 - avar;
    let anarray = [["#" + y, "initHidden"], ["#top", "flexCenter"]];

    $(anarray[avar][0]).hide();
    $(anarray[nonavar][0]).show();
    $("#targetDisplay").removeClass(anarray[avar][1]);
    $("#targetDisplay").addClass(anarray[nonavar][1]);
    
}
