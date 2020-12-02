// clickCounter = 0;

function displayInstructions(firstPage, lastPage, nextFunc, nextParams=[]) {    // create content display within the main-display div
    console.log("nextParams");
    console.log(nextParams);
    let currentPage = 0;
    console.log(firstPage);
    instrOnOff("On", "instructionDisplay");
    // pagination.firstPage = firstPage;
    // pagination.lastPage = lastPage;
    // console.log(pagination);
    
    // const numOfInstructions = lastPage - firstPage + 1;
    // const instrPageArray = [...Array(numOfInstructions)];
    
    ['pageUp', 'pageDown', 'beginExp'].forEach((button) => $('#' + button).off('click'));
    // $('pageUp').off('click');
    // $('pageDown').off('click');
    console.log("lastPage = " + lastPage);
    console.log("firstPage = " + firstPage);
    const nextPages = Array(lastPage - firstPage + 1).fill(firstPage).map((x, y) =>
        'pg' + (x + y));
        
//     console.log("noodle");
//     console.log(nextPages);
//     // pagination.id = nextPages;

    nextPages.forEach((element) => {
        $( "#" + element ).load( "instructions/" + element + ".html" );
    })
    console.log(nextPages.entries())

    // nextPages.forEach((element) => {
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
            // ['#pageUp', value].forEach((element) => $(element).show());
            // $('#pageUp').show();
            // console.log("value = " + value);
            // $(value).show();
            // ['pageDown', 'beginExp'].forEach((button) => $("#" + button).hide());
        } else {
            console.log("hmmm")
            $(value).hide();
        }
    }
    
    // $('pageUp').off('click');
    // $('pageDown').off('click');
    
    $("#pageUp").click(function() {
        console.log("currentPage = " + currentPage);
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
        console.log("blue");
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
        console.log("nextParams");
        console.log(nextParams);
        nextFunc.apply(this, nextParams);
    });
    //     ['pageDown', 'beginExp'].forEach((button) => $(button).hide());
    //     instrOnOff("Off", "instructionDisplay");
    // });
    //     console.log("blue");
    //     $("#" + nextPages[currentPage]).hide();
    //     currentPage--;
    //     $("#" + nextPages[currentPage]).show();
    //     $("#pageUp").show();
    //     $("#beginExp").hide();
    //     if (currentPage === 0) {
    //       $("#pageDown").hide();
    //     }
    // });

    //     $('pageUp').off('click');
    // $('pageDown').off('click');
    
//     // pagination.setup(nextPages);
//         $('pageUp').off('click');
//     $('pageDown').off('click');
    

        // if (element === "pg" + lastPage) nextFunc(nextParams);
    // })
    
//     // $(document).ready(function(){
//     $('pageUp').off('click');
//     $('pageDown').off('click');
//     // $('p').click(function(){
//     // console.log(24);
//     //Other code etc.
// // });
// // });

    
    // $("#beginExp").click(function () {
    //     $('#pg' + lastPage).hide();
    //     nextFunc(nextParams);
    // });
    
    
    
    
    
    
    // $("#beginExp").click(function () {
    //     $('#pg' + lastPage).hide();
    //     nextFunc(nextParams);
    // });
    
        
    // const numOfInstructions = lastPage - firstPage + 1;
    
    // const instrPageArray = [...Array(numOfInstructions)];
    // for (const x of _.range(firstPage, lastPage + 1)) {
    //     instrPageArray[x - firstPage] = "pg" + x;
    //     $( "#pg" + x ).load( "instructions/pg" + x + ".html" );
    //     console.log("x = " + x);
    //     console.log("lastPage = " + lastPage);
    //     if (x === lastPage && x!=8) nextFunc(nextParams);
    // }

    // pagination.setup(instrPageArray);

}

function proceedToTask(){
    // preLoad.loadImages("#loading", "#progress", function() {
    //     console.log("tomomo")
    //     finishedLoading();
    //     $("#loading").hide();
    // });

    // $("#beginExp").click(function () {
        console.log("hey");
        if (preLoad.manualCheck()) {
            console.log("ho");
            // clickCounter++;
            finishedLoading();
        } else {
            $("#instructionDisplay").css("display", "none");
            $("#loading").show();
        }
    // });
}

// function babyfunc() {
//     console.log("bojangles")
//     // $("#beginExp").click(function () {
//         // if (clickCounter === 2) {
//         //     clickCounter++;
//         pre_fixate();
//         // }
//     // });
// }


function finishedLoading() {
    
    // instrOnOff("Off", "instructionDisplay");
    console.log('testing');
    // displayInstructions(4, 8, babyfunc); // remove after testing
    // fail_loop(() => displayInstructions(4, 8, pre_fixate, 2000));
    failLoop(displayInstructions, [4, 8, pre_fixate, [2000,]]); //comment back in
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
