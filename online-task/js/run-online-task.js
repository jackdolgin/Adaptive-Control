let instrPages = Array("<div id='pg1' class=' page'></div>");
let instrPagesConcat = instrPages.concat(Array(13 - 2 + 1).fill(2).map((x, y) =>
    'pg' + (x + y)));
instrPagesConcat = instrPagesConcat.reduce((x, y) =>
    x + "<div id='" + y + "' class=' page'></div>");
$("#instructionDisplay").html(
    instrPagesConcat +
    '<div class="pgButtons"><button id="pageDown" class="button-blue" style="float: left;">BACK</button><button id="pageUp" class="button-blue" style="float: right; margin-right: 4%">NEXT</button><button id="beginExp" class="button-green " style="float: right; margin-right: 4%">START</button></div>'
);


async function startTask() {
    const csvIngested = await loadCSV;
    trialArray = await buildArray(csvIngested);
    
    setTimeout(function() {
        navigator.mediaDevices.getUserMedia({ audio: true }); }, 1200);
        
    preLoad.loadImages("#loading", "#progress", function() {
        finishedLoading();
        $("#loading").hide();
    });
    displayInstructions(1, 3, proceedToTask);
    
}
(async () => {
    if (localStorage.getItem("consented") === "yes") {
        await startTask();
    }
})();