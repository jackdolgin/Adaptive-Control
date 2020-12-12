const instrPages = (Array(13)
                    .fill(1)
                    .map((x, y) => 'pg' + (x + y)))
                .reduce((x, y) =>
    (x + "<div id='" + y + "' class=' page'></div>"), '');

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
    $("#instructionDisplay").html(
      instrPages +
        '<div class="pgButtons"><button id="pageDown" class="button-blue" style="float: left;">BACK</button><button id="pageUp" class="button-blue" style="float: right; margin-right: 4%">NEXT</button><button id="beginExp" class="button-green " style="float: right; margin-right: 4%">START</button></div>'
    );
    
    if (localStorage.getItem("consented") === "yes") {
        await startTask();
    }
})();