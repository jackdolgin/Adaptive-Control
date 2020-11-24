async function testPractice() {
    let recPracticeWords = [];
    let test;
    if (annyang) {
        test = function(check) {
            splitWords = check.split(" ")
            for (word in splitWords) {
                recPracticeWords.push(splitWords[word].toUpperCase())
            }
        }
    }
    const abort = function() {
        annyang.abort();
    };
    // Define commands
    const commandsPracticeTest = {
        '*shell': test,
        'turn off mic(rophone)': abort
    };
    annyang.debug(); // Debug info for the console
    annyang.removeCommands();
    annyang.addCommands(commandsPracticeTest); // Initialize annyang with our commands
    annyang.start();

    return new Promise(
        (resolve, reject) => {
            eventTimer.setTimeout(() => {
                annyang.abort();
                resolve([recPracticeWords, practiceWords])
            }, 15 * 1000)
        }
    )
}

async function fail_loop() {
    let failCount = 0
    , somevals
    , practicePass;
    
    while (failCount <= failMax){
        somevals = await testPractice();
        practicePass = await _.isEqual(somevals[0], somevals[1])
        if (practicePass) {
            break
        } else {
            failCount++
        }
    }
    
    (failCount <= failMax) ? pre_fixate() : end_exp("early")
}
