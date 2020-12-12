async function checkSpoken() {
    eventTimer.setTimeout(() => {
        document.querySelector("#stimText").style.visibility = "visible";
    }, 500);
    matching = await hearWords();
    spokenCorrect = await _.isEqual(matching[0], matching[1]);
    return spokenCorrect;
}

async function failLoop(stage, nextFunc, nextParams) {
    let failCount = 0
    , matching, practicePass, tries, triesWarning;
    
    targetSide(0);
    
    eventTimer.setTimeout(() => {
        document.querySelector("#stimText").innerHTML =
            practiceWords.reduce((x, y) => x + '<br>' + y);
    }, 500);
    
    await (async () => {
        while (failCount <= failMax) {
            practicePass = await new Promise(
                (resolve) => {
                    if (failCount > 0) {
                        if (stage === "break") {
                            triesWarning = "";
                            failCount--;
                        }
                        else {
                            tries = (failMax + 1 - failCount > 1) ? " tries" : " try" ;
                            triesWarning = "You still have " + (failMax + 1 - failCount) + tries + " left! ";
                        }
                        document.querySelector("#warning").innerHTML =
                            "<p>It looks like that attempt did not work. " + triesWarning + "Remember to avoid filler words if you can help it, to speak slowly, and to leave a second or two between each word.</p><button id='removeWarning' class='button-beige'>Try Again</button>";
                        instrOnOff("On", "warning");
                        document.querySelector("#removeWarning").addEventListener("click", function() {
                            instrOnOff("Off", "warning");
                            resolve(checkSpoken());
                        });
                    } else {
                        resolve(checkSpoken());
                    }
                }
            )

            if (practicePass) {
                break;
            } else {
                failCount++;
            }
        }
    })();
    
    (failCount <= failMax) ? nextFunc.apply(this, nextParams) : audioCheckFailed();
}

async function hearWords() {
    let recPracticeWords = [];
    let test;
    if (annyang) {
        test = function(check) {
            splitWords = check.split(" ");
            for (const word in splitWords) {
                recPracticeWords.push(splitWords[word].toLowerCase());
            }
        }
    }
    
    // Define command
    const commandsPracticeTest = {'*shell': test};
    annyang.debug(); // Debug info for the console
    annyang.removeCommands();
    annyang.addCommands(commandsPracticeTest); // Initialize annyang with our command
    annyang.start();
    
    return new Promise(
        (resolve) => {
            eventTimer.setTimeout(() => {
                annyang.abort();
                document.querySelector("#stimText").style.visibility = "hidden";
                resolve([recPracticeWords, practiceWords]);
            }, speechRecognitionDuration * 1000);
        }
    )
}

function audioCheckFailed() {
        document.querySelector("#warning").innerHTML =
            "<p>Since the automated system still could not successfully process what you said, you will not be able to complete the study. You can reach out to " + experimenterEmail + "  for any questions or concerns. Sorry about that!</p>";
        instrOnOff("On", "warning");
}