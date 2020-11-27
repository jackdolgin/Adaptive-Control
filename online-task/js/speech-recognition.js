async function checkSpoken() {
    document.querySelector("#stimText").style.visibility = "visible";
    matching = await hearWords();
    spokenCorrect = await _.isEqual(matching[0], matching[1]);
    console.log("spokenCorrect = " + spokenCorrect);
    return spokenCorrect;
}

async function fail_loop(nextfunc) {
    let failCount = 0
    , matching
    , practicePass
    , tries;
    
    document.querySelector("#stimText").innerHTML =
        practiceWords.reduce((x, y) => x + '<br>' + y);
    console.log("brody");
    
    await (async () => {
        while (failCount <= failMax) {
            practicePass = await new Promise(
                (resolve) => {
                    if (failCount > 0) {
                        tries = (failMax + 1 - failCount > 1) ? " tries" : " try" ;
                        document.querySelector("#warning").innerHTML =
                            "<p>It looks like that attempt did not work. You still have " + (failMax + 1 - failCount) + tries + " left! Remember to avoid filler words if you can help it and to speak slowly, leaving a moment between each word.</p><button id='removeWarning' class='button-beige'>Try Again</button>";
                        instrOnOff("On", "warning");
                        console.log("soo");
                        document.querySelector("#removeWarning").addEventListener("click", async function() {
                            console.log("boo");
                            // practicePass = 
                            instrOnOff("Off", "warning");
                            resolve(checkSpoken())
                        });
                        // eventTimer.setTimeout(() => {
                        //     annyang.abort();
                        //     console.log("yoyo")
                        //     document.querySelector("#stimText").style.visibility = "hidden";
                        //     resolve([recPracticeWords, practiceWords])
                        // }, 10 * 1000)
                    } else {
                        resolve(checkSpoken())
                    }
                }
            )
        
        
            // await (async () => {
            //     tries = (failMax + 1 - failCount > 1) ? " tries" : " try" ;
            //     document.querySelector("#warning").innerHTML =
            //         "<p>It looks like that attempt did not work. You still have " + (failMax + 1 - failCount) + tries + " left! Remember to avoid filler words if you can help it and to speak slowly, leaving a moment between each word.</p><button id='removeWarning' class='button-beige'>Try Again</button>";
            //     instrOnOff("On", "warning");
            //     console.log("soo");
            //     document.querySelector("#removeWarning").addEventListener("click", async function() {
            //         console.log("boo");
            //         practicePass = await checkSpoken();
            //         instrOnOff("Off", "warning");
            //     });
            // })();
            
            if (practicePass) {
                console.log("hi")
                break;
            } else {
                console.log("bye")
                failCount++;
            }
        }
    })();
    
    // await (async () => {
    //     while (failCount <= failMax) {
    //         console.log("comcast");
    //         await (async () => {
    //             console.log("munger");
    //             if (failCount > 0) {
    //                 console.log("norman");
    //                 document.querySelector("#stimText").style.visibility = "hidden";
    //                 tries = (failMax + 1 - failCount > 1) ? " tries" : " try" ;
    //                 document.querySelector("#warning").style.visibility = "visible";
    //                 document.querySelector("#warning").innerHTML =
    //                     "<p>It looks like that attempt did not work. You still have " + (failMax + 1 - failCount) + tries + " left! Remember to avoid filler words if you can help it and to speak slowly, leaving a moment between each word.</p><button id='removeWarning' class='button-beige'>Try Again</button>";
    //                 console.log("soo");
    //                 document.querySelector("#removeWarning").addEventListener("click", async function() {
    //                     console.log("boo");
    //                     document.querySelector("#warning").style.visibility = "hidden";
    //                     practicePass = await checkSpoken();
    //                 })
    //                 console.log("voo");
    //             } else {
    //                 practicePass = await checkSpoken();
    //                 console.log("bobo");
    //             }
    //         })();
    
    //         if (practicePass) {
    //             console.log("hi")
    //             break;
    //         } else {
    //             console.log("bye")
    //             failCount++;
    //         }
            
    //         // document.querySelector("#stimText").style.visibility = "visible";
    //         // console.log("smog");
    //         // matching = await hearWords();
    //         // console.log("matching = " + matching);
    //         // practicePass = await _.isEqual(matching[0], matching[1])
    //         // console.log(practicePass)
    //         // console.log(failCount)
    
    //         // if (practicePass) {
    //         //     break
    //         // } else {
    //         //     failCount++;
    //         // }
    //     }
    // })();
    console.log("toho");
    (failCount <= failMax) ? nextfunc(4, 8, pre_fixate) : end_exp("early");
}

async function hearWords() {
    let recPracticeWords = [];
    let test;
    if (annyang) {
        test = function(check) {
            splitWords = check.split(" ")
            for (word in splitWords) {
                recPracticeWords.push(splitWords[word].toLowerCase());
            }
        }
    }
    
    // const abort = function() {
    //     annyang.abort();
    // };
    // // Define commands
    // const commandsPracticeTest = {
    //     '*shell': test,
    //     'turn off mic(rophone)': abort
    // };
    // annyang.debug(); // Debug info for the console
    // annyang.removeCommands();
    // annyang.addCommands(commandsPracticeTest); // Initialize annyang with our commands
    // console.log("mono");
    // annyang.start();
    // console.log("pono")
    
    // Define command
    const commandsPracticeTest = {'*shell': test};
    annyang.debug(); // Debug info for the console
    annyang.removeCommands();
    annyang.addCommands(commandsPracticeTest); // Initialize annyang with our command
    console.log("mono")
    annyang.start();
    console.log("pono")
    
    return new Promise(
        (resolve) => {
            eventTimer.setTimeout(() => {
                annyang.abort();
                console.log("yoyo")
                document.querySelector("#stimText").style.visibility = "hidden";
                resolve([recPracticeWords, practiceWords])
            }, sppechRecognitionDuration * 1000)
        }
    )
}
