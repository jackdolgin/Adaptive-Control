async function testPractice() {
    let recPracticeWords = [];
    let test;
    // let practicePass;
    if (annyang) {
        test = function(check) {
            splitWords = check.split(" ")
            console.log("tajikistan")
            for (word in splitWords) {
                console.log("norway")
                console.log(word)
                recPracticeWords.push(splitWords[word].toUpperCase())
            }
            console.log('recorded:', recPracticeWords)
            console.log('practice words:', practiceWords)
            console.log(_.isEqual(recPracticeWords, practiceWords))
            // if (_.isEqual(recPracticeWords, practiceWords)) {
            //     practicePass = true;
            //     console.log('test passed');
            //     // $(".mic").remove();
            //     // var $micSuccessMessage = $("<div class='instructions'><p id='mic-success-message'><p>That's it!</p><p>Try to do the same throughout the experiment. You are now ready to start.</p><p>Let's move on.</p></div>")
            //     // $(".jspsych-display-element").append($micSuccessMessage);
            //     // annyang.abort();
            //     // return practicePass
            // } else {
            //     practicePass = false;
            //     console.log("sad")
            //     // return practicePass
            // }
        }
        console.log("testing.sa.da")
    }
    const abort = function() {
        annyang.abort();
    };
    console.log('check')
    // Define commands
    const commandsPracticeTest = {
        '*shell': test,
        'turn off mic(rophone)': abort
    };
    console.log('hmmm')
    annyang.debug(); // Debug info for the console
    annyang.removeCommands();
    annyang.addCommands(commandsPracticeTest); // Initialize annyang with our commands
    annyang.start();
    // annyang.addCallback('end', console.log('callback working?'))
    console.log('Microphone turned on.');
  //   annyang.trigger(
  //   ["OPTIMAL", "MEMORY", "ABSOLUTE", ]//"OPTIMAL MEMORY ABSOLUTE"]
  // );
    // startTimer(practiceRecTime);

    return new Promise(
        (resolve, reject) => {
            eventTimer.setTimeout(() => {
                annyang.abort();
                resolve([recPracticeWords, practiceWords])
                            // resolve(_.isEqual(recPracticeWords, recPracticeWords))
            }, 15 * 1000)
                // let practicePass = _.isEqual(recPracticeWords, practiceWords)
                // console.log(recPracticeWords)
                // console.log(practiceWords)
                // resolve(practicePass)},

            // await eventTimer.setTimeout(function() {
            //     annyang.abort();
            //     console.log('Microphone turned off.');
            // }, 15 * 1000);
            // // annyang.addCallback('end', console.log('callback working?????????'))
            // console.log("dojojo")
        }
    )

    //
    // let promise = new Promise((resolve, reject) => {
    //   eventTimer.setTimeout(() => resolve(practicePass), 15 * 1000)
    // });
    //
    // let result = await promise; // wait until the promise resolves (*)
    //

}

async function fail_loop() {
    let failCount = 0;
    while (failCount <= failMax){
        if (failCount > 0){
            console.log("failCount = " + failCount)
        }
        let somevals = await testPractice();
        practicePass = await _.isEqual(somevals[0], somevals[1])
        if (practicePass) {
            break
        } else {
            failCount++
        }
    }

}

// // go here if the practice test doesn't work
// const fail = {
//     type: 'text',
//     text: "<div class='instructions'><p>The practice test didn't work...</p><p>Please say <strong>" + practiceWords[0] + "..."
//     + practiceWords[1] + "..."
//     + practiceWords[2] +
//     "</strong> clearly and close to the microphone.</p>" +
//         "<p>Press any key to try again.</p></div>",
//     is_html: true,
// };
//
// // create practice fail loop
// const fail_loop = {
//     timeline: [fail, practiceTest],
//     loop_function: function() {
//         if (practicePass === false) {
//         // if (false === false) { **
//             return true;
//         } else if (practicePass === true) {
//         // } else if (true === true) { **
//             return false;
//         }
//     }
// };
//
// // only go into this loop if the practice test fails
// const if_failed = {
//     timeline: [fail_loop],
//     conditional_function: function() {
//         if (practicePass === false) {
//         // if (false === false) { **
//             return true;
//         } else if (practicePass === true) {
//             // } else if (true === true) { **
//             return false;
//         }
//     }
// };
