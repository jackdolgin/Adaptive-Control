const task = _.sample(possibleTasks);
// create a random subject code
//set participant values		
function getRandomString(length, chars) {
    var result = '';
    for (var i = length; i > 0; --i) result += chars[Math.round(Math.random() * (chars.length - 1))];
    return result;
}

const Sub_Code = getRandomString(8, '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ');


const loadCSV = new Promise(
    (resolve, reject) => {
        
        let csvGoesHere;

        Papa.parse(spreadsheetLocationIPNP, {
            download: true,
            dynamicTyping: true,
            header: true,
            complete: function(results) {
                csvGoesHere = results.data;
                resolve(csvGoesHere);
            }
        });
    }
);

async function buildArray(csvInput) {
    return new Promise(
        (resolve, reject) => {
            ////////////////////////////////////////////////////// Condition Assigment /////////////////////////////////////////////////////
            const firstCongruencyInt = _.random(1)
            , congruencyA = majorityLeft = conditionsPerTask[firstCongruencyInt]
            , congruencyB = majorityRight = conditionsPerTask[1 - firstCongruencyInt];

            
            ////////////////////////////////////////////////////////////////////////////////////////////////////////

            let congruentBlockDominance
            , incongruentBlockDominance
            , congruentSideDominance
            , incongruentSideDominance;


            ///////////////////////////////////////////////////////// Trial Proportions //////////////////////////////////////////////////////////
            if (task === "Predictive_Blocks") {                                                                                              // For task `Predictive_Blocks`, set what percent of trials during a mostly-congruent block are congruent, and during a mostly-incongruent block are incongruent

                congruentBlockDominance = predictiveBlockCongruentBlockDominance;
                incongruentBlockDominance = predictiveBlockIncongruentBlockDominance;

            } else if (task === "Neutral" || task === "Predictive_Locations") {
                congruentBlockDominance = nonpredictiveBlockCongruentBlockDominance;
                incongruentBlockDominance = nonpredictiveBlockIncongruentBlockDominance;
            }
            
            let numerator = 0;                                                                                                                   // We add to this starting point of zero as we loop through each block, tallying the percent of trials that are congruent and ultimatley dividing by all...
                                                                                                                                                            // ...the blocks in the task to find the percent of trials per block (on average) that are congruent (and therefore incongruent)
            for (let i = 0; i < blockSequence.length; i++) {
                if (blockSequence[i] === "A") {
                    blockSequence[i] = congruencyA;
                } else if (blockSequence[i] === "B") {
                    blockSequence[i] = congruencyB;
                }

                if (blockSequence[i] === "Congruent") {
                    numerator += congruentBlockDominance;
                } else if (blockSequence[i] === "Incongruent") {
                    numerator += 1 - incongruentBlockDominance;
                }
            }

            const blocks = blockSequence.length;
            
            const congruentOverall = numerator / blocks;

            const incongruentOverall = 1 - congruentOverall;
            const incongruentMultiplier = 1 + incongruentOverall;                                                          // Gets used later to determine how many pictures we need to work with so that there are enough (extra) items/text from incongruent trials that they never have to also be used as text or pics in any other trial in either the practice or main sessions


            if (task === "Predictive_Locations"){
                congruentSideDominance = predictiveSideCongruentSideDominance;
                incongruentSideDominance = predictiveSideIncongruentSideDominance;

            } else if (task === "Neutral" || task === "Predictive_Blocks") {
                congruentSideDominance = nonpredictiveSideCongruentSideDominance;
                incongruentSideDominance = nonpredictiveSideIncongruentSideDominance;
            }
            /////////////////////////////////////////////////////////////////////////////


            ///////////////////////////////////////////////////////// Matrix Assembly //////////////////////////////////////////////////////////
            csvInput = _.chain(csvInput)
                .filter(function(x){ return x.Keep; })
                .sortBy('Mean_RT_All')                                          // // Sorts remaining rows in csv by these columns, which indicate how difficult the images are to identify
                .value()

            const actualPics = csvInput.length;                                                                                  // This is just a starting point, indicating how many pictures we even have access to in the database
            const pracPics = Math.trunc(pracTrials * incongruentMultiplier);                              // As we'll do for `mainTrials`, we need `pracTrials` * `incongruent<ultiplier` number of pics reserved for practice trials so the words used for incongruent practice trials never otherwise appear in the task either as text or pics in either the practice or main sessions
            let mainPics = Math.trunc(mainTrials * incongruentMultiplier);                                  // Ditto as `pracPics` above
            let picsNeeded = pracPics + mainPics;
            
            if (picsNeeded > actualPics) {                                                                                    // If we are asking for more pics than there are pics available (and by pics available, that means only pics that are set to `TRUE` in the `Keep` column of the pic csv), then we need to reduce the number of trials we're working with so we can meet the number of available pictures
                picsNeeded = actualPics;                                                                                      // The new max number of pictures we can work with is literally the number of pics avaiable
                mainPics = picsNeeded - pracPics;
                mainTrials = Math.trunc(mainPics/ incongruentMultiplier);                                     // New number of main experiment trials is the number of pictures needed after accounting for practice trials, divided by `incongruentMultiplier` to essentially leave room for the number of incongruent labels (equaling all available pics minus main trials),...
            }                                                                                                                                   // ...which we know is supposed to be `incongruentOverall`% of all `mainTrials` leaves; sort of like seeing how many main trials we can squeak out while satisfying the `incongruentMultiplier` demand faithfully

            mainTrials = Math.trunc(Math.floor(1.0 * mainTrials / blocks) *  blocks);                    // Allows for slicing off any extra `mainTrials` so that the number of `mainTrials` can be split exactly evenly into the number of blocks (if we do slice any `mainTrials` off, that would mean `picsNeeded` would be slightly greater...
            /////////////////////////////////////////////////////////////////////////////                // ...than `mainTrials` * `incongruentMultiplier`, but that's ok because then we just wouldn't be using a few incongruent labels on the back end (so maybe just a few pics would go to 'waste'))


            ////////////////////////////////////////////////////// Condition Assigment (pt 2?)/////////////////////////////////////////////////////
            csvInput = _.chain(csvInput)
                .first(picsNeeded) // Keep only the top remaining rows in the csv so that the number of rows equals the number of pratice trials + the number of main trials + the number of incongruent labels for practice and main trials + the number of incongruent trials
                .shuffle() // Then randomize the remaining rows
                .value();

            let xDistance;
            if (task === "Neutral" || task === "Predictive_Blocks"){
                xDistance = 0;                                                                                                        // Unless the task is `Predictive_Locations`, the stimuli (pics and words) should be presented at the center of the screen
            } else if (task === "Predictive_Locations") {
                xDistance = 1;                                                                                                        // For task `Predictive_Locations`, the stimuli are presented this many centimeters on either the right or left of the screen (side depends on the trial number, which in turn depends on the congruency of the trial)*** might have to update the meaning of this value for Javascript, might not mean cm anymore, might be like pixels
                let sideRandomizer = _.sample([-1, 1]);                                                                     // Also note this is a starting point for all trials; it's not inside the for loop, so not saying on each trial we randomize the side; it's just randomizing whether left or right is the side that, for a given participant, gets associated with congruent or incongruent
                xDistance *= sideRandomizer;                                                                               // This randomizer is used so that, later on, we randomize whether rows above/below a certain number get assigned to either the right or left of the screen, and this randomization will allow this sorting to differ from participant to participant
            }

            let selectedImages = Array(mainPics.length);
            function abigfunc (picTotal, trialTotal, topOrBottom) {
                let df = topOrBottom(csvInput, picTotal);                                                                              // We're either working with the top of the csv data frame (`mainPics` number of trials) or the bottom end of the data frame (`pracPics`); this way, no two rows are shared by the practice and main trials
                const incTrials =  Math.trunc(trialTotal * incongruentOverall);
                const congTrials = trialTotal * congruentOverall;
                for (let i = 0; i < df.length; i++) {

                    if (i < trialTotal) {

                        selectedImages[i] = new Image();
                        selectedImages[i].src = folderlocationIPNP + "trimmed/" + df[i].Pic_Num + df[i].Dominant_Response + ".png"; // Preload all of the stimuli pictures into the trial matrix/array; we do this before running any of the trials so we stomach the image loading times on the front end
                        preLoad.yourImages[i] = folderlocationIPNP + "trimmed/" + df[i].Pic_Num + df[i].Dominant_Response + ".png";
                        df[i].Image_Path = folderlocationIPNP + "trimmed/" + df[i].Pic_Num + df[i].Dominant_Response + ".png";
                        ['Full_Screen', 'Audio_Connected', 'Audio_Permitted'].forEach(x => df[i][x] = true);
                        df[i].Sub_Code = Sub_Code;

                        if (i < congTrials) {
                            df[i].Label = df[i].Dominant_Response;                                                         // If a trial is congruent, its label is its pic identity; if incongruent, its label is the lead of the pic identity
                            df[i].Congruency = "Congruent";
                        } else {
                            df[i].Label = df[i + incTrials].Dominant_Response;
                            df[i].Congruency = "Incongruent";
                        }
                    }

                    // Splits up congruent and incongruent trials into side of screen
                    if (i > congTrials * congruentSideDominance &&                                               // Works with a table that is only congruent trials on the top, followed by incongruent trials; finds cutoff among congruent trials where `congruentSideDominance`...
                        i < congTrials + incTrials * incongruentSideDominance) {                              // ...% of trials are above it, and those trials are on the congruent dominant side; remaining congruent trials and first `incongruentSideDominance` % of...
                            df[i].Xpos = xDistance;                                                                               // ...incongruent trials are assigned to the other (mostly \) half of the screen; finally, remaining incongruent trials are assigned back to the first side and are...
                    } else {                                                                                                                 // ...among the minority of incongruent trials on their side; the flexibility of this setup allows the percent of overall congruent trials to be orthogonal from the...
                            df[i].Xpos = -xDistance;                                                                             // ...percent of congruent trials on each side (and in turn, each side's number of congruent trials are independent of one another)
                    }
                }

                df = _.chain(df).first(trialTotal).shuffle().value();                                                      // Strips out the rows that supplied the incongruent labels; then shuffle the rows, otherwise trials will be ordered by congruency and (in task `Predictive_Locations` case) side of the screen of appearance
                return df
            }

            let mainTrialsDf = abigfunc(mainPics, mainTrials, _.first);

            //////////////////////////////////////////// Crucial Matrix Reordering ////////////////////////////////////////////

            // This section basically loops through the `mainTrialsDf` dataframe we've just
            // created and reorders it, starting with the first trial of the existing df
            // and essentially asking, 'Which block are we looking for trials for in the new
            // df, is the trial I'm looking at in the old df a congruent or incongruent
            // trial, have I already included you into the new df, and has the current block
            // for the new df already met its quota for the number of congruent and
            // incongruent trials? If so, then skip to the next row in the old df, otherwise,
            // add it to the new df (as manifested in appending to the `newRowOrder` list),
            // make note of how many congruent/incongruent trials have now been included in
            // this ongoing block of the new df, and move on to the next row in the new df

            const trialsPerBlock = Math.trunc(mainTrialsDf.length / blocks)

            // Depending on whether the block is predominantly congruent or incongruent,...
            // ...each block's quota for dominant and rarer trials will vary
            , congruentPerCongBlock = Math.trunc(trialsPerBlock *
                                               congruentBlockDominance)
            , incongruentPerCongBlock = trialsPerBlock - congruentPerCongBlock
            , incongruentPerIncongBlock = Math.trunc(trialsPerBlock *
                                                   incongruentBlockDominance)
            , congruentPerIncongBlock = trialsPerBlock - incongruentPerIncongBlock;

            let block = -1
            , newRowOrder = Array(mainTrialsDf.length)
            , blockDominance
            , matchingPerBlock
            , nonMatchingPerBlock
            , matchingCount
            , nonMatchingCount
            , rowCounter
            , gruency;

            // Loops through the index of the `newRowOrder` list that we're appending to
            // that will ultimately determine the new order of the `mainTrialsDf` df
            for (let aRow = 0; aRow < mainTrialsDf.length; aRow++){

                // This if statement is for resetting the quotas for congruent and
                // incongruent trials when we've reached the start of a new block
                if (aRow % trialsPerBlock === 0){
                    block++;
                    blockDominance = blockSequence[block];

                    if (blockDominance === "Congruent"){
                        matchingPerBlock = congruentPerCongBlock;
                        nonMatchingPerBlock = incongruentPerCongBlock;
                    } else if (blockDominance === "Incongruent"){
                        matchingPerBlock = incongruentPerIncongBlock;
                        nonMatchingPerBlock = congruentPerIncongBlock;
                    }

                    matchingCount = nonMatchingCount = 0;
                }

                rowCounter = -1;
                
                // Loops through rows of old df to search for trials with a congruency
                // within the ongoing block's quota and that haven't already been included
                // in the `newRowOrder` list
                while(true){

                    rowCounter ++;

                    if (! _.pluck(mainTrialsDf, 'Row_Num').includes(rowCounter)){

                        gruency = mainTrialsDf[rowCounter].Congruency;
                        if (gruency === blockDominance && matchingCount < matchingPerBlock){
                            matchingCount++;
                            break
                        } else if (gruency !== blockDominance && nonMatchingCount < nonMatchingPerBlock){
                            nonMatchingCount++;
                            break
                        }
                    }
                }
                mainTrialsDf[aRow].Old_Num = aRow;
                mainTrialsDf[aRow].Row_Num = rowCounter;
                mainTrialsDf[aRow].Block = Math.ceil((rowCounter  + 1) / trialsPerBlock);           // Assigns block numbers to each trial, which will eventually get saved into the exported experiment csv
            }

            mainTrialsDf = _.chain(mainTrialsDf).shuffle().sortBy('Block').value();                       // If we don't shuffle, then for task `Predictive_Blocks` the end of each block is going to filled with trials that match the congruency that is dominant in the block (like for a ...
                                                                                                                                                //  ...congruent-dominant block 1, we'll presumably pass say 15 incongruent trials before 50 congruent trials, and the loop would just pile on the ~45 congruent trials on the back of the block)
            let pracTrialsDf = abigfunc(pracPics, pracTrials, _.last);

            pracTrialsDf = pracTrialsDf.map(obj=> ({ ...obj, Block: -1}));
            pracTrialsDf = _.shuffle(pracTrialsDf);
            trialArrayLocal = pracTrialsDf.concat(mainTrialsDf);


            // For task `Neutral`, swaps the labels on the screen between different pictures,
            // then convert each of these labels to gibberish words composed only of consonants;
            // So the avg length of the label stays the same, but there's no relationship between
            // the picture's true label length and the length of label that overlays it

            if (task === "Neutral"){

                const alphabet = [...Array(26)].map((x,i)=>String.fromCharCode(i + 97))
                , remainingLetters = _.difference(alphabet, skipLetters)
                , randomizedTrialArray = _.shuffle(pracTrialsDf.concat(mainTrialsDf));

                let gibberishWordLength;

                randomizedTrialArray.forEach(function(item, index){
                    gibberishWordLength = item.Label.length;
                    trialArrayLocal[index].Label = _.sample(remainingLetters, gibberishWordLength).join("");
                })
            }

            /////////////////////////////////////////////////////////////////////////////

            resolve(trialArrayLocal);
        }
    )
}