///////////////////////////////////// SET EXPERIMENTAL PARAMETERS //////////////////////////////////////////////

const conditionsPerTask = ["Congruent", "Incongruent"]

, practiceWords = ['colleague', 'mischievous', 'zaniest'] // words for practice block

, folderlocationIPNP = "../images/IPNP/"                                                      // Path  to IPNP folder

, spreadsheetLocationIPNP = folderlocationIPNP + "IPNP_spreadsheet.csv"                                                      // Path  to IPNP spreadhseet file

, pracTrials = 12                                                                                                                      // Presumably practice trial total will never encroach on the total number of pics we have access to, so doesn't need to be updated in the rest of the file unlike `mainTrials`

, skipLetters = ["a", "e", "i", "l", "o", "u", "y"]                                                                                // Removes these characters from possibly being in the gibberish word during the `Neutral` task

, possibleTasks = ["Neutral", "Predictive_Blocks", "Predictive_Locations"]

, blockSequence = ["A", "B", "A", "B"]                                                                                      //  Sets up not only the order of the blocks (like A-B-A-B), but also allows for a disproportionate number of congruent or incongruent trials overall if we opted for an odd number of blocks (e.g. A-A-B-A-B), and also implicitly dictates how many blocks will be in the task

, predictiveBlockCongruentBlockDominance = .75                                                               // Because in our study these two variables are equal, we could have combined these variables into one; however...
, predictiveBlockIncongruentBlockDominance = .75                                                             // ...this set-up accomodates specifying different ratios for congruent- vs incongruent-dominant blocks

, nonpredictiveBlockCongruentBlockDominance = .5                                                           // For tasks other than `Predictive_Blocks`, all blocks are equally split betwen congruent and incongruent...
, nonpredictiveBlockIncongruentBlockDominance = .5                                                         //  ...trials (and not just randomized; in fact later on details how each block is always 49-51% congruent and 49-51% incongruent)

, predictiveSideCongruentSideDominance = .75                                                                   // Orthogonal (***double check that indeed they are based on the coding***) to the percent of trials/blocks that are congruent/incongruent, specify how much each side (right or left) corresponds to a trial's congruency; again, like above, could ...
, predictiveSideIncongruentSideDominance = .75                                                                 // ...have set this and the below variable as just one variable since they take on the same value here, but for scalability in theory mostly congruent and mostly incongruent sides could have different proportions

, nonpredictiveSideCongruentSideDominance = .5                                                               // If task is `Neutral` or `Predictive_Blocks` then the sides don't indicate congruency (and...
, nonpredictiveSideIncongruentSideDominance = .5                                                            // ...actually trials in those tasks all have an `xDistance` of 0 anyways)

, failMax = 6                                                                                        // Max number of fails allowed in practice with annyang before experiment terminates

, trialDuration = {"pre_fixate": 500,
                   "fixate": 1000,
                   "post_fixate": 500,
                   "stimulus": 2750}

, speechRecognitionDuration = 10

, experimenterEmail = "jdwk04@gmail.com"

, hosturl = "https://cdmlab.a2hosted.com?"

, mainSQLTable = 'adaptive_control'

, demographicsSQLTable = 'adaptive_control_demographics'

, feedbackSQLTable = 'adaptive_control_feedback';

let mainTrials = 252                                                                                                                // Like `actualPics` in `prep-trial-array.js`, this is just a starting point

, numScreenSizeWarnings = 3

, trialArray;
