# -*- coding: utf-8 -*-
# PsychoPy is required for this experiment
# Install dplython using "pip install dfply"; i.e. on my Mac I typed into terminal: "pip install dfply"; if you're running script from the PscyhoPy app you'll also need to (at least on a Mac) copy the dfply folder into PsychoPy3 -> Contents -> Resources -> lib -> python3.6
#https://github.com/kieferk/dfply/issues/86#issuecomment-533351436
# or on PC add it inside the following folder: PsychoPy3\Lib\site-packages; can also just download the folder from https://github.com/kieferk/dfply;
# once you have the dfply folder, you want to take its inner folder that is also called dfply and that's the folder to copy into psychopy's python directory
# once you have the dfply folder, you want to take its inner folder that is also called dfply and that's the folder to copy into psychopy's python directory
from dfply import *
from psychopy import locale_setup, prefs, gui, visual, core, data, event, logging, clock, prefs, microphone
from psychopy.visual import textbox
import numpy as np
import pandas as pd
import fractions
import random
import glob
import os
import psychopy.voicekey as vk
import math
import re
import string

microphone.switchOn()
vk.pyo_init()                                                                   # Defaults PsychoPy's settings to using the pyo library, which is used for the audio throughout the experiment


# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

# Store info about the experiment session
expInfo = {'Participant':''}                                                   # Creates dictionary of experiment information
if gui.DlgFromDict(dictionary=expInfo).OK == False:                             # Creates popup at beginning of experiment that asks for participant number
    core.quit()                                                                 # Says, "if you press escape or click cancel when that popup appears, then don't run the experiment"; if this if-statement didn't exist, experiment would run regardles of whether you hit escape/click cancel
expInfo['Date'] = data.getDateStr()                                             # Add a simple timestamp
participant_number = expInfo['Participant']
filelocation = _thisDir + os.sep + u'data/%s' % (participant_number)        # Creates data file name
filename = os.path.join(filelocation, 'exp_data')
thisExp = data.ExperimentHandler(extraInfo = expInfo, dataFileName = filename)
logFile = logging.LogFile(filename + '.log', level = logging.EXP)               # Save a log file for detail verbose info

# Setup the Window
win = visual.Window(
    size=(1280, 800), fullscr=True, allowGUI=False,
    monitor='testMonitor', color=[1,1,1], useFBO=True)


##--------------------------CONDITION ASSIGNMENT---------------------------##

tasks = ("Neutral", "Predictive Blocks", "Predictive Locations")
num_of_tasks = len(tasks)
conditions_per_task = ("Congruent", "Incongruent")
num_conditions_per_task = len(conditions_per_task)                                                           # 0 for congruent, 1 for incongruent

def floor_to_multiple(x, y):
    return int(math.floor(1.0 * x / y) * y)

participants_per_task = 20
participants_per_task = floor_to_multiple(participants_per_task, num_conditions_per_task)          # needs to be divisible by `conditions_per_task` to counterbalance whether congruent or incongruent block starts first for each task
total_participants = participants_per_task * num_of_tasks

task_order = np.repeat(tasks, participants_per_task)                                         # Randomizes assignment of session for participants, counterbalanced
first_congruency_order = np.tile(range(num_conditions_per_task), int(total_participants / num_conditions_per_task))
task_and_first_congruency_order = list(zip(task_order, first_congruency_order))
random.Random(123).shuffle(task_and_first_congruency_order)

task_order[:], first_congruency_order[:] = zip(*task_and_first_congruency_order)

task = task_order[int(participant_number) - 1]
expInfo['Task'] = task
first_congruency_int = first_congruency_order[int(participant_number) - 1]
first_congruency = majority_left = conditions_per_task[first_congruency_int]

if task == "Neutral":
    first_congruency_upload = 'NA'
elif task == "Predictive Blocks":
    first_congruency_upload = first_congruency                                  # Record whether the first block is either mostly congruent or incongruent
elif task == "Predictive Locations":
    first_congruency_upload = 'Even_Split'
expInfo['First_Congruency'] = first_congruency_upload
second_congruency = majority_right = conditions_per_task[1 - first_congruency_int]



##----------------------------EXPERIMENT TIMING-------------------------------##

expInfo['FrameRate'] = win.getActualFrameRate()                                 # Store frame rate of monitor

framelength = win.monitorFramePeriod
def to_frames(t):                                                               # Converts time to frames accounting for the computer's refresh rate (aka framelength); input is the desired time on screen, but the ouput is the closest multiple of the refresh rate
    return int(round(t / framelength))


earliest_possible_timeout_input = 1.5                                             # No matter how quickly participant finishes talking, stimulus will stay on screen for at least this amount of time (in seconds)
earliest_possible_timeout = to_frames(earliest_possible_timeout_input)
timeout_1_input = 3                                                             # If participant hasn't started talking by this time point, jump to the next trial
timeout_1 = to_frames(timeout_1_input)

talk_spillover_input = .6                                                       # Keep pic/word on the screen for this amount of time after talking is finished, because sometimes offset timing can be a little imprecise and don't want stimuli removed from screen while participant still talking
talk_spillover = to_frames(talk_spillover_input)
fix_duration_input = .75                                                        # length of fixation cross on the screen during ITI
fix_duration = to_frames(fix_duration_input)
ITI_talking_penalty_input = .04                                                  # For every frame (i.e. run through the while loop) in which talking is detected during ITI, add this much additional time before presenting next trial; this is so someone doesn't keep talking into the start of the next trial, especially if talking only started during ITI (like if they were late to give the response to the previous trial)
ITI_talking_penalty = to_frames(ITI_talking_penalty_input)


##-----------------------------STIMULI LOADING--------------------------------##

fix = visual.TextStim(win=win, color='black', text='+')                         # Create fixation cross

recordlocation = _thisDir + os.sep + u'data/%s/%s/%s' % (participant_number,  # Location for the eventual recording of participant's voice response; gets saved in a subfolder within the `audio` folder...
                                                         'audio',               # ...so that during analyses the audio recordings from each trial are the only files directly in the `audio` folder (without needing to be recurively accessed); allows...
                                                         'full_recording')      # ...for just analyzing every .wav file R finds directly in the `audio` folder

os.makedirs(recordlocation)                                                     # This actually creates the directory specified in the above line so that when we go to save the audio, the folder already exists (and therefore doesn't return an error)
mic_1 = microphone.AdvAudioCapture(name = 'mic_1',
                                   filename = os.path.join(recordlocation,
                                                           'full_recording') +
                                                           '.wav',
                                   stereo=False, chnl=0)

if task == "Neutral" or task == "Predictive Blocks":
    x_distance = 0                                                              # Unless the task is "Predictive Locations", the stimuli (pics and words) should be presented at the center of the screen
elif task == "Predictive Locations":
    x_distance = 7                                                              # For task "Predictive Locations", the stimuli are presented this many centimeters on either the right or left of the screen (side depends on the trial number, which in turn depends on the congruency of the trial)
    side_randomizer = random.choice([-1, 1])                                    # This randomizer is used so that, later on during the `assign_sides_to_rows` function, we randomize whether rows above/below a certain number get assigned to either the right or left of the screen, and this randomization will allow this sorting to differ from participant to participant
    x_distance *= side_randomizer



##----------------------------------------------------------------------------##
##------------------------------TRIAL MATRIXING-------------------------------##
##----------------------------------------------------------------------------##


##-----------------------------TRIAL PROPORTIONS------------------------------##


if task == "Predictive Blocks":                                                                   # For task "Predictive Blocks", set what percent of trials during a mostly-congruent block are congruent, and during a mostly-incongruent block are incongruent

    congruent_block_dominance = .75                                             # Because in our study these two variables are equal, we could have combined these variables into one; however this set-up accomodates specifying different ratios for congruent- vs incongruent-dominant blocks
    incongruent_block_dominance = .75

else:                                                                           # For tasks other than #1, all blocks are equally split betwen congruent and incongruent trials (and not just randomized; in fact later on details how each block is always 49-51% congruent and 49-51% incongruent)
    congruent_block_dominance = .5
    incongruent_block_dominance = .5

block_sequence = [first_congruency, second_congruency, first_congruency,        # Sets up not only the order of the blocks (like A-B-A-B), but also allows for a disproportionate number of congruent or incongruent trials overall if we opted...
                  second_congruency]                                            # ...for an odd number of blocks (e.g. A-A-B-A-B), and also implicitly dictates how many blocks will be in the task

blocks = len(block_sequence)

numerator = 0                                                                   # We add to this starting point of zero as we loop through each block, tallying the percent of trials that are congruent and ultimatley dividing by all...
                                                                                # ...the blocks in the task to find the percent of trials per block (on average) that are congruent (and therefore incongruent)
for i in block_sequence:
    if i == "Congruent":
        numerator += congruent_block_dominance
    elif i == "Incongruent":
        numerator += 1 - incongruent_block_dominance

congruent_overall = numerator / blocks

incongruent_overall = 1 - congruent_overall
incongruent_multiplier = 1 + incongruent_overall                                # Gets used later to determine how many pictures we need to work with so that there are enough (extra) items/text from incongruent trials that they never have to also be used as text or pics in any other trial in either the practice or main sessions


if task == "Predictive Locations": # orthogonal (double check that indeed they are based on the coding) to the percent of trials/blocks that are congruent/incongruent, specify how much each side (right or left) corresponds to a trial's congruency
    congruent_side_dominance = .75                                         # again, like above, could have set this and the below variable as just one variable since they take on the same value here, but for scalability in theory left and right could have different proportions
    incongruent_side_dominance = .75

elif task == "Neutral" or task == "Predictive Blocks":                                                                           # If task is "Neutral" or "Predictive Blocks" then the sides don't indicate congruency (and actually trials in those tasks all have an `x_distance` of 0 anyways)
    congruent_side_dominance = incongruent_side_dominance = .5




pic_csv = (pd.read_csv(os.path.join(IPNP_dir, 'IPNP_spreadsheet.csv')) >>       # Read in the csv listing the different image names and relevant info like agreement factor
            mask(X.Keep) >>
            arrange(X.Mean_RT_All))                                             # Sorts remaining rows in csv by these columns, which indicate how difficult the images are to identify

actual_pics = len(pic_csv)                                                      # This is just a starting point, indicating how many pictures we even have access to in the database
prac_trials = 12                                                                 # Presumably practice trial total will never encroach on the total number of pics we have access to, so doesn't need to be updated in the next few lines unlike `main_trials`
prac_pics = int(prac_trials * incongruent_multiplier)                           # As we'll do for `main_trials`, we need `prac_trials` * `incongruent_multiplier` number of pics reserved for practice trials so the words used for incongruent practice trials never otherwise appear in the task either as text or pics in either the practice or main sessions
main_trials = 500                                                                # Like `actual_pics`, this is just a starting point
main_pics = int(main_trials * incongruent_multiplier)                           # Ditto as `prac_pics` above
pics_needed = prac_pics + main_pics

if pics_needed > actual_pics:                                                   # If we are asking for more pics than there are pics available (and by pics available, that means only pics that are set to `TRUE` in the `Keep` column of the pic csv), then we need to reduce the number of trials we're working with so we can meet the number of available pictures
    pics_needed = actual_pics                                                   # The new max number of pictures we can work with is literally the number of pics avaiable
    main_trials  = int((pics_needed - prac_pics) / incongruent_multiplier)      # New number of main experiment trials is the number of pictures needed after accounting for practice trials, divided by `incongruent_multiplier` to essentially leave room for the number of incongruent labels (equaling all available pics minus main trials), which we know is supposed to be `incongruent_overall`% of all main_trials leaves; sort of like seeing how many main trials we can squeak out while...
                                                                                # ...satisfying the incongruent_multiplier demand faithfully

main_trials = floor_to_multiple(main_trials, blocks)                             # Allows for slicing off any extra `main_trials` so that the number of `main_trials` can be split exactly evenly into the number of blocks (if we do slice any `main_trials` off, that would mean `pics_needed` would be slightly greater...
                                                                                  # ...than `main_trials` * `incongruent_multiplier`, but that's ok because then we just wouldn't be using a few incongruent labels on the back end (so maybe just a few pics would go to 'waste'))


##------------------------------MATRIX ASSEMBLY-------------------------------##

pic_csv = (pic_csv >>
    head(pics_needed) >>                                                        # Keep only the top remaining rows in the csv so that the number of rows equals the number of pratice trials + the number of main trials + the number of incongruent labels for practice and main trials + the number of incongruent trials
    sample(frac = 1)                                                            # Randomize the remaining rows
)

@make_symbolic                                                                  # This line is required to apply the function to column names like dplyr, which as a reminder also uses unquoted symbols for column names
def to_ceil(x):                                                                 # Round to ceiling; this is outside the dfply chain because for whatever reason dfply doesn't like numpy
    return np.ceil(x)


# Splits up congruent and incongruent trials into side of screen
@make_symbolic                                                                  # Works with a table that is only congruent trials on the top, followed by incongruent trials; finds cutoff...
def assign_sides_to_rows(given_row_num, trial_total_arg):                       # ...among congruent trials where `congruent_side_dominance` % of trials are above it, and those trials are on...
    cong_side_cutoff = (trial_total_arg * congruent_overall *                   # ...the congruent dominant side; remaining congruent trials and first `incongruent_side_dominance` % of...
                        congruent_side_dominance)                               # ...incongruent trials are assigned to the other (mostly \) half of the screen; finally, remaining...
    incong_side_cutoff = (trial_total_arg * congruent_overall +                 # ...incongruent trials are assigned back to the first side and are among the minority of incongruent trials...
                          trial_total_arg * incongruent_overall *               # ... on their side; the flexibility of this setup allows the percent of overall congruent trials to be orthogonal...
                          incongruent_side_dominance)                           # ...from the percent of congruent trials on each side (and in turn, each side's number of congruent trials are...
    row_sorting = np.logical_and(cong_side_cutoff <= given_row_num,             # ...independent of one another)
                                 given_row_num < incong_side_cutoff)
    return if_else(row_sorting, x_distance, -x_distance)

@dfpipe
def df_pipe(df, pic_total, trial_total, top_or_bottom):
  df_for_piping = (df >>                                                        # Like in dplyr, this function's setup allows us to directly add this function into the pipe below
    top_or_bottom(pic_total) >>                                                 # We're either working with the top of the csv data frame (`main_pics` number of trials) or the bottom end of the data frame (`prac_pics`); this way, no two rows are shared by the practice and main trials
    mutate(Lead_Dominant_Response = lead(X.Dominant_Response,                   # New column where value is the pic identity from `trial_total` * `incongruent_overall` number of rows above; basically equal to a shift up...
                                        int(trial_total *                       # ...in the dominant reseponse column, but we apply this to a new column because for congruent trials, when creating the label column...
                                            incongruent_overall))) >>           # ...we'll still want access to the pic's actual identity so we can assign it to the label column down a few rows
    mutate(row_num_setup = 1) >>                                                # This column is a placeholder, we can't assign row numbers to the rows (at least as far as I know) unless all the rows are equal to the same value and then `row_number()` ranks the rows as a tie-breaker
    mutate(row_num = row_number(X.row_num_setup)) >>
    mutate(congruency = if_else(X.row_num <= trial_total * congruent_overall,   # Only rows above/below a certain row number get assigned congruent/incongruent; congruents come first, so that it's easier to use the lead function...
                                "Congruent", "Incongruent")) >>                 # ...to bump up the incongruent labels that don't get used in any other trials (since we're about to prune any row > `prac_trials` + `main_trials`)
    mutate(label = if_else(X.congruency == "Congruent",                         # If a trial is congruent, its label is its pic identity; if incongruent, its label is the lead of the pic identity
                           X.Dominant_Response, X.Lead_Dominant_Response)) >>
    mutate(x_pos = assign_sides_to_rows(X.row_num, trial_total)) >>
    head(trial_total) >>                                                        # Strips out the rows that supplied the incongruent labels
    sample(frac = 1)                                                            # Shuffle the rows, otherwise trials will be ordered by congruency and (in task "Predictive Locations" case) side of the screen of appearance
  )
  return df_for_piping


main_trials_df = (pic_csv >>
    df_pipe(main_pics, main_trials, head)
)

trials_per_block = int(len(main_trials_df) / blocks)

block = -1
new_row_order = []

##-------------------------CRUCIAL MATRIX REORDERING--------------------------##

# This section basically loops through the `main_trials` dataframe we've just
# created and reorders it, starting with the first trial of the existing df
# and essentially asking, 'Which block are we looking for trials for in the new
# df, is the trial I'm looking at in the old df a congruent or incongruent
# trial, have I already included you into the new df, and has the current block
# for the new df already met its quota for the number of congruent and
# incongruent trials? If so, then skip to the next row in the old df, otherwise,
# add it to the new df (as manifested in appending to the `new_row_order` list),
# make note of how many congruent/incongruent trials have now been included in
# this ongoing block of the new df, and move on to the next row in the new df

# Depending on whether the block is predominantly congruent or incongruent,
# each block's quota for dominant and rarer trials will vary
congruent_per_cong_block = int(trials_per_block * congruent_block_dominance)
incongruent_per_cong_block = trials_per_block - congruent_per_cong_block
incongruent_per_incong_block = int(trials_per_block *
                                   incongruent_block_dominance)
congruent_per_incong_block = trials_per_block - incongruent_per_incong_block


 # Loops through the index of the `new_row_order` list that we're appending to
 # that will ultimately determine the new order of the `main_trials_df` df
for a_row in range(len(main_trials_df)):

    # This if statement is for resetting the quotas for congruent and
    # incongruent trials when we've reached the start of a new block
    if len(new_row_order) % trials_per_block == 0:
        block += 1
        block_dominance = block_sequence[block]

        if block_dominance == "Congruent":
            matching_per_block = congruent_per_cong_block
            non_matching_per_block = incongruent_per_cong_block
        elif block_dominance == "Incongruent":
            matching_per_block = incongruent_per_incong_block
            non_matching_per_block = congruent_per_incong_block

        matching_count = non_matching_count = 0


    row_counter = -1

    # Loops through rows of old df to search for trials with a congruency
    # within the ongoing block's quota and that haven't already been included
    # in the `new_row_order` list
    while True:

        row_counter += 1

        if row_counter not in new_row_order:
            gruency = main_trials_df.iloc[row_counter].congruency
            if gruency == block_dominance and matching_count < matching_per_block:
                matching_count += 1
                break
            elif gruency != block_dominance and non_matching_count < non_matching_per_block:
                non_matching_count += 1
                break

    new_row_order.append(row_counter)

@make_symbolic
def implement_new_order(y):
    return new_row_order.index(y - 1)

main_trials_df = (main_trials_df >>
    mutate(row_num = row_number(X.row_num_setup)) >>
    mutate(row_num = X.row_num.apply(implement_new_order)) >>
    arrange(X.row_num) >>
    mutate(block = to_ceil((X.row_num  + 1) / trials_per_block)) >>             # Assigns block numbers to each trial, which will eventually get saved into the exported experiment csv
    group_by(X.block) >>
    sample(frac = 1)                                                            # If we don't shuffle, then for task "Predictive Blocks" the end of each block is going to filled with trials that match the congruency that is dominant in the block (like for a congruent-dominant block 1, we'll presumably pass say 15 incongruent trials before 50 congruent trials, and the loop would just pile on the ~45 congruent trials on the back of the block)
)

# Add in the practice trials at the front of the final trial matrix csv,
# then bind the main trials csv underneath
ready_for_matrix = (pic_csv >>
    df_pipe(prac_pics, prac_trials, tail) >>
    mutate(block = -1) >>
    sample(frac = 1) >>
    bind_rows(main_trials_df, join = 'inner')
)


# For task "Neutral", swaps the labels on the screen to 5-letter gibberish words
# composed only of consonants; selected five letters because that seemed like
# somewhere in the middle for the words typically presented, and used the
# same-length gibberish for every trial so there was no (and participants
# didn't bother to look for a) relationship betewen picture and word length
if task == "Neutral":

    gibberish_word_length = 5
    skip_letters = '[aeilouy]'                                                  # Removes these characters from possibly being in the gibberish word
    remaining_letters = re.sub(skip_letters, "", string.ascii_lowercase)

    all_gibberish_words = []
    for i in range(len(ready_for_matrix)):
        all_gibberish_words.append("".join(random.choices(remaining_letters, k = gibberish_word_length)))

    ready_for_matrix['label'] = all_gibberish_words

IPNP_dir = os.path.abspath(os.path.join(_thisDir, "..", "images", "IPNP")) # The directory hosting the stimuli pictures
adict = {}                                                                      # Ultimately the trial matrix is actually dictionary
for dominant_resp, a, b, x, bl, pic_num in zip(ready_for_matrix.Dominant_Response, ready_for_matrix.Pic_Num, ready_for_matrix.label, ready_for_matrix.x_pos, ready_for_matrix.block, ready_for_matrix.Pic_Num):
    adict[b.capitalize()] = ((visual.ImageStim(win, image = os.path.join(IPNP_dir, "trimmed", a + dominant_resp + ".png"), pos = (x, 0), units = 'cm'), # Preload all of the stimuli pictures into the trial matrix/dictionary; we do this before running any of the trials so we stomach the image loading times on the front end
                            visual.TextBox(window = win, text = b.upper(), font_name = bytes("Consolas", encoding= 'utf-8'), font_color=(-1,-1,-1), background_color = [1,1,1,.8], textgrid_shape=[len(b), 1], pos = (x, 0), units = 'cm')), # Crucially, we use `TextBox()` instead of `TextStim()` because `TextBox()` can be overlaid translucently on top of the pis; though it does work best only with monospaced font)
                            b, dominant_resp, x, bl, pic_num)



##----------------------------------------------------------------------------##
##-------------------------------TRIAL SETUP----------------------------------##
##----------------------------------------------------------------------------##


##------------------------------DEFINE TRIAL----------------------------------##

def runTrial():

     # Quit out of task, shut down microphone seeession if participant presses
     # escape during a trial
    def stop_if_esc():
        if event.getKeys(keyList = ["escape"]):
            mic_1.stop()
            core.quit()
        win.flip()

    def fix_as_reference():
        if x_distance != 0:
            fix.draw()

    timeout_2 = talk_spillover + fix_duration
    vpvkOff = vk.OffsetVoiceKey()                                               # Tracks voice offset
    vpvkOn = vk.OnsetVoiceKey(sec = 40)                      # Tracks voice onset (note- without the `sec = ` parameter, it's possible there will be an error that the baseline is too quiet; 40 is just so that it lasts longer than the trial
    [i.start() for i in (vpvkOff, vpvkOn)]

    start_recording = globalClock.getTime()                                     # Timer used as reference for voice onset and offset
    vpvkOn.event_onset = vpvkOff.event_offset = "NA"
    frames_transpired_1 = frames_transpired_2 = 0


    while (vpvkOff.event_offset == "NA" or
           frames_transpired_1 < earliest_possible_timeout):

        [i.draw() for i in trial_vals[0]]

        fix_as_reference()

        if frames_transpired_1 >= timeout_1 and vpvkOn.event_onset == "NA":
            break

        frames_transpired_1 += 1
        stop_if_esc()


##---------------------------SAVE TRIAL DATA----------------------------------##

    for i in (('Trial', trial_num + 1),                                         # Means that the first experimental trials ends up with a value of `1`
                   ('Picture_Number', trial_vals[5]),
                   ('Picture_Identity', trial_vals[2]),
                   ('Picture_Label', trial_vals[1]),
                   ('Task_Side', trial_vals[3]),
                   ('Response_Time_in_PsychoPy', vpvkOn.event_onset),
                   ('Response_Finish_in_PsychoPy', vpvkOff.event_offset),
                   ('Stim_Onset', start_recording),
                   ('Block', trial_vals[4])):

        thisExp.addData(i[0], i[1])


##-------------------------------DEFINE ITI-----------------------------------##

    while frames_transpired_2 < timeout_2:

        if frames_transpired_2 < talk_spillover:

            [i.draw() for i in trial_vals[0]]

            fix_as_reference()

        elif (trial_num + 1) % trials_per_block != 0:
            fix.draw()

        if frames_transpired_2 == talk_spillover:
            stim_offset_fix_onset = globalClock.getTime()

        if vpvkOn.power[-1] >= 100:
            timeout_2 += ITI_talking_penalty

        frames_transpired_2 += 1

        stop_if_esc()



    thisExp.addData('Stim_Offset_Fix_Onset',stim_offset_fix_onset)
    if trial_num == -prac_trials:
        thisExp.addData('Fix_Offset', 0)

    thisExp.nextEntry()

    if trial_num < main_trials - 1:
        thisExp.addData('Fix_Offset', globalClock.getTime())


##-----------------------------WRITE THE INSTRUCTIONS-------------------------##

def create_inst(t):
    return visual.TextStim(win = win, text = t, units = 'deg', pos = (0, 0),
        height = 1, wrapWidth = 18, color = 'black')

def continue_goback(s):
    return "\n\nPress space to " + s + " or \"B\" to go back."

if task == "Neutral":
    sensical = "nonsense characters"
elif task == "Predictive Blocks" or task == "Predictive Locations":
    sensical  = "word"

if task == "Neutral" or task == "Predictive Blocks":
    location_info = ""
elif task == "Predictive Locations":
    location_info = ", either on the left or right of the screen,"

inst1 = create_inst("Welcome to the study! It's a pretty straight-forward " +
                    "design. You'll repeatedly see images" + location_info +
                    " overlaid with a word. All you have to do is say aloud " +
                    "the identity of the picture (regardless of the " +
                    sensical + " overlaid on top).\n\nPress " +
                    "space to continue.")

inst2 = create_inst("One important part of the study is that you avoid using " +
                     "filler words like 'um'. This will significantly help " +
                     "the experimenter when he analyzes your data." +
                     continue_goback("continue"))

inst3 = create_inst("The task moves pretty fast, so you'll start off with " +
                    str(prac_trials) + " trials to get the hang of it, " +
                    "before advancing to the main portion of the experiment. " +
                    "Please do let the experimenter know either now or at " +
                    "any time if you have any questions."+
                    continue_goback("begin"))

welcmmain = create_inst("Welcome to the main portion of the experiment! " +
                        "It'll work just like the practice trials, only " +
                        "there will be many more trials. The trials will be " +
                        "split among " + str(blocks - 1) + " breaks, and " +
                        "the whole study should total no more than 25 " +
                        "minutes (probably less).\n\nPress space to continue.")

main_prev = create_inst("Again, please do avoid using filler words if you " +
                        "can. Let the experiment know if you have any " +
                        "questions, or otherwise, press the spacebar to begin!")

def break_message(block_num):
    return create_inst("You've reached break " + str(int(block_num) - 1) +
                       " of " + str(blocks - 1) + ". This break is " +
                       "self-timed, so whenever you're ready press spacebar " +
                       "to continue the study.")

thanks = create_inst("Thank you so much for your participation! Let the " +
                     "experimenter know that you're finished, and he'll set " +
                     "up the 1-minute, post-study demographic survey on " +
                     "this computer.")

def blockdelay():
    frameN = -1
    while frameN < to_frames(1.5):
        frameN += 1
        win.flip()

##-------------------------------RUN THE INSTRUCTIONS-------------------------##

def instr_list(thelist):
    advance = 0                                                                 # A variable that advances the instruction screen, as well as lets them go back to see a previous instruction screen

    while advance < len(thelist):
        if event.getKeys(keyList = ["space"]):
            advance += 1
        elif event.getKeys(keyList = ["b"]):
            if advance > 0:
                advance -= 1
        for i in range(len(thelist)):
            if advance == i:
                thelist[i].setAutoDraw(True)
            else:
                thelist[i].setAutoDraw(False)
        win.flip()

    blockdelay()


##--------------------------FINALLY RUN THE TRIAL-----------------------------##

instr_list([inst1, inst2, inst3])
mic_1.record(sec=100000, block=False)                                           # Sets an impossibly long time-out window so recording will last the whole experiment
globalClock = core.MonotonicClock()                                             # To track the time since experiment started
for trial_num, (trial_key, trial_vals) in enumerate(adict.items()):
    trial_num -= prac_trials
    if trial_num % trials_per_block == 0:
        if trial_num == 0:                                                      # When practice trials are over, run the instructions for the main task
            fix.setAutoDraw(False)
            instr_list([welcmmain, main_prev])
        elif trial_num < main_trials:
            instr_list([break_message(trial_vals[4])])
        fix.setAutoDraw(True) #ideally get rid of this line and the one two below
        blockdelay()
        fix.setAutoDraw(False)
    runTrial()

mic_1.stop()
instr_list([thanks])


thisExp.saveAsWideText(filename + '.csv')
thisExp.saveAsPickle(filename)                                                  # https://psychopy.org/builder/settings.html#data-settings
thisExp.abort()                                                                 # ensure everything is closed down or data files will save again on exit
win.close()
core.quit()
