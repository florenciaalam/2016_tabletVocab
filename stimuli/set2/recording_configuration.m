clear;

%% get parameter for recording (CAN BE PERSONALIZED)
%% recording parameter
record_parameter.speaker_id                         = 'ac'; % code name for the speaker: string or number
record_parameter.n_bloc                             = 1; % code name for the bloc: string or number:(to change each time you launch a new recording session) 
% stimuli range for recording
record_parameter.starting_trial                     = 1; % the number of the first stimulus
record_parameter.ending_trial                       = 'end'; % the number the last stimulus; put 'end' if the last stimulus is the last stimulus of the table

%% visual display
record_parameter.stimuli_visual_size                = 25; % stimuli size on the screen
% if you record long sentences, you can consider using the following
% display parameters.
record_parameter.string_to_paragraph                = 'no'; % 'yes' in the case that you want to automatically return lines which are too long.
record_parameter.inter_line_space                   = 2; % inter line space: must be >= 1
record_parameter.visual_range_horizontal            = 0.8; % the horizontal range that the paragraph will cover on the monitor.

%% audio display
% Normally you do not need to change the audio display. You might need to change
% it when you are using an audio card other than Lexicon Lambda. 
% Go to 'project_stimuli_record.m' to change them. and remember to change
% it back when you are done.

%% manual control
record_parameter.next_trial_key                     = 'Right'; % key name to go to the next trial (Right by default)
record_parameter.current_trial_key                  = 'Down'; % key to redo the current trial (Down by default)
record_parameter.last_trial_key                     = 'Left'; % key to redo the last trial (Left by default) 

%% input file
% path and filename
record_parameter.mother_folder                      = 'C:\lscp_manip\audio_recording\alexcristia'; % folder containing the the file for input_table
record_parameter.stimuli_table_file_name            = 'stimuli.txt'; % filename for the file containing the input_table: xls, mat, txt, dat, csv, etc.
% specific parameters for different file types
record_parameter.input_file_sheet                   = 1; % only for xls files: number of sheet where 
record_parameter.stimuli_table_variable_name        = 'stimulus'; % only for mat file: the variable name for the input_table
record_parameter.input_file_delimiter               = '\t'; % only for text (txt, dat, csv, etc), not important if you only have 1 column; '\t' for tab
% stimuli randomzation
record_parameter.randomize_table                    = 'no'; % no, overall or pseudo (ask Yue if you want to use pseudo)
record_parameter.randomize_parameter                = {'condition', 3};

%% output files
record_parameter.filename.filename_formula          = {'name', 'n_record'}; % the elements to create name for recorded audio files: these elements correspond to the column titles in your stimuli table              
record_parameter.filename.file_type                 = 'wav'; % output audio file type
record_parameter.output_file_type                   = 'xls'; % the file type of the output table: 'xls', 'mat' or text file types (dat, txt, csv etc.) This table will contain your input stimuli table and add additional columns during the recording
record_parameter.output_file_delimiter              = ';'; % only for text files

%% specific need
record_parameter.UTF8                               = 'no'; % 'yes', if you have UTF8 characters in your stimuli_table (e.g., Chinese characters)

%% break
record_parameter.pause_go                           = 'manual'; % no, manual or systematic
record_parameter.manual_pause_key                   = 'p'; % the key that triggers the break: 'p' by default. only for manually controled break
                                                           % you can get out of the break by pressing one of three 'trial_control_keys' in the section 'manual control'.
% if you use systematic break, configure the following parameters.
record_parameter.pause_per_block                    = 0; % total number of breaks within a block. Only for systematic break
record_parameter.min_pause_time                     = 0; % minimal time for a break (in second). Only for systematic break
record_parameter.pause_instruction                  = ['Nous allons reprendre la tâche\n\n\n\n'...
                                                       'Pour chaque phrase ou mot, veuillez le lire silencieusement pour une première fois\n\n\n'...
                                                       'et, par la suite, le lire à voix haute d''une manière naturelle,\n\n\n'...
                                                       'comme si vous le disiez dans une conversation reelle.']; % instructions that will appear before that the recording restarts from the pause. 
record_parameter.instruction_size                   = 20; % instruction size (could be different with that for stimuli)

%% special recording control
%% automatic ending
% If 'yes', the program will automatically detect the end of the speaker's
% recording for each trial. This is done by measuring the accumulated
% length of silence after the onset of recording. 
record_parameter.automatic_ending                   = 'no'; % default is 'no'
record_parameter.max_recording_time                 = 5; % maximum recording time from the onset of the actually signal
record_parameter.min_silence_duration               = 0.7; % in second; minimal silence duration to be consider as the end of recording
% define the silence level: either through an initial measurement or with a
% manually entered value 
record_parameter.measuring_silence                  = 'yes'; % whether you measure the silence level in the beginning of the test
record_parameter.silence_level                      = 0.001; % this option is only useful with measuring_silence turned to 'no'; manually entered environmental silence level.
% after the automatic cutting, two button will appear on the screen for the
% speaker to decide whether he wants to re-record the current stimulus or continue
% to the next stimulus
record_parameter.redo_button_name                   = 'redo'; % the name of the redo button (appears on the left side of the screen)
record_parameter.continue_button_name               = 'continue';% the name of the contintue button (appears on the right side of the screen)

%% recording amplitude check
% check the amplitude of the recorded stimuli to assure the
% signal-to-noise ratio
record_parameter.audio_amplitude_check              = 'no'; % default is 'no'
record_parameter.audio_amplitude_measure            = 'peak'; % 'peak': peak amplitude 
record_parameter.audio_amplitude_threshold          = 0.01; % minimal amplitude for audio stimuli (need to be adapted to either peak or rms measurements)   
% message in the case of too weak amplitude
record_parameter.error_message                      = ['Le niveau de la voix est trop faible.\n\n\n'...
                                                       'Nous allons réenregistrer ce stimulus.\n\n\n'...
                                                       'Veuillez parler un peu plus fort, s''il vous plaît'];
record_parameter.error_message_display_mode         = 'manual'; % put a number to determine the duration (in second) for the message display; 
                                                         % put 'manual' for manual triggering of the next trial, in this case you need to press the current_trial_key to re-record the current stimulus
% The following option is only relevant when "error_message_display_mode"
% is set to 'manual'
record_parameter.manual_control_message            = '-- Press the "continue" key to continue --'; 
 

%% execute the recording system (DO NOT CHANGE)
output_table = project_stimuli_record(record_parameter);