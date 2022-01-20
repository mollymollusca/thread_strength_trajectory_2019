# threads
1. Determine the change in force at each individual thread break
2. Perform statistics on the effect of food and temperature on thread strength and thread number. 

Information about github accounts: This code was originally published under earobert github account. I am switching to using a different github account, mollymollusca, and so will be publishing any and all updates here. 

In this code there are a number of 'step' scripts and I saved updated versions as higher #'s, (e.g."v3" would be a later version compared to ".v2")


step_2_threads_2.3.1.R
The code version step_2_threads_2.2 doesn't capture all peaks and valleys
so I'm checking out how to get them all
Note that this code is based off of step_2_threads_2.0 and not 2.1
This code steps through the files and generates a set of potential deltas for each.
There are two functions, FindValley, and PeaksAndValleys written by Mike Pantanos.
Note that there are issues with a few deltas here and there not getting picked up by the code.
This is why we created 2.1. However with 2.1 there is the issue that there are too many false positives due to sinusoidal oscillations in force following a thread break. 

step_3.A.QC_threads.v3.R
This is Part A of step 3 threads.
First we do the QC and save new delta files
Part B will be collating all of the delta files together, and will be very simple.

step_3.B.QC_threads.v3.R
This is Part B of step 3 threads.
I take the delta files wich now include QC info
and use this to plot histograms, calculate medians, and create one summary sheet.

step_4_threads_v2.R
Organization for stats and plotting
The v2 version is more organized.

step_5_threads_catching_all.R
This code finds threads that fell through the cracks in previous code.
Goal - make sure no samples fall through the cracks.
Check that the samples has less than 5 good deltas identified within the normal range of deltas
Run the analysis on the extra samples

step_6_threads_byhand.R
"Final last plots for anything I need to do by eye"
"Using web plot digitizer"

step_7_sameas3.B.QC_threads.v3.R
"This is just repeating step3B, where all the deltas are put in one file"

step_8_threads_sameas4_.v3.R
"Organization for stats and plotting"
"This is the same as step 4 but I'm also incorporating the inputted data"
