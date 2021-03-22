**Scripts and data to replicate the analyses in: Karjus et al 2021, "Conceptual similarity and communicative need shape colexification: an experimental study"**

Code:

- analysis.R - runs the models descibed in the paper
- stimgenerator.R - generates stimuli for the experiments
- expgen_scripts.R - all the functions required to do the above; includes list of required R packages (installed automatically on first run if required, when sourced from one of the above files)
- game_app - folder with the source code of the Shiny app used to run the game (there's also a "demo mode" that can easily be run without the need to set up a server or data transfer options, just requires running one line of code from app.R)

Data:

- parsed_results_for_glmm.csv - precompiled tab-separated data (see paper for parsing details), load this in analysis.R to easily run the models
- RESULTS.zip - a zipped folder with all the raw data; to replicate the parsing process, unpack and set the relevant path in analysis.R
- examplegame.csv - full log of a single game as an example; all game data follows this format (see paper for details on how we analyze colexification in this)

