**Scripts and data to replicate the analyses in:<br>Karjus, A., Blythe, R.A., Kirby, S., Wang, T. and Smith, K. (2021), Conceptual Similarity and Communicative Need Shape Colexification: An Experimental Study.** Cognitive Science, 45: e13035. https://doi.org/10.1111/cogs.13035

Code:

- All analyses were run on R version 4.0.3 (see expgen_scripts.R for package details)
- analysis.R - runs the models descibed in the paper
- stimgenerator.R - generates stimuli for the experiments
- expgen_scripts.R - all the functions required to do the above; includes list of required R packages (installed automatically on first run if missing when this script is sourced)
- game_app - folder with the source code of the Shiny app used to run the game (there's also a "demo mode" that can easily be run without setting up a server and data transfer pipeline; it just requires running one line of code in the header of app.R)

Data:

- parsed_results_for_glmm.csv - precompiled tab-separated data (see paper for parsing details), load this in analysis.R to easily run the models
- RESULTS.zip - a zipped folder with all the raw data; to replicate the parsing process, unpack and set the relevant path in analysis.R
- examplegame.csv - full log of a single game as an easily accessible example; all game data follows this format (see paper for details on how we analyze colexification in this)
- Two external datasets are used: [Simlex999](https://fh295.github.io/simlex.html) word similarity scores and Fasttext English word embeddings [https://fasttext.cc/](Fasttext) (see paper for details)

