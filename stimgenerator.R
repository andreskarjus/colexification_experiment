
# Script to generate stimuli for the colexification experiment
# Uses a combination of computational similarity (wikipedia embeddings) and crowd-sourced (Simlex999) data


SCRIPTPATH = "expgen_scripts.R" # full path to where the expgen_scripts.R file is
DROPDIR    = "experimentdb"     # full path to where the stims should be saved

source(SCRIPTPATH)  # load required scripts


# Define  paths
simlexpath = "/SimLex-999.txt"   # path to the Simlex999  dataset txt file
dictpath   = "/en_GB-large.dic"  # path to some English wordlist
# we used https://sourceforge.net/projects/wordlist/files/speller/2019.10.06/hunspell-en_GB-large-2019.10.06.zip/download
vectorspath = "/fasttext_wikipedia2019.txt" # path to embeddings; we used fastext trained on wikipedia dump 2019
freeassocpath = "norms.txt"
# path to a single file containing the The University of South Florida word association, rhyme, and word fragment norms
# Nelson, D. L., McEvoy, C. L., & Schreiber, T. A. (1998). The University of South Florida word association, rhyme, and word fragment norms. http://w3.usf.edu/FreeAssociation/



# Set parameters
params = list(
  wordlen_min       = 3     , # length of words from simlex to use
  wordlen_max       = 7     ,
  assocmax          = 1     , # filter out "free-associated" words ([0,10] but most are <1)
  poslist           = "N"   , # which POS to use
  nwords            = 10    , # size of natural language lexicon/meanings to use per stim set (see also nlang below!)
  nstims            = 80    , # how many stim sets to create (i.e. how many dyads will be run)
  nbaseline         = 40    , # how many stims sets (out of nstims) are in the baseline/control condition
  nsimilar          = 3     , # how many pairs to have which are > simthreshold_high
  allowduplicates   = 24,     # how many max duplicate target pairs across stims 
                              # 3 for 40 stims if 1 pair, 5 if 2)
                              # making it equal to nstims will disable the check alltogether
  simthreshold_high = 8     , # above this is a "similar" word [0,10] - this is for simlex
  simthreshold_low  = 0.2   , # below this is a "not similar" word [0,1] - this is for the vector cosine
  freeassoc_threshold=0.02  , # below this is "not freely associated" [0,1] - for the USF norms
                              # with nwords=20 and nsimilar=2 0.3 was minimum feasible, can do less if smaller lexicon  
  editmin          = 3      , # edit distance threshold for both natural and artificial words; 
                              # uses Optimal String Alignment (Levenshtein + transposition)
  allowsameinitial  = F     , # allow target pairs to have the same initial letter? (substrings are not allowed though, so e.g. hero-heroine won't be in there)
  
  #### artificial language and stims:
  #
  nlang     = 7             , # size of artificial language lexicon (see also nwords above!)
  langlen   = 2             , # uniform length of artificial words, in number of CV syllables
  vowels    = "aeoui"       , # string of allowed vovels
  cons      = "qwtpsfhnmrl" , # string of allowed consonants
  transl    = c("vdbz", 
                "wtps")       , # letters in the English wordlist to homogenize to catch stims that are 
  # similar-sounding o English words; must be 2 strings of equal(!) length, 
  # those in 1 transformed into those in 2
  maxinitial = 2          , # times each consonant is allowed to start an artificial word per stim set (high=disables)
  constraininitials = T     , # Constrain artificial words: initial letter cannot overlap with English stims
  
  ### experiment options
  #
  #nrounds   = 114           , # number of rounds for each dyad
  pairmultiplier = c(targets=11,     # in each game in expm condition, how many pairs will be targets
                     distractors=5,  # ...distractor pairs (don't include target meanings)
                     halftargets=2,  # ...pairs of mixed target+distractor (also targets from different pairs)
                     baseline=3      # multiplier for all pairs/uniform distribution in the baseline
                     ), 
  burnin    = 3   # first (1/burnin)*(n rounds) will be training period; will attempt to balance pairs
) 



# LOAD DATA
simlex = read.csv(simlexpath, sep="\t", header=T, stringsAsFactors = F); w=unique(unlist(simlex[simlex$POS %in% params$poslist,1:2]))
engdict = readLines(dictpath) %>% gsub("/.*", "", .) %>% tolower()
freenorms = read.csv(freeassocpath, sep=",", header=T, stringsAsFactors = F, strip.white = T) %>% filter(QPS=="N" ) %>% .[,c(1:2,6:7)] %>% mutate(CUE=tolower(CUE), TARGET=tolower(TARGET), BSG=as.numeric(BSG), FSG=as.numeric(FSG)) %>% filter(CUE %in% w | TARGET %in% w) %>% mutate(score=pmax(FSG, BSG,na.rm = T)) %>% .[c(1:2,5)]
freenorms = rbind(freenorms, setnames(freenorms[, c(2,1,3)], c("CUE", "TARGET", "score"))  )

## Import and subset vectors; data.table speeds the import up
# 
simmat = fread(vectorspath, sep=" ", header=F, skip = 1, quote="")
simmat[which(is.na(simmat[,1])),1] = as.character(runif(1)) # one missing rowname for some reason
simmat = data.frame(simmat, row.names = 1)
simmat = simmat[unique(unlist(simlex[,1:2])), ]
simmat = data.matrix(simmat)
simmat = sim2(simmat)   # symmetric similarity matrix the size of the simlex lexicon


## Run stim generator:
# 
set.seed(1)
stims = list()
stims$words = wordsampler(
   params
 , simlex
 , simmat
 , freenorms
 , excludes=c("bible", "log", "suds", "god", "mood") # exclude these from simlex
 , balancetimes = 2
 )
sort(table( attr( stims$words, "globalx" ))) # how many pairs repeated in set

stims$lang = langgen(
   params$langlen
 , engdict
 , params$maxinitial
 , params$vowels
 , params$cons
 , params$transl)

stims$stims = stimgen(
   stims$words
 , stims$lang
 , params$nlang
 , params$editmin
 , params$cons
 , params$maxinitial
 , params$constraininitials)


for(i in seq_along(stims$words)){
  print(paste(stims$words[[i]], collapse=" "), quote=F)
  print(paste(stims$stims[[i]], collapse=" "), quote=F)
  print(" ", quote=F)
}
# 

generated_stims = stimcombinator(stims, params)
c(sapply(generated_stims, function(x) x$targets)) %>% table() %>% sort()

saveRDS(generated_stims, file=file.path(DROPDIR, "generated_stims.RDS"))


## Generate bookkeeping file
#
bookkeeping = data.frame(nexp = 1:length(generated_stims),
                         isbaseline=sapply(generated_stims, function(x) x$pairs$isbaseline[1]),
                         starttime = as.POSIXct(NA), 
                         endtime = as.POSIXct(NA),
                         f1 = as.character(NA),
                         f2 = as.character(NA),
                         names = as.character(NA),
                         code1 = codesgen(length(generated_stims), 1),
                         code2 = codesgen(length(generated_stims), 2),
                         stringsAsFactors = F
                         )
saveRDS(bookkeeping, file="bookkeeping.RDS")

