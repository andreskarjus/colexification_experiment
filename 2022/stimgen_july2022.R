


# install those if not installed:
library(tidyverse)
library(data.table)
library(text2vec)
library(stringdist)

# define paths here - replace mine
#
vectorspath = "C:/Users/Andres/korpused/fasttext/cc.en.300.vec"  # full path to English fasttext (cc.en.300.vec)
nounlistpath = "http://www.desiquintans.com/downloads/nounlist/nounlist.txt"  # don't change this
# wordfreqpath = "C:/Users/Andres/korpused/wordfreq/en.txt"        # full path to wordfreq 1.4 if that is used

# load data
vecs = fread(vectorspath, sep=" ", header=F, skip = 1, quote="") %>% 
  filter(!is.na(V1)) %>% 
  data.frame(row.names = 1) %>% 
  as.matrix()
nounlist = readLines(nounlistpath)
# wordfreq = read.csv(wordfreqpath, header=F, quote="") %>%   # optional
#   mutate(V2=as.numeric(V2)) %>% 
#   filter(!duplicated(V1) & !is.na(V2))


# defines function; run below
stimgen = function(vecs, 
                   #wordfreq, 
                   nounlist, nstims=10, charlength=c(6,6), 
                   maxsim=0, mineditdist=5, 
                   exlude_same_initial=T,
                   exclude="s$|ed$|ing$|dn$|[[:punct:][:space:]]|[^a-z]",
                   #freqrange=c(exp(-18), exp(-8)),
                   maxoverlaps=7,
                   stopwhen=50, 
                   ntries = 100
                   ){
  # oknouns = wordfreq %>% filter(V2> freqrange[1] & V2 < freqrange[2] & V1 %in% nounlist)
  oknouns = nounlist
  
  vecs2=vecs[(
    (nchar(rownames(vecs)) %in% (charlength[1]:charlength[2])) & 
               !grepl(exclude, rownames(vecs)) & 
               rownames(vecs) %in% oknouns
  ),]
  
  print(paste("Looping over", nrow(vecs2), "available words (left after initial filtering)"))
  stims=matrix(NA,0,10)
  ii=0
  for(i in 1:nrow(vecs2)){
    iword=rownames(vecs2)[i] # seed word
    
    # first filter word that fit with seed word
    sims = sim2(vecs2, vecs2[iword,,drop=F])[,1]
    editsims = stringdist(iword, rownames(vecs2))
    wordpart =   # relevant if larger nchar range used
      grepl(iword, rownames(vecs2),ignore.case = T) | 
      sapply(rownames(vecs2), grepl,x=iword,ignore.case=T )  
   
    sameini = F
    if(exlude_same_initial){
      sameini = substr(rownames(vecs2), 1,1) == substr(iword,1,1)
    }
    samp = c(iword, 
             names(sims)[sims <= maxsim & 
                           editsims >= mineditdist & 
                           !wordpart & 
                           !sameini ])
    
    # then filter those further so that conditions are matched
    # if there's enough to sample from
    if(length(samp)>= (nstims)){
      xsims = sim2(vecs2[samp,]) %>% {diag(.)=-Inf;.}
      xedit = stringdistmatrix(samp) %>% as.matrix() %>% {diag(.)=Inf;.}
      if(exlude_same_initial){
        xinis = substr(samp,1,1) %>% sapply(function(x)x==.) %>% {diag(.)=F;.}
      } else {
        xinis=matrix(F, length(samp), length(samp))
      }
      for(j in nrow(xsims):1){ # backwards, prioritizing seed word
        if(any(xsims[j,] > maxsim) | 
           any(xedit[j,] < mineditdist) |
           any(xinis[j,])   # same initial letter (if using this option)
           ){
          # exclude word from further comparison if violates any conditions, remove later
          xsims[,j] = -Inf
          xedit[,j] = Inf
          xinis[,j] = F
        }
      }
      samp2 = apply(xsims, 2, function(x) all(is.infinite(x))) %>% {names(.)[!.]}
      
      # if still anything to sample from:
      if(length(samp2)>= nstims ){
        nfound=nrow(stims)
        if(nfound>1){
          # weighted random search (because full combination space is too large)
          weights = unlist(stims, F,F) %>% table() %>% 
            {max(.)-.+1}
          weights2 = weights[samp2] %>% replace(is.na(.), max(weights)*100)
          candidates = sapply(1:ntries, function(x) sample(samp2, nstims, prob = weights2) %>% sort) %>% 
            t %>% rbind(stims, .) %>% unique()
          
        } else {
          candidates = sapply(1:ntries, function(x) sample(samp2, nstims) %>% sort) %>% t %>% 
            unique() 
        }
        noverlaps = apply(candidates,1, function(x1) 
          apply(candidates,1, function(x2) length(intersect(x1, x2)))) %>% 
          {.[upper.tri(.,diag = T)]=-Inf;.} %>% apply(.,1,max)
        stims = candidates[noverlaps <= maxoverlaps, ]
        
        if( (nrow(stims)-nfound) > 0 ){
          print(paste("Found", (nrow(stims)-nfound), "stim set(s) after", i, "attempts;", 
                      nrow(vecs2)-i, "seed words remaining."))
        }
        
      }
      # if target reached:
      if(nrow(stims)>=stopwhen){
        stims2 = stims[sample(1:nrow(stims), stopwhen),]
        print(paste(nrow(stims), "stim sets found, stopping early, returning requested", 
                    stopwhen, "sets, consisting of",
                    length(unique(c(stims2))), "unique words."))
        return(stims2)
      }
    }

  }
  print(paste(nrow(stims), "stim sets found, containing", length(unique(c(stims))), "words. Exhausted word list (consider relaxing conditions)"))
  return(stims)
}


# run this to generate stims:
stims = stimgen(
  vecs,                           # fasttext vectors (import above)
  # wordfreq,                     # could also filter by wordfreq1.4 frequencies, see function above
  nounlist,                       # list of common nouns to check against
  nstims=10,                      # number of stimuli words per set
  charlength=c(6,6),              # word length min, max
  maxsim=0.3,                     # max cosine sim, ranges [-1,1] (but mostly >0)
  mineditdist=3,                  # edit distance threshold
  exlude_same_initial=F,          # disallow repeating same initial letter in stim sets (big constraint)
  exclude="s$|ed$|ing$|dn$|[[:punct:][:space:]]|[^a-z]",  # words to exclude based on regex
  maxoverlaps=7,                  # max n overlaps (intersection) allowed between candidate stim sets 
                                  # lower to get more unique stim words, increase to make finding stims easier
  # freqrange=c(exp(-18), exp(-8)), # not used for now
  stopwhen=100,                    # target number of stim sets
  ntries=1000                     # number of weighted random searches to do to find new unique stim combos
)

table(c(stims)) %>% sort # words in stim sets
save(stims, file="stims.RData")


