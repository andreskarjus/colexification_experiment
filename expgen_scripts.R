#
#
# All the functions used in the stim generation process and later analysis
# Sourced in the respective scripts (see stimgenerator.R and analysis.R)
#
# Installs these packages if missing on first run:
#
reqpac = setdiff(c("tidyverse", "Hmisc", "data.table", "stringdist", "lme4", "entropy", "gtools", "patchwork", "ggbeeswarm", "ggrepel", "shadowtext", "scales", "colorspace"), installed.packages()[,"Package"] )
if(length(setdiff(reqpac, ))>0){
  install.packages(reqpac)
}

# Load packages
# the important stuffs
library(tidyverse)
library(Hmisc)
library(data.table)
library(stringdist)
library(lme4)
library(entropy)
library(gtools)

# only for plotting
library(patchwork)
library(ggbeeswarm)
library(ggrepel)
library(shadowtext)
library(scales)
library(colorspace)


# these analyses were run using these version:
# tidyverse  "1.3.0"  
# Hmisc      "4.4-2"  
# data.table "1.13.6" 
# stringdist "0.9.6.3"
# lme4       "1.1-26" 
# entropy    "1.2.1"  
# gtools     "3.8.2"  
# patchwork  "1.1.1"  
# ggbeeswarm "0.6.0"  
# ggrepel    "0.9.1"  
# shadowtext "0.0.7"  
# scales     "1.1.1"  
# colorspace "2.0-0"



# function to sample suitable sets from simlex
wordsampler = function(params 
                       , simlex
                       , simmat
                       , freenorms
                       , excludes=c()
                       , balancetimes=1
                       , balanceweighted=T
                       ){
  
  nwords  =  params$nwords
  nstims  =  params$nstims
  nsimilar  =  params$nsimilar
  simthreshold_high  =  params$simthreshold_high
  simthreshold_low  =  params$simthreshold_low
  assocmax  =  params$assocmax
  editmin  =  params$editmin
  allowduplicates  =  params$allowduplicates
  allowsameinitial  =  params$allowsameinitial
  poslist  =  params$poslist
  wordlen_min  =  params$wordlen_min
  wordlen_max  =  params$wordlen_max
  freeassoc_threshold  =  params$freeassoc_threshold
  
  if(nsimilar*2 > nwords){stop("Requested number of similar words exceeds number of total words")}
  if(wordlen_min > wordlen_max){stop("Min word length cannot be > max word length!")}
  
  simlex = simlex %>% 
    dplyr::filter(nchar(word1) %in% (wordlen_min:wordlen_max)  & 
             nchar(word2) %in% (wordlen_min:wordlen_max) ) %>% 
    dplyr::filter(POS %in% poslist) #%>% 
  #filter(Assoc.USF. < assocmax) %>% 
  #filter({stringdist(word1,word2)>=editmin })
  vocab = unique(unlist(simlex[,1:2]))
  vocab = setdiff(vocab, excludes)  # exclude some words incl proper nouns which appear as nouns in Simlex
  simlex = simlex[!(simlex$word1 %in% excludes | simlex$word2 %in% excludes),  ] # same filter for target set

  
  is_substring = rep(F, nrow(simlex))
  # in_freenorms = rep(F, nrow(simlex))
  for(i in 1:nrow(simlex)){
    is_substring[i] = grepl(simlex$word1[i], simlex$word2[i]) | grepl(simlex$word2[i], simlex$word1[i])
    # in_freenorms[i] = all(simlex$word1[i] %in% freenorms$CUE & simlex$word2[i] %in% freenorms$CUE)
  }
  
  # subset where targets will be drawn from:
  s = simlex[
    which(
      simlex$SimLex999 >= simthreshold_high &  # similar
        simlex$Assoc.USF. < assocmax &           # but not associated
        stringdist(simlex$word1, simlex$word2) >= editmin & # and not form-similar
        !is_substring # &   # AND also one word should not be substring in other, eg hero-heroine
        # in_freenorms       # And has free assoc norms score (for at least something)
    ) , ,drop=F
    ]
  # leaves 13 suitable candidate pairs only
  if(!allowsameinitial){
    s = s[which(
      substr(s$word1,1,1) != substr(s$word2,1,1)
    ),,drop=F]
  } # then 15
  
  # make labels
  s$labs = apply(s[, c("word1", "word2"), drop=F], 1, paste0,collapse=" " )
  ns = 1:nrow(s)
  
  if(balanceweighted){
  # weighted sampling: to have more uniform target pair distribution
    ssample=function(s,ns,globalx,sq=1){ 
      w = table(globalx)[s$labs] %>% as.numeric() %>% replace_na(0)
      w = (max(w)+1-w)^sq
      return( s[sample(ns, 1, prob = w), c("word1", "word2"), drop=F] )
    }
  } else{   # hacky patch to retain past compatibility
    ssample = function(s,ns,globalx=NULL,sq=NULL){
      return( s[sample(ns, 1), c("word1", "word2"), drop=F] )
    }
  }
  
  stims = vector("list", nstims)
  vocabdist = stringdistmatrix(vocab, useNames = T) %>% as.matrix()
  globalx = c() # storage for all targets from all stims; used for allowduplicates check
  adup2 = rep(ceiling(allowduplicates/(rev(1:balancetimes))), 
              each=ceiling(nstims/balancetimes))
  
  for(i in 1:nstims){
    # print(" ", quote=F); print(i)
    if(nsimilar>0){
      # Basic case, just one pair:
      # if required try first if n duplicates in stims already
      if(allowduplicates<nstims & i>1){
        x = ssample(s,ns,globalx)
        it=0
        while( sum(globalx %in% paste(x, collapse=" ")) >= adup2[i] ){
          it=it+1
          #cat(".") # debug
          if(it>1000){
            stop(paste("Found", i, "stims, but now stuck at finding more suitable target pairs, so stopping; adjust allowduplicates to higher value and restart."))
          }
          x = ssample(s,ns,globalx)
        }
      } else {
        x = ssample(s,ns,globalx)
      }
      
      
      # More than one pair within single stim:
      if(nsimilar>1){
        npairs = 1
        it=0
        it3=0
        while(npairs<nsimilar){ # skips if only 1 nsimilar pair requested  
          # look for similar pairs that would not be inter-similar, keep at it until found:
          it=it+1
          if(it>100){
            # to avoid local minima:
            print("Looking for 1+ pairs per stim, stuck, tried 100 times, seeding with a new one to get out of dead end")
            it=0; npairs = 1; it2=0; it3=it3+1
            if(it3>100){stop("Re-seeded still 100x couldn't find solution")}
            x = ssample(s,ns,globalx,2)
            while( sum(globalx %in% paste(x, collapse=" ")) >= adup2[i] ){
              it2=it2+1
              if(it2>100){stop("Restarted but can't find solution; adjust allowduplicates!")}
              x = ssample(s,ns,globalx,2)
            }
          }
          xnew = ssample(s,ns,globalx)
          # avoid duplicates, skip if found:
          xnewtmp = unlist(xnew, F,F)
          if( any(xnewtmp %in% unlist(x, F,F) ) ){ next }
          # if stims must have unique pairs, also this check:
          if(allowduplicates<nstims){
            if( sum(globalx %in% paste(xnewtmp, collapse=" ") ) >= adup2[i] ){ next }
            # (checking for first member is enough, given simlex has no duplicate pairs
            # so words are currently allowed to recur with allowduplicates, but pairs not
          }
          
          x2 = rbind(x, xnew)
          xm = max(
            c(simmat[x2[,1],x2[,1] ] %>% {diag(.)=0; return(.)}, 
              simmat[x2[,2],x2[,2] ] %>% {diag(.)=0; return(.)}
            ))
          xe = min(c(stringdistmatrix(unlist(x2, use.names = F)))) # dist object, diagonal excluded
          # if new pairs are ok, add (and keep looking for more if necessary):
          if(xm <= simthreshold_low & xe >= editmin){
            x = x2
            npairs=npairs+1
            gtmp = c(globalx, unname(apply(x,1, paste, collapse=" ")) )
            if(any(table(gtmp)>allowduplicates)){next}
          }
        } # end while
      } # end if
      
      globalx = c(globalx, unname(apply(x,1, paste, collapse=" ")) ) 
      x = c(t(x), use.names = F) # dataframe to vector
      
    } else {
      x = c() # if no pairs should be similar
    } # done finding target pairs, now:

    
    ### keep adding y distractors until limit reached:
    nneeded = nwords-length(x)
    y = rep(NA, nneeded)  # distractor array
    # see if actually possible:
    if(nneeded>length(vocab)){  
      stop("Requested word set is larger than Simlex, adjust or use larger pool")
    } else {
      if(nneeded>(length(vocab)/2)){
        print("Requested word set is half the size of Simlex, solution might be hard to find")
      }
    }
    # start:
    it=0
    while(any(is.na(y))){ # if the loop below fails forever, stop; brute force search basically
      if(it>1000){ 
        stop("Restarted distractor set search 1000 times already, couldn't find solution so stopping, maybe adjust something? (probably relax sim and association thresholds if larger word sets requested")
      }
      it=it+1
      candidates = sample(setdiff(vocab, x) ) # shuffle the vector so no need to keep sampling
      cl=length(candidates)
      i2=1
      for(ii in 1:nneeded){
        ok = c(F,F,F,F, F)
        while(!all(ok) & i2 <= cl ){
          y2 = candidates[i2]
          # test that new one has low similarity and form similarity to rest of y set:
          if(ii>1){
            ok[1] = all(simmat[y2, na.omit(y), drop=T] < simthreshold_low  )
            #if(!ok[1]) i2 = i2+1; next()
            ok[3] = all(vocabdist[y2, na.omit(y), drop=T] >= editmin )
           # if(!ok[3]) i2 = i2+1; next()
          } else {ok[c(1,3)]=T} # skip on first word onbviously
          # these assume there is actually an x target list btw --> change if that would be empty!
          # test that new one in y set is not similar to any in the x set:
          ok[2] = all(simmat[y2, x, drop=T] < simthreshold_low )
         # if(!ok[2]) i2 = i2+1; next()
          ok[4] = all(vocabdist[y2, x, drop=T] >= editmin )
         # if(!ok[4]) i2 = i2+1; next()
          tmp = unlist(freenorms[which(freenorms$CUE == y2 & 
                                         freenorms$TARGET %in% c(x,na.omit(y))), "score"])
          if(length(tmp)>0){ # logic: if no score, then no free association i.e. implicit 0 score 
            ok[5] = all(tmp<freeassoc_threshold) 
          } else {
            ok[5] = T 
          }
          i2 = i2+1
          #print(i2) # debug
        }
        if(!all(ok) | i2>cl ){
          break  # if couldn't find next word or ran out of candidates, the restart, try until works basically
        } else { # if candidate matches conditions then add and move on
          y[ii] = y2
        }
      }
      # print(it, quote=F) # debug
    } # this ends if required number of words found
    stims[[i]] = c(x,y)
    
  } # end stims for-loop
  stims = stims[sample(1:length(stims))] 
  # shuffle the word samples (target distro somewhat biased otherwise, depending on max duplicates value)
  attr(stims, "globalx") = globalx
  #stims = lapply(stims, unlist, use.names=F) # concatenate targets and distractors
  return(stims)
}

                                                   
                                                   


# function to generate an artificial lexicon
langgen = function(langlen, engdict, maxinitial, vowels, cons, transl){
  # homogenize kpt-gbd, w-v, z-s -- so filters also out similar sounding ones
  if(!is.null(transl)) engdict = translate(engdict, transl[1], transl[2])
  vowels = strsplit(vowels, "")[[1]]
  cons   = strsplit(cons, "")[[1]]
  
  syllables = expand.grid(cons, vowels, stringsAsFactors = F) %>%  
    apply(1,paste0, collapse="")
  # 60 combos
  partlist = replicate(langlen, syllables) %>% 
    as.data.frame(stringsAsFactors=F) %>% 
    as.list()
  lang = do.call(expand.grid, partlist) %>% 
    apply(1,paste0, collapse="") %>%
    setdiff(engdict)  # exclude english and english-sounding words
  # 2998 left
  print(paste("Generated a language of", length(lang), "unique words"))
  return(lang)
}

# function to go through stim word sample sets, and find a suitable artificial language lexicon
stimgen = function(stims_words, lang, nlang, editmin, cons, maxinitial, constraininitials){
  stimlist = vector("list", length(stims_words))
  vocabdist = stringdistmatrix(lang, unique(unlist(stims_words, use.names = F)), useNames = T )
  # langdist  = stringdistmatrix(lang, useNames = T) %>% as.matrix()
  cons=strsplit(cons, "")[[1]]
  ncons = length(cons)
  if(constraininitials){ # if initials should not overlap with language stims
      initials = substr(lang, 1,1)
  }
  for(s in seq_along(stims_words)){
    candidates = lang
    if(constraininitials){ # if initials should not overlap with language stims
      candidates = candidates[ which(!(initials %in% substr(stims_words[[s]],1,1) ) ) ]
    }
    candidates = sample(candidates) # reshuffle language
    cl=length(candidates)
    i2=1
    y = rep(NA, nlang)
    for(ii in 1:nlang){
      ok = c(F,F,F)  # not super efficient, could speed up by next'ing loop at any fail
      while(!all(ok) & i2 <= cl ){
        y2 = candidates[i2]
        if(ii>1){
          # test that new one has low form similarity to rest of y set:
          ok[1] = all(as.vector(stringdistmatrix(y2, na.omit(y))) >= editmin )
          # and that initial consonant is not repeated too many times:
          ok[3] = all( table(substr(c(y,y2),1,1)) <= maxinitial)
        } else {ok[c(1,3)]=T} # skip on first word obviously
        # test that new one in y set is not form-similar to any in the x set:
        ok[2] = all(vocabdist[y2, stims_words[[s]], drop=T] >= editmin )
        i2 = i2+1
        #print(i2) # debug
      }
      if(!all(ok) | i2>cl ){
        stop(paste("Could not find artificial lang solution for stim no", s,
                   "(which is weird, the list should be huge - unless the input letter strings are tiny, or the constraints conflict, see above)"))
      } else { # if candidate matches conditions then add and move on
        y[ii] = y2
      }
    }
    stimlist[[s]] = y
  }
  return(stimlist)
}


# for each experiment, generate the dataframe of stimuli to be shown

stimcombinator = function(stims, params, actualtargets=NULL){
  # NB! The script below uses "-" as a reserved character for dealing with meaning pairs,
  # i.e. the English nouns (meanings) should not include hyphens.
  
  pm = params$pairmultiplier
  generated_stims = vector("list", length(stims$words))
  isbaseline = c(rbind(  rep(F, params$nstims-params$nbaseline),
                         rep(T, params$nbaseline) 
                         )) # assumes equal n
  for(s in seq_along(stims$words)){
    
    # for the weak-hyp extra condition:
    if(!is.null(actualtargets)){
      ti = actualtargets*2   # select first n pairs; other target pairs will be "others"
      nsimilar2 = actualtargets
    } else {
      ti = (params$nsimilar*2)  # all targets are targets
      nsimilar2 = params$nsimilar
    }
    
    # save those in random order for display later:
    generated_stims[[s]]$stims = sample(stims$stims[[s]]) 
    generated_stims[[s]]$words = (stims$words[[s]])
    generated_stims[[s]]$targets = stims$words[[s]][1:ti] # save targets (assumes there are >0)
    targs = generated_stims[[s]]$targets
    targs2=rep(NA, length(targs)/2)
    for(j in seq(1,length(targs),2 ) ){
      targs2[j] = sort( c(targs[j],targs[j+1]) ) %>% 
                       paste0(collapse="-" ) 
    }
    generated_stims[[s]]$targetlabels = targs2[!is.na(targs2)]
    
    # prepare meaning combos
    targs = rep(NA, nsimilar2); j=1
    for(i in 1:nsimilar2){
      targs[i] = paste(stims$words[[s]][j:(j+1)], collapse="-")
      j=j+2
    }
    twords = unlist(strsplit(targs, "-"))
    com = combn(stims$words[[s]], 2)
    # this is a bit hacky but works:
    others = apply(com, 2, paste, collapse="-") # everything first
    others = setdiff(others, targs)
    halftargs = strsplit(others, "-") %>% sapply(., function(x) any(x %in% twords) ) %>% others[.]
    others = setdiff(others, halftargs)   # actual final others list now
    if(isbaseline[s]){
      pairs = c(rep(targs, each=pm["baseline"]), 
                rep(others, each=pm["baseline"] ), 
                rep(halftargs, each=pm["baseline"] ) 
                )
    } else {
      pairs = c(rep(targs, each=pm["targets"]), 
                rep(others, each=pm["distractors"] ), 
                rep(halftargs, each=pm["halftargets"] ) 
                )
    }
    
    pairs2 = as.data.frame(cbind( sapply(strsplit(pairs, "-"), `[[`, 1),
                                  sapply(strsplit(pairs, "-"), `[[`, 2) ),stringsAsFactors = F)
    colnames(pairs2) = c("pair1", "pair2")
    nr = nrow(pairs2)
    # add burnin period column
    burn = c(F,T, rep(F, params$burnin-2))
    pairs2$burnin = rep(burn, ceiling(nr/params$burnin))[1:nr]
    
    
    # next block: try to also balance the distribution of prompts, what they're told to say
    #
    nx = 0; sdx=Inf
    while(sdx>1){  # not this might need adjusting if different sample size, will not try forever:
      if(nx>100){stop("balancing prompt distribution has failed 100x times, fix something")}
      # randomize order while preserving balance in and out of burnin, and burnin first
      pairs2 = rbind(
        {pairs2[pairs2$burnin,] %>% .[sample(1:nrow(.)),] },
        {pairs2[!pairs2$burnin,] %>% .[sample(1:nrow(.)),] }
      )
      
      # which of the pair to message
      pairs2$say = as.character(NA)
      t1 = pairs2[!pairs2$burnin,1:2] %>% unlist(use.names = F) %>% table %>% {./2} %>% as.matrix() %>% .[,1] %>% mean
      t2 =  pairs2[!pairs2$burnin,1:2] %>% unlist(use.names = F) %>% table %>% {./2} %>% as.matrix() %>% .[,1]; 
      t2[] = 0
      for(i in which(!pairs2$burnin)){
        px = unlist(pairs2[i, 1:2],use.names = F,recursive = F)
        px = px[order(t2[px], decreasing = F)]
        x = which( t2[px] <= t1 )[1] %>% names()
        x = ifelse(is.na(x), sample(px,1), x) # shouldn't be needed but failsafe
        pairs2$say[i] = x
        t2[x] = t2[x]+1
      }
      sdx = table(pairs2$say[!pairs2$burnin]) %>% sd
      nx=nx+1
    }
    
    # lazy, will just copypaste above block for the burnin section (notice lack of "!"):
    nx = 0; sdx=Inf
    while(sdx>1){  # note this might need adjusting if different sample size, will not try forever:
      if(nx>100){stop(paste("balancing prompt distribution has failed 100x times in",s,"fix something"))}
      # randomize order while preserving balance in and out of burnin
      pairs2 = rbind(
        {pairs2[pairs2$burnin,] %>% .[sample(1:nrow(.)),] },
        {pairs2[!pairs2$burnin,] %>% .[sample(1:nrow(.)),] }
      )
      pairs2$say[pairs2$burnin] = NA
      t1 = pairs2[pairs2$burnin,1:2] %>% unlist(use.names = F) %>% table %>% {./2} %>% as.matrix() %>% .[,1] %>% mean
      t2 =  pairs2[pairs2$burnin,1:2] %>% unlist(use.names = F) %>% table %>% {./2} %>% as.matrix() %>% .[,1]; 
      t2[] = 0
      for(i in which(pairs2$burnin)){
        px = unlist(pairs2[i, 1:2],use.names = F,recursive = F)
        px = px[order(t2[px], decreasing = F)]
        x = which( t2[px] <= t1 )[1] %>% names()
        x = ifelse(is.na(x), sample(px,1), x) # shouldn't be needed but failsafe
        pairs2$say[i] = x
        t2[x] = t2[x]+1
      }
      sdx = table(pairs2$say[pairs2$burnin]) %>% sd
      nx=nx+1
    }
    
    pairs2$label = ""   # new - make labels, abc-sorted so easier to deal with
    for(i in 1:nrow(pairs2)){
      pairs2$label[i] = paste0(sort(c(pairs2$pair1[i], pairs2$pair2[i])),
                               collapse="-")
    }
    
    # one last doublecheck, make sure the prompts are ok:
    if(any(!(pairs2$say %in% pairs2$pair1 | pairs2$say %in% pairs2$pair2))){
      stop(paste("misalignment in prompts in",s,"something is very wrong"))
    }
    
    
    # print summary and examples on first one
    if(s==1){  
      ptabl = as.data.frame(sort(table(pairs),decreasing = T),stringsAsFactors = F)
      print(paste("The meanings are, for example:", paste(stims$words[[s]],collapse=" "), ". The game will have a total of", sum(ptabl[,2]), "rounds:" ))
      print(head(ptabl,15))
      print("...")
      print(paste("Target pair(s) make up", 
                  round(length(rep(targs, pm[1]))/length(pairs)*100,1)
                  , "% of the game,",  round(length(rep(targs, pm[1]))/length(pairs)/params$nsimilar*100,1)
                  ,"% per each pair though. The frequencies of individual meanings in the game are:"))
      print(strsplit(pairs, "-") %>% unlist() %>% table() %>% as.data.frame() %>% .[match(stims$words[[s]],.[,1], ),] )
      print("In the burnin period:")
      print(strsplit(pairs[burn], "-") %>% unlist() %>% table() %>% as.data.frame() %>% .[match(stims$words[[s]],.[,1], ),] )
      print("In the scored part of the game:")
      print(strsplit(pairs[!burn], "-") %>% unlist() %>% table() %>% as.data.frame() %>% .[match(stims$words[[s]],.[,1], ),] )
      print("How many times each gets prompted to say in the burn-in part:")
      print(table(pairs2$say[pairs2$burnin]))
      print("How many times each gets prompted to say in the scored part:")
      print(table(pairs2$say[!pairs2$burnin]))
    }
    
    generated_stims[[s]]$pairs = pairs2
    # doublecheck
    if(any(is.na(generated_stims[[s]]$pairs)) |
       nr != nrow(generated_stims[[s]]$pairs)
    ){
      stop(paste("Stim assignement failed for some reason for game no",i))
    }

   
    # add columns for experiment results:
    generated_stims[[s]]$pairs$sender = rep(1:2, ceiling(nr/2))[1:nr] # for bookkeeping
    generated_stims[[s]]$pairs$sent    = as.character(NA)
    generated_stims[[s]]$pairs$guess   = as.character(NA)
    generated_stims[[s]]$pairs$correct = as.logical(NA)
    generated_stims[[s]]$pairs$sendtime = as.POSIXct(NA)
    generated_stims[[s]]$pairs$guesstime = as.POSIXct(NA)
    generated_stims[[s]]$pairs$isbaseline = isbaseline[s]
    generated_stims[[s]]$pairs$stimn = s   # stim number
  }
  attr(generated_stims, "params") = params
  return(generated_stims)
}

# mturk code gen

codesgen = function(n, p) {
  x = rep(NA, n)
  for (i in 1:n){
    x[i]= paste0("e",i,"p",p,"c",
                   paste0( sample(c(letters,0:9),5,replace = T),collapse="" ) )
  }
  return(x)
}





### plotting ####

plotpilot = function(y, targ, ppmi=F, onlycorrect=F, vmax=11){
  vmin=ifelse(ppmi, 0,2)
  ly=c(0.56,2.44)
  y = y[!y$burnin,]
  y$say = toupper(y$say)
  titl = ifelse(y$isbaseline[1], "baseline condition, ", "target condition, ")
  titl = paste0("Expm no. ",y$stimn[1],", ", titl, round(sum(y$correct[!y$burnin])/sum(!y$burnin)*100),"%" )
  stims = targ[[y$stimn[1]]]$stims
  targ = targ[[y$stimn[1]]]$targets %>% toupper()
  ord2 = table(y$say, y$sent) %>% reshape2::melt() %>% group_by(Var2) %>% summarise(sum=sum(value)) %>% arrange(desc(sum)) %>% pull(Var2) %>% as.character() %>% union(., stims)
  if(onlycorrect){
    y = y[y$correct,]
  }
  if(ppmi){
    sims = ppmisim(y, sim=T,removesingles = T, removeallsingles = F)
    tmp = ppmisim(y, sim=F, removesingles = T, removeallsingles = F)
    titl = paste0(titl, ", PPMI")
  } else {
    tmp = table(y$say, y$sent) %>% reshape2::melt() 
    titl = paste0(titl, ", counts")
  }
  ord = c(targ, setdiff(levels(tmp$Var1), targ))
  ord = lapply(seq(1,length(ord),2), function(x) sort(c(ord[x],ord[x+1])) ) %>% unlist(F,F)

  cls = c("gray85", brewer_pal()(9)[c(4,5,7)]) # %>% show_col()
  if(ppmi){
    tmp = tmp %>% mutate(value=ifelse(value==0,NA,value)) %>% mutate(value=round(value,2))
    sims = sapply(c(1,3,5), function(x) sims$value[sims$lab==paste0(ord[x],"-", ord[x+1])])
    sims2 = rescale(sims, ly,0:1)
             
  } else {
    tmp = tmp %>% mutate(value=ifelse(value==0,NA,value))# %>% mutate(value=as.factor(value))
  }
  g = 
  ggplot(tmp, aes(y=Var1,x=Var2,fill=value, color=value))+ 
    #annotate("rect", xmin=-Inf,ymin = 0.7,xmax = Inf,ymax = 6.3, fill="forestgreen",color=NA, alpha=0.1)+
    geom_tile(width=0.7,height=0.7, linejoin="round", size=1.5)+
    geom_text(aes(label=value), color="black", size=3.5) +
    scale_y_discrete(limits=ord)+
    scale_x_discrete(limits=ord2)+
    coord_cartesian(clip = 'off', xlim=c(1,length(ord2)+0.2)) +
    theme_bw()+theme(legend.position = "none", 
                     plot.margin = unit(c(0,1,0,0),"mm"),
                     panel.background = element_rect(fill="white"),
                     panel.grid.major = element_blank(),  # color="gray12"
                     panel.border = element_blank(),
                     axis.text = element_text(size=14),
                     axis.text.y = element_text(colour = c(rep("black",6),rep("darkgray",4)),size=rel(0.8)),
                     axis.text.x = element_text(colour = "black" ),
                     axis.ticks = element_blank(),
                     axis.title = element_blank()
                     )+
    xlab("")+ylab("")+ggtitle(titl) +
    scale_fill_gradientn(colours =  cls, limits=c(vmin,vmax),na.value="gray97" )+
    scale_color_gradientn(colours =  cls, limits=c(vmin,vmax),na.value="gray97" )
  
  if(!ppmi){
   g = g +
    annotate("segment", arrow=grid::arrow(angle=90,ends="both",length=unit(0,"lines")),
              x = rep(0.4,3), xend = rep(0.4,3), y=(ly[1])+c(0,2,4),yend=(ly[2])+c(0,2,4), size=0.25)  +
     annotate("segment", arrow=grid::arrow(angle=90,ends="both",length=unit(0,"lines")), 
              x = rep(length(stims)+0.5,3), xend = rep(length(stims)+0.5,3), y=(ly[1])+c(0,2,4),yend=(ly[2])+c(0,2,4), size=0.25)
  } else {
    g = g + 
      annotate("segment", arrow=grid::arrow(angle=90,ends="both",length=unit(0,"lines")),
               x = rep(0.4,3), xend = rep(0.4,3), y=(ly[1])+c(0,2,4),yend=(ly[2])+c(0,2,4), size=0.25) +
      annotate("segment", arrow=grid::arrow(angle=90,ends="both",length=unit(0,"lines")), 
               x = rep(7.5,3), xend = rep(7.5,3), y=(ly[1])+c(0,2,4),yend=(ly[2])+c(0,2,4), size=0.25) + 
    annotate("text", x = rep(7.5,3), y=sims2+c(0,2,4), size=2, shape=45, label="-",vjust=0.3) +
      annotate("text", x = rep(7.6,3), y=sims2+c(0,2,4), label=round(sims,2), size=4,hjust=0)
  }
  g
}


#### data analysis ####
do_expmdat_binary4 = function(res,generated_stims, accthreshold, alsoburnin=F, dyadsuffix=NA, doinfandcompl=F){
  # digs out target pairs again: 
  # (should have saved separately right away)
  targs = lapply(generated_stims, function(x) x$targets)
  tarx = vector("list", length(generated_stims))
  for(i in seq_along(targs)){
    for(j in seq(1,length(targs[[i]]),2 ) ){
      tarx[[i]] = c( tarx[[i]] ,     # homogenize alphabetically
                     sort( c(targs[[i]][j],targs[[i]][j+1])) %>% 
                       paste0(collapse="-" ) )
    }
  }
  tarx = unlist(tarx) %>% unique() %>% sort()
  # another hacky version for later lookup:
  tmp=strsplit(tarx, "-")
  tarx2=c()
  for(i in seq_along(tmp)){
    x = tmp[[i]]; names(x) = rev(tmp[[i]])
    tarx2=c(tarx2, x)
  }
  if(doinfandcompl){ # additional structures if doing this
    tmptarx = sapply(tarx2, function(x) tarx[grepl(x, tarx)]) # grepl hack works bc stims all different
    dat2 = vector("list", length(res))
  }
  
  
  dat = vector("list", length(res))
  accs = sapply(res, function(x) sum(x$correct[!x$burnin])/sum(!x$burnin) )
  ok = which(accs>=accthreshold)
  for(i in ok){
    burnin = which(res[[i]]$burnin)
    x = res[[i]] %>%
      mutate(rown=1:nrow(.))   # proper trial numbers (can be rescaled later)
    
    meanings = generated_stims[[ res[[i]]$stimn[1] ]]$words
    stimtargets = generated_stims[[ res[[i]]$stimn[1] ]]$targets
    # -> need to check against stim set directly not tarx2, as weakhyp uses target words in distractors
    
    if(doinfandcompl){ # additional structures if doing this
      x$xpair = tmptarx[x$say]
      df2 = tibble()
    }
    
    df = tibble()
    for(j in 1:nrow(x)){
      meaning = x$say[j]; signal = x$sent[j]; sender = x$sender[j]
    
      if(!doinfandcompl){
        last = tail(which(x$sender == sender & x$sent == signal & x$rown < j), 1)
        # only if colexification:
        if(length(last)==1 &&           # signal has been used before by same sender
             meaning %in% stimtargets &&  # sent meaning is target in this stim set
             x$say[last] != meaning       # but it's a colexification event
             )
            {                             # using && so it won't check others if false
            a1 = x$say[last] == tarx2[meaning] # match against target pair member
            a2 = ifelse(a1, "yes", "no")
            d = tibble(colextarget  = a2,
                       copair=x$say[last],
                       meaning=meaning, 
                       signal=signal,
                       sender=x$sender[j], 
                       row=x$rown[j]
            )
            df = rbind(df, d) 
          }
      } # or:
      
      if(doinfandcompl){   # if doing the info-compl calc
        # treats as collaborative language, ignoring speaker now
        if(meaning %in% stimtargets){ # only look at targets
          lastself  = tail(which(x$say == meaning & x$rown < j), 1) 
          lastother = tail(which(x$say == tarx2[meaning] & x$rown < j), 1) 
          if(length(lastself)==1 & length(lastother)==1){ # if meaning and pairmeaning have been used
            comp = ((x$sent[lastself] != signal)+(x$sent[lastother] != signal)) # plus complexity
            cost = ((x$sent[lastself] != signal)+(x$sent[lastother] == signal)) # plus comm.cost
            d = tibble(complexity  = comp,
                       commcost = cost,
                       meaning=meaning,
                       signal=signal,
                       #sender=x$sender[j],
                       row=x$rown[j],
                       xpair=x$xpair[j]
            )
            df2 = rbind(df2, d)
          }
        }
         # old simplicity: if target xpair comes up, chech if other member last used same signal
         #last = tail(which(x$sender == sender & x$say == tarx2[meaning] & x$rown < j), 1) # only other
         #last = tail(which(x$sender == sender & x$xpair == x$xpair[j] & x$rown < j), 1) 
         # get last meaning pair ref
         # the irrelevalt distractor ones will na out, and which() will not match
         #
      }
    } # done with looping over 1 experiment
    
    if(!doinfandcompl){
      # fix experiment df:
      if(!alsoburnin){
        df$isburnin = df$row %in% burnin
        df = df %>% dplyr::filter(!(row %in% burnin) ) 
      } else {
        df$isburnin = df$row %in% burnin
      }
      df$condition = ifelse(x$isbaseline[1],"baseline", "target")
      df$dyad = x$stimn[1]
      
      if(!is.na(dyadsuffix)){
        # if concatenating multiple datasets, will need to distinguish expm/dyad numbers somehow
        # so just adding a large number there
        df$dyad = df$dyad + dyadsuffix
      }
      df$sender = paste0(df$dyad, "_",df$sender)
      dat[[i]] = df
      rm(df)
    }
    
    if(doinfandcompl){ 
      # fix experiment df:
      if(!alsoburnin){
        df2$isburnin = df2$row %in% burnin
        df2 = df2 %>% dplyr::filter(!(row %in% burnin) ) 
      } else {
        df2$isburnin = df2$row %in% burnin
      }
      df2$condition = ifelse(x$isbaseline[1],"baseline", "target")
      df2$dyad = x$stimn[1]
      df2$sender = paste0(df2$dyad, "_",df2$sender)
      dat2[[i]] = df2
    }
    
  } # done with looping over all experiments, concatenate all:
  
  if(!doinfandcompl){ 
    dat = do.call(rbind, dat) 
    dat$condition = as.factor(dat$condition)

    dat$dyad = as.factor(dat$dyad)
    dat$sender = as.factor(dat$sender)
    dat$colextarget=as.factor(dat$colextarget)
    # dat$relatedness=as.factor(dat$relatedness)
    dat$meaningpair=NA
    for(jj in 1:nrow(dat)){ # horribly inefficient but small data whatev
      dat$meaningpair[jj] = paste0(sort(unlist(dat[jj, c("copair", "meaning")],use.names = F,recursive = F)),collapse="-")
    }
    dat$row2 = range01(dat$row, 68, 135)  # might as well do the rescale here; all experiments have same number of rounds so ok to hard-code. now centered at middle, so -1...1
    #
    return(dat)
  #
  } else { 
    # if doing complexity posthoc, do other stuff
    dat2 = do.call(rbind, dat2) 
    dat2$dyad = as.factor(dat2$dyad)
    
    dat3 = dat2 %>% group_by(dyad, xpair) %>% 
      summarise(dyad=first(dyad), xpair=first(xpair), 
                          condition=first(condition), 
                          commcost = mean(commcost), 
                          complexity = mean(complexity)
                ) %>% ungroup()
    #return(dat3)
    
    ### simulate possible values
    #meaning = rep(c(1,2,1, 2,1,2), 4)
    tmp = permutations(3, 6, 1:3, F, T)
    tmp = cbind(tmp, tmp, tmp, tmp)
    n=ncol(tmp)
    # add more random combos to fill out the space:
    tmp2 = matrix(rep(rep(1, 24), 100), nrow=100)
    tmp3 = matrix(rep(rep(1:2, 12), 100), nrow=100)
    tmp4 = cbind(matrix(rep(rep(1, 12), 100), nrow=100), matrix(rep(rep(1:2, 6), 100), nrow=100)  )
    tmp = rbind(tmp,
                {tmp2[sample(ncol(tmp2)*nrow(tmp2), 100 )]=sample(2:3,100,T); tmp2},
                {tmp3[sample(ncol(tmp3)*nrow(tmp3), 100 )]=sample(1:3,100,T); tmp3},
                {tmp3[sample(ncol(tmp2)*nrow(tmp3), 100 )]=sample(1:10,100,T); tmp3},
                {tmp3[sample(ncol(tmp3)*nrow(tmp3), 100 )]=sample(1:10,100,T); tmp3},
                {tmp4[sample(ncol(tmp4)*nrow(tmp4), 100 )]=sample(1:2,100,T); tmp4},
                {tmp4[sample(ncol(tmp4)*nrow(tmp4), 1000 )]=sample(1:2,1000,T); tmp4},
                {tmp4[sample(ncol(tmp4)*nrow(tmp4), 100 )]=sample(1:10,100,T); tmp4},
                {tmp[sample(ncol(tmp)*nrow(tmp), 100 )]=sample(1:3,100,T); tmp},
                {tmp[sample(ncol(tmp)*nrow(tmp), 1000 )]=sample(1:3,1000,T); tmp},
                {tmp[sample(ncol(tmp)*nrow(tmp), 5000 )]=sample(1:3,5000,T); tmp},
                {tmp[sample(ncol(tmp)*nrow(tmp), 10000 )]=sample(1:3,10000,T); tmp},
                matrix(sample(1:2, n*1000, T), nrow=1000, byrow = T),
                matrix(sample(1:3, n*1000, T), nrow=1000, byrow = T),
                matrix(1:(n*1000), nrow=1000, byrow = T)
    )
    dim(tmp)
    
    compl=commc=rep(NA, nrow(tmp))
    for(i in 1:nrow(tmp)){                # self                             # other
      compl[i] = mean((tmp[i, 3:n] != tmp[i, 1:(n-2)]) + (tmp[i, 3:n] != tmp[i, 2:(n-1)]))
      commc[i] = mean((tmp[i, 3:n] != tmp[i, 1:(n-2)]) + (tmp[i, 3:n] == tmp[i, 2:(n-1)]))
    }
    tmp = data.frame(complexity=compl, commcost=commc, dyad=NA, xpair=".", condition="xsimu")
    tmp = tmp[!duplicated(tmp),]
    
    dat4 = rbind(dat3, tmp)
    
    return(dat4) # return compl infor data plus simus
    }
  
}

# current one, calls do_expmdat_binary4
do_expmdat_fromfile = function(
  subfolder = "",
  dyadsuffix = NA,
  condprefix = "",
  bogus = c(),
  accthreshold = 0.59,
  edb=resultsfolder, 
  doentropy=F, doinfandcompl=F
){
  path = file.path(edb, subfolder)
  generated_stims = readRDS(file.path(path,"generated_stims.RDS"))
  
  f = list.files(path, full.names = F, pattern="2020")
  f = f[order(sapply(f, function(x) as.numeric(gsub("^([0-9]{1,3}).*","\\1",x) )))]
  res = lapply(f, function(x) readRDS(file.path(path,x)) )
  res =  res[!(sapply(res, function(x) x$stimn[1]) %in% bogus) ]   # filter out obvious cheaters etc
  
  # quick report:
  accs = sapply(res, function(x) sum(x$correct[!x$burnin])/sum(!x$burnin) )
  ok = which(accs>=accthreshold)
  print(paste0("bogus: ", length(bogus), " ok: ", length(accs), " but accurate enough: ", length(ok)))
  
  # warning and fix if game engine messed up stim numbers (at least once)
  if(any(duplicated(sapply(res, function(x) x$stimn[1])))){
    isdup = which(duplicated(sapply(res, function(x) x$stimn[1]), fromLast = T))
    for(d in isdup){
      res[[d]]$stimn = res[[d]]$stimn+300
    }
    warning(paste("Duplicated stim IDs found, adding big number (500) to id to avoid confusing dyads (shouldn't interfere with dyad suffixes which are 1000+)"))
  }
  which(duplicated(sapply(res, function(x) x$stimn[1]), fromLast = T))
  
  if(!doentropy){ # normal
    
    dat5 = do_expmdat_binary4(res,generated_stims, accthreshold=accthreshold, 
                              alsoburnin=F, dyadsuffix=dyadsuffix, doinfandcompl=doinfandcompl)
    levels(dat5$condition) = paste0(condprefix, levels(dat5$condition))
    dat5 %>% count(condition, dyad) %>% count(condition) %>% as.data.frame() %>% print()
    
    
    return(dat5)
    
  } else { # entropy one instead
    res = res[sapply(res, function(x) sum(x[!x$burnin,"correct"], na.rm = T)/nrow(x[!x$burnin,]) )>=accthreshold ]
    y = lapply(res, function(x) data.frame(condition = 
                                             paste0(condprefix, ifelse(x$isbaseline[1], "baseline","target") ), 
                                           entropy = entropy(table(x[!x$burnin,"sent"])),
                                           dyad = x$stimn[1] + dyadsuffix,
                                           acc = sum(x[!x$burnin,"correct"], na.rm = T)/nrow(x[!x$burnin,])
                                           ) 
    ) %>% do.call(rbind, .)
    y$condition = as.factor(y$condition)
    return(y)
  }
}


range01 <- function(x,mn,mx){(x-mn)/(mx-mn)}

