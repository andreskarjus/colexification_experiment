#######
# The Espionage Game app used in Karjus et al 2021, 
# "Conceptual similarity and communicative need shape colexification: an experimental study".
# (c) Andres Karjus, 2021
#
# This app is open source, but if you use this app or derivatives of it for your own research, do kindly notify the authors.
#
# This little piece of software is provided "as is", without warranty of any kind, express or implied. In no event shall the authors be liable for any claim, damages or other liability, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the software or the use or other dealings in the software.
#

#### How to run this thing: ####
#
# To run this locally in demo mode: uncomment and run this line; 
# to play against yourself, copy the (local) IP address to another tab/window,
# the game will recognize this as two players being present and resumes:
#
# shiny::runApp("path/to/game_app/app.R", launch.browser = TRUE)  # uncomment, set path, run
#
# But make sure these are installed first, and check the parameters below too:
library(shiny)
library(shinyjs)
library(shinyWidgets)
# library(rdrop2)  # to save data (not required in demo mode)

#### Switches ####
#
DROPDIR = "experimentdb"  # where to save and load from in Dropbox
BREAKTIME = 1800          # ms, feedback length
SHORT = F                 # to run with saving but just 2 rounds to debug
DEMO = T                  # run in demo mode? (doesn't require dropbox auth)
####


# Tips:
#
# This was running on the shinyapps.io service
# If on server: the data is read and saved using rdrop2 and a Dropbox account; this is disabled by setting the DEMO flag above.
# A dropbox access token should be placed in the app folder, named droptoken.RDS, to make the saving work.
# If using local server, should restart after every game. Currently the endgame stopApp() makes it impossible to start a new game. If running online, either manually restart, or figure out some server restarter thing (shell?).
# Also check bookkeeping file periodically to make sure every run leaves start and end stamps
# For running multiple dyads in parallel, one way is to upload multiple clones of this app and set up a waiting room that matches up new players and funnels them into the games.
#
# Known bug: if a player clicks the consent form button while the other is writing their name in the box, the name will revert to blank, because the page refreshes due to the built-in reactivity; low probability occurrence, will not bother fixing.



###################### Ready to run ###############

# Data storage #
starttime <<- Sys.time()
fn <<- NULL
collectnames <<- c("","")    # not used in mTurk
SAVED=F    # hard switch to avoid multiple saves if somebody refreshes the page

# droptoken <- rdrop2::drop_auth(); saveRDS(droptoken, file = "droptoken.rds") 
# commented out; run once to generate dropbox access token for rdrop2 to work
# security risk: take care to not share/upload the droptoken!
if(!DEMO){
  drop_auth(rdstoken = "droptoken.rds")
}


# save/load functions
saveData <- function(data, filename, mode, datadir=DROPDIR) {
  filePath <- file.path(tempdir(), paste0(filename, ".RDS") )
  saveRDS(data, file = filePath)
  drop_upload(filePath, path = datadir, mode=mode)
}

loadData <- function(filename, datadir=DROPDIR) {
  dd <- drop_download(path = file.path(datadir,paste0(filename,".RDS")), overwrite = T ) # pulls file from dropbox
  readRDS(paste0(filename,".RDS")) # actually load downloaded file
}

# server loads stim pack from Dropbox:
if(!DEMO){
    generated_stims = loadData("generated_stims")
} else {
  # if demo mode, will construct some stims here:
  generated_stims = vector("list", 1)
  generated_stims[[1]]$stims = c("niwa", "leho", "huwo", "soqa", "wusa", "paqu", "qolu")
  generated_stims[[1]]$words = c("trip", "tax", "zone", "dentist", "music", "grade", "abdomen", "area", "belly", "journey")
  generated_stims[[1]]$pairs = data.frame(pair1=c("abdomen", "journey"), pair2=c("belly", "zone"), burnin=T, say=c("belly", "zone"), sent=NA, guess=NA, correct=NA, sendtime=NA, guesstime=NA, isbaseline=T, stimn=1 )
}






# Most of the texts, except the feedback ones which are interactive (see below)
texts = list(
  titlewelcome = "Welcome to a game of Espionage! <br>(cue Bond music)",
  titletext = "
  In this game, you and your partner will be teaming up and taking turns sending 
  each other encrypted messages using secret codes.
  To make it impossible for the Enemy to decode your correspondence, even you won't know 
  at the start how the secret code language works!<br>
  On each round, both of you will be shown two single-word messages on the screen, for example 'today' and 'tomorrow' (the order of those is random on each screen!).
  If it's your turn to send a message, pick a code word from the list to represent the indicated message.
  As the receiver, try to guess which of the two messages your partner had in mind.
  Every time you guess correctly, your team's score increases.
  Tip: when sending and guessing the coded messages, try to remember how you 
  and your partner used them in the previous rounds.
  <br>
  Please communicate only using the game interface; if you signed up with a friend, don't talk during the game. Don't write anything down (we're interested in what your brain can do!). Do NOT use the 'back' or 'refresh' buttons in your browser.
  ",
  titleconfirm =  "I have carefully read the instructions above.",
  titlewait = "Other player still reading/not present yet. Please wait...",
  feedback_correct = "Correct guess! &nbsp;&nbsp;&nbsp; ",
  feedback_incorrect = "Incorrect guess.",
  endtitle = "That's it. Thanks for participating!",
  feedbackask = "Please briefly describe your strategy for picking the code words to communicate the messages. Any other feedback is also welcome.",
  feedbacknotes = "Also, please tick the box below if you used any written notes - it won't affect your payment but allows us to correct for it in our analysis.",
  finishbutn = "Copied the code? Now click here to finish (server will disconnect)",
  finishbutn2= "Thanks! Now please close this browser window.",
  error = "Something is wrong! This is probably a connectivity issue: <br>- if you were in the middle of playing, click the button below to force the game to continue (it might take a couple of tries, click it again if this page reappears!)" ,
  error2 = "<br><br>- but if you just arrived here to play and are seeing this, then it means there are already 2 people playing - please exit now. <br>"
#  <br>- if you nevertheless think you should be scheduled to play right now, please contact the experimenter to clear it up.<br>"
)


#### Functions ####

# determines which word is on which line to add randomness, and line placement:
# update: don't make it jump lines, just random order
rndposition = function(wrds, wo){
  #we = sample(wrds) 
  we = wrds[wo] # now doing one random throw outside
  #lb = sample(c(T,T,T,F),1) # 3/4 of times words on separate lines
  #top = sample(c(T,F), 1)
  HTML(paste0(
    #ifelse(!lb & top, "<br>&nbsp;&nbsp;", ""),
    #paste(rep("&nbsp;", sample(0:15,1)),collapse=""),
    we[1] , 
    #ifelse(lb, "<br>", paste(rep("&nbsp;", sample(3:8,1)),collapse="")),
    #paste(rep("&nbsp;", sample(0:15,1)),collapse=""),
    paste(rep("&nbsp;", 4),collapse=""),
    we[2]
    #ifelse(!lb & !top, "<br>&nbsp;&nbsp;", "")
  ) )
}
coloredposition = function(wrds){
  HTML(paste0(
    "<span style='background-color: #114200;'>", #  #DAF7A6
    wrds[1],
    "</span>",
    paste(rep("&nbsp;", 4),collapse=""),
    wrds[2]
  )
  )
}

h1f = function(){
  return(
    h1(
  paste0("Players connected: ", length(global$playerspresent), 
         ". Score: ", global$score, "/", global$pairsmax)
    )
  )
}


# Global reactive values
global <- reactiveValues(
  amountUser = 0, 
  userIdToPlay = 1,
  userIdToGuess = 2,
  ipair = 1,             # set to large number>400 to test how final screen looks like
  pairsmax    = Inf,     # these two - set once data is loaded
  guessvector = NULL,    # --
  word = "",
  lastguess = "",
  correctguess = logical(),
  feedbackbreak = F,
  score = 0,
  titleconfirmed=c(F,F),  # 0
  finish=c(F,F),   # when both are finished, save files
  titlescreen=T,
  wo = c(2,1),  # random word order rememberer for persistent GUI
  playerspresent=c(),
  consent=c(F,F),
  consentscreen=T
)


# UI elements and styling
ui <- fluidPage(
  useShinyjs(), # for delay
  tags$head(
    tags$style(HTML("
      body {
        background-color: black;
        color: white;
        font-family: 'Lucida Console', Courier, sans-serif;
      }
      .centered {
        margin: auto; 
        width: 490px;
        margin-top: 50px;
      }
      h1 {
        font-size: 12px;
        line-height: 1.1;
        color: gray;
        text-align: right;
        font-style: normal;
      }
      h2 {
        font-size: 20px;
        line-height: 1.5;
        /* color: black; */
        text-align: left;
        font-style: normal;
      }
      h3 {
        font-size: 18px;
        line-height: 1.5;
        /* color: black; */
        text-align: left;
        font-style: normal;
        font-weight: normal;
      }
      h4 {
        font-size: 16px;
        line-height: 1.5;
        /* color: black; */
        font-style: normal;
        font-weight: normal;
        text-align: justify;
        text-justify: inter-word;
      }
      .form-group .control-label {
        font-size: 18px;
        line-height: 1.5;
        /* color: black; */
        text-align: left;
        font-style: normal;
        font-weight: normal;
      }
      input[type='radio']{
        display:none;
      } 
      .radio {
        font-size: 18px;
        line-height: 2;
      }

    ")) # color: #357a00;
  ),
  tags$div(class="centered",   {uiOutput("moreControls")} )
  
)



server <- function(input, output, session){
  
  # manages players and their roles
  local <- reactiveValues()
  observe({
    isolate({global$amountUser <- global$amountUser + 1 }  )
    isolate({
      if(length(global$playerspresent)<1){
        p=1
      } else {
        p=setdiff(1:2, global$playerspresent)
      }
      global$playerspresent = c(global$playerspresent, p)
      local$userId = p
      #local$userId <- (2-(global$amountUser %% 2))
    })   
    # (2-(global$amountUser %% 2)) # (global$amountUser %% 2)+1 # global$amountUser
  })
  #session$onSessionEnded(function(x=global$amountUser){isolate({global$amountUser=x-1}) })
  session$onSessionEnded(function(x=global$playerspresent){isolate({
    global$playerspresent = setdiff(x, local$userId) 
    }) })
  
  
  ### action observers ###
  #
  # error fixer
  observeEvent(eventExpr = input$refreshbutton, {
    session$reload()
  })
 
  
  # when word clicked, swap userIdToPlay
  observeEvent(eventExpr =  #input$send, 
                 input$wordchoice,
               handlerExpr = {
                 if(length(input$wordchoice) > 0 ){
                   pairs2[global$ipair, "sendtime"] <<- Sys.time() # needs <<-
                   global$userIdToPlay =  3 - global$userIdToPlay  
                   # assumes two players
                   # ---this might be easy to break if 2+ windows open
                   # solution, display empty screen to any 3rd player/accidental login
                   # pairs2$sender[global$ipair] # depends on the "sender" column in stims
                   global$word = input$wordchoice
                 }
               }
  )
  observeEvent(eventExpr = #input$guess
                 input$vocabchoice,  
               # without actionbutton use: input$vocabchoice
               handlerExpr = {
                 if(length(input$vocabchoice)>0){ 
                   # this also makes it so clicking the button doesn't do anything if nothing selected
                   # (was only needed for the radio+button gui, but might as well let it stay)
                   pairs2[global$ipair, "guesstime"] <<- Sys.time() #  timestamp
                   global$userIdToGuess = 3 - global$userIdToGuess
                   global$lastguess = input$vocabchoice
                   # print( paste(global$lastguess, 
                   #              pairs2[global$ipair, pairs2[global$ipair,3] ], input$wordchoice) )
                   global$correctguess = 
                     global$lastguess == pairs2$say[global$ipair]
                   if(global$correctguess){
                     global$score = global$score + 1
                     global$guessvector[global$ipair] = T
                   }
                   global$feedbackbreak = T
                #   delay(BREAKTIME, {      # pause to display feedback
                     #print(global$guessvector)   # debug
                     
                     # save round results
                     pairs2[global$ipair, "sent"]    <<- global$word
                     pairs2[global$ipair, "guess"]   <<- global$lastguess
                     pairs2[global$ipair, "correct"] <<- global$correctguess
                     print(paste(pairs2[global$ipair,,drop=T],collapse=" "), quote=F) # prints to shinyapps log
                     
                     # carry on
                     global$ipair = global$ipair + 1
                     
                      # first save attempt of 2 redundant ones
                     if(global$ipair > global$pairsmax){
                       endtime <<- Sys.time()
                       fn <<- paste0(whichstim,  # make it global so can reuse
                                     isbaseline_forfilename, 
                                     "_",
                                     format(starttime, "%Y-%m-%d_%H-%M-%S"),
                                     "--",
                                     format(endtime,   "%Y-%m-%d_%H-%M-%S") 
                                     )
                       if(!DEMO){
                          saveData(pairs2, fn, "overwrite")
                         }
                     }
                     
                   #  global$feedbackbreak = F
                     
                  # } ) # end delay
                 }
               }
  )
  #
  # feedback operator:
  observe({
    global$feedbackbreak
    if(global$feedbackbreak){
      delay(BREAKTIME, { isolate({global$feedbackbreak=F}) })
    }
  })
  
  # consent screen operator
  observeEvent(eventExpr = input$consentbutton, {
    isolate({
  #    if(nchar(input$consentname)>4){   # not used in mturk
        disable(id = "consentbutton")  
        updateActionButton(session, "consentbutton", # 
                           label = texts$titlewait) #
        #global[[paste0("consent", local$userId)]] = T
        global$consent[local$userId] = T
  #      collectnames[local$userId] <<- input$consentname
  #      print(input$consentname)  # prints to shinyapps log, can see if missing
        print(global$consent)
  #    }
    })
  })
  
  observeEvent(input$consentbutton, {
    isolate({
      if(all(global$consent)){
        global$consentscreen = F
      }
    })
  })
  
  # title screen operator
  # disable button while waiting:
  observeEvent(eventExpr = input$titleconfirm,
               handlerExpr = {
                 disable(id = "titleconfirm")                # all
                 updateActionButton(session, "titleconfirm", # 
                                    label = texts$titlewait) #
                 #global$titleconfirmed = global$titleconfirmed + 1
                 global$titleconfirmed[local$userId] = T  # this is important
               }
  )
      
  # load stuff when both clicked ready:
  observeEvent(input$titleconfirm, {
    if(all(global$titleconfirmed)){
      isolate({
      # load stims here instead; so multiple instances won't load the same one!
      # also save start time for the same reason:
      # these will sit in the global environment; therefore later <<-'s too:
      # print("loading and overwriting bookkeeping now")
      if(!DEMO){
        bookkeeping = loadData("bookkeeping") 
      } else {
        bookkeeping = data.frame(nexp=1,isbaseline=T,starttime=NA, endtime=NA, f1=NA, f2=NA,names=NA)
      }
      whichstim <<- which(is.na(bookkeeping$starttime))[1] # determine which stimset to load
      #print(whichstim)
      mturkcodes <<- c(bookkeeping$code1[whichstim], bookkeeping$code2[whichstim])
      bookkeeping$starttime[whichstim] = starttime
      bookkeeping$names[whichstim] = paste0(collectnames, collapse=", ") # 1st=player 1
      isbaseline_forfilename <<- ifelse(bookkeeping[whichstim, "isbaseline",drop=T],"b","a")
      feedbackframe <<- bookkeeping[whichstim,,drop=F]
      if(!DEMO){
        saveData(bookkeeping, "bookkeeping", "overwrite") # save start time so this stim is "booked"
      }
      rm(bookkeeping)
      #
      pairs2 <<- generated_stims[[whichstim]]$pairs  # "pairs" is a graphics function, clashes
      lang  <<- generated_stims[[whichstim]]$stims
      vocab <<- generated_stims[[whichstim]]$words
      
      # ----------debug------------------ -
      if(DEMO | SHORT){
        pairs2 <<- pairs2[1:2,,drop=F]
        BREAKTIME = 300 
      }
      # --------------------------------- -
      
      global$pairsmax = nrow(pairs2)
      global$guessvector = rep(F, nrow(pairs2))
      
      global$titleconfirmed = c(F,F) # extra caution to avoid more saves
      global$titlescreen = F   # this starts the game proper
    })
      }
  })
  
  
  # Important part: when done, save experiment results
  observeEvent(eventExpr = global$finish, 
               handlerExpr = {
                 isolate({
    #print(global$finish) # debug
    if( all(global$finish)){
        try(rm(bookkeeping),silent = T)
        #print("saving bookkeeping and data")
        bookkeeping = loadData("bookkeeping") # re-load in case it was modified in the meanwhile by another instance
        bookkeeping$f1[whichstim] = feedbackframe$f1[1]
        bookkeeping$f2[whichstim] = feedbackframe$f2[1]
        bookkeeping$starttime[whichstim] = starttime
        bookkeeping$endtime[whichstim]   = endtime 
        if(!DEMO & !SAVED){
          saveData(bookkeeping, "bookkeeping", "overwrite") # updated; will be loaded in next game
          saveData(bookkeeping, paste0("bookkeeping_",whichstim), "add") # add a copy for backup
          # # redundant re-save to be on the safe side:
          # fn = paste0(whichstim, 
          #                 isbaseline_forfilename,
          #                 "_",
          #                 format(starttime, "%Y-%m-%d_%H-%M-%S"),
          #                 "--",
          #                 format(endtime, "%Y-%m-%d_%H-%M-%S"))
          # saveData(pairs2, fn, "overwrite") # overwrite if first attempt was successful
          SAVED <<- T # hard switch
        }
  #      global$finish = c(F,F) # extra caution to avoid more saves
    #  }
        # Sys.sleep(2)
        #shiny::stopApp()
        shinyjs::delay(7000, {shiny::stopApp()} ) # delay so last player can also see the thankyou message
    }
                 })
  })
  
  observeEvent(eventExpr = input$finishgame,
               handlerExpr = {
                 fbtext = paste0(input$feedback, 
                                 " ||| Written notes: ",as.character( input$feedbacknotestick))
                 feedbackframe[1, paste0("f", local$userId)] <<- fbtext  # store feedback; temporary structure
                 isolate({
                   disable(id = "finishgame")
                   disable(id = "feedback")
                   disable(id = "feedbacknotestick")
                   updateActionButton(session, "finishgame",
                                      label = texts$finishbutn2)
                 })
                 global$finish[local$userId] = T # will trigger saving when both done
                 #print(local$userId); print(global$finish);print(feedbackframe)   # debug
                 
               }
  )
  

  
  
  # This part decides which screen to show 
  # when one of the reactive values change
  output$moreControls <- renderUI({
    # the mentions here make the reactivity work properly
    
    #if(global$consent2) global$consent2
    if((global$ipair <= global$pairsmax) ){
      # if, so it doesn't refresh the screen on the consent page yet
      # but don't update on game over screen, might delete somebody's feedback
     # global$playerspresent
      global$userIdToPlay   
      global$userIdToGuess
      global$titlescreen
      global$titleconfirmed
      global$consentscreen
    }
    if(!global$consentscreen & (global$ipair <= global$pairsmax) ){
      global$playerspresent
    }
    global$feedbackbreak
    
    isolate({
    tryCatch({ 
      ###
      # if somehow 3rd player connected, lock them out; could be used to monitor though.
      # no longer works, forcing players to be 1 or 2 - but now refreshing won't break game!
      # if(local$userId > 2){
      #   p=pairs2[1:(global$ipair),c(1,2,4,6,7,9),drop=F]; p$sendtime=format(p$sendtime, "%H:%M:%S")
      #   return(
      #     tagList(
      #     h3("Something is wrong, it seems 2 players are already connected. 
      #        Please ask the experimenter for help."),
      #     HTML("<br>"),
      #     renderPrint({ print(p, row.names=F) })
      #     )
      #   )
      # }
      #
      #
      # Game over screen:
      if(global$ipair > global$pairsmax){
        # show end screen:
        return(
          tagList(
            h2(texts$endtitle),
            h3(paste0("The score between you and your partner was ", 
                      global$score, "/", global$pairsmax, 
                      " or ", round(global$score/(global$pairsmax)*100),
                      "% communicative accuracy."
                      # -1 because server already incremented ipair
            ) 
            ),
            # h3(paste0(
            #   "In the last quarter of the game, accuracy was ",
            #   round(sum(tail(global$guessvector, 
            #                  round(length(global$guessvector)/4) ))/
            #           round(length(global$guessvector)/4)*100),
            #   "%."
            # )),
            # h3(paste0("The game lasted for ", 
            #           as.character(round( difftime(Sys.time(),starttime,units="mins"))), 
            #           " minutes.")),
            HTML("<br>"),
            h4(texts$feedbackask),
            textAreaInput("feedback", 
                          label=NULL, 
                          width = "489px", height="100px"),
            h4(texts$feedbacknotes),
            checkboxInput("feedbacknotestick", label = "I used written notes",value=F,width = "489px" ),
           # HTML("<br>"),
            h4(HTML("<br>Finally, make sure to copy this completion code before exiting, without this you cannot get paid:")),
            # h2(HTML( paste0("<span style='color: red; text-align: center !important;'>",
            #                 mturkcodes[local$userId], "</span><br>"))),
            tagList(
              HTML(paste0('<input type="text" id="codecopy" name="fname" size=20 readonly value="',
                          mturkcodes[local$userId],'">')),
              tags$style("#codecopy{background-color:white; color:red;font-size: 20px;line-height: 1.5;text-align:center}")
            ),
            HTML("<br><br><br>"),
            actionButton("finishgame",  texts$finishbutn)
          )
        )
      }
      #
      
      #
      
      
      # Initial consent form screen:
      if(global$titlescreen && global$consentscreen  ){
        return(
          tagList(
            tags$iframe(src="consentform.html", height=450, width=490, frameBorder="0"),
            # HTML('<div style="height:450px;width:490px;overflow:auto;
            #      text-align: justify;
            #       text-justify: inter-word;
            #       background-color: white;
            #       color: red;">'),
            # includeHTML("consentform.html"),
            HTML('<br>'),
            HTML(    # no named for mturk:
           #   '<div style="font-family:Helvetica,Arial,sans-serif !important;size: 14pt; background-color: white; color: black; width: 490px; padding: 5px; margin-bottom: 10px;"> By entering your full name in the field below and clicking the "I consent to take part in this study" button, you consent to all of the above.</div>'
              '<div style="font-family:Helvetica,Arial,sans-serif !important;size: 14pt; background-color: white; color: black; width: 490px; padding: 5px; margin-bottom: 10px;"> By clicking the button below, you consent to all of the above.</div>' 
              ),
          #  textInput(inputId="consentname",label = NULL, placeholder = "Full Name Here", width="1490px"),   # name field, remove for mturk
          
            actionButton(inputId="consentbutton", 
                         label="I consent to take part in this study",
                         width="490px"),
            HTML("<br><br>")
          )
        )
      }
      
      ## Title screen with instructions - now also checking consent form
      if(global$titlescreen
         # && global$consent1 && global$consent2er
         #  global[[paste0("consent", local$userId)]]  
      ){
        # if(global$titleconfirmed[local$userId]){
        #     but = h4(texts$titlewait)
        # } else {
        but = actionButton(inputId = "titleconfirm",
                           label = texts$titleconfirm)
        #}
        return(
          tagList(
            h1(HTML(paste0(
              #"<span style='font-size: 9px;'>",
              "Players connected: ", length(global$playerspresent)
              # ". Running experiment no. ",
              # whichstim, 
              # ifelse(pairs2$isbaseline[1], "b", "a"),
              #"</span>"
            ))), # tiny text with stim n and b=baseline
            h2(HTML(texts$titlewelcome)),
            h4(HTML(texts$titletext)),
            h3(""),
            but,
            HTML("<br><br><br>")
          )
        )
      } 
      
        # Feedback break screen:
        if(global$feedbackbreak){
          # if(global$titleconfirmed < 2){
          #   return(h2("Waiting for partner. Stand by..."))
          # } else {
          return(   
            tagList(
              h1f(),
              h2( HTML(  # feedback
                ifelse(
                  global$correctguess,
                  paste0(texts$feedback_correct, 
                         "<span style='color: #357a00;'>",
                         global$word, 
                         "</span>",
                         " = ", 
                         "<span style='font-style: italic;'>",
                         global$lastguess,
                         "</span>"
                         ), 
                  texts$feedback_incorrect
                )
              )
              )
            )
          )
          #        }
        } else {
          
          # send message:
          if(local$userId == global$userIdToPlay && local$userId != global$userIdToGuess){
            #print(paste(pairs2[global$ipair, 1:2, drop=T], collapse=" " ) ) # debug
            return(
              tagList(
                #h1(paste0("Score: ", global$score, "/", global$pairsmax)),
                h1f(),
                # old:
                # h2(paste0(pairs2[global$ipair, 1], " | ", pairs2[global$ipair, 2]
                #           #  , '  | send "', pairs2[global$ipair, pairs2[global$ipair,3] ], '"' 
                # ) ),
                #
                h2( 
                  #rndposition( c(pairs2$pair1[global$ipair], pairs2$pair2[global$ipair]) )
                  coloredposition( c(pairs2$say[global$ipair],  # new: one to send always left+colored
                                     setdiff(c(pairs2$pair1[global$ipair], pairs2$pair2[global$ipair]),
                                             pairs2$say[global$ipair]
                                     )
                  )
                  )
                ),
                ## Use radio or actionButtons?
                radioButtons(inputId = "wordchoice",
                             label = HTML( paste0("Communicate ",
                                            "<span style='font-style: italic;'>",
                                            pairs2$say[global$ipair],
                                            "</span>",
                                            " using..." 
                                            )),
                             # replace with fixed
                             choices = lang,
                             # randomize order? harder to play, but then cannot make pattern
                             # better: now the message placement is randomized instead
                             selected = character(0)
                )
                # , actionButton(inputId="send", label="Send") # not needed anymore
                
                # h3(paste0('Communicate "',
                #           pairs2[global$ipair, pairs2[global$ipair,3] ], 
                #            '" using...' )),
                # actionGroupButtons(
                #   inputIds = rep("wordchoice", length(lang)),
                #   labels = lang,
                #   status = "danger",
                #   size = "normal",
                #   direction = "vertical",
                #   fullwidth = FALSE
                # )
              )
            )
          }
          # take a guess:
          if(local$userId == global$userIdToPlay && local$userId == global$userIdToGuess){
            return(
              tagList(
                h1f(),
                #h2(paste0(pairs[global$ipair, 1], " | ", pairs[global$ipair, 2]) ) ,
                h2(rndposition( c(pairs2$pair1[global$ipair], pairs2$pair2[global$ipair]), global$wo) ),
                h3(HTML( paste0(
                           "Message: ",
                           "<span style='color: #357a00;'>",
                           global$word, 
                           "</span>"
                           ) ) ) ,
                radioButtons(inputId =  "vocabchoice", 
                             label =  "This means:",
                             choices = c(pairs2[global$ipair, 1], pairs2[global$ipair, 2])[global$wo],
                             selected = character(),
                             inline = F
                             # if radio buttons, should disable submitbutton until selection made! (using shinyjs disable/enable toggle)
                             )
                #,actionButton(inputId="guess", label="Continue")
                
              )
            )
          }
          # wait for message
          if(local$userId != global$userIdToPlay && local$userId == global$userIdToGuess){
            global$wo = sample(1:2)
            return(
              tagList(
                h1f(),
                h2(rndposition( c(pairs2$pair1[global$ipair], pairs2$pair2[global$ipair]) , global$wo)),
                h3("Waiting for message...")
              )
            )
          }
          # wait for guess:
          if(local$userId != global$userIdToPlay & 
             local$userId != global$userIdToGuess){
            return(
              tagList(
                h1f(),
                #h2(paste0(pairs2[global$ipair, 1], " | ", pairs2[global$ipair, 2])), 
                h2(" "),h2(" "), # just blank
                h3(HTML(paste0(
                  "Sent ", 
                  "<span style='font-style: italic;'>", 
                  pairs2$say[global$ipair], 
                  "</span>",
                  " using ",
                  "<span style='color: #357a00;'>",
                  global$word, 
                  "</span>"
                  )) 
                  ),
                h3("Stand by..." )
              )
            )
          }
          
       # } # end else
      } # end else 
      # THE FOLLOWING FIX WORKS ON SHINYAPPS.IO - but unwanted behaviour on local (presumably also server?), lets a third player join if they click the button.
      }, error = function(e){
        print(e)
        return(
        tagList(
          h3(HTML(paste0("<span style='color: red;'>", texts$error,   "</span>"))),
          actionButton("refreshbutton", "CLICK HERE to continue playing (click again if it's still stuck)"),
          h3(HTML(paste0("<span style='color: red;'>", texts$error2,   "</span>"))),
          h4(HTML(paste0('<br><br><br><br><span style="color: darkred;">If you are unable to continue even after trying to click the button above multiple times, please report this error to the experimenter: ',
                         gsub("'","",e), '</span>')))
        ) 
        ) 
        } ) # end trycatch 
    }) ### end isolate
  })
}


# this makes the app run on the server:
shinyApp(ui, server)

