#
# Scripts to replicate the analyses in: Karjus et al 2021, 
# "Conceptual similarity and communicative need shape colexification: an experimental study"
# See also: the stimgenerator.R for the script to generate the stims, 
# and the labapp folder for the game app source code
# The pre-compiled results dataframe is also included as a csv in the git repo.
#
#

# Set these first:
source("expgen_scripts.R") # full path to the scripts file
resultsfolder = "RESULTS"  # full path to this unpacked folder, which contains the folders of RData files with the raw data (only needed if parsing from raw data), if not:
megadat = as_tibble(read.table("parsed_results_for_glmm.csv", header = T, quote=""))  # if not parsing from raw, set full path to this file, it has all the data



#### Data parser ####

# Uncomment to load and parse from scratch instead
# 
# origdat = do_expmdat_fromfile("first", 0, "first_",bogus = c(15), accthreshold = 0.59, edb=resultsfolder)
# repldat = rbind(do_expmdat_fromfile("repli", 1000, "repli_", c(44,11),edb=resultsfolder), do_expmdat_fromfile("repli2", 2000, "repli_", edb=resultsfolder))
# weakdat = rbind(do_expmdat_fromfile("weakhyp", 3000, "weak_",edb=resultsfolder), do_expmdat_fromfile("weakhyp2", 4000, "weak_", bogus = c(5,24), edb=resultsfolder))
# newdat = do_expmdat_fromfile("new", 5000, "new_",edb=resultsfolder)
# megadat = rbind(origdat,repldat, weakdat, newdat)
# length(table(megadat$dyad))

# - data collection took place over multiple months and required generating stimuli multiple times; the function above collects all the data from all the folders and makes sure IDs don't overlap.


#### experiment 1 model and plot ####

origmodel = glmer(colextarget ~ condition*row2   +
              (1  + condition | meaning) + 
              (1  | dyad/sender ), 
            data=origdat,  
            family="binomial",
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(origmodel) 




# ...at the end of a game, the pooled probability estimate of that is only
c(predict(origmodel,newdata=data.frame(condition="first_target", row2=1), re.form=NA, type="response"),
predict(origmodel,newdata=data.frame(condition="first_baseline", row2=1), re.form=NA, type="response")) %>% round(2)

# This procedure, repeated for all players across all 41 games, yields a dataset of...
nrow(origdat)
# median of .. per dyad
origdat %>% group_by(dyad) %>% summarise(n=n()) %>% pull(n) %>% median


# the intercept value of  therefore stands for the log odds of target meanings being colexified in the baseline condition, mid-game (i.e. a
summary(origmodel)$coefficients[1,1] %>% plogis() %>% round(2)

#  indicating participants were less likely to colexify related meanings in the target condition (by the end of a game, the pooled probability estimate of that is only  
predict(origmodel,newdata=data.frame(condition="first_target", row2=1), re.form=NA, type="response") %>% round(2)
# compared to 
predict(origmodel,newdata=data.frame(condition="first_baseline", row2=1), re.form=NA, type="response")%>% round(2)
# in the baseline condition)
  
  
ggplot(origdat %>% group_by(dyad) %>%  mutate(row2 = 1:n()) %>% transform(condition = c("Baseline condition (each row represents a dyad)", "Target condition")[as.numeric(condition)]) , aes(y=dyad,x=row2,fill=colextarget, color=colextarget )) + 
  geom_tile( width=0.6, height=0.7, linejoin="round", size=1.1) +
  facet_wrap(~condition, scales="free_y" ) +
  scale_fill_manual(values= brewer_pal()(9)[c(3,6)], name="Target\nmeanings\ncolexified?") +
  scale_color_manual(values=brewer_pal()(9)[c(3,6)], name="Target\nmeanings\ncolexified?") +
  theme_minimal()+
  annotate("text", x = 3, y = -0.2, label = "Total:", size=10/.pt) +
  coord_cartesian(ylim=c(1,NA), clip = "off") +
  scale_x_continuous( expand = c(0.01,0.01), breaks=seq(10,50,10)) +
  xlab("Total:") +
  theme(legend.position = "right",
        legend.justification=c(1, 1), 
        legend.margin = margin(0,0,0,1),
        strip.background = element_blank(),
        strip.text = element_text(size=13,hjust=0),
        plot.margin = unit(c(0,0,0,1),"mm"),
        panel.background = element_rect(fill="white", color=NA),
        panel.grid.minor = element_blank(),  # color="gray12"
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        #axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        #axis.title.x=element_text(hjust=0, vjust=7, size=11),
        #panel.border = element_rect(color="darkgray",fill=NA, size=0.2),
        panel.spacing = unit(1, "lines")
  )  +
  NULL


#### Experiment 2 replication ####

# a dataset of...
nrow(repldat)
# median of .. per dyad
repldat %>% group_by(dyad) %>% summarise(n=n()) %>% pull(n) %>% median

replmodel = glmer(colextarget ~ condition*row2   +
                    (1  + condition | meaning) + 
                    (1  | dyad/sender ), 
                  data=repldat,  
                  family="binomial",
                  control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(replmodel) 

c(
predict(replmodel,newdata=data.frame(condition="repli_target", row2=1), re.form=NA, type="response") %>% round(2),
predict(replmodel,newdata=data.frame(condition="repli_baseline", row2=1), re.form=NA, type="response") %>% round(2) )



#### Experiment 3, weaker condition ####

# weak condition, 3-way
weakdat3 = megadat %>% subset(condition %in% c("repli_baseline", "repli_target", "weak_target")) %>% droplevels()
weakdat3$condition = relevel(weakdat3$condition, ref="weak_target")

wm = glmer(colextarget ~ condition*row2   +
              (1   | meaning) +    # with condition slope won't converge
              (1  | dyad/sender ), 
            data=weakdat3,  
            family="binomial",
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(wm) 
nrow(weakdat)
nrow(weakdat3)

rownames(summary(wm)$coefficients) %>% cbind()


#### Experiment 4, no-pressure conditions ####

nm = glmer(colextarget ~ condition*row2   +
              (1   | meaning) +    # with condition slope won't converge
              (1  | dyad/sender ), 
            data=newdat,  
            family="binomial",
            control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=100000)))
summary(nm) 


#### All models comparison plot ####

modcomp=tibble()
for(i in levels(megadat$condition)){
  # need model; but since plotting condtion by condition, won't need it in the model:
  m = glmer(colextarget ~ row2   +
              (1   | dyad ) # + (1  | meaning) 
                    , data=megadat %>% filter(condition==i) %>% droplevels() ,
                    family="binomial",
                    control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=50000)))
  
  nd = megadat %>% filter(condition==i) %>% group_by(dyad #, meaning
                                                     ) %>% slice(1) %>% 
    mutate(row2=1)  %>% as.data.frame()
  d = predict(m,newdata=nd, re.form=~ #(1  | meaning) + 
                (1   | dyad )
              , type="response") 
  modcomp = rbind(modcomp, tibble(condition=i, d)  )
}
l = c("original\nbaseline", "original\ntarget condition", 
      "replication\nbaseline", "replication\ntarget condition",
      "weaker hypothesis\ntarget condition",
      "no-pressure\nbaseline", "no-pressure\ntarget condition"
)
modcomp$condition = factor(modcomp$condition, levels=unique(modcomp$condition)) # keep order
modcomp$cond0 = modcomp$condition
levels(modcomp$condition) = l
modcomp$bt = modcomp$condition
levels(modcomp$bt) = c("bcol", "tcol", "bcol", "tcol", "tcol", "bcol", "tcol")
pnts = megadat %>% group_by(dyad, condition) %>% count(colextarget) %>% mutate(n=n/sum(n)) %>% filter(colextarget=="yes")
ns = megadat %>% count(condition, dyad) %>% count(condition) %>% mutate(n=paste0("n=",n))


bcol=brewer_pal()(9)[6]; tcol=brewer_pal()(9)[8]

ggplot(modcomp  , #aes(y=fit,ymin=lwr, ymax=upr, x=condition )) + 
       aes(x=condition, y=d, group=condition, color=bt, fill=bt)) +
  
  geom_vline(xintercept = c(2.5,4.5,5.5, 7.485), color="grey90", size=0.23) +
  annotate("text", label=paste0("Experiment ", 1:4), x=c(1.5, 3.5, 5, 6.5), y=1, vjust=-0.5) +
  #geom_hline(yintercept = c(0), color="white", size=3) +
  geom_hline(yintercept = c(0,0.25,0.5,0.75,1), color="grey90", size=0.3) +

  geom_text(label="-", size=4,  fontface="bold") +
  stat_summary(fun=median,geom="errorbar",aes(ymax = ..y.., ymin = ..y..), size=1.4, width=0.3,alpha=1)+
  #geom_beeswarm(shape=22, priority = "density", cex=1.2, size=1.7, stroke=1.1)+
  scale_color_manual(values=c(bcol, tcol))+
  scale_fill_manual(values=c(bcol, tcol) )+
  #geom_linerange() +
  #scale_color_manual(values=brewer_pal()(9)[c(4,9)], name="Target\nmeanings\ncolexified?") +
  #coord_cartesian(expand=0)+
  scale_y_continuous(expand=expansion(mult=c(0,0), add=c(0,0) ), limits=c(0,1.1))+
  scale_x_discrete(expand=c(0.081,0), labels= paste0(l,"\n", ns$n #, "\naccuracy~", c(0.83, 0.81, 0.77, 0.65, 0.66, 0.69, 0.7)*100, "%"
  )
  )+
  ylab("Probability to colexify target pairs") +
  theme_minimal()+
  theme(legend.position = "none",
        legend.justification=c(1, 1), 
        legend.margin = margin(0,0,0,1),
        strip.background = element_blank(),
        strip.text = element_text(size=13,hjust=0),
        plot.margin = unit(c(2,2,1,1),"mm"),
        panel.background = element_rect(fill="white", color=NA),
        panel.grid.minor = element_blank(),  # color="gray12"
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        #axis.text.x = element_blank(),
        axis.line = element_line(),
        #axis.ticks.y = element_line(color="grey90", size=0.3),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=10, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        #axis.title.x=element_text(hjust=0, vjust=7, size=11),
        #panel.border = element_rect(color="darkgray",fill=NA, size=0.2),
        panel.spacing = unit(1, "lines")
  )  +
  NULL


#### informativeness vs complexity plots ####

ytitle = c("Communicative cost")
xtitle = c("Cognitive cost")

# schematic plot for the start:
nc=11
coinf = matrix(
  c(
  c(NA, rep("o", nc-1)),
  c(rep(NA,1),"l",  rep("o", nc-2)),
  c(rep(NA,2),"a",  rep("o", nc-3)),
  c(rep(NA,3),"n",  rep("o", nc-4)),
  c(rep(NA,4),"g",  rep("o", nc-5)),
  c(rep(NA,5),"u",  rep("o", nc-6)),
  c(rep(NA,6),"a",  rep("o", nc-7)),
  c(rep(NA,7),"g",  rep("o", nc-8)),
  c(rep(NA,8),"e",  rep("o", nc-9)),
  c(rep(NA,9),"s",  rep("o", nc-10))
#  c(rep(NA,10), rep("o", nc-10))
  ), ncol=nc, byrow=T
) %>% t() %>%  reshape2::melt() %>% 
  mutate(col=ifelse(value=="o",1,2) %>% as.factor()) %>% mutate(Var2=max(Var2)-Var2)

coinf2 = coinf %>% mutate(value = case_when((Var1 %in% 5:7 & !is.na(value)) ~ "o", TRUE~value)) %>% mutate(col=ifelse(value=="o",1,2) %>% as.factor())


ggplot(coinf, aes(Var1, Var2, label=value, color=col, size=col))+
  #geom_text(fontface = "bold")+
  geom_point(shape=15) +
  #scale_color_manual(values=c("gray", "black"))+
  scale_color_manual(values=c("gray90", brewer_pal()(9)[c(7)]) ) +
  scale_size_manual(values=c(3.5,4.5))+
  scale_y_continuous(expand=c(0.1,0), limits=c(0,9))+
  scale_x_continuous(expand=c(0.03,0), limits=c(0,11))+
  labs(y=ytitle, x=xtitle)+
  annotate("text", x=1, y=-Inf, label="\u2039 simple\n\u2039 easier to learn", hjust=0, vjust=-0.2, size=4.5, color="gray40", lineheight=0.9)+
  annotate("text", x=9.4, y=-Inf, label="complex \u203A\n harder to learn \u203A", hjust=1, vjust=-0.2, size=4.5, color="gray40", lineheight=0.9)+
  annotate("text", x=1.1, y=1, label="\u2039 expressive\n\u2039 informative", hjust=0, vjust=0, angle=90, size=4.5, color="gray40", lineheight=0.9)+
  annotate("text", x=1, y=9, label="ambiguous \u203A\nerror-prone \u203A", hjust=1, vjust=0, angle=90, size=4.5, color="gray40", lineheight=0.9)+
  theme_minimal(base_size = 15)+
  theme(legend.position = "none",
        legend.justification=c(1, 1), 
        legend.margin = margin(0,0,0,1),
        strip.background = element_blank(),
        strip.text = element_text(size=13,hjust=0),
        plot.margin = unit(c(1,10,1,2),"mm"),
        panel.background = element_rect(fill="white", color=NA),
        panel.grid.minor = element_blank(),  # color="gray12"
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text = element_blank()  ,  #(size=10, color="white"),
        axis.title = element_text(size=12),
        #axis.line = element_line(),
        axis.line = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), ends = "last", angle=20,type = "closed"), lineend="round")
        #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        #axis.title.x=element_text(hjust=0, vjust=7, size=11),
        #panel.border = element_rect(color="darkgray",fill=NA, size=0.2),
        #panel.spacing = unit(1, "lines")
  )  +
  ggplot(coinf2, aes(Var1, Var2, label=value, color=col, size=col))+
  #geom_text(fontface = "bold")+
  geom_point(shape=15) +
  scale_color_manual(values=c("gray90", brewer_pal()(9)[c(7)])) +
  scale_size_manual(values=c(3.5,4.5))+
  scale_y_continuous(expand=c(0.1,0), limits=c(0,9))+
  scale_x_continuous(expand=c(0.03,0), limits=c(0,11))+
  labs(y=ytitle, x=xtitle)+
    annotate("text", x=8.5, y=0, label="High communicative\nneed", hjust=1, vjust=0.5, size=4.5)+
    annotate("text", x=0.5, y=7, label="Low\ncommuni-\ncative need", hjust=0, vjust=1, size=4.5)+
  geom_segment(aes(x = 4, y = 4.5, xend = 7, yend = 1.5), size=0.5,color="black", arrow = arrow(length = unit(0.15, "cm"), ends="both",angle=20,type = "closed"))+
  theme_minimal(base_size = 15)+
  theme(legend.position = "none",
        legend.justification=c(1, 1), 
        legend.margin = margin(0,0,0,1),
        strip.background = element_blank(),
        strip.text = element_text(size=13,hjust=0),
        plot.margin = unit(c(1,2,1,10),"mm"),
        panel.background = element_rect(fill="white", color=NA),
        panel.grid.minor = element_blank(),  # color="gray12"
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        #axis.text.x = element_blank(),
        #axis.ticks.x = element_blank(),
        axis.text = element_blank()  ,  #(size=10, color="white"),
        axis.title = element_text(size=12),
        #axis.line = element_line(),
        axis.line = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), ends = "last", angle=20, type = "closed"), lineend="round")
        #axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        #axis.title.x=element_text(hjust=0, vjust=7, size=11),
        #panel.border = element_rect(color="darkgray",fill=NA, size=0.2),
        #panel.spacing = unit(1, "lines")
  )  + 
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(size = 14), plot.tag.position = c(0.01, 0.99)) 



simus = gray_agent_simu()
graypoints = gray_result_analyzer(simus)
compdat = do_expmdat_fromfile("first", 0, "first_",bogus = c(15), accthreshold = 0, doinfandcompl = T)

ggplot(compdat %>% filter(acc>=0.59), aes(y=commcost, x=complexity, color=condition,fill=condition, shape=condition))+
  geom_point(aes(y=commcost, x=complexity), data=graypoints, color="gray94", size=8, shape=15, inherit.aes = F)+
  geom_point(aes(y=commcost, x=complexity), data=compdat %>% filter(acc<0.59), color="gray88",fill="gray88", size=2.3, shape=22, stroke=0, inherit.aes = F)+   # below threshold ones
  geom_count(stroke=1.3)+
  scale_shape_manual(values=c(22,0), name="Condition:")+
  scale_radius(  guide=F, range=c(0.9,15), limits = c(1,13))+
  scale_color_manual(values=brewer_pal()(9)[c(5,8)], name="Condition:") +
  scale_fill_manual(values=brewer_pal()(9)[c(5,8, NA)], name="Condition:", guide=F) +
  scale_x_continuous(limits=c(-0.05,2), expand=c(0.03,0))+
  labs(y=ytitle, x=xtitle)+
  annotate("text", x=0.1, y=-Inf,label="\u2039 simple\n\u2039 easier to learn", hjust=0, vjust=-0.2, size=4, color="gray40", lineheight=0.9)+
  annotate("text", x=Inf, y=-Inf,  label="complex \u203A\n harder to learn \u203A", hjust=1, vjust=-0.2, size=4, color="gray40", lineheight=0.9)+
  annotate("text", x=-Inf, y=0.1,label="\u2039 expressive\n\u2039 informative", hjust=0, vjust=1.1, angle=90, size=4, color="gray40",lineheight=0.9)+
  annotate("text", x=-Inf, y=1.7,label="ambiguous \u203A\nerror-prone \u203A", hjust=1, vjust=1.1, angle=90, size=4, color="gray40", lineheight=0.9)+
  guides(shape = guide_legend(override.aes = list(shape = c(22), fill=c(brewer_pal()(9)[c(5)], NA) )))+
  theme_minimal()+
  theme(legend.position = c(1,1),
        legend.background = element_rect(fill="gray98", color="gray98"),
        legend.justification=c(1, 1), 
        legend.margin = margin(0,0,0,1),
        strip.background = element_blank(),
        strip.text = element_text(size=13,hjust=0),
        plot.margin = unit(c(2,2,1,1),"mm"),
        panel.background = element_rect(fill="white", color=NA),
        panel.grid.minor = element_blank(),  # color="gray12"
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        #axis.text.x = element_blank(),
        axis.line=element_line(color="black"),
        #axis.line = element_line(arrow = grid::arrow(length = unit(0.2, "cm"), ends = "last", angle=20, type = "closed"), lineend="round"),
        #axis.ticks.x = element_blank(),
        axis.text = element_text(size=10, color="black"),
        #axis.title.x = element_blank(),
        axis.title = element_text(size=12)
        #axis.title.x=element_text(hjust=0, vjust=7, size=11),
        #panel.border = element_rect(color="darkgray",fill=NA, size=0.2),
        #panel.spacing = unit(1, "lines")
  )  +
  NULL




#### signal usage entropy ####


edat = rbind(
  do_expmdat_fromfile("first", 0, "first_",bogus = c(15), accthreshold = 0.59, doentropy = T),
  do_expmdat_fromfile("repli", 1000, "repli_", c(44,11), doentropy = T), 
  do_expmdat_fromfile("repli2", 2000, "repli_", doentropy = T),
  do_expmdat_fromfile("weakhyp", 3000, "weak_", doentropy = T), 
  do_expmdat_fromfile("weakhyp2", 4000, "weak_", bogus = c(5,24), doentropy = T),
  do_expmdat_fromfile("new", 5000, "new_", doentropy = T)
)

l = c("original\nbaseline", "original\ntarget condition", 
      "replication\nbaseline", "replication\ntarget condition",
      "weaker hypothesis\ntarget condition",
      "no–pressure\nbaseline", "no–pressure\ntarget condition"
)

ggplot(edat, aes(y=entropy, x=condition, group=condition)) + 
  #geom_bar( stat="identity", width = 0.9, color=NA) +
  # geom_boxplot(color=brewer_pal(palette="Blues")(9)[c(4)], 
  #              fill=brewer_pal(palette="Blues")(9)[c(1)], width=0.4, outlier.colour = NA ) +
  geom_vline(xintercept = c(2.5,4.5,5.5, 7.5), color="grey90", size=0.23) +
  geom_hline(yintercept = c(1.5,2), color="grey90", size=0.23) +
  annotate("text", label=paste0("Experiment ", 1:4), x=c(1.5, 3.5, 5, 6.5), y=2.3, vjust=-0.5) +
  stat_summary(fun = "median",  color="black", size=14, shape=95, geom="point")+
  geom_beeswarm(shape=45, size=4.5, priority = "density", color=brewer_pal(palette="Blues")(9)[c(6)] ,cex = 1.2)+
  # geom_signif(comparisons = list(
  #   c("repli_baseline","new_baseline"),
  #   c("repli_baseline", "new_target")
  # ) ,
  # tip_length = 0.01, annotations = c("", "***"), 
  # vjust=0.4, test=NULL, textsize=4) +
  # #geom_text(data=ns, mapping=aes(y=1, x=condition, label=n), inherit.aes = F, vjust=1) +
  ylab("Signal usage entropy, per dyad") +
  # annotate(geom = "shadowtext", x=1.4,y=c( 0.53, 0.99), 
  #          label=c("similar pairs\ncolexified", "similar pairs\nnot colexified"),
  #          angle=90, hjust=1,vjust=0, size=4.5
  #          )+
  geom_text(aes(x=x,y=y, label=label), data.frame(x=-Inf, y=c( 1.4, 2.2), 
                        label=c("\u2193 less varied signals used", "\u2191 more varied signals used")),
            inherit.aes = F,color="gray40",
            angle=0, hjust=-0.01,vjust=0.5, size=4.5
  )+
  #coord_cartesian(expand=c(1,0))+
  scale_y_continuous(expand = c(0,0), limits = c(1.3,2.4), breaks=c(1.5,2)  ) +
  scale_x_discrete( labels= l, expand=c(0.08,0) )+
  theme_minimal()+
  theme(legend.position = "none",
        legend.justification=c(1, 1), 
        legend.margin = margin(0,0,0,1),
        strip.background = element_blank(),
        strip.text = element_text(size=13,hjust=0),
        plot.margin = unit(c(2,2,1,1),"mm"),
        panel.background = element_rect(fill="white", color=NA),
        panel.grid.minor = element_blank(),  # color="gray12"
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        #axis.text.x = element_blank(),
        axis.line = element_line(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(size=10, color="black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        #axis.title.x=element_text(hjust=0, vjust=7, size=11),
        #panel.border = element_rect(color="darkgray",fill=NA, size=0.2),
        panel.spacing = unit(1, "lines")
  )  




