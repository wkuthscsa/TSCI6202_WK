source("global.R")

table(is.na(demographics$DEATHDATE), demographics$STATE)

#plot(survivalmodel,col = c('red','green'))
# Default publication ready plot
survfit2(survival~STATE, demographics) %>%
  ggsurvfit() + ylab("Fraction Alive") +
  xlab("Years Since Birth") + scale_color_manual(values=c('red','darkgreen')) +
  add_censor_mark() +
  add_confidence_interval() +
  add_quantile() +
  add_risktable()

# Bad Fit survival curves
survfit2(survival ~ STATE, data = demographics) %>% 
  # Bad Basic survival curves
  ggsurvplot(data = demographics)

table(demographics$RACE, demographics$ETHNICITY, demographics$GENDER) %>% prop.table %>% data.frame()

ggplot(demographics, aes(x=RACE, y=timetoevent, color=GENDER)) +
  geom_boxplot(outliers=FALSE, notch=FALSE, color="black", fill='white') +
  geom_jitter(aes(color=as.numeric(timetoevent)), width=0.1) +
  #geom_jitter(color="brown", width=0.1, data=subset(demographics,STATE!="Massachusetts"),shape=22) +
  scale_color_continuous(palette = c("black","white"),
                         aesthetics = "color",
                         guide = "colourbar",
                         na.value = "red",
                         type = getOption("ggplot2.continuous.colour")) +
  #geom_jitter(color="blue", width=0.1, data=subset(demographics,ETHNICITY=="hispanic"),shape="\u2665",size=5) +           
  
  facet_grid(rows=vars(MARITAL), cols=vars(STATE))

subset(demographics,is.na(MARITAL)&RACE=="asian") %>% pander()

pchShow <-
  function(extras = c("*",".", "o","O","0","+","-","|","%","#"),
           cex = 3, ## good for both .Device=="postscript" and "x11"
           col = "red3", bg = "gold", coltext = "brown", cextext = 1.2,
           main = paste("plot symbols :  points (...  pch = *, cex =",
                        cex,")"))
  {
    nex <- length(extras)
    np  <- 26 + nex
    ipch <- 0:(np-1)
    k <- floor(sqrt(np))
    dd <- c(-1,1)/2
    rx <- dd + range(ix <- ipch %/% k)
    ry <- dd + range(iy <- 3 + (k-1)- ipch %% k)
    pch <- as.list(ipch) # list with integers & strings
    if(nex > 0) pch[26+ 1:nex] <- as.list(extras)
    plot(rx, ry, type = "n", axes  =  FALSE, xlab = "", ylab = "", main = main)
    abline(v = ix, h = iy, col = "lightgray", lty = "dotted")
    for(i in 1:np) {
      pc <- pch[[i]]
      ## 'col' symbols with a 'bg'-colored interior (where available) :
      points(ix[i], iy[i], pch = pc, col = col, bg = bg, cex = cex)
      if(cextext > 0)
        text(ix[i] - 0.3, iy[i], pc, col = coltext, cex = cextext)
    }
  }

pchShow()



#Continuous variables
ggplot(demographics, aes(x=INCOME, y=HEALTHCARE_EXPENSES))+
  geom_point(color="darkgreen", size=0.5, alpha=0.11)+
  geom_smooth(fill="blue",color="darkred",alpha=0.2)+
  geom_smooth(method="lm", se=FALSE)+
  geom_abline(slope=1,intercept=0,color="darkorange",linetype=2)+
  theme_classic()

ggplot(mtcars, aes(x=INCOME, y=HEALTHCARE_EXPENSES))+
  geom_point(color="darkgreen", size=0.5, alpha=0.11)+
  geom_smooth(fill="blue",color="darkred",alpha=0.2)+
  geom_smooth(method="lm", se=FALSE)+
  geom_abline(slope=1,intercept=0,color="darkorange",linetype=2)+
  theme_classic()

myplot0<-list(geom_point(color="darkgreen", size=0.5, alpha=0.7), #alpha= transparency
              geom_smooth(fill="blue", color="darkred", alpha=0.2),
              geom_smooth(method = "lm", se=FALSE),
              geom_abline(slope = 1, intercept = 0, color="darkorange", linetype = 2),
              theme_classic())

myplot0
ggplot(demographics, aes(x=HEALTHCARE_COVERAGE, y=log(HEALTHCARE_EXPENSES)))+myplot0
ggplot(demographics, aes(x=INCOME, y=log(HEALTHCARE_EXPENSES)))+myplot0       
ggplot(mtcars, aes(x=wt, y=mpg))+myplot0


superheat(mtcars,scale=TRUE,pretty.order.row = TRUE)
arrange(mtcars,desc(mpg)) %>% superheat(scale=TRUE)
cor(mtcars,use="pairwise.complete.obs") %>% superheat(scale=FALSE)
abs(cor(mtcars,use="pairwise.complete.obs")) %>% superheat(scale=FALSE)
