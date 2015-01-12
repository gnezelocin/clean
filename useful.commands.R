################## ENVIRONMENT ##################
# remove all but x in environment
rm(list=setdiff(ls(), "x")) 
# remove all elements with regex pattern from environment
rm(list=ls(pattern="###"))
# list all elements with a regex pattern
ls(pattern="###")
# remove package
detach("package: ")

############### PACKAGES #############
library("data.table")
library("plyr")
library("dplyr")
library("tm")
library("wordcloud")
library("stringr")
library("lubridate")
library("ggplot2")
library("stargazer")
library("psych")

########## USEFUL COMMANDS ##########
# Counter, within group
msg.awake[, t:=.GRP, by=date]
?iconv
# Create factor order
mydata$v1 <- factor(mydata$v1, levels = c(1,2,3), labels = c("red", "blue", "green"))
################## FUNCTIONS ##################
# standard error
std <- function(x) sd(x, na.rm=TRUE)/sqrt(length(x))
# Conficence Intervals
CI <- function(x) (sd(x, na.rm=TRUE)/sqrt(length(x)))*1.96

# Logical x without y
without <- function(x, y) 
  x[!x %in% y] 
# Logical - x between a and b (inclusive). Change to ">" if you want it to be exclusive
is.between <- function(x, a, b) {
  (x - a)  *  (b - x) >= 0 
}
# mean without na's
mmean <- function(x) mean(x, na.rm= TRUE)
# string count (char = charac. to search for; s = object)
countCharOccurrences <- function(char, s) {
  s2 <- gsub(char,"",s)
  return (nchar(s) - nchar(s2))
}
# get widths for various string variables
getWidths <- function(var) {
  xdf <- as.list(var)
  xdf.split <- lapply(xdf, function(x) strsplit(x, ","))
  xdf.width <- lapply(xdf.split, function(x) length(unlist(x)))
  xdf.width1 <- data.frame(matrix(unlist(xdf.width), nrow=length(xdf.width), byrow=TRUE))  
  return(xdf.width1)
}
# split strings in string variable
stringExtract <- function(var, s) {
  xdf <- as.list(var)
  xdf.2 <- lapply(xdf, function(x) str_extract(x, s))
  xdf.3 <- data.frame(matrix(xdf.2, nrow=length(xdf.2), byrow=TRUE))
  colnames(xdf.3)[1] <- tolower(paste(s))
  return(xdf.3)
}

################## PLOTS ##################
# parameter window adjustment
dev.off()
par(mfrow=c(1,1))
# transparency plots
plot(dt$varx, dt$vary,  pch = 21, col = "#0000EE44")
plot(dt$varx, dt$vary, subset=bad==0, pch=21, col="#0000EE44")
plot(dt$varx,  subset=var==0, pch = 21, col = "#0000EE44")
# colors for plots
col=terrain.colors(length("var"))
col=topo.colors(length("var"))
col=cm.colors(length("var"))
col=heat.colors(length("var"))
col=rainbow(length("var"))
magenta = "#00B2FF"
purple = "#FF0099"
green = "#6600CC"
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # color-blind friendly palette


############ ggplot2 of 2 groups ##########
graph <- ggplot(dt, aes(var, fill = group)) 
graph + geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity', binwidth=1)

# Other things to add:
+ ggtitle("  ")
+ xlab(" ") + ylab(" ")
+ scale_fill_discrete(name=" ")
+ geom_histogram(position="fill", binwidth=1) # position="fill" is 100% bars, = "identity" is normal, and = "stack" is stacked bar graph
+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) # turns x axis text 90 degress
+ scale_fill_manual(values=brewer.pal(12,"Dark2"))
last_plot() + coord_flip() # flips x and y axis of last plot
+ scale_fill_manual(values = c("orange", "purple"))
###### base plot of 2 groups ##########
# first plot
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10))  
# second
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)  
# alternative base histogram, multi-groups
hist(df$var[df$var==" "], breaks= , col="   ")
hist(df$var[df$var==" "], breaks= , col="   ", add = TRUE)
# bty = "n"

######### R COOKBOOK FOR GGPLOT2 #########
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

######## Text Analysis ########
clean.text = function(x) {
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

######### Word Clouds #########
require(wordcloud)
getCompCloud.brew <- function(m, sc1, sc2, mwords, brcol, ntitle) { comparison.cloud(m, scale=c(sc1, sc2), max.words=mwords, random.order=FALSE, rot.per=FALSE, colors=brewer.pal(ncol(m), paste(brcol)), use.r.layout=FALSE, title.size=ntitle)
}
getCompCloud.pair <- function(m, sc1, sc2, mwords, c1, c2, ntitle) { comparison.cloud(m, scale=c(sc1, sc2), max.words=mwords, random.order=FALSE, rot.per=FALSE, colors=c(paste(c1), paste(c2)), use.r.layout=FALSE, title.size=ntitle)
}
getCommonCloud <- function(m, sc1, sc2, mwords) { commonality.cloud(m, scale=c(sc1, sc2), max.words=mwords, random.order=FALSE, rot.per=FALSE, colors=brewer.pal(5, "Set1"), use.r.layout=FALSE)
}
cloudText <- function(text1, text2) { text(0.5, 1, paste(text1, text2, sep="\n"))}

######### Barplot for Text #########
MakeWordBar <- function(text, min.freq=2, top = 50, format = 'count', col = "steelblue", color,...){
  if(!missing(color)) {
    stop("Remember to use the argument 'col' and not 'color'.")
  }
  if (class(text)[1]!="VCorpus"){
    stop("Remember to initialize text using initializeText()")
  }
  if (!missing(format) & (format != 'count' & format != 'percent')) {
    stop("invalid 'format' argument. 'format' must either be 'count' or 'percent'")
  }
  old.par <- par(no.readonly = TRUE)$mar
  tdm <- TermDocumentMatrix(text)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing=T)
  d <- data.frame(word=names(v), freq=v, row.names=NULL)
  d <- d[d$freq>=min.freq,]
  if (format == 'count') {
    if (!missing(top) & (top <= 0)) {
      stop("'top' must be a number greater than 0")
    }
    top.words <- head(d$freq, n = top)
    top.words.names <- head(d$word, n = top)
    largest.word <- max(nchar(as.character(top.words.names)))
    if (largest.word > 8) {
      adj = (largest.word - 7) / 3
      par(mar = old.par + c(adj, 0, 0, 0))
    } else {
      par(mar = old.par)
    }
  }
  if (format == 'percent') {
    if (missing(top)) {
      stop("'top' argument must be specificed when using format = 'percent'")
    }
    if (!missing(top) & (top <= 0 | top > 100 )) {
      stop("'top' must be a number greater than 0 and less than or equal to 100")
    }
    n <- floor(nrow(d) * top/100)
    top.words <- head(d$freq, n = n)
    top.words.names <- head(d$word, n = n)
    largest.word <- max(nchar(as.character(top.words.names)))
    if (largest.word > 8) {
      adj = (largest.word - 7) / 3
      par(mar = old.par + c(adj, 0, 0, 0))
    } else {
      par(mar = old.par)
    }    
  }  
  barplot(top.words, names.arg=top.words.names, las = 2, col = col, border = "white", ...)
  par(mar = old.par)
}


########## Test for Z-scores ###########
z <- # assign some value
2*pnorm(-abs(z))
