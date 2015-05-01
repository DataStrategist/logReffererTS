
library(dplyr)
library(tidyr)
library(ggplot2)

# a <- read.table("Logfile.log")

# 1= ip
# 2= ?
# 3= ?
# 4= datestamp xx
# 5= -0400? Part of the datestamp
# 6= Requested page
# 7= Code
# 8= Number of bytes
# 9=  Referring site!
# 10= User Agent

c <- a[,c(4,9)]

## Fix time and round to nearest hour
c$V4 <- gsub("\\[","",c$V4)
c$ts <- strptime(c$V4,format="%d/%b/%Y:%H:%M:%S")
c$ts <- round(c$ts,"hour")
names(c)[2] <- "dat"

## Figure out what are my top referrers
table(c$dat) %>%
  data.frame() %>% 
  arrange(desc(Freq)) %>% head(20)

## Exctract interesting links
c %>% 
  tbl_df() %>%
  mutate(facebook=grepl("facebook",dat)) %>%
  mutate(reddit=grepl("reddit",dat)) %>%
  mutate(ycombinator=grepl("ycombinator",dat)) %>% 
  mutate(ts=as.character(ts)) %>% 
  select(ts,facebook,reddit,ycombinator) -> d

## Get plottable tidy dataset
d %>% 
  gather(TrafficSource,val,-ts) %>%
  filter(val==TRUE) %>%
  mutate(Val=1) %>%
  select(ts,TrafficSource,Val) %>%
  group_by(ts,TrafficSource) %>%
  summarize(tot=sum(Val)) %>%
  arrange(ts) -> f

## Can't convert back to posix using tidyr, so do it like this :-\
f$tss= as.POSIXlt(f$ts)



ggplot(f,aes(x=tss,y=tot,group=TrafficSource,
             color=TrafficSource)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position=c(.9, .8)) +
  scale_x_datetime( breaks=("1 day"), 
                    minor_breaks=("8 hour"), 
                    labels=date_format("%d-%b"),
                    limits=c(as.POSIXct('2015/04/15'), 
                             as.POSIXct('2015/04/21'))) +
  ggtitle("Effect of posting something on Reddit") +
  xlab("Time") + ylab("Number of hits / hour") 

