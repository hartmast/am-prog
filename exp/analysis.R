# set options
options(stringsAsFactors = F)
Sys.setlocale("LC_ALL", "de_DE")


# read data
duration     <- read.csv("duration.csv", sep="\t", head=T, quote="",
                     encoding = "UTF-8")

acceptability <- read.csv("acceptability.csv", sep="\t", head=T, quote="",
                     encoding = "UTF-8")


##############################
# plot acceptability ratings #
##############################

# sort by mean acceptability
acceptability_mean <- c()

for(i in 1:nrow(acceptability)) {
  acceptability_mean[i] <- mean(na.omit(as.numeric(acceptability[i,(5:length(acceptability))])))
}

acceptability <- acceptability[order(acceptability_mean),]


# sort by pres vs prog
acceptability <- acceptability[order(acceptability$Type),]


# make a list

acceptability_list <- list()

for(i in 1:nrow(acceptability)) {
  acceptability_list[i] <- list(as.numeric(acceptability[i,(5:length(acceptability))]))
}


# plot

# jpeg("acceptability001.jpg", width=10, height=6, un="in", res=400)
par(mar=c(12, 4.1, 4.1, 2.1))
boxplot(acceptability_list, names=acceptability$Question,
        col=c(rep("green",22), rep("blue",22)), xaxt="n", main="Akzeptabilit\u00e4t")
axis(1, at=1:44, labels=FALSE)
text(x=seq_along(acceptability$Question), y = par("usr")[3] - 0.2,
     labels=acceptability$Question, srt=45, adj=1, xpd=TRUE,
     cex=0.6)
# dev.off()
par(mar=c(5, 4, 4, 2) + 0.1)



######################
# DURATION ESTIMATES #
######################

# make a list (again)

allquestionslist <- list()
for(i in 1:nrow(duration)) {
  allquestionslist[i] <- list(as.numeric(duration[i,6:length(duration)]))
}


# plot

# jpeg("duration_short.jpg", width=15, height=10, un="in", res=400)
par(xpd=T)
par(mar=c(17, 4.1, 4.1, 2.1))
boxplot(allquestionslist[which(duration$Length=="short")], xaxt="n", col=rep(c("green", "blue"), 7),
        main="Kurze Ereignisse: Sch\u00e4tzung der Dauer", cex.main=0.8, cex.axis=0.8)
axis(1, at=1:14, labels=FALSE)
lines(-1:44, rep(500, 46), lty=2, col="blue")
text(x=seq_along(duration$Question[which(duration$Length=="short")]), y = par("usr")[3] - 0.2,
     labels=duration$Question[which(duration$Length=="short")], srt=45, adj=c(1.05,1.7), xpd=TRUE,
     cex=0.7)
# dev.off()
par(mar=c(5, 4, 4, 2) + 0.1)
par(xpd=F)

# jpeg("duration_long.jpg", width=15, height=10, un="in", res=400)
par(xpd=T)
par(mar=c(18, 4.1, 4.1, 2.1))
boxplot(allquestionslist[which(duration$Length %in% c("medium", "long"))], xaxt="n", col=rep(c("green", "blue"), 15),
        main="(Mittel-)Lange Ereignisse: Sch\u00e4tzung der Dauer", cex.main=0.8, cex.axis=0.8)
axis(1, at=1:30, labels=FALSE)
lines(-1:44, rep(500, 46), lty=2, col="blue")
text(x=seq_along(duration$Question[which(duration$Length %in% c("medium", "long"))]), y = par("usr")[3] - 0.3,
     labels=duration$Question[which(duration$Length %in% c("medium", "long"))], srt=45, adj=c(1.05,1.7), xpd=TRUE,
     cex=1)
# dev.off()
par(mar=c(5, 4, 4, 2) + 0.1)
par(xpd=F)
