pdf("figure1.pdf")
e1data = read.table("e1.csv", header=T, sep=",")
attach(e1data)
par(mfcol=c(2,2), mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
plot(e1data[Trial==1,2], e1data[Trial==1,3], pch=20, xlim=c(65, 80), ylim=c(5600, 7290), xlab="", ylab=expression(paste("Yearly ",SAE[hour]," (kWh)")))
abline(lm(e1data[Trial==1, 3]~e1data[Trial==1, 2]))
text(65, 7200, "r=0.9697", pos=4)
text(65, 7050, expression(paste("r"^"2", "=0.9403")), pos=4)
text(72.5, 5650, "Trial 1")
plot(e1data[Trial==3,2], e1data[Trial==3,3], pch=20, xlim=c(65, 80), ylim=c(5600, 7290), xlab=expression(paste("Four Day ",SAE[hour]," (kWh)")), ylab=expression(paste("Yearly ",SAE[hour]," (kWh)")))
abline(lm(e1data[Trial==3, 3]~e1data[Trial==3, 2]))
text(65, 7200, "r=0.9704", pos=4)
text(65, 7050, expression(paste("r"^"2", "=0.9417")), pos=4)
text(72.5, 5650, "Trial 3")
plot(e1data[Trial==2,2], e1data[Trial==2,3], pch=20, xlim=c(65, 80), ylim=c(5600, 7290), xlab="", ylab="")
abline(lm(e1data[Trial==2, 3]~e1data[Trial==2, 2]))
text(65, 7200, "r=0.9472", pos=4)
text(65, 7050, expression(paste("r"^"2", "=0.8972")), pos=4)
text(72.5, 5650, "Trial 2")
plot(e1data[Trial==4,2], e1data[Trial==4,3], pch=20, xlim=c(65, 80), ylim=c(5600, 7290), xlab=expression(paste("Four Day ",SAE[hour]," (kWh)")), ylab="")
abline(lm(e1data[Trial==4, 3]~e1data[Trial==4, 2]))
text(65, 7200, "r=0.9382", pos=4)
text(65, 7050, expression(paste("r"^"2", "=0.8802")), pos=4)
text(72.5, 5650, "Trial 4")
dev.off()
detach(e1data)


pdf("figure2.pdf")
e5data = read.table("e5.csv", header=T, sep=",")
attach(e5data)
colors = gray.colors(8, start=0.0, end=0.7)#rainbow(8)
plot(e5data[Trial==1, 2], e5data[Trial==1, 3], type="n", xlim=c(0, 47), ylim=c(10, 14.2), xlab="Generation", ylab=expression(paste("Four-day ",SAE[hour]," (kWh)")))
for(i in 1:8) {
    lines(e5data[Trial==i, 2], e5data[Trial==i, 3], col=colors[i], lwd=2)
}
abline(v=12, lty="dashed")
legend(40, 14.2, c("1", "2", "3", "4", "5", "6", "7", "8"), col=colors, lty="solid", lwd=2, title="Trial")
dev.off()
detach(e5data)


pdf("figure3.pdf")
library(ggplot2)
library(scales)
sumdata = read.table("summary.csv", header=T, sep=",")
methodOrd = c("Baseline", "Abbreviated", "Full", "Serial", "Parallel")
p <- ggplot(sumdata, aes(x=model, y=average, fill=factor(method, as.character(methodOrd)))) + 
    geom_bar(position=position_dodge(), stat="identity",
             #colour="black", # Use black outlines,
             size=.3) +      # Thinner lines
    geom_errorbar(aes(ymin=average-se, ymax=average+se),
                  size=.3,    # Thinner lines
                  width=.2,
                  position=position_dodge(.9)) +
    xlab("Model") +
    ylab("Average SAE (kWh)") +
    scale_fill_grey(name="Method", 
                    breaks=methodOrd, 
                    labels=c("Baseline", "Abbreviated (E2)", "Full (E3)", "Serial (E4)", "Parallel (E5)"), start=0.1, end=0.9) +
    ggtitle("Summary of Experimental Results") +
    scale_y_continuous(limits=c(5000, 8100), oob=rescale_none) +
    theme_bw() + 
    theme(legend.position=c(0.85, 0.85)) 
print(p)
dev.off()
