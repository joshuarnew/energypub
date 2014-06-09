library(ggplot2)
library(scales)
library(grid)
library(gridExtra)


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pdf("figure1.pdf")
e1data = read.table("e1.csv", header=T, sep=",")
attach(e1data)

p1 <- ggplot(data=e1data, aes(x=e1data[Trial==1, 2], y=e1data[Trial==1, 3])) + 
      geom_point(colour=cbbPalette[2]) +
      theme(panel.background = element_rect(fill = "#FFFFEE")) + 
      theme(plot.background = element_rect(fill = "transparent", colour = NA)) +
      theme(legend.position = "none") + 
      xlab(expression(paste("Four Day ",SAE[hour]," (kWh)"))) +
      ylab(expression(paste("Yearly ",SAE[hour]," (kWh)"))) +
      annotate("text", x=66.1, y=7225, label="R == 0.9697", parse=TRUE, colour="black") + 
      annotate("text", x=66.2, y=7150, label="R^{2} == 0.9403", parse=TRUE, colour="black") + 
      stat_smooth(method="lm", se=FALSE, colour="black")
      
p2 <- ggplot(data=e1data, aes(x=e1data[Trial==3, 2], y=e1data[Trial==3, 3])) + 
      geom_point(colour=cbbPalette[3]) +
      theme(panel.background = element_rect(fill = "#FFFFEE")) + 
      theme(plot.background = element_rect(fill = "transparent", colour = NA)) +
      theme(legend.position = "none") + 
      xlab(expression(paste("Four Day ",SAE[hour]," (kWh)"))) +
      ylab(expression(paste("Yearly ",SAE[hour]," (kWh)"))) +
      annotate("text", x=66.5, y=7225, label="R == 0.9704", parse=TRUE, colour="black") + 
      annotate("text", x=66.6, y=7150, label="R^{2} == 0.9417", parse=TRUE, colour="black") + 
      stat_smooth(method="lm", se=FALSE, colour="black")
      
p3 <- ggplot(data=e1data, aes(x=e1data[Trial==2, 2], y=e1data[Trial==2, 3])) + 
      geom_point(colour=cbbPalette[4]) +
      theme(panel.background = element_rect(fill = "#FFFFEE")) + 
      theme(plot.background = element_rect(fill = "transparent", colour = NA)) +
      theme(legend.position = "none") + 
      xlab(expression(paste("Four Day ",SAE[hour]," (kWh)"))) +
      ylab(expression(paste("Yearly ",SAE[hour]," (kWh)"))) +
      annotate("text", x=66.5, y=7225, label="R == 0.9472", parse=TRUE, colour="black") + 
      annotate("text", x=66.6, y=7150, label="R^{2} == 0.8972", parse=TRUE, colour="black") + 
      stat_smooth(method="lm", se=FALSE, colour="black")
      
p4 <- ggplot(data=e1data, aes(x=e1data[Trial==4, 2], y=e1data[Trial==4, 3])) + 
      geom_point(colour=cbbPalette[6]) +
      theme(panel.background = element_rect(fill = "#FFFFEE")) + 
      theme(plot.background = element_rect(fill = "transparent", colour = NA)) +
      theme(legend.position = "none") + 
      xlab(expression(paste("Four Day ",SAE[hour]," (kWh)"))) +
      ylab(expression(paste("Yearly ",SAE[hour]," (kWh)"))) +
      annotate("text", x=66.5, y=7225, label="R == 0.9382", parse=TRUE, colour="black") + 
      annotate("text", x=66.6, y=7150, label="R^{2} == 0.8802", parse=TRUE, colour="black") + 
      stat_smooth(method="lm", se=FALSE, colour="black")
      
grid.arrange(p1, p2, p3, p4, ncol=2)
# par(mfcol=c(2,2), mar=c(4,4,0.5,0.5), oma=c(1.5,2,1,1))
# plot(e1data[Trial==1,2], e1data[Trial==1,3], pch=20, xlim=c(65, 80), ylim=c(5600, 7290), xlab="", ylab=expression(paste("Yearly ",SAE[hour]," (kWh)")))
# abline(lm(e1data[Trial==1, 3]~e1data[Trial==1, 2]))
# text(65, 7200, "r=0.9697", pos=4)
# text(65, 7050, expression(paste("r"^"2", "=0.9403")), pos=4)
# text(72.5, 5650, "Trial 1")
# plot(e1data[Trial==3,2], e1data[Trial==3,3], pch=20, xlim=c(65, 80), ylim=c(5600, 7290), xlab=expression(paste("Four Day ",SAE[hour]," (kWh)")), ylab=expression(paste("Yearly ",SAE[hour]," (kWh)")))
# abline(lm(e1data[Trial==3, 3]~e1data[Trial==3, 2]))
# text(65, 7200, "r=0.9704", pos=4)
# text(65, 7050, expression(paste("r"^"2", "=0.9417")), pos=4)
# text(72.5, 5650, "Trial 3")
# plot(e1data[Trial==2,2], e1data[Trial==2,3], pch=20, xlim=c(65, 80), ylim=c(5600, 7290), xlab="", ylab="")
# abline(lm(e1data[Trial==2, 3]~e1data[Trial==2, 2]))
# text(65, 7200, "r=0.9472", pos=4)
# text(65, 7050, expression(paste("r"^"2", "=0.8972")), pos=4)
# text(72.5, 5650, "Trial 2")
# plot(e1data[Trial==4,2], e1data[Trial==4,3], pch=20, xlim=c(65, 80), ylim=c(5600, 7290), xlab=expression(paste("Four Day ",SAE[hour]," (kWh)")), ylab="")
# abline(lm(e1data[Trial==4, 3]~e1data[Trial==4, 2]))
# text(65, 7200, "r=0.9382", pos=4)
# text(65, 7050, expression(paste("r"^"2", "=0.8802")), pos=4)
# text(72.5, 5650, "Trial 4")


dev.off()
detach(e1data)


pdf("figure2.pdf")
e5data = read.table("e5.csv", header=T, sep=",")
attach(e5data)
e5data$Trial <- factor(e5data$Trial)
p <- ggplot(data=e5data, aes(x=Generation, y=SAE, group=Trial, colour=Trial)) + 
     geom_line() +
     scale_color_manual(values=cbbPalette) +
     xlab("Generation") +
     ylab(expression(paste("Four Day ",SAE[hour]," (kWh)"))) +
     theme(panel.background = element_rect(fill = "#FFFFEE")) + 
     theme(plot.background = element_rect(fill = "transparent", colour = NA)) +
     theme(legend.position=c(1, 1), legend.justification=c(1, 1)) +
     theme(legend.key = element_blank()) +
     theme(legend.background = element_rect(fill = "#FFFFFF", colour="#000000", size=0.5)) +
     theme(legend.key.width = unit(0.5, "cm")) +
     geom_vline(xintercept=12, linetype="dashed")
print(p)
dev.off()


pdf("figure3.pdf")
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
    scale_fill_manual(values=cbbPalette, name="Method", breaks=methodOrd, 
                      labels=c("Baseline", "Abbreviated (E2)", "Full (E3)", "Serial (E4)", "Parallel (E5)")) +
    #ggtitle("Summary of Experimental Results") +
    scale_y_continuous(limits=c(5000, 8100), oob=rescale_none) +
    theme(panel.background = element_rect(fill = "#FFFFEE")) + 
    theme(plot.background = element_rect(fill = "transparent", colour = NA)) +
    theme(legend.position=c(0.85, 0.85), legend.background = element_rect(fill="#FFFFEE"))

print(p)
dev.off()
