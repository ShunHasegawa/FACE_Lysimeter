head(lys)

lys$SumN <- lys$no + lys$nh
lys$Nratio <- lys$SumN/lys$tn

lys <- lys[complete.cases(lys),]

pdf("output//figs/TNComparison.pdf", width = 6, height = 3)
par(mfrow = c(1, 2), mar = c(5, 4, 1, 1))
plot(SumN ~ tn, data = lys, xlab = "Total N by TOC analyser", ylab = "")
mtext(side = 2, expression(NH[4]^"+"~+~NO[3]^"-"~by~AQ2), line = 2)
abline(0, 1, lty = 2, col = "red")
text(10, 30, labels = "y = x", col = "red")

plot(lys$Nratio, ylab = "")
mtext(side = 2, expression(frac(NH[4]^"+"~+~NO[3]^"-"~by~AQ2, Total~N~by~TOC)),
      line = 2, cex = .8)
abline(h = 1, lty = 2, col = "red")
dev.off()
