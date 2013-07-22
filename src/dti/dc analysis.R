

	library(gtools)

setwd('/Users/dti/Dropbox/Inbox/TACM results DC/new')


lf <- mixedsort(list.files(pattern="T.+dat"))



out<-list()

for (fn in lf) {

	load(fn)

	out[[fn]] <- tmp

}



violations.dc <- data.frame(model = ordered(rep(c("UNC","MON","IIO","DM","LCR","RSH"), each = 50), levels = c("UNC","MON","IIO","DM","LCR","RSH")), 
                              unwei = sapply(out, function(x) x@means$unweighted), 
                              weigh = sapply(out, function(x) x@means$weighted))

pdf.options(family = 'Palatino')

boxplot(weigh ~ model, violations.dc, at =rev(1:nlevels(violations.dc$model)), ylim = c(0,0.8), horizontal = TRUE)
dev.print(device = pdf, file = 'violations_dc_weighted.pdf')

boxplot(unwei ~ model, violations.dc, ylim = c(0,0.8))
dev.print(device = pdf, file = 'violations_dc_unweighted.pdf')

boxplot(weigh ~ model, violations.dc, at =rev(1:nlevels(violations.dc$model)), ylim = c(0,0.6), horizontal = TRUE, boxwex = .5, las = 1, xlab = "Percentage of Violations", col = "grey70")
dev.print(device = pdf, file = 'violations_dc_weighted.pdf')
