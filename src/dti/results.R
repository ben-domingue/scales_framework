##set working directory
##change for each person!
setwd("C:/Users/Ronli/Documents/GitHub/scales_framework/src/dti")
setwd("/Users/dti/Dropbox/Research/simdata3")
##read in simulation results
load("scale_cols.Rdata")  #obj called out_cols
load("scale_rows.Rdata")  #obj called out_rows

##reorder
out_cols_ord <- out_cols[mixedsort(names(out_cols))]
out_rows_ord <- out_rows[mixedsort(names(out_rows))]

##pull out proportion of violations
violations.cols <- data.frame(model = ordered(rep(c("UNC","MON","IIO","DM","LCR","RSH"), each = 50), levels = c("UNC","MON","IIO","DM","LCR","RSH")), 
                              unwei = sapply(out_cols_ord, function(x) x@means$unweighted), 
                              weigh = sapply(out_cols_ord, function(x) x@means$weighted))

violations.rows <- data.frame(model = ordered(rep(c("UNC","MON","IIO","DM","LCR","RSH"), each = 50), levels = c("UNC","MON","IIO","DM","LCR","RSH")), 
                              unwei = sapply(out_rows_ord, function(x) x@means$unweighted), 
                              weigh = sapply(out_rows_ord, function(x) x@means$weighted))



library(psych)
describeBy(violations.cols, violations.cols$model)


##plot results

boxplot(weigh ~ model, violations.cols, at =rev(1:nlevels(violations.cols$model)), ylim = c(0,0.4), horizontal = TRUE, boxwex = .5, las = 1, xlab = "Percentage of Violations", col = "grey70")
#boxplot(weigh ~ model, violations.cols, ylim = c(0,0.4))
dev.print(device = pdf, file = 'violations_columns_weighted.pdf')

boxplot(weigh ~ model, violations.rows, at =rev(1:nlevels(violations.rows$model)), ylim = c(0,0.4), horizontal = TRUE, boxwex = .5, las = 1, xlab = "Percentage of Violations", col = "grey70")
#boxplot(weigh ~ model, violations.rows, ylim = c(0,0.4))
dev.print(device = pdf, file = 'violations_rows_weighted.pdf')

boxplot(weigh ~ model, violations.cols, at =rev(1:nlevels(violations.cols$model)), ylim = c(0,0.4), horizontal = TRUE, boxwex = .5, las = 1, xlab = "Percentage of Violations", col = "grey70")
#boxplot(unwei ~ model, violations.cols, ylim = c(0,0.4))
dev.print(device = pdf, file = 'violations_columns_unweighted.pdf')

boxplot(weigh ~ model, violations.rows, at =rev(1:nlevels(violations.rows$model)), ylim = c(0,0.4), horizontal = TRUE, boxwex = .5, las = 1, xlab = "Percentage of Violations", col = "grey70")
#boxplot(unwei ~ model, violations.rows, ylim = c(0,0.4))
dev.print(device = pdf, file = 'violations_rows_unweighted.pdf')



####################################################################################################################
##random code for figuring stuff out
load("./simdata/T66.Rdata")  #obj called model.LOResp
table(colSums(model.LOResp$obsData))


out_rows[[66]]@n / out_cols[[66]]@N
