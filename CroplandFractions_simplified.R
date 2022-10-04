# this was a quickly strewn together script for producing a simplified dataset from  "results//N_Est_0930.csv" and will need to be integrated with the main scripts at the same time I do some housekeeping with the repository.

blah <- read.csv("results//N_Est_0930.csv")
blah <- blah[which(blah$source!='default'),]
for (iso in unique(blah$Country)){
  print(nrow(blah[which(blah$Country==iso),]))
}

blah <- blah[,c(2,5,6)]



nrow(blah[which(blah$Country=='New Zealand'),])
nrow(blah[which(blah$Country=='Chile'),])
tst2 <- blah[which(blah$Country=='Chile'),]


blah_tst <- reshape(blah, idvar = "year", timevar = "Country", direction = "wide")
names(blah_tst)[2:ncol(blah_tst)]=str_sub(names(blah_tst),7)[2:ncol(blah_tst)]

write.csv(blah_tst,"results//N_Est_0930_simple.csv")
