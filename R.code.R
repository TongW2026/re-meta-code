library(meta)
library(metafor)
library(metan)
library(dmetar)



file_path <- "C:/Users/Desktop/data.csv" 
rawdata <- read.csv(file_path)



# Meta-analysis of Main Effects

meta <- metacont(
  n.e = n.e,
  mean.e = mean.e,
  sd.e = sd.e,
  n.c = n.c,
  mean.c = mean.c,
  sd.c = sd.c,
  studlab = study_ID,
  data = rawdata,
  sm = "SMD",
  method.smd = "Hedges",
  method.tau = "DL",
  common = FALSE,
  random = TRUE
)

# Prediction Interval
meta <- update(meta, prediction = TRUE)
forest(meta, 
       col.predict = "black",
       test.overall = TRUE,   
       print.stat = TRUE,
       digits.sd = 2)

funnel(meta)

# Egger’s test
funnel(meta, main = "Funnel Plot") 
egger_test <- metabias(meta, method.bias = "linreg", k.min = 3)
print(egger_test$p.value, digits = 3)

# Find outliers
outliers <- find.outliers(meta, control = list(maxiter = 10000, stepadj = 0.5))
outliers

# Excess significance test
m.rma <- rma(
  yi = meta$TE,
  sei = meta$seTE,
  method = meta$method.tau
)

es <- tes(m.rma)
print(es$pval, digits = 6)