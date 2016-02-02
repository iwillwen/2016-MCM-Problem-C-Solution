library(e1071)
library(neuralnet)
library(stats)
library(ggplot2)

# Read and clean the database
schools <- read.csv("data.csv", na.strings = c("NULL", "PrivacySuppressed"), header = TRUE)
potential.candidate.schools <- read.csv("potential_candidate_schools.csv", header = TRUE)

schools[is.na(schools)] <- 0
names(schools)[121:122] <- c("MD_EARN_WNE_P10", "GT_25K_P6")

schools$MD_EARN_WNE_P10 <- as.numeric(as.character(schools$MD_EARN_WNE_P10))
schools$GT_25K_P6 <- as.numeric(as.character(schools$GT_25K_P6))

# Using SVM to classify the schools with type *Science* and *Art*
# according to the degrees awarded of different subjects
academics.table <- schools[46:83]
academics.svm.model <- svm(academics.table)
academics.svm.pred <- predict(academics.svm.model, academics.table)

type.1.schools <- schools[as.numeric(names(academics.svm.pred[academics.svm.pred])), ]
type.2.schools <- schools[as.numeric(names(academics.svm.pred[!academics.svm.pred])), ]

# Type 1 Schools
#   SAT scores and ACT scores as the input values and repayments as the output values
type.1.schools.train <- type.1.schools[!is.na(type.1.schools$MD_EARN_WNE_P10), ]
type.1.schools.train <- type.1.schools.train[!is.na(type.1.schools.train$GT_25K_P6), ]
type.1.schools.train <- subset(type.1.schools.train, select = c(SATVR25,SATVR75,SATMT25,SATMT75,SATWR25,SATWR75,SATVRMID,SATMTMID,SATWRMID,ACTCM25,ACTCM75,ACTEN25,ACTEN75,ACTMT25,ACTMT75,ACTWR25,ACTWR75,ACTCMMID,ACTENMID,ACTMTMID,ACTWRMID,SAT_AVG,SAT_AVG_ALL,MD_EARN_WNE_P10,GT_25K_P6))
type.1.schools.train$MD_EARN_WNE_P10 <- type.1.schools.train$MD_EARN_WNE_P10  / 1e5
attach(type.1.schools.train)
type.1.schools.train.matrix <- model.matrix(~MD_EARN_WNE_P10+GT_25K_P6+SATVR25,SATVR75,SATMT25,SATMT75,SATWR25,SATWR75,SATVRMID,SATMTMID,SATWRMID,ACTCM25,ACTCM75,ACTEN25,ACTEN75,ACTMT25,ACTMT75,ACTWR25,ACTWR75,ACTCMMID,ACTENMID,ACTMTMID,ACTWRMID,SAT_AVG,SAT_AVG_ALL, data = type.1.schools.train)
#   Training the ANN to calculate the repayment data that which the schools did not provide
type.1.schools.net <- neuralnet(MD_EARN_WNE_P10+GT_25K_P6~SATVR25+SATVR75+SATMT25+SATMT75+SATWR25+SATWR75+SATVRMID+SATMTMID+SATWRMID+ACTCM25+ACTCM75+ACTEN25+ACTEN75+ACTMT25+ACTMT75+ACTWR25+ACTWR75+ACTCMMID+ACTENMID+ACTMTMID+ACTWRMID+SAT_AVG+SAT_AVG_ALL, type.1.schools.train.matrix,
                                hidden = 7, rep = 10, linear.output = FALSE, algorithm = "rprop+")

#   Merge the database
type.1.schools.compute <- rbind(type.1.schools[type.1.schools$MD_EARN_WNE_P10 == 0, ], type.1.schools[type.1.schools$GT_25K_P6 == 0, ])
UNITIDs <- subset(type.1.schools.compute, select = c(UNITID))
type.1.schools.compute <- subset(type.1.schools.compute, select = c(SATVR25,SATVR75,SATMT25,SATMT75,SATWR25,SATWR75,SATVRMID,SATMTMID,SATWRMID,ACTCM25,ACTCM75,ACTEN25,ACTEN75,ACTMT25,ACTMT75,ACTWR25,ACTWR75,ACTCMMID,ACTENMID,ACTMTMID,ACTWRMID,SAT_AVG,SAT_AVG_ALL))
type.1.schools.compute[is.na(type.1.schools.compute)] <- 0

type.1.schools.compute.res <- as.data.frame(compute(type.1.schools.net, type.1.schools.compute)$net.result)
names(type.1.schools.compute.res) <- c("MD_EARN_WNE_P10", "GT_25K_P6")

type.1.schools.compute[type.1.schools.compute == 0] <- NA

type.1.schools.compute.res <- cbind(UNITIDs, type.1.schools.compute, type.1.schools.compute.res)
type.1.schools.compute.res$MD_EARN_WNE_P10 <- type.1.schools.compute.res$MD_EARN_WNE_P10 * 1e5
type.1.schools.compute.res$MD_EARN_WNE_P10 <- round(type.1.schools.compute.res$MD_EARN_WNE_P10)

# Type 2 Schools
#   The same processing way as type 1
type.2.schools.train <- type.2.schools[!is.na(type.2.schools$MD_EARN_WNE_P10), ]
type.2.schools.train <- type.2.schools.train[!is.na(type.2.schools.train$GT_25K_P6), ]
type.2.schools.train <- subset(type.2.schools.train, select = c(SATVR25,SATVR75,SATMT25,SATMT75,SATWR25,SATWR75,SATVRMID,SATMTMID,SATWRMID,ACTCM25,ACTCM75,ACTEN25,ACTEN75,ACTMT25,ACTMT75,ACTWR25,ACTWR75,ACTCMMID,ACTENMID,ACTMTMID,ACTWRMID,SAT_AVG,SAT_AVG_ALL,MD_EARN_WNE_P10,GT_25K_P6))
type.2.schools.train$MD_EARN_WNE_P10 <- type.2.schools.train$MD_EARN_WNE_P10  / 1e5
attach(type.2.schools.train)
type.2.schools.train.matrix <- model.matrix(~MD_EARN_WNE_P10+GT_25K_P6+SATVR25,SATVR75,SATMT25,SATMT75,SATWR25,SATWR75,SATVRMID,SATMTMID,SATWRMID,ACTCM25,ACTCM75,ACTEN25,ACTEN75,ACTMT25,ACTMT75,ACTWR25,ACTWR75,ACTCMMID,ACTENMID,ACTMTMID,ACTWRMID,SAT_AVG,SAT_AVG_ALL, data = type.2.schools.train)
type.2.schools.net <- neuralnet(MD_EARN_WNE_P10+GT_25K_P6~SATVR25+SATVR75+SATMT25+SATMT75+SATWR25+SATWR75+SATVRMID+SATMTMID+SATWRMID+ACTCM25+ACTCM75+ACTEN25+ACTEN75+ACTMT25+ACTMT75+ACTWR25+ACTWR75+ACTCMMID+ACTENMID+ACTMTMID+ACTWRMID+SAT_AVG+SAT_AVG_ALL, type.2.schools.train.matrix,
                                hidden = 7, rep = 10, linear.output = FALSE, algorithm = "rprop+")

type.2.schools.compute <- rbind(type.2.schools[type.2.schools$MD_EARN_WNE_P10 == 0, ], type.2.schools[type.2.schools$GT_25K_P6 == 0, ])
UNITIDs <- subset(type.2.schools.compute, select = c(UNITID))
type.2.schools.compute <- subset(type.2.schools.compute, select = c(SATVR25,SATVR75,SATMT25,SATMT75,SATWR25,SATWR75,SATVRMID,SATMTMID,SATWRMID,ACTCM25,ACTCM75,ACTEN25,ACTEN75,ACTMT25,ACTMT75,ACTWR25,ACTWR75,ACTCMMID,ACTENMID,ACTMTMID,ACTWRMID,SAT_AVG,SAT_AVG_ALL))
type.2.schools.compute[is.na(type.2.schools.compute)] <- 0

type.2.schools.compute.res <- as.data.frame(compute(type.2.schools.net, type.2.schools.compute)$net.result)
names(type.2.schools.compute.res) <- c("MD_EARN_WNE_P10", "GT_25K_P6")

type.2.schools.compute[type.2.schools.compute == 0] <- NA

type.2.schools.compute.res <- cbind(UNITIDs, type.2.schools.compute, type.2.schools.compute.res)
type.2.schools.compute.res$MD_EARN_WNE_P10 <- type.2.schools.compute.res$MD_EARN_WNE_P10 * 1e5
type.2.schools.compute.res$MD_EARN_WNE_P10 <- round(type.2.schools.compute.res$MD_EARN_WNE_P10)

# Combining Type 1 and Type 2
for (i in 1:length(type.1.schools.compute.res$UNITID)) {
  id <- as.integer(type.1.schools.compute.res$UNITID[i])
  schools[schools$UNITID == id, ]$MD_EARN_WNE_P10 <- type.1.schools.compute.res[i, ]$MD_EARN_WNE_P10
  schools[schools$UNITID == id, ]$GT_25K_P6 <- type.1.schools.compute.res[i, ]$GT_25K_P6
}
for (i in 1:length(type.2.schools.compute.res$UNITID)) {
  id <- as.integer(type.2.schools.compute.res$UNITID[i])
  schools[schools$UNITID == id, ]$MD_EARN_WNE_P10 <- type.2.schools.compute.res[i, ]$MD_EARN_WNE_P10
  schools[schools$UNITID == id, ]$GT_25K_P6 <- type.2.schools.compute.res[i, ]$GT_25K_P6
}

# Preparing for weights calculation
expections.salaries <- schools$MD_EARN_WNE_P10
expections.salaries.counts <- as.data.frame(table(expections.salaries))
expections.salaries.counts <- expections.salaries.counts[order(-expections.salaries.counts$Freq), ]

total.count <- sum(expections.salaries.counts$Freq)
total.SAT_AVG_ALL <- sum(schools$SAT_AVG_ALL)
total.ACTCMMID <- sum(schools$ACTCMMID)
total.SATVRMID <- sum(schools$SATVRMID)
total.SATMTMID <- sum(schools$SATMTMID)
total.SATWRMID <- sum(schools$SATWRMID)
total.ACTENMID <- sum(schools$ACTENMID)
total.ACTMTMID <- sum(schools$ACTMTMID)
total.ACTWRMID <- sum(schools$ACTWRMID)
total.PCTPELL <- sum(schools$PCTPELL)
total.PCTFLOAN <- sum(schools$PCTFLOAN)

schools.fields <- names(schools)

weights.data.frame <- as.data.frame(t(apply(schools, 1, function(school.row) {
  # MD_EARN_WNE_P10: Median earnings of students working and not enrolled 10 years after entry
  MD_EARN_WNE_P10.weight <- expections.salaries.counts[expections.salaries.counts$expections.salaries == as.numeric(as.character(school.row[match("MD_EARN_WNE_P10", schools.fields)])), ]$Freq / total.count * 200
  
  # RPY_3YR_RT_SUPP: 3-year repayment rate
  RPY_3YR_RT_SUPP.weight <- as.numeric(as.character(school.row[match("RPY_3YR_RT_SUPP", schools.fields)])) * 200
  
  # SAT_AVG_ALL: Average SAT equivalent score of students admitted for all campuses rolled up to the 6-digit OPE ID
  SAT_AVG_ALL.weight <- log(as.numeric(as.character(school.row[match("SAT_AVG_ALL", schools.fields)])) / total.SAT_AVG_ALL * 1e4) / 1e2
  if (SAT_AVG_ALL.weight < 0) SAT_AVG_ALL.weight <- 0
  SAT_AVG_ALL.weight <- SAT_AVG_ALL.weight * 200
  
  # SATVRMID: Midpoint of SAT scores at the institution
  SATVRMID.weight <- log(as.numeric(as.character(school.row[match("SAT_AVG_ALL", schools.fields)])) / total.SATVRMID * 1e4) / 1e2
  if (SATVRMID.weight < 0) SATVRMID.weight <- 0
  SATVRMID.weight <- SATVRMID.weight * 200
  
  # ACTCMMID: Midpoint of the ACT cumulative score
  ACTCMMID.weight <- log(as.numeric(as.character(school.row[match("ACTCMMID", schools.fields)])) / total.ACTCMMID * 1e4) / 1e2
  if (ACTCMMID.weight < 0) ACTCMMID.weight <- 0
  ACTCMMID.weight <- ACTCMMID.weight * 200
  
  # SATMTMID: Midpoint of SAT scores at the institution (math)
  SATMTMID.weight <- log(as.numeric(as.character(school.row[match("SATMTMID", schools.fields)])) / total.SATMTMID * 1e4) / 1e2
  if (SATMTMID.weight < 0) SATMTMID.weight <- 0
  SATMTMID.weight <- SATMTMID.weight * 100
  
  # SATWRMID: Midpoint of SAT scores at the institution (writing)
  SATWRMID.weight <- log(as.numeric(as.character(school.row[match("SATWRMID", schools.fields)])) / total.SATWRMID * 1e4) / 1e2
  if (SATWRMID.weight < 0) SATWRMID.weight <- 0
  SATWRMID.weight <- SATWRMID.weight * 100
  
  # ACTENMID: Midpoint of the ACT English score
  ACTENMID.weight <- log(as.numeric(as.character(school.row[match("ACTENMID", schools.fields)])) / total.ACTENMID * 1e4) / 1e2
  if (ACTENMID.weight < 0) ACTENMID.weight <- 0
  ACTENMID.weight <- ACTENMID.weight * 100
  
  # ACTMTMID: Midpoint of the ACT math score
  ACTMTMID.weight <- log(as.numeric(as.character(school.row[match("ACTMTMID", schools.fields)])) / total.ACTMTMID * 1e4) / 1e2
  if (ACTMTMID.weight < 0) ACTMTMID.weight <- 0
  ACTMTMID.weight <- ACTMTMID.weight * 100
  
  # ACTWRMID: Midpoint of the ACT writing score
  ACTWRMID.weight <- log(as.numeric(as.character(school.row[match("ACTWRMID", schools.fields)])) / total.ACTWRMID * 1e4) / 1e2
  if (ACTWRMID.weight < 0) ACTWRMID.weight <- 0
  ACTWRMID.weight <- ACTWRMID.weight * 100
  
  # PCTPELL: Percentage of undergraduates who receive a Pell Grant
  PCTPELL.weight <- log(as.numeric(as.character(school.row[match("PCTPELL", schools.fields)])) / total.PCTPELL * 1e4) / 1e2
  if (PCTPELL.weight < 0) PCTPELL.weight <- 0
  PCTPELL.weight <- PCTPELL.weight * 100
  
  # PCTFLOAN: Percentage of undergraduates who receive a Pell Grant
  PCTFLOAN.weight <- log(as.numeric(as.character(school.row[match("PCTFLOAN", schools.fields)])) / total.PCTFLOAN * 1e4) / 1e2
  if (PCTFLOAN.weight < 0) PCTFLOAN.weight <- 0
  PCTFLOAN.weight <- PCTFLOAN.weight * 100
  
  # UG25abv: Percentage of undergraduates aged 25 and above - Negative Weight
  UG25abv.weight <- (1 - as.numeric(as.character(school.row[match("UG25abv", schools.fields)]))) / 10 * 100

  # GRAD_DEBT_MDN_SUPP: Median debt of completers
  GRAD_DEBT_MDN_SUPP.weight <- (10 - log(as.numeric(as.character(school.row[match("GRAD_DEBT_MDN_SUPP", schools.fields)])))) / 5 * 100
  if (is.infinite(GRAD_DEBT_MDN_SUPP.weight)) GRAD_DEBT_MDN_SUPP.weight <- 0
  
  # GRAD_DEBT_MDN10YR_SUPP: Median debt of completers expressed in 10-year monthly payments
  GRAD_DEBT_MDN10YR_SUPP.weight <- ((-log(as.numeric(as.character(school.row[match("GRAD_DEBT_MDN10YR_SUPP", schools.fields)])) + 1 / 1e6)) / 1e2) * 100
  
  weights <- c(MD_EARN_WNE_P10.weight, RPY_3YR_RT_SUPP.weight, SAT_AVG_ALL.weight, SATVRMID.weight, ACTCMMID.weight,
               SATMTMID.weight, SATWRMID.weight, ACTENMID.weight, ACTMTMID.weight, ACTWRMID.weight, PCTPELL.weight, PCTFLOAN.weight,
               UG25abv.weight, GRAD_DEBT_MDN_SUPP.weight, GRAD_DEBT_MDN10YR_SUPP.weight)
  
  return(c(as.numeric(as.character(school.row[match("UNITID", schools.fields)])), weights, sum(weights)))
})))

# Using K-Means to analyze the schools into 5 clusters
schools.cl <- kmeans(weights.data.frame[, 2:17], 5)
schools.clusters <- data.frame(id=1:5, schools.cl$center, size=schools.cl$size, rate = 1 / 5)
schools.clusters <- schools.clusters[order(-schools.clusters$V17), ]
schools.clusters <- cbind(schools.clusters, order=1:5)

# Calculating the rate offsets
k <- log((max(schools.clusters$V17) - min(schools.clusters$V17)) / pi^2) / 5
schools.clusters$rate <- schools.clusters$rate + (1 / 5) * sin((3 - schools.clusters$order)^2 / 0.5) * k * ((3 - schools.clusters$order) / 2)

weights.data.frame <- cbind(weights.data.frame, cluster = schools.cl$cluster)
potential.candidate.schools.weight <- weights.data.frame[weights.data.frame$V1 %in% potential.candidate.schools$UNITID, ]

school.ids <- apply(schools.clusters, 1, function(r) potential.candidate.schools.weight[potential.candidate.schools.weight$cluster == as.numeric(r[1]), ]$UNITID)

# Drawing plot
ggplot(schools.clusters, aes(x=rate, y=size, label=paste(round(rate*1000) / 10, "%/", round(V17), sep=""), fill=size)) +
  geom_bar(stat = "identity") +
  geom_text(vjust = -0.2, nudge_y = 0.5) +
  scale_y_continuous(limits=c(min(schools.clusters$size) - 20, max(schools.clusters$size) + 20),oob = rescale_none) +
  xlab("donation rate") +
  ylab("count - rate/score") +
  ggtitle("Donation rates and average scores of schools in 5 clusters") +
  theme_bw() +
  ggsave("problem_c_output.pdf", width = 10, height = 7)

# Output the result data
df <- as.data.frame(t(apply(schools.clusters, 1, function(cluster) {
  return(c(cluster[1], cluster[19], length(potential.candidate.schools.weight[potential.candidate.schools.weight$cluster == as.numeric(cluster[1]),]$V1), paste(potential.candidate.schools.weight[potential.candidate.schools.weight$cluster == as.numeric(cluster[1]), ]$V1, collapse="|")))
})))
names(df) <- c("id", "rate", "size", "UNITIDs")
write.csv(df, "problem_c_output.csv", row.names = FALSE, quote = FALSE)