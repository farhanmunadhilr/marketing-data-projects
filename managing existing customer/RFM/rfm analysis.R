###################
# RFM Analysis 	#
###################

## Install Packages (if needed)
install.packages("dplyr")

## Load Packages and Set Seed
library(dplyr)
set.seed(1)

## Read in RFM data
rfm <- read.csv(file.choose()) ## Choose retail_rfm.csv file

## How many levels for each
groups <- 5 ## This will use quintiles to sort and give 125 total groups

## Run RFM Analysis with Independent Sort
rfm$recency_score_indep <- ntile(rfm$recency_days*-1, groups)
rfm$frequency_score_indep <- ntile(rfm$number_of_orders, groups)
rfm$monetary_score_indep <- ntile(rfm$revenue, groups)
rfm$rfm_score_indep <- paste(rfm$recency_score_indep*100 + rfm$frequency_score_indep * 10 + rfm$monetary_score_indep)

## Run RFM Analysis with Sequential Sort
rfm$recency_score_seq <- ntile(rfm$recency_days*-1, groups)
r_groups <- NULL; rf_groups <- NULL; temp <- NULL ## Initialize empty matrices
for (r in 1:groups) {
	r_groups[[r]] <- filter(rfm, rfm$recency_score_seq == r)
	r_groups[[r]]$frequency_score_seq <- ntile(r_groups[[r]]$number_of_orders, groups)
	for (m in 1:groups) {
		rf_groups[[m]] <- filter(r_groups[[r]], r_groups[[r]]$frequency_score_seq == m)
		rf_groups[[m]]$monetary_score_seq <- ntile(rf_groups[[m]]$revenue, groups)
		temp <- bind_rows(temp, rf_groups[[m]])
	}	
}
rfm_result <- temp[order(temp$customer_id),]
rfm_result$rfm_score_seq <- paste(rfm_result$recency_score_seq*100 + rfm_result$frequency_score_seq * 10 + rfm_result$monetary_score_seq)

## Export RFM Results with Independent and Sequential Sort
write.csv(rfm_result, file = file.choose(new=TRUE), row.names = FALSE) ## Name file rfm_result.csv

