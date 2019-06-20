## from https://github.com/MarkEdmondson1234/googleAnalyticsR_public
library(googleAnalyticsR);library(data.table);library(dplyr);library(tidyr);library(clickstream);library(igraph);library(markovchain)

ga_auth()

gaId <- 317179 # GA ViewId

## dimension3 contains userId in format:
## u={cid}&t={timestamp}
raw <- google_analytics(gaId,
                        date_range = c("2018-08-18","2018-09-01"),
                        metrics = c("pageviews"),
                        dimensions = c("ga:dimension3","ga:dimension4","ga:pagePath"))

##Transforming the data into a form suitable for the the model

processed<- setnames (raw, c("cid","timestamp","pagePath", "pageviews"))

processed<-as.data.table(processed)
processed$pagePath[processed$pagePath == "/"] <- "/home/"

processed$pagePath[grepl("msclkid", processed[["pagePath"]])]<- 0
processed<-processed[pagePath!=0,]

## javascript to R timestamp
processed$timestamp <- as.POSIXct(as.numeric(processed$timestamp) / 1000,
                                  origin = "1970-01-01")

## find users with session length > 1
nonbounce <- processed %>% group_by(cid) %>%
  summarise(session_length = n()) %>% filter(session_length > 1) %>% ungroup()

processed <- nonbounce %>% left_join(processed)

processed <- processed %>% arrange(cid, timestamp)

write.csv(processed,file="Output/processed.csv")

processed

## for each cid, make a string of pagePath in timestamp order
sequence <- processed %>% group_by(cid) %>% summarise(sequence = paste(pagePath, collapse = ","))

sequence <- paste(sequence$cid, sequence$sequence, sep=",")

sequence

##Creating the model

# fitting a simple Markov chain and predicting the next click
clickstreams <- sequence
csf <- tempfile()
writeLines(clickstreams, csf)
cls <- readClickstreams(csf, header = TRUE)

## Make the model:

## 1612 users - 285 seconds
model <- fitMarkovChain(cls, verbose=TRUE)

### Using the model:

## get the likely pages a user starts from
likely_start <- as.data.frame(model@start)
likely_start <- likely_start[order(likely_start$Freq, decreasing = TRUE),]

## List of pages in the model
states(model)

## Prediction:
startPattern <- new("Pattern", sequence = "home")
predict(model, startPattern)

plot(model)

## pages that absorb (e.g. are last in session)
last_pages <- absorbingStates(model)

## model is saved so it can be uploaded to the R package for the predictions:
save(model, file="model.RData")


plot(model)


