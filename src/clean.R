# clean.R

# Data cleaning focusing on correcting observed problems with
# the initial attempts at data entry.
## We are going to move the data from 'raw' to the appropriate
## columns in 'Data', whilst also making type conversions and
## fixing the earlier observed wrong entries.

packrat::off()
library(tidyverse)

raw <- read.csv("data/database.csv", stringsAsFactors = FALSE) %>% 
  as_tibble()
tbl_df(raw)

load("data/sensitisation-dataentry.dte")
if (!exists('Data') || !is.data.frame(Data))
  stop('\'Data\' could not be loaded')
Data <- as_tibble(Data)
glimpse(Data)

## Function: Takes a character variable of a given column 
## and if necessary, carries out conversion to a factor.
shift_df_col <- function(original, new, factor = TRUE) {
  if (!is.atomic(original) & !is.atomic(new))
    stop("Expected 'original' and 'new' to be atomic")
  if (factor)
    factor(original, levels = levels(new))
  else
    as.character(original)
}

## Extend 'Data' to existing row numbers
for (i in 1:nrow(raw))
  Data[nrow(Data) + 1, ] <-  NA

## Enable inspection of possible factor levels
if (interactive()) {
  tmp <- tempfile(fileext = '.txt')
  capture.output(lapply(raw, unique), file = tmp)
  file.show(tmp)
}

## Cleaning
Data$who.cleans <- raw$Who.does.the.cleaning. %>% 
  str_replace(regex("^.*owners", ignore_case = TRUE), "Shop owners") %>% 
  str_replace("marketers", "Others") %>% 
  shift_df_col(Data$who.cleans)

Data$when.cleans <- raw$At.what.time.is.cleaning.done %>% 
  str_replace("Morning", "Morning Only") %>% 
  shift_df_col(Data$when.cleans)

Data$how.dispose <- raw$How.do.you.dispose.of.your.waste.

Data$who.evacuates <- raw$Who.evacuates.the.waste. %>% 
  str_replace("AEPB", "Government") %>% 
  str_replace(".+collectors$", "Private collectors") %>% 
  str_replace("scavengers", "Others") %>% 
  shift_df_col(Data$who.evacuates)

Data$freq.evacuates <- raw$How.often.is.waste.evacuated. %>% 
  str_replace("Twice a week", "At least twice a week") %>% 
  str_replace("Twice a day", "Daily") %>% 
  str_replace("^not", "Not") %>% 
  shift_df_col(Data$freq.evacuates)

Data$know.effects.dirt <-
  raw$Do.you.know.the.effects.of.dirty.environment.on.your.health. %>% 
  str_replace("^y", "Y") %>% 
  str_replace("^n", "N") %>% 
  shift_df_col(Data$know.effects.dirt)

Data$what.effect.dirt <- raw$If.yes..what.is.the.impact.

Data$have.toilet <- raw$Do.you.have.a.toilet. %>% 
  str_replace("^y", "Y") %>% 
  str_replace("^n", "N") %>% 
  shift_df_col(Data$have.toilet)

Data$use.toilet <- raw$Do.you.use.the.toilet. %>% 
  str_replace("no", "No") %>% 
  replace(grep('nil', .), NA_character_) %>% 
  shift_df_col(Data$use.toilet)

Data$why.no.use.toilet <- raw$If.no..why.

## Please check this variable. Something's not right.
Data$toilet.owner <- raw$Is.it.a.private.or.public.toilet. %>% 
  str_replace("^p", "P") %>% 
  replace(grep('no toilet', .), NA_character_) %>%
  replace(which(. == ''), NA_character_) %>% 
  replace(grep('nil', .), NA_character_) %>% 
  shift_df_col(Data$toilet.owner)

Data$public.toilet.manager <- raw$If.public..who.manages.it. %>% 
  str_replace("govt", "Government") %>% 
  str_replace('other', 'Others') %>% 
  str_replace('management', 'Facility managers') %>% 
  replace(grep('no toilet|nil', .), NA_character_) %>%  
  replace(which(. == ''), NA_character_) %>% 
  shift_df_col(Data$public.toilet.manager)

Data$toilet.payment <- raw$Is.it.free.or.paid. %>% 
  str_replace('^Free.*$', 'No') %>% 
  str_replace('Paid', 'Yes') %>% 
  replace(grep('no toilet|nil', .), NA_character_) %>%
  replace(which(. == ''), NA_character_) %>% 
  shift_df_col(Data$toilet.payment)

## TODO: Reconsider whether to use list instead of factor
## or add a level for combinations
Data$where.eat <- raw$Where.do.you.eat. %>% 
  str_replace('^.+bring.+$', 'Bring own food') %>% 
  str_replace('^Restaurant.+$', 'Others') %>% 
  str_replace('^mobile.+$', 'Mobile vendor') %>% 
  str_replace('^.*at home$', 'Others') %>% 
  shift_df_col(Data$where.eat)

Data$know.effects.work <-
  raw$Do.you.know.that.the.nature.of.your.work.can.affect.your.health.environment. %>% 
  str_replace('^n', 'N') %>% 
  shift_df_col(Data$know.effects.work)

Data$what.effect.work <- raw$If.yes..what.is.the.impact.2

Data$previous.visit <- 
  raw$Is.this.your.first.time.receiving.such.visit. %>% 
  str_replace('^NO', 'No') %>% 
  replace(which(. == ''), NA_character_) %>%
  shift_df_col(Data$previous.visit)

Data$changes.postvisit <- 
  raw$If.no..what.changeshave.you.made.following.the.last.visit.

Data$visit.useful <- raw$do.you.find.this.exercise.useful. %>% 
  replace(which(. == ''), NA_character_) %>%
  shift_df_col(Data$visit.useful)

Data$visit.comments <- 
  raw$Give.your.comments.about.the.exercise

Data$visible.waste.receptacle <- 
  raw$Do.they.have.waste.receptacle. %>% 
  str_replace('^n', 'N') %>% 
  replace(which(. == ''), NA_character_) %>%
  shift_df_col(Data$visible.waste.receptacle)

Data$receptacle.provider <- raw$Who.provides.it. %>% 
  str_replace('^Personal.*$', 'Others') %>% 
  str_replace('^.*waste.*$', 'Waste vendor') %>% 
  str_replace('^market', "Market") %>% 
  replace(grep('nil', .), NA_character_) %>% 
  replace(which(. == ''), NA_character_) %>%
  shift_df_col(Data$receptacle.provider)

Data$waste.bin <- raw$Do.they.have.waste.bin. %>% 
  replace(which(. == ''), NA_character_) %>% 
  replace(grep('nil', .), NA_character_) %>% 
  shift_df_col(Data$waste.bin)

Data$officer.comments <- raw$Officer.s.comments %>% 
  shift_df_col(Data$officer.comments, factor = FALSE)

## This looks like a repetition of Data$wastebin. Check!!!
Data$has.wastebin <- raw$Do.they.have.waste.bin. %>% 
  replace(which(. == ''), NA_character_) %>% 
  replace(grep('nil', .), NA_character_) %>% 
  shift_df_col(Data$has.wastebin)

glimpse(Data)
## Data storage
# dbcon <- dbConnect(SQLite(), "data/awareness.db")
# try({
#   if (dbWriteTable(dbcon, 'ques_input', Data, overwrite = TRUE))
#     cat("Data were successfully written to the database")
#   else
#     stop("Data could not be written to the database")
# })
# 
# dbDisconnect(dbcon)

saveRDS(Data, 'data/cleaned-data.rds')

## Clear workspace
rm(list = ls())
