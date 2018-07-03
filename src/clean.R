# clean.R

# Data cleaning focusing on correcting observed problems with
# the initial trials at data entry

packrat::off()
library(tidyverse)
library(RSQLite)

## Import data from the original CSV file as well as the data
## entry framework that was developed and stored as a .dte file
raw <- read.csv("data/database.csv", stringsAsFactors = FALSE)
load("data/sensitisation-dataentry.dte")

Data <- as_tibble(Data); raw <- as_tibble(raw)
glimpse(raw)
glimpse(Data)

## We are going to move the data from 'raw' to the appropriate
## columns in 'Data', whilst also making type conversions and
## fixing the earlier observed wrong entries

## First we will create a text file that has all the unique
## values for each column. That way, we can easily spot the
## values that need to be altered.
uni <- "data/csv-unique-values.txt"
sink(file = uni)
lapply(raw, unique)
sink()
file.show(uni)

## Write a function that will take a character variable of
## a given column and a column in the new data frame. If
## necessary, carry out conversion to a factor.
shift_df_col <- function(original, new, factor) {
  if (!is.atomic(original) & !is.atomic(new))
    stop("Expected 'original' and 'new' to be atomic")
  if (missing(factor) || !is.logical(factor))
    stop("Please supply logical value to 'factor'")
  if (factor)
    new <- factor(original, levels = levels(new))
  else
    new <- as.character(original)
}

## Create an empty data frame with dimensions similar
## to our original dataset
for (i in 1:nrow(raw))
  Data[nrow(Data) + 1, ] <-  NA


Data$who.cleans <- raw$Who.does.the.cleaning. %>% 
  str_replace(regex(".*owners", ignore_case = TRUE), "Shop owners") %>% 
  str_replace("marketers", "Others") %>% 
  shift_df_col(Data$who.cleans, factor = TRUE)

Data$when.cleans <- raw$At.what.time.is.cleaning.done %>% 
  str_replace("Morning", "Morning Only") %>% 
  shift_df_col(Data$when.cleans, factor = TRUE)

## Data$how.dispose   TODO: Review

Data$who.evacuates <- raw$Who.evacuates.the.waste. %>% 
  str_replace("AEPB", "Government") %>% 
  str_replace(".+collectors$", "Private collectors") %>% 
  str_replace("scavengers", "Others") %>% 
  shift_df_col(Data$who.evacuates, factor = TRUE)

Data$freq.evacuates <- raw$How.often.is.waste.evacuated. %>% 
  str_replace("Twice a week", "At least twice a week") %>% 
  str_replace("Twice a day", "Daily") %>% 
  str_replace("^not", "Not") %>% 
  shift_df_col(Data$freq.evacuates, factor = TRUE)

Data$know.effects.dirt <-
  raw$Do.you.know.the.effects.of.dirty.environment.on.your.health. %>% 
  str_replace("^y", "^Y") %>% 
  str_replace("^n", "^N") %>% 
  shift_df_col(Data$know.effects.dirt, factor = TRUE)

# Data$what.effect.dirt <- raw$If.yes..what.is.the.impact.

Data$have.toilet <- raw$Do.you.have.a.toilet. %>% 
  str_replace("^y", "^Y") %>% 
  shift_df_col(Data$have.toilet, factor = TRUE)

Data$use.toilet <- raw$Do.you.use.the.toilet. %>% 
  str_replace("^n", "^N") %>% 
  str_replace('nil', NA_character_) %>% 
  shift_df_col(Data$use.toilet, factor = TRUE)

# Data$why.no.use.toilet <- raw$If.no..why. %>% 

## Please check this variable. Something's not right.
Data$toilet.owner <- raw$Is.it.a.private.or.public.toilet. %>% 
  str_replace("^p", "^P") %>% 
  str_replace('^no', '^No') %>% 
  str_replace('', NA_character_) %>% 
  str_replace('nil', NA_character_) %>% 
  shift_df_col(Data$toilet.owner, factor = T)

Data$public.toilet.manager <- raw$If.public..who.manages.it. %>% 
  str_replace("govt", "Government") %>% 
  str_replace('other', 'Others') %>% 
  str_replace('management', 'Facility managers') %>% 
  str_replace('no toilet|nil', NA_character_) %>% 
  str_replace('', NA_character_) %>% 
  shift_df_col(Data$public.toilet.manager, factor = TRUE)

Data$toilet.payment <- raw$Is.it.free.or.paid. %>% 
  str_replace('^Free.*', 'No') %>% 
  str_replace('Paid', 'Yes') %>% 
  str_replace('no toilet|nil', NA_character_) %>% 
  str_replace('', NA_character_) %>% 
  shift_df_col(Data$toilet.payment, factor = TRUE)

## TODO: Reconsider whether to use list instead of factor
## or add a level for combinations
Data$where.eat <- raw$Where.do.you.eat. %>% 
  str_replace('.+bring.+', 'Bring own food') %>% 
  str_replace('^Restaurant.+', 'Others') %>% 
  str_replace('^mobile', '^Mobile') %>% 
  str_replace('at home$', 'Others') %>% 
  shift_df_col(Data$where.eat, factor = T)

Data$know.effects.work <-
  raw$Do.you.know.that.the.nature.of.your.work.can.affect.your.health.environment. %>% 
  str_replace('^n', '^N') %>% 
  shift_df_col(Data$know.effects.work, factor = TRUE)

# Data$what.effect.work <- raw$If.yes..what.is.the.impact.2

Data$previous.visit <- 
  raw$Is.this.your.first.time.receiving.such.visit. %>% 
  str_replace('^NO', '^No') %>% 
  str_replace('', NA_character_) %>% 
  shift_df_col(Data$previous.visit, factor = TRUE)

# Data$changes.postvisit

Data$visit.useful <- raw$do.you.find.this.exercise.useful. %>% 
  str_replace('', NA_character_) %>% 
  shift_df_col(Data$visit.useful, factor = TRUE)

# Data$visit.comments

Data$visible.waste.receptacle <- 
  raw$Do.they.have.waste.receptacle. %>% 
  str_replace('^n', '^N') %>% 
  strrep('', NA_character_) %>% 
  shift_df_col(Data$visible.waste.receptacle, factor = TRUE)

Data$receptacle.provider <- raw$Who.provides.it. %>% 
  str_replace('^Personal.*', 'Others') %>% 
  str_replace('^.*waster.*', 'Waste vender') %>% 
  str_replace('^market', "^Market") %>% 
  str_replace('nil', NA_character_) %>% 
  str_replace('', NA_character_) %>% 
  shift_df_col(Data$receptacle.provider, factor = TRUE)

Data$waste.bin <- raw$Do.they.have.waste.bin. %>% 
  str_replace('', NA_character_) %>% 
  str_replace('nil', NA_character_) %>% 
  shift_df_col(Data$waste.bin, factor = TRUE)

Data$officer.comments <- raw$Officer.s.comments %>% 
  shift_df_col(Data$officer.comments, factor = FALSE)

## This looks like a repetition of Data$wastebin. Check!!!
Data$has.wastebin <- raw$Do.they.have.waste.bin. %>% 
  str_replace('', NA_character_) %>% 
  str_replace('nil', NA_character_) %>% 
  shift_df_col(Data$has.wastebin, factor = TRUE)


## Data storage
dbcon <- dbConnect(SQLite(), "data/awareness.db")
dbWriteTable(dbcon, 'ques_input', Data)
dbDisconnect(dbcon)

## Clear workspace
rm(list = ls())
