# helpers.R

## Make sure that factors are set where appropriate
## @param df A data frame
## @attributeList A list of variable attributes encapsulated in the VarAttr
## object created with the 'DataEntry' package
enforceFactors <- function(df, attributeList = '')
{
  if (!inherits(df, 'data.frame'))
    stop(sQuote(df), 'is not a data frame')
  stopifnot(is.character(attributeList))
  if (file.exists(attributeList) & endsWith(attributeList, '.dte')) {
    if (!exists("VarAttr"))
      load(attributeList)
  }
  else
    stop('The file', sQuote(basename(attributeList)), 'was not found')
  
  if (!identical(colnames(df), names(VarAttr)))
    stop('There is a mismatch between the 2 data frames')
  
  for (i in seq_along(VarAttr)) {
    if (identical(VarAttr[[i]]$class, 'factor'))
      df[[i]] <- as.factor(df[[i]])
  }
  invisible(df)
}