replaceForward <- function(myData, nameColNa = names(dfIn)[1])
{
  setnames(myData, nameColNa, "colNa")
  myData[, segment := cumsum(!is.na(colNa))]
  myData[, colNa := colNa[1], by = "segment"]
  myData[, segment := NULL]
  setnames(myData, "colNa", nameColNa)
  return(myData)
}