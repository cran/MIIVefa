## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(MIIVefa)

## ----demo, eval=FALSE---------------------------------------------------------
#  miivefa(
#    data = yourdata,
#    sigLevel = 0.05,
#    scalingCrit = 'sargan+factorloading_R2',
#    correlatedErrors = NULL
#  )

