# Repair warnings are wrapped to the console width, so a phrase that is checked for can be split
# across two lines. Collapsing the whitespace lets a test match on the wording rather than on where
# the console happened to break it.
warning_text <- function(expr) {
  messages <- character(0)
  withCallingHandlers(expr,
                      warning=function(w) {
                        messages <<- c(messages,conditionMessage(w))
                        invokeRestart("muffleWarning")
                      })
  gsub("\\s+"," ",paste(messages,collapse=" "))
}
