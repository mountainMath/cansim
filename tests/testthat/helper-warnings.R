# Warnings built from several sentences start each sentence on its own line, so a phrase that spans
# two sentences is split across two lines. Collapsing the whitespace lets a test match on the wording
# rather than on the layout.
warning_text <- function(expr) {
  messages <- character(0)
  withCallingHandlers(expr,
                      warning=function(w) {
                        messages <<- c(messages,conditionMessage(w))
                        invokeRestart("muffleWarning")
                      })
  gsub("\\s+"," ",paste(messages,collapse=" "))
}
