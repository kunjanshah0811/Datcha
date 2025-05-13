library(diffobj)
original <- c("this is an awesome post", "isn't it") 
changed <- c("this is an awful post", "isn't it") 
tmp <- tempfile(fileext='.html')
writeLines(
  as.character(
    diffChr(original, changed, format="html", style=list(html.output="diff.w.style"))
  ), 
  tmp
)
#browseURL(tmp)


