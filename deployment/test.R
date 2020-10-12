#flowchart figure for manuscript. 

install.packages("DiagrammeRsvg")
install.packages("rsvg")

library(DiagrammeRsvg)
library(rsvg)


pdf(file='prisma.pdf', width = 18, height = 39) 


prsm <- prisma(31894, 90, 17955, 17955, 17588, 367, 304, 61, 54, width=800, height=800)
tmp_pdf <- tempfile()
PRISMAstatement:::prisma_pdf(prsm, tmp_pdf)
knitr::include_graphics(path = tmp_pdf)
unlink(tmp_pdf)



