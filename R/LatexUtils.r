## Utility functions for R to LaTeX formating
##
## source: http://cameron.bracken.bz/sweave-xtable-booktabs

#' Table pretty printer

#' @title Table pretty printer
#' @param xtab a table
#' @export

print.xb <- function(xtab, ...){

    print(xtab,
        floating=F,
        hline.after=NULL, 
        add.to.row=list(pos=list(-1,0, nrow(xtab)), 
        command=c(
             '\\toprule \n',
            '\\midrule  \n', 
             '\\bottomrule \n')), ...)
}



