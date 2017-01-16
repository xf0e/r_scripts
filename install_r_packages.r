# Install function for packages    
packages<-function(x) {    
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)) {
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
# and call it like this
packages(ggplot2)
packages(reshape2)
packages(sqldf)
packages(readr)
packages(caret)
packages(ggbiplot)
packages(ggplot2)
packages(dplyr)
packages(rgl)
