#infile <- "test.06.txt"
infile <- "input.06.txt"

input <- readLines(infile)
for(i in 1:length(input)){

    x <- unlist(strsplit(input, ''))
    n <- length(x)
#    m <- 4  # marker length - part 1
    m <- 14 # marker length - part 2
    
    ## build shifted array
    y <- c()
    for(i in 1:m){
        y <- cbind(y, x[i:(n-m+i)])
    }

    # answer = *end* of first marker
    print(min(which(apply(y, 1, \(x){length(unique(x))}) == m)) + m - 1)
}
