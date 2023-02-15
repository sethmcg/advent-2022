
## Part 1: split each string in half & find common char

strhalves <- function(x){
    N <- nchar(x)/2
    ch <- unlist(strsplit(x, ''))
    list(head(ch, N), tail(ch, N))
}

#infile <- "test.03.txt"
infile <- "input.03.txt"

input <- readLines(infile)
rucks <- lapply(input, strhalves)

common <- function(x,y){ unique(x[x %in% y]) }
wrong <- sapply(rucks, \(x){common(x[[1]],x[[2]])})

priority <- function(x){match(x, c(letters, LETTERS))}

print(sum(priority(wrong)))


## Part 2: find common char in each triplet of (unsplit) strings

groups <- split(input, rep(each=3, seq(length(input)/3)))

items <- lapply(groups, strsplit, '')

badges <- sapply(items, \(x){common(common(x[[1]],x[[2]]),x[[3]])})

print(sum(priority(badges)))
