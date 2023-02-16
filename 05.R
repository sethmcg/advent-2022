## Part 1: what crates end up on top?

#infile <- "test.05.txt"
infile <- "input.05.txt"

lines <- readLines(infile)
input <- split(lines, cumsum(lines==""))

nc <-  tail(input[[1]], 1) |> strsplit('') |> unlist() |>
    as.numeric() |> max(na.rm=TRUE)

nr <- length(input[[1]]) - 1

index <- function(x,i,j){ x[i,j] }


## convert drawing to list of stacks

crates <- strsplit(input[[1]], '') |>
    head(-1) |>
    do.call(what=rbind) |>
    index(nr:1, (1:nc*4-2)) |>
    t() |>
    split(1:nc) |>
    lapply(\(x){x[x!=" "]})


parse <- function(x){
    y <- unlist(strsplit(x, " "))
    z <- as.numeric(y[(1:3)*2])
    names(z) <- y[(1:3)*2-1]
    return(z)
}

moves <- lapply(input[[2]], parse) |>
    tail(-1) |>
    do.call(what=rbind) |>
    data.frame()


domove <- function(m){
    ## Part 1
#    crates[[m$to]] <<- c(crates[[m$to]], rev(tail(crates[[m$from]], m$move)))
    ## Part 2
    crates[[m$to]] <<- c(crates[[m$to]], tail(crates[[m$from]], m$move))
    ## both
    crates[[m$from]] <<- head(crates[[m$from]], -m$move)
    }

for(i in 1:nrow(moves)){ domove(moves[i,]) }

print(paste(sapply(crates, tail, 1), collapse=''))

## Part 2: move crates all at once not one at a time

## just drop the rev() from domove()
