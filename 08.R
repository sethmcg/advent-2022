#infile <- "test.08.txt"
infile <- "input.08.txt"

trees <- readLines(infile) |>
    strsplit('') |>
    lapply(as.numeric) |>
    do.call(what=rbind)

nx <- ncol(trees)
ny <- nrow(trees)

visible <- function(x){
    mx <- -Inf
    y <- c()
    for(i in 1:length(x)){
        y[i] <- x[i] > mx
        mx <- max(mx, x[i])
    }
    return(y)
}

W <- apply(trees, 1, visible) |> t()
N <- apply(trees, 2, visible)
E <- (apply(trees[,nx:1], 1, visible) |> t())[,nx:1]
S <- (apply(trees[ny:1,], 2, visible))[ny:1,]

vis <- W | N | E | S

print(sum(vis))

## Part 2

#view <- function(x,h){ min(max(which(x >= h), 1)) }
view <- function(x,h){
    if(all(x < h)){return(length(x)-1)}
    min(which(x >= h))
} 

v <- trees+NA

for(i in 1:ny){
    for(j in 1:nx){
        h = trees[i,j]
        row <- c(-Inf,trees[i,],-Inf)
        col <- c(-Inf,trees[,j],-Inf)
        L <- row[0:j] |> rev()
        R <- row[j:nx+2]
        U <- col[0:i] |> rev()
        D <- col[i:ny+2]
        
        v[i,j] <- sapply(list(L,R,U,D), view, h=h) |> prod()
    }
}

print(max(v))
