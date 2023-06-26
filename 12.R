
#orog <- -1 + readLines("test.12.txt") |>
orog <- -1 + readLines("input.12.txt") |>
    strsplit('') |>
    lapply(match, c("S", letters, "E")) |>
    do.call(what=rbind)

# image(orog, zlim=c(0,4))
nx <- nrow(orog)
ny <- ncol(orog)

start  <- which(orog==0, arr.ind=TRUE) |> paste(collapse=',')
finish <- which(orog==27, arr.ind=TRUE) |> paste(collapse=',')

library(fields)
library(pals)
image.plot(orog, x=1:nx, y=1:ny, zlim=c(1,26), col=alphabet(26))

inds <- outer(1:nx, 1:ny, paste, sep=',')

RL <- orog[-1, ] - orog[-nx,] < 2
LR <- orog[-nx,] - orog[-1, ] < 2
UD <- orog[,-1 ] - orog[,-ny] < 2
DU <- orog[,-ny] - orog[,-1 ] < 2

edgelist <- rbind(cbind(inds[-1, ][RL], inds[-nx,][RL]),
                  cbind(inds[-nx,][LR], inds[-1, ][LR]),
                  cbind(inds[,-1 ][UD], inds[,-ny][UD]),
                  cbind(inds[,-ny][DU], inds[,-1 ][DU]))

## these connectivities are from higher to lower, so you want to trace
## the path backwards from finish (height=27) to start (height=0)

library(igraph)

adj <- graph_from_edgelist(edgelist)

print(shortest_paths(adj, finish, start)$vpath[[1]] |> length() - 1)

shortest_paths(adj, finish, start)$vpath[[1]] |> names() |> strsplit(',') |> lapply(as.numeric) |> do.call(what=rbind) |> lines()

## part 2

## shortest path to any square with height=1

## By visual inspection of the plot, it's 2 shorter: 454

