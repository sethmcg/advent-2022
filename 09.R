source("names.R")

move <- list(U=c(0,1), D=c(0,-1), L=c(-1,0), R=c(1,0))

#instrux <- read.table("test.09.txt", col.names=c("x","times"))
instrux <- read.table("input.09.txt", col.names=c("x","times"))

instr <- apply(instrux, 1, \(x){do.call(rep, as.list(x))}) |> unlist()

hpos <- do.call(rbind, move[instr]) |>
    setname(NULL, "row") |>
    setname(c('x','y'), "col") |>
    apply(2,cumsum)


update <- function(txy, hxy){
    delta <- hxy - txy
    if(max(abs(delta)) > 1){        
        delta[delta >  1] <-  1
        delta[delta < -1] <- -1
        txy <- txy + delta
    }
    return(txy)
}

walk <- t(hpos) |> as.data.frame() |> as.list()

tpos <- Reduce(update, walk, c(0,0), accumulate=TRUE)

print(length(unique(tpos)))


## Part 2

pos <- walk
for(i in 1:9){
    pos <- Reduce(update, pos, c(0,0), accumulate=TRUE)
}

print(length(unique(pos)))
