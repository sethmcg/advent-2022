## Part 1: number of pairs with one fully contained in the other

#infile <- "test.04.txt"
infile <- "input.04.txt"

secs <- readLines(infile)  |>
    strsplit("[,-]") |>
    do.call(what=rbind) |>
    as.data.frame() |> 
    apply(2, as.numeric)

colnames(secs) <- c("min1","max1","min2","max2")

within <- function(x){
    return(
    (x["min1"] >= x["min2"] && x["max1"] <= x["max2"] ) ||
    (x["min2"] >= x["min1"] && x["max2"] <= x["max1"] )
    )
}

print(sum(apply(secs, 1, within)))

## Part 2:

overlap <- function(x){
    return(
       !( (x["max1"] < x["min2"]) || (x["max2"] < x["min1"]) )
    )
}

print(sum(apply(secs, 1, overlap)))
