library(data.tree)

#infile <- "test.07.txt"
infile <- "input.07.txt"

input <- readLines(infile) |> strsplit(' ')

fs <- Node$new("/", size=0)
cwd <- fs

for(cmd in input[-1]){
    if(cmd[1] == '$') {
        if(cmd[2] == 'cd'){
            cwd <- Navigate(cwd, cmd[3])
        } else {
            # noop
        }
    } else {
        if(cmd[1] == "dir"){
            cwd$AddChild(cmd[2], type='dir', size=NULL)
            ## don't set size to 0 or Aggregate doesn't recurse
        } else {
            cwd$AddChild(cmd[2], type='file', size=as.numeric(cmd[1]))
        }
    }
}

total <- function(n){n$total <- Aggregate(n, attribute="size", aggFun=sum)}

fs$Do(total)
print(fs, "total")

## it would be better to use type=dir instead of isNotLeaf, but docco
## is not obvious on how to define filter functions
dirsize <- Get(Traverse(fs, filterFun=isNotLeaf), "total")
print(sum(dirsize[dirsize <= 100000]))


## Part 2: total space is 70M; update needs 30M.  Find smallest
## directory to delete that will bring grand total down below 40M.

target <- dirsize["/"] - 4e7

print(min(dirsize[dirsize >= target]))
