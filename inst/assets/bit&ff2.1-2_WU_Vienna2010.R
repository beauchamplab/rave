library(ff)

# Basic example: creating atomic vectors
ri <- integer(10)

fi <- ff(vmode="integer", length=10)
fb <- ff(vmode="byte", length=10)

rb <- byte(10) # in R this is integer
fb <- ff(rb)

vmode(ri)
vmode(fi)
vmode(rb)
vmode(fb)

cbind(.rambytes, .ffbytes)[c("integer","byte"),]

# Advanced example: creating atomic vectors

rf <- factor(levels= c("A","T","G","C"))
length(rf) <- 10
rf

frf <- ff(rf)
length(frf) <- 1e8
frf
frf[11:1e8] <- NA
ff(vmode="quad", length=1e8, levels=c("A","T","G","C"))
ff(vmode="quad", length=10, levels=c("A","B","C","D"), ordered=TRUE)
ff(Sys.Date()+0:9, length=10)
ff(Sys.time()+0:9, length=10)
ff(0:9, ramclass="Date")
ff(0:9, ramclass=c("POSIXt", "POSIXct"))
str(ff(as.POSIXct(as.POSIXlt(Sys.time(), "GMT")), length=12))


# Basic example: working with atomic vectors

rd <- double(100)
rd[] <- runif(100)  # write
rd[] # this is the proper non-lazy way to read

fd <- ff(vmode="double", length=1e8)

system.time(
for (i in chunk(fd)) fd[i] <- runif(sum(i))
)
system.time(
s <- lapply( chunk(fd)
, function(i)quantile(fd[i], c(0.05, 0.95)) )
)

crbind(s)


# Limiting R's RAM consumption through chunked processing

str(chunk(fd))
args(chunk.default)
args(chunk.ff_vector)
getOption("ffbatchbytes") / 1024^2 / memory.limit()


# Advanced example: working parallel with atomic vectors

library(snowfall)
finalizer(fd)
# let slaves not delete fd on shutdown
finalizer(fd) <- "close"

sfInit(parallel=TRUE, cpus=2, type="SOCK")
sfLibrary(ff)
sfExport("fd")  # do not export the same ff multiple times
sfClusterEval(open(fd)) # explicitely opening avoids a gc problem
system.time(
sfLapply( chunk(fd), function(i){
fd[i] <- runif(sum(i))
invisible()
})
)
system.time(
s <- sfLapply( chunk(fd)
, function(i) quantile(fd[i], c(0.05, 0.95)) )
)
sfClusterEval(close(fd)) # for completeness
csummary(s)
sfStop()


# Basic example: working with arrays (and matrices)

array(1:12, dim=c(3,4))       # read by column
matrix(1:12, 3,4, byrow=TRUE) # read by row

# ff example: physically stored by column ? like columnar OLAP
ff(1:12, dim=c(3,4))                    # read by column
ff(1:12, dim=c(3,4), bydim=c(2,1))      # read by row

# ff example: physically stored by row ? like OLTP database
ff(1:12, dim=c(3,4), dimorder=c(2,1))   # read by column
ff(1:12, dim=c(3,4), dimorder=c(2,1), bydim=c(2,1)) # read by row

fm <- ff(1:12, dim=c(3,4), dimorder=c(2,1))
get.ff(fm, 1:12) # note the physical order
fm[1:12]         # [. exhibits standard R behaviour
ncol(fm) <- 1e8  # not possible with this dimorder
nrow(fm) <- 1e8  # possible with this dimorder

fm <- ff(vmode="double", dim=c(1e4, 1e4))
system.time( fm[1,] <- 1 )  # column store: slow
system.time( fm[,1] <- 1 )  # column store: fast
# even more pronounced difference for caching="mmeachflush"



# Basic example: working with bit filters

l <- rd > 0.99
rd[l]
1e8 * .rambytes["logical"] / (1024^2) # 381 MB for logical

b1 <- b2 <- bit(length(fd))
system.time( b1[] <- c(FALSE, TRUE) )
system.time( for (i in chunk(fd)) b2[i] <- fd[i] > 0.99 )
system.time( b <- b1 & b2 )
object.size(b) / (1024^2)
system.time( x <- fd[b] )
x[1:10]

sum(b) / length(b)  # less dense than 1/32

w <- as.bitwhich(b)
sum(w) / length(w)
object.size(w) / (1024^2)
system.time( x <- fd[w] )
x[1:10]


# Advanced example: working with hybrid indexing

hp <- as.hi(b)		# ignores pack=FALSE
object.size(hp) / (1024^2)
system.time( x <- fd[hp] )
x[1:10]

hu <- as.hi(w, pack=FALSE)
object.size(hu) / (1024^2)
system.time( x <- fd[hu] )
x[1:10]


# Hybrid copying semantics: physical and virtual attributes

x <- ff(1:12, dim=c(3,4))
x
str(physical(x))
str(virtual(x))


# Hybrid coyping semantics in action: different virtual ?views? into same ff

a <- ff(1:12, dim=c(3,4))
b <- a
dim(b) <- c(4,3)
dimorder(b) <- c(2:1)
a
b
vt(a)  # shortcut to virtually transpose
t(a)   # == clone(vt(a))


# Basic example: working with data.frames

id <- 1:12
gender <- sample(factor(c("male","female","unknown")), 12, TRUE)
rating <- matrix(sample(1:6, 12*10, TRUE), 12, 10)
colnames(rating) <- paste("r", 1:10, sep="")
df <- data.frame(id, gender, rating)
df[1:3,]

fid <- as.ff(id); fgender <- as.ff(gender); frating <- as.ff(rating)
fdf <- ffdf(id=fid, gender=fgender, frating)
identical(df, fdf[,])
fdf[1:3,]   # data.frame
fdf[,1:4]   # data.frame
fdf[1:4]    # ffdf
fdf[]       # ffdf
fdf[[2]]    # ff
fdf$gender  # ff


# Advanced example: physical structure of data.frames

# remember that 'rating' was a matrix
# but data.frame has copied to columns (unless we use I(rating))
str(df)

physical(fdf)  # ffdf has *not* copied anything
# lets' physically copy
fdf2 <- ffdf(id=fid, gender=fgender, frating, ff_split=3)
physical(fdf2)
filename(fid)
filename(fdf$id)
filename(fdf2$id)
nrow(fdf2) <- 1e6
fdf2[1e6,] <- fdf[1,]
fdf2
nrow(fdf2) <- 12
# understand this error: pros and cons of embedded ff_matrix
nrow(fdf) <- 16
# understand what this does to the original fid and fgender
fdf3 <- fdf[1:2]
nrow(fdf3) <- 16
fgender
nrow(fdf3) <- 12


# Basic example: reading and writing csv

write.csv(df, file="df.csv")
cat(readLines("df.csv"), sep="\n")
df2 <- read.csv(file="df.csv")
df2

write.csv.ffdf
args(write.table.ffdf)
write.csv.ffdf(fdf, file="fdf.csv")
cat(readLines("fdf.csv"), sep="\n")
fdf2 <- read.csv.ffdf(file="fdf.csv")
fdf2
identical(fdf[,], fdf2[,])


# Advanced example: physical specification when reading a csv

vmode(fdf2)
args(read.table.ffdf)
fdf2 <- read.csv.ffdf(file="fdf.csv", asffdf_args = list(vmode=list(quad="gender", nibble=3:12))
, first.rows = 1, next.rows = 4, VERBOSE=TRUE)
fdf2
fdf3 <- read.csv.ffdf(file="fdf.csv", asffdf_args = list( vmode=list(nibble=3:12)
                    , ff_join=list(3:12)
                )
)
fdf3
# understand this
read.csv.ffdf(file="fdf.csv", asffdf_args = list(vmode=list(quad="gender", nibble=3:12))
, appendLevels = FALSE
, first.rows = 2, VERBOSE=TRUE)


# Basic example: file locations and file survival

# R example: objects are not permanent
# rm(df) simply removes the object
# when closing R with q() everything is gone

# ff example: object data in files is POTENTIALLY permanent
# rm(fdf) just removes the R object,
# the next gc() triggers a finalizer which acts on the file
# when closing R with q()
# the attribute finonexit decides whether the finalizer is called
# finally fftempdir is unlinked
physical(fd)
dir(getOption("fftempdir"))
#file.info(file.path(getOption("fftempdir"), dir(getOption("fftempdir"))))[,"size", drop=FALSE]

# changing file locations and finalizers
sapply(physical(fdf2), finalizer)
filename(fdf2)                    # filename(ff) <- changes one ff
pattern(fdf2) <- "./cwdpat_"
filename(fdf2)                    # ./ renamed to cwdpat_ in getwd()
sapply(physical(fdf2), finalizer) # AND set finalizers to "close"
pattern(fdf2) <- "temppat_"
filename(fdf2)                    # renamed to temppat_ in fftempdir
sapply(physical(fdf2), finalizer) # AND set finalizers to "delete"

# Closing example: managing ff archives

# R example
# save.image() saves all R objects (but not ff files)
# load() restores all R objects (if ff files exist, ff objects work)

# ff example
# get rid of the large objects (takes too long for demo)
delete(frf); rm(frf)
delete(fd); rm(fd)
delete(fm); rm(fm)
rm(b, b1, b2, w, x, hp, hu)

ffsave.image(file="myff")  # => myff.RData + myff.ffData

ffinfo(file="myff")
ffload(file="myff", list="fdf2")
sapply(physical(fdf2), finalizer)

ffdrop(file="myff")

