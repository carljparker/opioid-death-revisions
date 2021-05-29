
csv.data.file.ch <- "data/overdose-values-df.txt" 

csv.data.df <- read.csv( 
                         csv.data.file.ch, 
                         header = TRUE, 
                         quote="\"",
                         sep = ",", 
                         na.strings = c( "NA", "-" ),
                         allowEscapes = TRUE,
                         strip.white  = TRUE,
                         stringsAsFactors = TRUE,
                         comment.char="#" 
                       )

names( csv.data.df )


#
# Line chart (time series) from two columns in a dataframe
#

#
# Note the leading commas before the column names which gets us the
# values in the columns as vectors rather than lists.
#
plot( csv.data.df[ ,"year" ], csv.data.df[ ,"orig" ], type = "l" )


#
# Stacked bar chart (time series) from three columns in a dataframe
#

#
# Create the third column for the amount each year was reduced
#
( csv.data.df$delta <- csv.data.df$orig - csv.data.df$revised ) 

#
# `rbind` gives us a matrix
#
( my.matrix <-  rbind( csv.data.df$orig, csv.data.df$delta ) )

#
# Convert the matrix to a table
#
my.table <- as.table( my.matrix )

#
# Use a column from the dateframe for the column names of the table
#
( colnames( my.table ) <-  csv.data.df$year )


#
# Convert RGB for tumblr background to color arguments for png bg param 
#
# #f8f5ec
#
# rgb( 248, 245, 236, maxColorValue = 255 ) 
#
strtoi( c( "f8", "f5", "ec" ), 16 )
is( strtoi( c( "f8", "f5", "ec" ), 16 ) )

png( "viz/overdose-death-revisions.png", width = 2400, height = 1800, units = "px", pointsize = 24, bg = rgb( 248, 245, 236, maxColorValue = 255 ) )

#
# The `cex.*` parameters enlarge or reduce the font size for various
# textual components of the graph. 
#
# Use `mar` (margin) to increase the left margin to accomodate the
# enlarged y-axis label.
#
par( 
    cex.main = 3, cex.axis = 1.75, cex.lab = 2,
    # margins: bottom, left, top and right
    par( mar = c( 6, 7, 4, 4 ) + 0.1 )  
)

barplot(my.table, main="Drug overdose deaths revised down", bg="yellow",
        xlab="Year", ylab="Number of deaths", col = c( "blue", "red" ) ) 

dev.off()


#
# Plot the deltas as percentages
#

#
# Create an additional column with the percentage change
#
( csv.data.df$delta.percent <- ( csv.data.df$delta / csv.data.df$orig ) * 100 )
( delta.percent.ma <-  rbind( csv.data.df$year, csv.data.df$delta.percent ) )

png( "viz/overdose-death-revisions-percentages.png", 
      width = 2400, height = 1800, 
      units = "px", pointsize = 24, 
      bg = rgb( 248, 245, 236, maxColorValue = 255 ) 
)

par( 
    cex.main = 3, cex.axis = 1.75, cex.lab = 2,
    # margins: bottom, left, top and right
    par( mar = c( 6, 7, 4, 4 ) + 0.1 )  
)

x <- barplot( 
        csv.data.df$delta.percent, names = csv.data.df$year, 
        main = "Percentage that overdose deaths revised down by year",
        ylim = c( 0, 6 ), xlab="Year", ylab="Percentage change", 
        col = c( "orange" ) 
) 

y.loc <- csv.data.df$delta.percent + 0.1
#
# Label the bars of a barchart with data.
#
# The `text()` . . . procedure(?) . . . wants vectors for its first
# three parameters.
#
text( x[ , 1], y.loc, paste( round( csv.data.df$delta.percent, 2 ), "%", sep = "" ) )

dev.off()


#
# Create a smaller rendering of the bar chart to see if it is more
# legible on Tumblr.
#
# Dimensions: 500 x 375
#

#
# We need to reconfigure values for png and par. Otherwise, the
# previously configured values (from above) continue to prevail.
#
png( "viz/reduced-overdose-death-revisions-percentages.png", 
      width = 500, height = 375, 
      units = "px", pointsize = 10, 
      bg = rgb( 248, 245, 236, maxColorValue = 255 ) 
)

par( 
    cex.main = 1, cex.axis = 1, cex.lab = 1,
    # margins: bottom, left, top and right
    par( mar = c( 4, 4, 4, 4 ) + 0.1 )  
)

x <- barplot( 
        csv.data.df$delta.percent, names = csv.data.df$year, 
        main = "Percentage that overdose deaths revised down by year",
        ylim = c( 0, 6 ), xlab="Year", ylab="Percentage change", 
        col = c( "orange" ) 
) 

text( x[ , 1], y.loc, paste( round( csv.data.df$delta.percent, 2 ), "%", sep = "" ) )

dev.off()


#
# Sort the data in order of increasing percentage.
#
csv.data.sort.df <- csv.data.df[ order( csv.data.df$delta.percent, decreasing = FALSE ), ] 

csv.data.sort.df

png( "viz/overdose-death-revisions-sorted-percentages.png", 
      width = 1000, height = 750, 
      units = "px", pointsize = 16, 
      bg = rgb( 248, 245, 236, maxColorValue = 255 ) 
)

par( 
    cex.main = 1.50, cex.axis = 1.10, cex.lab = 1.25,
    # margins: bottom, left, top and right
    par( mar = c( 6, 7, 4, 4 ) + 0.1 )  
)

x <- barplot( 
        csv.data.sort.df$delta.percent, names = csv.data.sort.df$year, 
        main = "Downward adjstments ordered by percentage change",
        ylim = c( 0, 6 ), xlab="Year", ylab="Percentage change", 
        col = c( "orange" ) 
) 

( y.loc <- csv.data.sort.df$delta.percent + 0.1 )

text( x[ , 1], y.loc, paste( round( csv.data.sort.df$delta.percent, 2 ), "%", sep = "" ) )

dev.off()


#
# We need to reconfigure values for png and par. Otherwise, the
# previously configured values (from above) continue to prevail.
#
png( "viz/reduced-overdose-death-revisions-sorted-percentages.png", 
      width = 500, height = 375, 
      units = "px", pointsize = 9, 
      bg = rgb( 248, 245, 236, maxColorValue = 255 ) 
)

par( 
    cex.main = 1, cex.axis = 1, cex.lab = 1,
    # margins: bottom, left, top and right
    par( mar = c( 4, 4, 4, 4 ) + 0.1 )  
)

x <- barplot( 
        csv.data.sort.df$delta.percent, names = csv.data.sort.df$year, 
        main = "Downward adjstments ordered by percentage change",
        ylim = c( 0, 6 ), xlab="Year", ylab="Percentage change", 
        col = c( "orange" ) 
) 

( y.loc <- csv.data.sort.df$delta.percent + 0.2 )

text( x[ , 1], y.loc, paste( round( csv.data.sort.df$delta.percent, 2 ), "%", sep = "" ) )

dev.off()

mean( csv.data.sort.df$delta.percent )
median( csv.data.sort.df$delta.percent )


# --- END --- #

