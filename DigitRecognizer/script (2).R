# Plots 49 random handwritten digits drawn from the training set

library(ggplot2)
library(grid)
library(readr)
train <- data.frame(read_csv("C:/Users/hp/Desktop/Kaggle/DigitRecognizer/train.csv"))

labels   <- train[,1]
features <- train[,-1]

# Uncomment for reproducability
# set.seed(1) 
rowsToPlot <- sample(1:nrow(train), 49)

rowToMatrix <- function(row) {
  intensity <- as.numeric(row)/max(as.numeric(row))
  return(t(matrix((rgb(intensity, intensity, intensity)), 28, 28)))
}

geom_digit <- function(digits) 
{
  layer(geom = GeomRasterDigit, stat = "identity", position = "identity", data = NULL, 
        params = list(digits=digits))  
}

GeomRasterDigit <- ggproto("GeomRasterDigit", 
                           ggplot2::GeomRaster, 
                           draw_panel = function(data, panel_scales, coordinates, digits = digits) {
                             if (!inherits(coordinates, "CoordCartesian")) {
                               stop("geom_digit only works with Cartesian coordinates",
                                    call. = FALSE)
                             }
                             corners <- data.frame(x = c(-Inf, Inf), y = c(-Inf, Inf))
                             bounds <- coordinates$transform(corners, panel_scales)
                             x_rng <- range(bounds$x, na.rm = TRUE)
                             y_rng <- range(bounds$y, na.rm = TRUE)
                             rasterGrob(as.raster(rowToMatrix(digits[data$rows,])), 
                                        x = mean(x_rng), y = mean(y_rng), 
                                        default.units = "native", just = c("center","center"), 
                                        interpolate = FALSE)
                           }) 

p <- ggplot(data.frame(rows=rowsToPlot, labels=labels[rowsToPlot]), 
            aes(x=0.1, y=.9, rows=rows, label=labels)) + 
       geom_blank() + xlim(0,1) + ylim(0,1) + xlab("") + ylab("") + 
       facet_wrap(~ rows, ncol=7) +
       geom_digit(features) +
       geom_text(colour="#53cfff") +
       theme(panel.background = element_rect(fill = 'black'),
             panel.border = element_rect(fill = NA, colour = "#cfff53"),
             panel.grid = element_blank(),
             strip.background = element_blank(),
             strip.text.x = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             axis.line = element_blank()) +
       ggtitle("Example Handwritten Digits")

plot(p)

ggsave("example_digits.png", p, width=10, height=10)