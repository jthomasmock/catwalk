mydat <- data.frame(date = c(seq(as.Date("2010/01/01"), as.Date("2010/01/31"), by=1)),
                    value1 = abs(round(rnorm(31), 2)),
                    value2 = abs(round(rnorm(31), 2)),
                    value3 = abs(round(rnorm(31), 2)))
head(mydat)

cat_df2 <- as.data.frame(cat_sum_df)

viz_func <- function(data, x, y){
  ggplot(data, aes_string(x=x, y=y)) +
    geom_line(lwd=1.05) + geom_bar(stat = "identity") + 
    ggtitle("Insert Title Here") +
    xlab("Date") + ylab("Value") + 
    theme(axis.text.x=element_text(colour="black")) +
    theme(axis.text.y=element_text(colour="black"))
}

viz_func(cat_sum_df, 'age', 'run_speed_mean')

str(cat_sum_df)
