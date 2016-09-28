###################### ========================================================== ######################
######################  Plot to show how gradient descent jumps are not constant  ######################
###################### ========================================================== ######################

## From Section "Initial conditions and choice of learning rate"

## Plot simple Gaussian curve with the colour highlighting the gradient value
x = seq(0, 5, length.out = 2000)

my_data <- data.table(x = seq(0, 5, length.out = 2000), y = x^2 + 0.5)
my_dots <- data.table(Neg.Gradient = c(2, 4, 6, 8, 10), y = c(1.5, 4.5, 9.5, 16.5, 25.5))

step = 0.1

my_grad <- ggplot(data = my_data, aes(x, y, colour = 2*x)) +
    scale_color_gradient(limits = c(0, 10), low = "#cce0ff", high = "#0000ff", expression("Neg.Gradient")) +
    xlab("Prediction function") +
    ylab("Loss function") +
    geom_line(size=1.5) +
    geom_point(data = my_dots, aes(x = Neg.Gradient/2, y = y, size = nu * Neg.Gradient, legend.title = "Step size", reverse = TRUE), colour = "red", show.legend = TRUE) +
    theme_bw() + ## Add or remove the tickers on the x-axis... ?
    theme(axis.title = element_text(size = 20), axis.text.x = element_blank(), legend.title = element_text(size = 20)) 

my_grad

ggsave(my_grad, width = 38, height = 15, units = "cm", filename = "learning_rate-neg_gradient1.png")
 
