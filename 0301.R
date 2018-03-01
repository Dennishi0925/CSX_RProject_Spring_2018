rm(list = ls())
p <- ggplot(mtcars, aes(mpg, wt)) +
  geom_point()
cat("foo\tbar", fill = TRUE)

cat("foo\nbar", fill = TRUE)

x <- c(  "I", "saw", "a", "saw", "that", "could", "out",  "saw", "any", "other", "saw", "I", "ever", "saw")
y <- noquote(x)
y
typeof(y)
x <- (1:15) ^ 2
toString(x)
x
toString(x, width = 10)

typeof(x)
length(x)
paste(c("red", "yellow"), "lorry", sep = "-")

paste(c("red", "yellow"), "lorry", collapse = ", ")
paste(c("red", "yellow"), "lorry", collapse = "")
paste(c("red", "yellow"), "lorry", sep = " ")
paste0(c("red", "yellow"), "lorry")
# Use theme_set() to completely override the current theme.
# Here we have the old theme so we can later restore it.
# Note that the theme is applied when the plot is drawn, not
# when it is created.
old <- theme_set(theme_gray())
p
theme_set(old)
p


# Modifying theme objects -----------------------------------------
# You can use + and %+replace% to modify a theme object.
# They differ in how they deal with missing arguments in
# the theme elements.

add_el <- theme_grey() +
  theme(text = element_text(family = "Times"))
add_el$text

rep_el <- theme_grey() %+replace%
  theme(text = element_text(family = "Times"))
rep_el$text

# theme_update() and theme_replace() are similar except they
# apply directly to the current/active theme.
