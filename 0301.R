heights <- data.frame(
  height_cm = c(153, 181, 150, 172, 165, 149, 174, 169, 198, 163),
  gender = c(
    "female", "male", "female", "male", "male",
    "female", "female", "male", "male", "female"
  )
)
str(heights$gender)
levels(heights$gender)

substr(rep("abcdef", 4), 1:4, 4:5)
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
substr(x, 1, 6)
substr(x, 1:5, 6)

substring(x, 2, 4:6)

substring(x, 2) <- c("..", "+++")
x
# }



woodchuck <- c(
  "How much wood would a woodchuck chuck",
  "If a woodchuck could chuck wood?",
  "He would chuck, he would, as much as he could",
  "And chuck as much wood as a woodchuck would",
  "If a woodchuck could chuck wood."
)
getwd()
length(woodchuck)
substring(woodchuck, 1:10, 1)
?substring
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
