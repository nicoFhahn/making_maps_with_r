# Making Maps with R
## Introduction
This repository hosts the code underlying Making Maps with R, a short book written by me with a lot of help by me, too. The online version of this book is free to read at [https://bookdown.org/nicohahn/making_maps_with_r5](https://bookdown.org/nicohahn/making_maps_with_r5/docs/introduction.html)

## Contributing
If you want to contribute, you can
- suggest improvements to the text, e.g. clarifying unclear sentences or fixing txpos
- suggest changes to make the code more efficient
- make suggestions on the design of the plots.

## Reproducing the book
To build a local version of the book, siimply run the code below:
```{r}
bookdown::render_book("index.Rmd") # to build the book
browseURL("_book/index.html") # to view it
```
