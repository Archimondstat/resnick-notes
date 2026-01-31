# Bookdown build

## Prerequisites
In R:

- install.packages(c("bookdown", "rmarkdown"))

## Build (HTML)
Open R in this folder and run:

```r
bookdown::render_book("index.Rmd", output_format = "bookdown::gitbook")
```

The output will be in `_book/`.

## Notes
- I removed the old floating TOC HTML/JS because `bookdown::gitbook` already provides a sidebar TOC.
- Your original `styles.css` is kept, with an extra `h1` rule added to keep chapter titles compact.
