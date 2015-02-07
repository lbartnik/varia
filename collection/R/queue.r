Queue <- setRefClass('Queue',
  fields = list(elements = "character"),
  methods = list(
    empty     = function () { length(elements) == 0 },
    push_back = function (x) {
      elements <<- append(elements, as.character(x))
    },
    pop_front = function () {
      ret <- elements[1]
      elements <<- elements[-1]
      ret
    },
    contains = function (x) {
      x %in% elements
    }
  )
)
