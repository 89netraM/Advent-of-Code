#set document(title: "Advent of Code 2025 Day 02", author: "Mårten Åsberg", date: datetime(
  year: 2025,
  month: 12,
  day: 2,
))
#set page(paper: "a5", flipped: true, margin: 0mm)
#set text(lang: "en")

#let code = read("main.typ").split("\n")
#let part(start, length, body) = pad(x: 1cm, grid(
  columns: (2fr, 1fr),
  gutter: 5mm,
  align(horizon, rect(height: 100%, stroke: none, raw(code.slice(start, count: length).join("\n"), lang: "typ"))),
  pad(y: 1cm, align(horizon, rect(height: 100%, stroke: none, body))),
))

#let input = read("input.txt").trim().split(",")

#let sum = 0
#for range in input {
  let (from, to) = range.split("-")
  let from = int(from)
  let to = int(to)
  for num in array.range(from, to + 1) {
    let n = str(num)
    if (n.len().bit-and(1) == 0
        and n.starts-with(n.slice(int(n.len() / 2)))) {
      sum += num
    }
  }
}

#part(18, 13)[
  = Part 1

  Answer: #raw(str(sum))
]

#pagebreak()

#let sum = 0
#for range in input {
  let (from, to) = range.split("-")
  let from = int(from)
  let to = int(to)
  for num in array.range(from, to + 1) {
    let n = str(num)
    if n.len() == 1 {
      continue
    }
    let length = 1
    while length <= int(calc.ceil(n.len() / 2)) {
      if calc.rem(n.len(), length) == 0 {
        let pattern = n.slice(0, count: length)
        let start = length
        let match = true
        while start < n.len() {
          if n.slice(start, count: length) != pattern {
            match = false
            break
          }
          start += length
        }
        if match {
          sum += num
          break
        }
      }
      length += 1
    }
  }
}

#part(40, 32)[
  = Part 2

  Answer: #raw(str(sum))
]
