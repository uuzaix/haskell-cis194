function skip(arr) {
  return arr.map((a, i) => {
    return arr.filter((b, j) => {
      return (j + 1) % (i + 1) === 0
    })
  })
}

function everyN(arr, n) {
  if (arr.length < n) {
    return []
  } else {
    return [arr[n - 1]].concat(everyN(arr.slice(n, arr.length), n))
  }
}

function skip2(arr) {
  return arr.map((a, i) => everyN(arr, i + 1))
}

// console.log(skip(["a"]))
// console.log(skip([]))


console.log(everyN(["a", "b", "c", "d", "e", "f"], 2))
console.log(skip(["a", "b", "c", "d", "e", "f"]))
console.log(skip2(["a", "b", "c", "d", "e", "f"]))