function test() {
  let input = [0, 2, 7, 0]
  console.log(untilLoop(input))
}

function untilLoop(a) {
    const maxIter = 10000;
    const size = a.length;
    let iter = 0;
    let seen = [];
    let index = 0;
    while (iter < maxIter) {
        iter++
        let max = Math.max(...a)
        let position = a.findIndex(x => x === max)
        a[position] = 0
        let allGet = Math.floor(max / size)
        let howManyGetOneMore = max % size;
        for (let i = 1; i <= size; i++) {
            a[(position + i) % size] += allGet + (i <= howManyGetOneMore ? 1 : 0)
        }
        index = seen.findIndex((x) => x.every((v, i) => v === a[i]))
        if (index > -1) {
            break;
        }
        //console.log(a);
        seen.push(a.slice())
    }
    return [iter, iter - 1 - index]
}
//test()
//

let realDeal = [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]
console.log(untilLoop(realDeal))
