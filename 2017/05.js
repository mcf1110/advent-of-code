let realDeal = fs.readFileSync('2017/05.txt').toString('utf-8').trim()

function testA(){
    let str = `0
                3
                0
                1
                -3`;
    console.log(partA(str) === 5)
}

function partA(str){
    let tramps = str.split("\n").map(s => s.trim())
    let i = 0;
    let iter = 0;
    const size = tramps.length;
    while(i < size){
        iter++
        i += tramps[i] ++
    }
    return iter
}

// testA()
// console.log(partA(realDeal))
function testB(){
    let str = `0
                3
                0
                1
                -3`;
    console.log(partB(str) === 10)
}

function partB(str){
    let tramps = str.split("\n").map(s => s.trim())
    let i = 0;
    let iter = 0;
    let offset;
    const size = tramps.length;
    while(i < size){
        iter++
        offset = Number(tramps[i])
        tramps[i] = offset + ((offset >= 3) ? -1 : 1)
        i += offset
    }
    return iter
}
// testB()
console.log(partB(realDeal))
