function getRealDeal(){
    return fs.readFileSync('2017/08.txt').toString('utf-8').trim()
}

let testInput = `
b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10`

const operations = {
    inc : (a,b) => a+b,
    dec : (a,b) => a-b,
}

function parseInput(input){
    let lines = input.split("\n")
    let regex = /(\w+) (inc|dec) (-?\d+) (if) (.+)/gi
    return lines.filter(x => x.length).map((line) => {
        let matches = regex.exec(input)
        if(matches){
            let [a, variable, operation, quantity, b, condition, ...rest] = matches
            return {variable,operation,quantity,condition}

        }else{
            return line
        }
    })
}

function createReg(sequence){
    let vars = new Set(sequence.map(x => x.variable))
    let obj = {}
    for(v of vars){
        obj[v] = 0
    }
    return obj
}

const doOperation = (register, line) => {
    if(eval('register.'+line.condition)){
        register[line.variable] = operations[line.operation](register[line.variable], parseInt(line.quantity, 10))
    }
    return register
}

function getAnswer(input){
    let parsed = parseInput(input)
    let reg = createReg(parsed)
    let final = parsed.reduce(doOperation, reg)
    final
    return Math.max(...Object.values(final))
}

function getAnswer2(input){
    let parsed = parseInput(input)
    let reg = createReg(parsed)
    let globalMax = 0;
    for(line of parsed){
        reg = doOperation(reg, line)
        globalMax = Math.max(Math.max(...Object.values(reg)), globalMax)
    }
    let final = parsed.reduce(doOperation, reg)
    return globalMax
}
// let final = getAnswer(testInput)
// final
let final = getAnswer2(getRealDeal())
final
