
let realDeal = fs.readFileSync('2017/07.txt').toString('utf-8').trim()

//Part 1
function parseInput(raw){
    let regex = /\w+/gi;
    return raw.split("\n").map(x => {
        x = x.trim()
        let arrowPos = x.indexOf('->')
        let children = arrowPos > -1 ? x.slice(arrowPos + 3).split(/,\s?/) : []
        let open = x.indexOf('(');
        let close = x.indexOf(')');
        let weight= parseInt(x.slice(open+1, close), 10)
        let name = x.slice(0, open - 1)
        return {name, children, weight}
    })
}

function makeTree(input){
    let tree = input.slice()

    function getChildren(node){
        if(node.children.length){
            let newChildren = [];
            for (child of node.children){
                if(typeof child == 'string'){
                let index = tree.findIndex(x => x.name === child)
                    if(index > -1){
                        let obj = getChildren(tree.splice(index,1)[0])
                        newChildren.push(obj)
                    }
                }else{
                    newChildren.push(child)
                }
            }
            node.children = newChildren
        }
        return node
    }

    for (node of input){
        getChildren(node);
    }
    return tree[0]
}

function test1(){
    let testRaw = `pbga (66)
    xhth (57)
    ebii (61)
    havc (66)
    ktlj (57)
    fwft (72) -> ktlj, cntj, xhth
    qoyq (66)
    padx (45) -> pbga, havc, qoyq
    tknk (41) -> ugml, padx, fwft
    jptl (61)
    ugml (68) -> gyxo, ebii, jptl
    gyxo (61)
    cntj (57)`
    let tree = makeTree(parseInput(testRaw))
    console.log(tree.name === 'tknk')
}

//test1()

// let a = makeTree(parseInput(realDeal))
// console.log(a.name) //hlhomy

//Part2
//
function mode(arr) {
    let m = arr.reduce((acc, x) => {
        acc[x] = (acc[x] || 0) + 1
        return acc
    }, {})
    let maxFreq = Math.max(...Object.values(m))
    let [result, _] = Object.entries(m).find(([_, value]) => value == maxFreq)
    return parseInt(result)
};

function findBalance(tree){
    let children = tree.children.map(findBalance)
    let totalWeight = tree.weight + children.reduce((a, b) => a+b, 0)
    let isBalanced = tree.children.length && children.reduce((x, acc) => x === acc ? x : false)
    if(isBalanced === false){
        let shouldBe = mode(children)
        let unbalanced = children.findIndex(x => x != shouldBe)
        let difference = children[unbalanced] - shouldBe
        //consider first answer only
        console.log('Answer: ', tree.children[unbalanced].weight - difference)
    }
    return totalWeight
}
function test2(){
    let testRaw = `pbga (66)
    xhth (57)
    ebii (61)
    havc (66)
    ktlj (57)
    fwft (72) -> ktlj, cntj, xhth
    qoyq (66)
    padx (45) -> pbga, havc, qoyq
    tknk (41) -> ugml, padx, fwft
    jptl (61)
    ugml (68) -> gyxo, ebii, jptl
    gyxo (61)
    cntj (57)`
    let tree = makeTree(parseInput(testRaw))
    findBalance(tree)
}

// test2()
let a = makeTree(parseInput(realDeal))
findBalance(a)
