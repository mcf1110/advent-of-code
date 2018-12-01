const realDeal = fs.readFileSync('2017/04.txt').toString('utf-8').trim()


function testA(){
    // aa bb cc dd ee is valid.
    // aa bb cc dd aa is not valid - the word aa appears more than once.
    // aa bb cc dd aaa is valid - aa and aaa count as different words.
    console.log(partA('aa bb cc dd ee') === true)
    console.log(partA('aa bb cc dd aa') === false)
    console.log(partA('aa bb cc dd aaa') === true)
}

const partA = (str) => str.split(' ').filter((val, i, arr) => arr.indexOf(val) !== i).length === 0

const anagrams = (str) => {
    let result = [];

    const permute = (arr, m = []) => {
        if (arr.length === 0) {
            result.push(m)
        } else {
            for (let i = 0; i < arr.length; i++) {
                let curr = arr.slice();
                let next = curr.splice(i, 1);
                permute(curr.slice(), m.concat(next))
            }
        }
    }

    permute(str.split(''))
    return result.map(a => a.join(''));
}
//testA()
//console.log(realDeal.split("\n").map(s => s.trim()).filter(partA).length)

function testB(){
    // abcde fghij is a valid passphrase.
    // abcde xyz ecdab is not valid - the letters from the third word can be rearranged to form the first word.
    // a ab abc abd abf abj is a valid passphrase, because all letters need to be used when forming another word.
    // iiii oiii ooii oooi oooo is valid.
    // oiii ioii iioi iiio is not valid - any of these words can be rearranged to form any other word.
    console.log(partB('abcde fghij') === true)
    console.log(partB('abcde xyz ecdab') === false)
    console.log(partB('a ab abc abd abf abj') === true)
    console.log(partB('iiii oiii ooii oooi oooo') === true)
    console.log(partB('oiii ioii iioi iiio') === false)
}

//Very slow, dont like it
// function partB(str) {
//     const splitted = str.split(' ')
//     return splitted.filter((val, i, arr) => {
//         let an = anagrams(val)
//         return arr.filter((val) => {
//             return an.includes(val)
//         } ).length > 1
//     } ).length === 0
// }

// Better :D
function partB(str) {
    const splitted = str.split(' ')
    const ordered = splitted.map(s=> {
        let sp = s.split('')
        sp.sort()
        return sp.join('')
    });
    return partA(ordered.join(' '))
}
//testB()
//console.log(realDeal.split("\n").map(s => s.trim()).filter(partB).length)
