const fs = require('fs');

function f(x){
  const s = x.split('');
  let lvl = 0;
  let score = 0;
  let garbage = false;
  let gcount = 0;
  for (let i = 0; i < s.length; i++){
    if(garbage){
      if(s[i] == '!'){
        i++
      }else if(s[i] == '>'){
        garbage = false
      }else{
          gcount++;
      }
    }else{
      if(s[i] == '<'){
        garbage = true;
      }else if(s[i] == '{'){
        lvl++
      }else if(s[i] == '}'){
        score += lvl;
        lvl--;
      }
    }
  }
  return [score, gcount];
}

let t1 = '{}';
let t2 = '{{{}}}';
let t3 = '{{},{}}';
let t4 = '{{{},{},{{}}}}';
let t5 = '{<a>,<a>,<a>,<a>}'
let t6 = '{{<ab>},{<ab>},{<ab>},{<ab>}}'
let t7 = '{{<!!>},{<!!>},{<!!>},{<!!>}}'
let t8 = '{{<a!>},{<a!>},{<a!>},{<ab>}}'

console.log(f(t1))
console.log(f(t2))
console.log(f(t3))
console.log(f(t4))
console.log(f(t5))
console.log(f(t6))
console.log(f(t7))
console.log(f(t8))

let content = fs.readFileSync('2017/09.txt').toString('utf-8').trim();
console.log(f(content));
