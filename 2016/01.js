/*
Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2 blocks away.
R5, L5, R5, R3 leaves you 12 blocks away.
*/

function test(){
  let test1 = ['R2', 'L3']
  let test2 = ['R2', 'R2', 'R2']
  let test3 = ['R5', 'L5', 'R5', 'R3']
  console.log(calculate(test1) === 5)
  console.log(calculate(test2) === 2)
  console.log(calculate(test3) === 12)
}

function Vector(x ,y){
    this.x = x;
    this.y = y;

    //Int -> Vector
    this.scale = function(s){
        return new Vector(this.x*s, this.y*s)
    }

    //Vector -> Vector
    this.add = function(v){
        return new Vector(this.x + v.x, this.y + v.y)
    }

    //Direction -> Vector
    this.turn = function(direction){
        if(direction === 'R'){
            return new Vector(this.y, -this.x)
        }else{
            return new Vector(-this.y, this.x)
        }
    }

    //Int
    this.manhattan = function(){
        return Math.abs(this.x) + Math.abs(this.y)
    }

    //String
    this.toString = () => `(${x}, ${y})`
}

function calculate(input){
    let currentDirection = new Vector(0,1)
    let position = new Vector(0,0)
    for(let [direction, ...distance] of input){
        distance = parseInt(distance.join(''), 10)
        currentDirection = currentDirection.turn(direction)
        position = position.add(currentDirection.scale(distance))
    }
    return position.manhattan()
}

let realDeal = ['L5', 'R1', 'R3', 'L4', 'R3', 'R1', 'L3', 'L2', 'R3', 'L5', 'L1', 'L2', 'R5', 'L1', 'R5', 'R1', 'L4', 'R1', 'R3', 'L4', 'L1', 'R2', 'R5', 'R3', 'R1', 'R1', 'L1', 'R1', 'L1', 'L2', 'L1', 'R2', 'L5', 'L188', 'L4', 'R1', 'R4', 'L3', 'R47', 'R1', 'L1', 'R77', 'R5', 'L2', 'R1', 'L2', 'R4', 'L5', 'L1', 'R3', 'R187', 'L4', 'L3', 'L3', 'R2', 'L3', 'L5', 'L4', 'L4', 'R1', 'R5', 'L4', 'L3', 'L3', 'L3', 'L2', 'L5', 'R1', 'L2', 'R5', 'L3', 'L4', 'R4', 'L5', 'R3', 'R4', 'L2', 'L1', 'L4', 'R1', 'L3', 'R1', 'R3', 'L2', 'R1', 'R4', 'R5', 'L3', 'R5', 'R3', 'L3', 'R4', 'L2', 'L5', 'L1', 'L1', 'R3', 'R1', 'L4', 'R3', 'R3', 'L2', 'R5', 'R4', 'R1', 'R3', 'L4', 'R3', 'R3', 'L2', 'L4', 'L5', 'R1', 'L4', 'L5', 'R4', 'L2', 'L1', 'L3', 'L3', 'L5', 'R3', 'L4', 'L3', 'R5', 'R4', 'R2', 'L4', 'R2', 'R3', 'L3', 'R4', 'L1', 'L3', 'R2', 'R1', 'R5', 'L4', 'L5', 'L5', 'R4', 'L5', 'L2', 'L4', 'R4', 'R4', 'R1', 'L3', 'L2', 'L4', 'R3', ]

//console.log(calculate(realDeal))
