function getSpiralRing(number){
    let maximum = 1
    let size = 0
    while(maximum < number){
        size++
        maximum += size * 2 * 4
    }
    return {size, maximum}
}

function findNumberAtRing(number, ring){
    let coords = {x: ring.size, y: ring.size}
    let n = ring.maximum;
    if(number === n){
        return coords
    }
    for(let i = 0; i<ring.size*8 - 1; i++){
        if (coords.y == ring.size && coords.x > -ring.size) {
            coords.x--
        }else if(coords.x == -ring.size && coords.y > -ring.size){
            coords.y--
        }else if (coords.x == ring.size && coords.y < ring.size) {
            coords.y++
        }else if (coords.y == -ring.size && coords.x < ring.size) {
            coords.x++
        }
        if(number === n - (i+1)){
            return coords
        }
    }
}

function manhattan(vector){
    return Math.abs(vector.x) + Math.abs(vector.y)
}

function partA(n){
    return manhattan(findNumberAtRing(n, getSpiralRing(n)))
}

function testA(){
    /*
    Data from square 1 is carried 0 steps, since it's at the access port.
    Data from square 12 is carried 3 steps, such as: down, left, left.
    Data from square 23 is carried only 2 steps: up twice.
    Data from square 1024 must be carried 31 steps.
    */
    console.log(partA(1) === 0)
    console.log(partA(12) === 3)
    console.log(partA(23) === 2)
    console.log(partA(1024) === 31)
}

//testA()
//console.log(partA(347991))

function SVector(x, y){
    this.coords = {x,y}
    this.direction = {x:1, y:0}
    this.next = () => {
        if(this.coords.x > 0){
            if(this.coords.x === -this.coords.y){
                this.direction = {x:-1, y:0}
            }else if(this.coords.x === this.coords.y + 1){
                this.direction = {x:0, y:-1}
            }
        }else{
            if(this.coords.x === -this.coords.y){
                this.direction = {x:1, y:0}
            }else if(this.coords.x === this.coords.y){
                this.direction = {x:0, y:1}
            }
        }

        this.coords = {x: this.coords.x + this.direction.x, y: this.coords.y + this.direction.y}
    }
}

function around(c){
    return [
        {x: c.x-1, y: c.y-1},
        {x: c.x-1, y: c.y},
        {x: c.x-1, y: c.y +1},
        {x: c.x, y: c.y-1},
        {x: c.x, y: c.y},
        {x: c.x, y: c.y +1},
        {x: c.x+1, y: c.y-1},
        {x: c.x+1, y: c.y},
        {x: c.x+1, y: c.y +1},
    ]
}

function partB(maxValue){
    let points = []
    let coord = new SVector(0,0)
    let iter = 0
    points.push({c: coord.coords, v: 1})
    let v = 1
    while(v < maxValue){
        coord.next()
        let c = coord.coords;
        let aroundCoords = around(c)
        v = points.filter((p) => aroundCoords.find((a) => a.x === p.c.x && a.y === p.c.y)).reduce((acc, obj) => acc + obj.v, 0)
        points.push({c, v})
    }
    return v

}

// console.log(partB(347991))
