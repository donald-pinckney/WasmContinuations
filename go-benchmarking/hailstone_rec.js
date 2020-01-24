let numTest = process.argv[2];


let start = getNanoSecTime();

console.log(hailstones(numTest));

let dt = getNanoSecTime() - start;

// fmt.Fprintln(os.Stderr, dt.Nanoseconds())
console.error(dt);
    
// Int -> Int
function hailstone(k) {
    if (k == 1) {
		return 0;
	} else if (k % 2 == 0) {
		return 1 + hailstone(k / 2);
	} else {
		return 1 + hailstone(3*k + 1);
	}
}

// Int -> Int
function hailstones(numTest) {
    let i = 0;
    for (let k = 1; k <= numTest; k++) {
		i += hailstone(k);
	}
	return i;
}

// https://stackoverflow.com/questions/6002808/is-there-any-way-to-get-current-time-in-nanoseconds-using-javascript
function getNanoSecTime() {
    var hrTime = process.hrtime();
    return hrTime[0] * 1000000000 + hrTime[1];
}