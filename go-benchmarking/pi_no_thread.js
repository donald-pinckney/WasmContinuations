let numTerms = process.argv[2];


let start = getNanoSecTime();

console.log(pi(numTerms));

let dt = getNanoSecTime() - start;

// fmt.Fprintln(os.Stderr, dt.Nanoseconds())
console.error(dt);
    
// Int -> Double
function pi(numTerms) {
    let f = 0.0;
    for(let k = 0; k < numTerms; k++) {
        f += term(k)
    }

	return f
}
// Double -> Double
function term(k) {
	return 4 * Math.pow(-1, k) / (2*k + 1)
}

// https://stackoverflow.com/questions/6002808/is-there-any-way-to-get-current-time-in-nanoseconds-using-javascript
function getNanoSecTime() {
    var hrTime = process.hrtime();
    return hrTime[0] * 1000000000 + hrTime[1];
}