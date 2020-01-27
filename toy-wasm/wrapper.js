const fs = require('fs');

var importObject = {
  console: {
    log_i32: function(arg) {
      console.log(arg);
    },
    log_f64: function(arg) {
      console.log(arg);
    }
  }
};

WebAssembly.instantiate(fs.readFileSync('toy.wasm'), importObject);
// WebAssembly.instantiate(fs.readFileSync('hail_iter.wasm'), importObject);
// WebAssembly.instantiate(fs.readFileSync('hail_iter_heap.wasm'), importObject);
