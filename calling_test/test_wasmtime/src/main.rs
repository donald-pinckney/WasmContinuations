//! Translation of hello example

extern crate alloc;

// use alloc::rc::Rc;
use core::cell::Ref;
use failure::{bail, format_err, Error};
use std::fs::read;
use wasmtime_api::*;
use std::rc::Rc;


static mut __THE_INST: Option<HostRef<Instance>> = None;

fn get_inst() -> &'static mut HostRef<Instance> {
    unsafe { 
        match &mut __THE_INST {
            Some(x) => x,
            None => panic!(),
        } 
    }
}

fn set_inst(i: HostRef<Instance>) {
    unsafe { __THE_INST = Some(i); }
}

struct PrintCallback(i32);

impl Callable for PrintCallback {
    fn call(&self, params: &[Val], _results: &mut [Val]) -> Result<(), HostRef<Trap>> {
        // let STUFF_val = unsafe { STUFF };
        let exports = Ref::map(get_inst().borrow(), |instance| instance.exports());
        let inc_func = exports[1].func().unwrap();

        match params[0] {
            Val::I32(x) => {
                match inc_func.borrow().call(&[Val::I32(x)]).expect("bad")[0] {
                    Val::I32(inc_res) => {
                        println!("> {}", inc_res)
                    },
                    _ => panic!()
                }
            },
            _ => panic!("Wrong argument type")
        }
        Ok(())
    }
}

fn main() -> Result<(), Error> {
    // Initialize.
    println!("Initializing...");
    let engine = HostRef::new(Engine::new(Config::default()));
    let store = HostRef::new(Store::new(engine));

    // Load binary.
    println!("Loading binary...");
    let binary = read("hello.wasm")?;

    // Compile.
    println!("Compiling module...");
    let module = HostRef::new(
        Module::new(store.clone(), &binary)
            .map_err(|_| format_err!("> Error compiling module!"))?,
    );

    // Create external print functions.
    println!("Creating callback...");
    let print_type = FuncType::new(Box::new([ValType::I32]), Box::new([]));

    let print_func = HostRef::new(Func::new(store.clone(), print_type, Rc::new(PrintCallback(5))));

    // Instantiate.
    println!("Instantiating module...");
    let imports = vec![print_func.into()];
    set_inst(HostRef::new(
        Instance::new(store.clone(), module, imports.as_slice()).map_err(|_| format_err!("> Error instantiating module!"))?
    ));

    // Extract export.
    println!("Extracting export...");
    let exports = Ref::map(get_inst().borrow(), |instance| instance.exports());
    if exports.len() == 0 {
        bail!("> Error accessing exports!");
    }
    let run_func = exports[0]
        .func()
        .ok_or_else(|| format_err!("> Error accessing exports!"))?;

    // Call.
    println!("Calling export...");
    if let Err(_) = run_func.borrow().call(&[]) {
        bail!("> Error calling function!");
    }

    // Shut down.
    println!("Shutting down...");
    drop(store);

    // All done.
    println!("Done.");
    Ok(())
}
