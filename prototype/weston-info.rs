extern crate wayland;

struct Callback {
    done: bool,
}

impl wayland::wl_callback::Interface for Callback {
    fn done(&mut self, _: u32) {
        self.done = true
    }
}

impl wayland::Object for Callback {
    fn get_id(&self) -> u32 {
        panic!()
    }
}

fn main() {
    let mut conn = wayland::Connection::connect_default().unwrap();
    let registry = conn.get_registry();
    let callback = conn.new_object(wayland::wl_callback::ClientProxy::new(Callback { done: false }));
    conn.sync(&*callback.borrow());

    while !callback.borrow().done {
        conn.wait_for_next_event().unwrap();
    }

    for &(ref interface, version, name) in registry.borrow().get_all().iter() {
        println!("interface: '{}', version: {}, name: {}", interface, version, name);
    }
}
