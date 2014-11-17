#![feature(unsafe_destructor)]

use std::io::net::pipe::UnixStream;
use std::collections::VecMap;

pub use wayland::{wl_display, wl_registry, wl_callback};

use std::any::Any;
use std::intrinsics::TypeId;
use std::cell::Cell;
use std::rc::{Rc, Weak};

pub trait Proxy: Any {
    fn set_id(&mut self, id: u32);
    fn set_channel(&mut self, tx: Sender<(u32, u16, Vec<u8>)>);
    fn dispatch(&mut self, opcode: u16, payload: &[u8]);
}

pub struct Guard<'a, T> {
    inner: &'a ProxyRefCell,
}

impl<'a, T: Proxy> Deref<T> for Guard<'a, T> {
    fn deref(&self) -> &T {
        match self.inner.borrows.get() {
            -1 => panic!("Already borrowed mutably"),
            count => {
                self.inner.borrows.set(count + 1);
                debug_assert_eq!((&*self.inner.value).get_type_id(), TypeId::of::<T>());
                unsafe {
                    let obj: std::raw::TraitObject = std::mem::transmute(&*self.inner.value);
                    std::mem::transmute(obj.data)
                }
            }
        }
    }
}

#[unsafe_destructor]
impl<'a, T> Drop for Guard<'a, T> {
    fn drop(&mut self) {
        self.inner.borrows.set(self.inner.borrows.get()-1)
    }
}

pub struct MutGuard<'a, T> {
    inner: &'a ProxyRefCell,
}

impl<'a, T: Proxy> Deref<T> for MutGuard<'a, T> {
    fn deref(&self) -> &T {
        match self.inner.borrows.get() {
            -1 => panic!("Already borrowed mutably"),
            count => {
                self.inner.borrows.set(count + 1);
                debug_assert_eq!((&*self.inner.value).get_type_id(), TypeId::of::<T>());
                unsafe {
                    let obj: std::raw::TraitObject = std::mem::transmute(&*self.inner.value);
                    std::mem::transmute(obj.data)
                }
            }
        }
    }
}

#[unsafe_destructor]
impl<'a, T> Drop for MutGuard<'a, T> {
    fn drop(&mut self) {
        match self.inner.borrows.get() {
            -1 => self.inner.borrows.set(0),
            count => self.inner.borrows.set(count - 1),
        }
    }
}

impl<'a, T: Proxy> DerefMut<T> for MutGuard<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        match self.inner.borrows.get() {
            0 => {
                self.inner.borrows.set(-1);
                debug_assert_eq!((&*self.inner.value).get_type_id(), TypeId::of::<T>())
                unsafe {
                    let obj: std::raw::TraitObject = std::mem::transmute(&*self.inner.value);
                    std::mem::transmute(obj.data)
                }
            },
            -1 => panic!("Already borrowed mutably"),
            _ => panic!("Already borrowed immutably"),
        }
    }
}

pub struct ProxyMutGuard<'a> {
    inner: &'a ProxyRefCell
}

impl<'a> Deref<Proxy> for ProxyMutGuard<'a> {
    fn deref(&self) -> &Proxy {
        match self.inner.borrows.get() {
            -1 => panic!("Already borrowed mutably"),
            count => {
                self.inner.borrows.set(count + 1);
                &*self.inner.value
            }
        }
    }
}

#[unsafe_destructor]
impl<'a> Drop for ProxyMutGuard<'a> {
    fn drop(&mut self) {
        match self.inner.borrows.get() {
            -1 => self.inner.borrows.set(0),
            count => self.inner.borrows.set(count - 1),
        }
    }
}

impl<'a> DerefMut<Proxy> for ProxyMutGuard<'a> {
    fn deref_mut(&mut self) -> &mut Proxy {
        match self.inner.borrows.get() {
            0 => {
                self.inner.borrows.set(-1);
                unsafe { std::mem::transmute(&*self.inner.value) }
            },
            -1 => panic!("Already borrowed mutably"),
            _ => panic!("Already borrowed immutably"),
        }
    }
}

struct ProxyRefCell {
    value: Box<Proxy>,
    borrows: Cell<uint>,

    no_copy: std::kinds::marker::NoCopy,
    no_sync: std::kinds::marker::NoSync,
}

impl ProxyRefCell {
    pub fn new<T: Proxy>(value: T) -> ProxyRefCell {
        ProxyRefCell {
            value: box value as Box<Proxy>,
            borrows: Cell::new(0),
            no_copy: std::kinds::marker::NoCopy,
            no_sync: std::kinds::marker::NoSync,
        }
    }

    pub fn borrow<'a, T: Proxy>(&'a self) -> Guard<'a, T> {
        Guard {
            inner: self,
        }
    }

    pub fn borrow_mut<'a, T: Proxy>(&'a self) -> MutGuard<'a, T> {
        MutGuard {
            inner: self,
        }
    }

    pub fn borrow_proxy_mut<'a>(&'a self) -> ProxyMutGuard<'a> {
        ProxyMutGuard {
            inner: self,
        }
    }
}

pub struct WeakTypedPtr<T: Proxy> {
    ptr: Weak<ProxyRefCell>
}

impl<T: Proxy> WeakTypedPtr<T> {
    pub fn upgrade(&self) -> Option<TypedPtr<T>> {
        self.ptr.upgrade().map(|ptr| TypedPtr { ptr: ptr })
    }
}

pub struct TypedPtr<T: Proxy> {
    ptr: Rc<ProxyRefCell>,
}

impl<T: Proxy> TypedPtr<T> {
    pub fn downgrade(&self) -> WeakTypedPtr<T> {
        WeakTypedPtr {
            ptr: self.ptr.downgrade(),
        }
    }

    pub fn borrow<'a>(&'a self) -> Guard<'a, T> {
        self.ptr.borrow()
    }

    pub fn borrow_mut<'a>(&'a mut self) -> MutGuard<'a, T> {
        self.ptr.borrow_mut()
    }
}

impl<T: Proxy> Clone for TypedPtr<T> {
    fn clone(&self) -> TypedPtr<T> {
        TypedPtr {
            ptr: self.ptr.clone(),
        }
    }
}

pub trait Object: 'static {
    fn get_id(&self) -> u32;
}

struct DummyObject {
    id: u32,
}

impl Object for DummyObject {
    fn get_id(&self) -> u32 {
        self.id
    }
}

// Generated from `wayland.xml`
pub mod wayland {
    pub mod wl_display {
        pub trait Interface: ::Object {
            fn sync(&mut self, _callback: &::wl_callback::Interface) {}
            fn get_registry(&mut self, _registry: &::wl_registry::Interface) {}
            fn error(&mut self, _object_id: &::Object, _code: u32, _message: &str) {}
            fn delete_id(&mut self, _id: u32) {}
        }

        pub struct ClientProxy<T: Interface> {
            id: u32,
            callbacks: T,
            tx: Option<Sender<(u32, u16, Vec<u8>)>>,
        }

        impl<T: Interface> ClientProxy<T> {
            pub fn new(callbacks: T) -> ClientProxy<T> {
                ClientProxy {
                    id: 0,
                    callbacks: callbacks,
                    tx: None,
                }
            }
        }

        impl<T: Interface> ::Proxy for ClientProxy<T> {
            fn set_id(&mut self, id: u32) {
                self.id = id
            }

            fn set_channel(&mut self, tx: Sender<(u32, u16, Vec<u8>)>) {
                self.tx = Some(tx)
            }

            fn dispatch(&mut self, opcode: u16, payload: &[u8]) {
                match opcode {
                    0 => {
                        let mut reader = ::std::io::BufReader::new(payload);
                        let object_id = reader.read_le_u32().unwrap();
                        let code = reader.read_le_u32().unwrap();
                        let message_len = reader.read_le_u32().unwrap() as uint;
                        let message = reader.read_exact(message_len).unwrap();
                        let message = String::from_utf8_lossy(message.slice_to(message_len-1));
                        if message_len % 4 != 0 {
                            reader.consume(4 - (message_len % 4));
                        }
                        self.callbacks.error(&::DummyObject { id: object_id }, code, message.as_slice())
                    },
                    1 => {
                        let mut reader = ::std::io::BufReader::new(payload);
                        let id = reader.read_le_u32().unwrap();
                        self.callbacks.delete_id(id)
                    },
                    _ => (),
                }
            }
        }

        impl<T: Interface> ::Object for ClientProxy<T> {
            fn get_id(&self) -> u32 {
                self.id
            }
        }

        impl<T: Interface> Interface for ClientProxy<T> {
            fn sync(&mut self, callback: &::wl_callback::Interface) {
                match self.tx {
                    Some(ref sender) => {
                        let mut payload = ::std::io::MemWriter::new();
                        payload.write_le_u32(callback.get_id()).unwrap();
                        sender.send((self.id, 0, payload.unwrap()));
                    },
                    None => panic!("No channel set"),
                }
            }

            fn get_registry(&mut self, registry: &::wl_registry::Interface) {
                match self.tx {
                    Some(ref sender) => {
                        let mut payload = ::std::io::MemWriter::new();
                        payload.write_le_u32(registry.get_id()).unwrap();
                        sender.send((self.id, 1, payload.unwrap()));
                    },
                    None => panic!("No channel set"),
                }
            }
        }

        impl<T: Interface> ::std::ops::Deref<T> for ClientProxy<T> {
            fn deref<'a>(&'a self) -> &'a T {
                &self.callbacks
            }
        }

        impl<T: Interface> ::std::ops::DerefMut<T> for ClientProxy<T> {
            fn deref_mut<'a>(&'a mut self) -> &'a mut T {
                &mut self.callbacks
            }
        }

        pub mod error {
            pub const INVALID_OBJECT: u32 = 0;
            pub const INVALID_METHOD: u32 = 1;
            pub const NO_MEMORY: u32 = 2;
        }
    }

    pub mod wl_registry {
        pub trait Interface: ::Object {
            fn bind(&mut self, _name: u32, _id: &::Object) {}

            fn global(&mut self, _name: u32, _interface: &str, _version: u32) {}
            fn global_remove(&mut self, _name: u32) {}
        }

        pub struct ClientProxy<T: Interface> {
            id: u32,
            callbacks: T,
            tx: Option<Sender<(u32, u16, Vec<u8>)>>,
        }

        impl<T: Interface> ClientProxy<T> {
            pub fn new(callbacks: T) -> ClientProxy<T> {
                ClientProxy {
                    id: 0,
                    callbacks: callbacks,
                    tx: None,
                }
            }
        }

        impl<T: Interface> ::Proxy for ClientProxy<T> {
            fn set_id(&mut self, id: u32) {
                self.id = id
            }

            fn set_channel(&mut self, tx: Sender<(u32, u16, Vec<u8>)>) {
                self.tx = Some(tx)
            }

            fn dispatch(&mut self, opcode: u16, payload: &[u8]) {
                match opcode {
                    0 => {
                        let mut reader = ::std::io::BufReader::new(payload);
                        let name = reader.read_le_u32().unwrap();
                        let interface_len = reader.read_le_u32().unwrap() as uint;
                        let interface = reader.read_exact(interface_len).unwrap();
                        let interface = String::from_utf8_lossy(interface.slice_to(interface_len-1));
                        if interface_len % 4 != 0 {
                            reader.consume(4 - (interface_len % 4));
                        }
                        let version = reader.read_le_u32().unwrap();
                        self.callbacks.global(name, interface.as_slice(), version)
                    },
                    1 => {
                        let mut reader = ::std::io::BufReader::new(payload);
                        let name = reader.read_le_u32().unwrap();
                        self.callbacks.global_remove(name)
                    },
                    _ => (),
                }
            }
        }

        impl<T: Interface> ::Object for ClientProxy<T> {
            fn get_id(&self) -> u32 {
                self.id
            }
        }

        impl<T: Interface> Interface for ClientProxy<T> {
            fn bind(&mut self, name: u32, object: &::Object) {
                match self.tx {
                    Some(ref sender) => {
                        let mut payload = ::std::io::MemWriter::new();
                        payload.write_le_u32(name).unwrap();
                        payload.write_le_u32(object.get_id()).unwrap();
                        sender.send((self.id, 0, payload.unwrap()));
                    },
                    None => panic!("No channel set"),
                }
            }
        }

        impl<T: Interface> ::std::ops::Deref<T> for ClientProxy<T> {
            fn deref<'a>(&'a self) -> &'a T {
                &self.callbacks
            }
        }

        impl<T: Interface> ::std::ops::DerefMut<T> for ClientProxy<T> {
            fn deref_mut<'a>(&'a mut self) -> &'a mut T {
                &mut self.callbacks
            }
        }
    }

    pub mod wl_callback {
        pub trait Interface: ::Object {

            fn done(&mut self, _callback_data: u32) {}
        }

        pub struct ClientProxy<T: Interface> {
            id: u32,
            callbacks: T,
            tx: Option<Sender<(u32, u16, Vec<u8>)>>,
        }

        impl<T: Interface> ClientProxy<T> {
            pub fn new(callbacks: T) -> ClientProxy<T> {
                ClientProxy {
                    id: 0,
                    callbacks: callbacks,
                    tx: None,
                }
            }
        }

        impl<T: Interface> ::Proxy for ClientProxy<T> {
            fn set_id(&mut self, id: u32) {
                self.id = id
            }

            fn set_channel(&mut self, tx: Sender<(u32, u16, Vec<u8>)>) {
                self.tx = Some(tx)
            }

            fn dispatch(&mut self, opcode: u16, payload: &[u8]) {
                match opcode {
                    0 => {
                        let mut reader = ::std::io::BufReader::new(payload);
                        let callback_data = reader.read_le_u32().unwrap();
                        self.callbacks.done(callback_data)
                    },
                    _ => (),
                }
            }
        }

        impl<T: Interface> ::Object for ClientProxy<T> {
            fn get_id(&self) -> u32 {
                self.id
            }
        }

        impl<T: Interface> Interface for ClientProxy<T> {
        }

        impl<T: Interface> ::std::ops::Deref<T> for ClientProxy<T> {
            fn deref<'a>(&'a self) -> &'a T {
                &self.callbacks
            }
        }

        impl<T: Interface> ::std::ops::DerefMut<T> for ClientProxy<T> {
            fn deref_mut<'a>(&'a mut self) -> &'a mut T {
                &mut self.callbacks
            }
        }
    }
}

pub struct Registry {
    objects: Vec<(String, u32, u32)>,
}

impl wl_registry::Interface for Registry {
    fn global(&mut self, name: u32, interface: &str, version: u32) {
        self.objects.push((String::from_str(interface), version, name));
    }

    fn global_remove(&mut self, name: u32) {
        self.objects.retain(|&(_, _, name_)| name_ != name);
    }
}

impl Object for Registry {
    fn get_id(&self) -> u32 {
        panic!()
    }
}

impl Registry {
    pub fn get_name(&self, interface: &str, version: u32) -> Option<u32> {
        self.objects.iter()
            .find(|&&(ref i, v, _)| interface == &**i && version == v)
            .map(|&(_, _, name)| name)
    }

    pub fn get_all(&self) -> &[(String, u32, u32)] {
        self.objects.as_slice()
    }
}

struct Display {
    objects: VecMap<Rc<ProxyRefCell>>,
    next_id: u32,
    tx: Sender<(u32, u16, Vec<u8>)>,
}

impl Display {
    fn new(tx: Sender<(u32, u16, Vec<u8>)>) -> Display {
        Display {
            objects: VecMap::new(),
            next_id: 2,
            tx: tx,
        }
    }

    fn new_object<T: Proxy>(&mut self, mut proxy: T) -> TypedPtr<T> {
        let id = self.next_id;

        proxy.set_id(id);
        proxy.set_channel(self.tx.clone());

        let ret = Rc::new(ProxyRefCell::new(proxy));

        self.objects.insert(id as uint, ret.clone());

        while self.objects.contains_key(&(self.next_id as uint)) {
            self.next_id += 1
        }

        TypedPtr { ptr: ret }
    }
}

impl wl_display::Interface for Display {
    fn error(&mut self, object_id: &::Object, code: u32, message: &str) {
        panic!("Fatal server error: {}.{}: {}", object_id.get_id(), code, message)
    }

    fn delete_id(&mut self, id: u32) {
        if id == 1 {
            panic!("Server deleted the display singleton.")
        }
        match self.objects.remove(&(id as uint)) {
            Some(object) => object.borrow_proxy_mut().set_id(0),
            None => (),
        }
        if id <= self.next_id {
            while self.objects.contains_key(&(self.next_id as uint)) {
                self.next_id -= 1
            }
        }
    }
}

impl Object for Display {
    fn get_id(&self) -> u32 {
        panic!()
    }
}

pub struct Connection {
    socket: UnixStream,
    rx: Receiver<(u32, u16, Vec<u8>)>,
    display: wl_display::ClientProxy<Display>,
    registry: Option<WeakTypedPtr<wl_registry::ClientProxy<Registry>>>,
}

impl Connection {
    pub fn connect_default() -> std::io::IoResult<Connection> {
        match std::os::getenv("WAYLAND_DISPLAY") {
            Some(display) => Connection::connect(display.as_slice()),
            None => Err(std::io::IoError {
                kind: std::io::FileNotFound,
                desc: "$WAYLAND_DISPLAY not defined",
                detail: None,
            }),
        }
    }

    pub fn connect(name: &str) -> std::io::IoResult<Connection> {
        let (tx, rx) = channel();
        match std::os::getenv("XDG_RUNTIME_DIR") {
            Some(rtdir) => {
                let mut path = Path::new(rtdir.as_slice());
                path.push(name);

                let mut display = wl_display::ClientProxy::new(Display::new(tx.clone()));
                display.set_id(1);
                display.set_channel(tx);

                Ok(Connection {
                    socket: try!(UnixStream::connect(&path)),
                    display: display,
                    rx: rx,
                    registry: None,
                })
            },
            None => Err(std::io::IoError {
                kind: std::io::FileNotFound,
                desc: "$XDG_RUNTIME_DIR not defined",
                detail: None,
            }),
        }
    }

    pub fn wait_for_next_event(&mut self) -> std::io::IoResult<()> {
        loop {
            match self.rx.try_recv() {
                Ok((0, _, _)) => panic!("A dead object is trying to send a request."),
                Ok((id, opcode, data)) => {
                    try!(self.socket.write_le_u32(id));
                    try!(self.socket.write_le_u32(((8+data.len() as u32) << 16) | opcode as u32));
                    try!(self.socket.write(data.as_slice()));
                },
                Err(_) => break,
            }
        }
        let object = try!(self.socket.read_le_u32());
        let len_opcode = try!(self.socket.read_le_u32());
        let payload_len = (len_opcode as uint >> 16) - 8;
        let opcode = len_opcode as u16;
        let payload = try!(self.socket.read_exact(payload_len));
        if object == 1 {
            Ok(self.display.dispatch(opcode, payload.as_slice()))
        } else {
            Ok(self.display.objects[object as uint].borrow_proxy_mut().dispatch(opcode, payload.as_slice()))
        }
    }

    pub fn get_registry(&mut self) -> TypedPtr<wl_registry::ClientProxy<Registry>> {
        use wl_display::Interface;

        match self.registry.as_ref().and_then(|ptr| ptr.upgrade()) {
            Some(registry) => registry,
            None => {
                let registry = Registry { objects: Vec::new() };
                let registry = self.display.new_object(wl_registry::ClientProxy::new(registry));
                self.display.get_registry(&*registry.borrow());
                self.registry = Some(registry.downgrade());
                registry
            }
        }
    }

    pub fn sync(&mut self, callback: &wl_callback::Interface) {
        use wl_display::Interface;

        self.display.sync(callback)
    }

    pub fn new_object<T: Proxy>(&mut self, callback: T) -> TypedPtr<T> {
        self.display.new_object(callback)
    }
}

