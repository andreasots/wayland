#![feature(plugin_registrar, tuple_indexing, macro_rules, struct_variant)]

extern crate syntax;
extern crate rustc;
extern crate xml;

use syntax::codemap::{DUMMY_SP, Span};
use syntax::parse::token;
use syntax::ast::{TokenTree, TtToken, Item, StructDef, Public, LitStr, CookedStr};
use syntax::ext::base::{ExtCtxt, MacResult, DummyResult};
use syntax::ext::build::AstBuilder;
use syntax::util::small_vector::SmallVector;
use syntax::ptr::P;

use std::io::{BufferedReader, File};
use std::from_str::FromStr;
use xml::reader::events;
use std::str::{MaybeOwned, Slice, Owned};

macro_rules! error(
    ($cx:expr, $sp:expr, $msg:expr) => ({
        $cx.span_err($sp, $msg);
        return DummyResult::any($sp);
    })
)

macro_rules! required_attr(
    ($attrs:expr, $tag:expr, $attr:expr) => (match find_attrib($attrs, $attr) {
        Some(value) => value,
        None => return Err(concat!("<", $tag, "> lacks required attribute \"", $attr, "\"")),
    })
)

struct MacItem(P<Item>);

impl MacResult for MacItem {
    fn make_items(self: Box<MacItem>) -> Option<SmallVector<P<Item>>> {
        Some(SmallVector::one(self.0.clone()))
    }
}

// Copy of `rustc::lint::builtin::NonCamelCaseTypes::to_camel_case(&str)`
fn to_camel_case(s: &str) -> String {
    s.split('_').flat_map(|word| word.chars().enumerate().map(|(i, c)| if i == 0 { c.to_uppercase() } else { c })).collect()
}

fn desmurf(symbols: &mut [String]) {
    if symbols.len() < 2 {
        return;
    }

    let prefix = {
        let mut prefix = std::uint::MAX;
        let ref head = symbols[0];
        let tail = symbols.slice_from(1);
        for symbol in tail.iter() {
            let ((count, _), _) = head.char_indices().zip(symbol.chars()).take_while(|&((_, a), b)| a == b).last().unwrap_or(((-1, 'a'), 'a'));
            prefix = std::cmp::min(prefix, count+1);
        }
        prefix
    };

    if prefix == 0 {
        return;
    }

    for e in symbols.iter_mut() {
        let e = unsafe { e.as_mut_vec() };
        let len = e.len();
        for i in range(prefix, len) {
            e[i-prefix] = e[i];
        }
        e.truncate(len-prefix);
    }
}

fn find_attrib<'a>(attributes: &'a Vec<xml::common::Attribute>, key: &str) -> Option<&'a str> {
    for attr in attributes.iter() {
        if attr.name.local_name.as_slice() == key {
            return Some(attr.value.as_slice());
        }
    }
    return None;
}

struct Description {
    summary: String,
    description: String,
}

struct Arg {
    description: Option<Description>,
    name: String,
    type_: Type,
    summary: Option<String>,
    allow_null: bool,
}

struct Entry {
    description: Option<Description>,
    value: u32,
    summary: Option<String>,
}

enum RequestType {
    Normal,
    Destructor,
}

enum Type {
    Int,
    Uint,
    Fixed,
    String_,
    Object(Option<String>),
    NewId(Option<String>),
    Array,
    Fd,
}

enum InterfaceMember {
    Request {
        description: Option<Description>,
        name: String,
        type_: RequestType,
        args: Vec<Arg>,
    },
    Event {
        description: Option<Description>,
        name: String,
        args: Vec<Arg>,
    },
    Enum {
        description: Option<Description>,
        name: String,
        entries: Vec<Entry>,
    }
}

struct Interface {
    name: String,
    version: String,
    children: Vec<InterfaceMember>,
    description: Option<Description>,
}

struct Protocol {
    name: String,
    copyright: Option<String>,
    interfaces: Vec<Interface>,
}

enum Tag {
    ProtocolTag,
    CopyrightTag,
    InterfaceTag,
    RequestTag,
    EventTag,
    EnumTag,
    EntryTag,
    ArgTag,
    DescriptionTag,
}

struct Schema {
    root: Option<Protocol>,
    state: Vec<Tag>,
}

impl Schema {
    fn new() -> Schema {
        Schema {
            root: None,
            state: Vec::new(),
        }
    }

    fn dispatch(&mut self, event: events::XmlEvent) -> Result<(), &'static str> {
        match event {
            events::StartDocument { ..} => Ok(()),
            events::EndDocument => Ok(()),
            events::ProcessingInstruction { ..} => Ok(()),
            events::StartElement { name, attributes, ..} =>
                self.start_element(name.local_name, attributes),
            events::EndElement { name } => self.end_element(name.local_name),
            events::CData(data) | events::Characters(data) => self.character_data(data),
            events::Comment(_) | events::Whitespace(_) => Ok(()),
            events::Error(_) => Err("XML parse error"),
        }
    }

    fn start_element(&mut self, name: String, attrs: Vec<xml::common::Attribute>)
            -> Result<(), &'static str> {
        match name.as_slice() {
            "protocol" => match self.state.last() {
                Some(_) => Err("<protocol> is only allowed as a top level element."),
                None => {
                    self.root = Some(Protocol {
                        name: String::from_str(required_attr!(&attrs, "protocol", "name")),
                        copyright: None,
                        interfaces: Vec::new(),
                    });
                    self.state.push(ProtocolTag);
                    Ok(())
                }
            },
            "copyright" => match self.state.last() {
                Some(&ProtocolTag) => {
                    self.state.push(CopyrightTag);
                    Ok(())
                },
                _ => Err("<copyright> is not allowed as a root element"),
            },
            "interface" => match self.state.last() {
                Some(&ProtocolTag) => {
                    self.root.as_mut().unwrap().interfaces.push(Interface {
                        name: String::from_str(required_attr!(&attrs, "interface", "name")),
                        version: String::from_str(required_attr!(&attrs, "interface", "version")),
                        children: Vec::new(),
                        description: None,
                    });
                    self.state.push(InterfaceTag);
                    Ok(())
                },
                _ => Err("<interface> is not allowed as a root element"),
            },
            "request" => match self.state.last() {
                Some(&InterfaceTag) => {
                    self.root.as_mut().unwrap().interfaces.last_mut().unwrap().children.push(Request {
                        description: None,
                        name: String::from_str(required_attr!(&attrs, "request", "name")),
                        type_: if find_attrib(&attrs, "type") == Some("destructor") {
                            Destructor
                        } else {
                            Normal
                        },
                        args: Vec::new(),
                    });
                    self.state.push(RequestTag);
                    Ok(())
                },
                _ => Err("<request> is not allowed as a root element"),
            },
            "event" => match self.state.last() {
                Some(&InterfaceTag) => {
                    self.state.push(EventTag);
                    Ok(())
                },
                _ => Err("<event> is not allowed in this context"),
            },
            "enum" => match self.state.last() {
                Some(&InterfaceTag) => {
                    self.state.push(EnumTag);
                    Ok(())
                },
                _ => Err("<enum> is not allowed in this context"),
            },
            "entry" => match self.state.last() {
                Some(&EnumTag) => {
                    self.state.push(EntryTag);
                    Ok(())
                },
                _ => Err("<entry> is not allowed in this context"),
            },
            "arg" => match self.state.last() {
                Some(&RequestTag) | Some(&EventTag) => {
                    self.state.push(ArgTag);
                    Ok(())
                },
                _ => Err("<arg> is not allowed in this context"),
            },
            "description" => match self.state.last() {
                Some(&InterfaceTag) | Some(&RequestTag) | Some(&EventTag) | Some(&EnumTag)
                        | Some(&EntryTag) | Some(&ArgTag) => {
                    self.state.push(DescriptionTag);
                    Ok(())
                },
                _ => Err("<description> is not allowed in this context"),
            },
            _ => Err("Unrecognised tag"),
        }
    }

    fn end_element(&mut self, name: String) -> Result<(), &'static str> {
        self.state.pop();
        Ok(())
    }

    fn character_data(&mut self, data: String) -> Result<(), &'static str> {
        match self.state.last() {
            Some(&CopyrightTag) => {
                let ref mut copyright = self.root.as_mut().unwrap().copyright;
                match *copyright {
                    Some(ref mut copyright) => copyright.push_str(data.as_slice()),
                    None => *copyright = Some(data),
                }
                Ok(())
            },
            Some(&DescriptionTag) => match self.state.get(self.state.len()-2) {
                Some(&InterfaceTag) => {
                    self.root.as_mut().unwrap()
                        .interfaces.last_mut().unwrap()
                        .description.unwrap()
                        .description.push_str(data.as_slice());
                    Ok(())
                },
                Some(&RequestTag) | Some(&EventTag) | Some(&EnumTag) => {
                    Ok(())
                },
                Some(&EntryTag) => {
                    Ok(())
                },
                Some(&ArgTag) => {
                    Ok(())
                },
                _ => unreachable!(),
            },
            _ => Err("Character data not allowed in this context"),
        }
    }

    fn get_module(&self, cx: &mut ExtCtxt) -> P<Item> {
        unimplemented!()
    }
}

fn import_protocol(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree]) -> Box<MacResult + 'static> {
    let path = match args {
        [TtToken(_, token::LitStr(path))] => Path::new(path.as_str()),
        _ => error!(cx, sp, "argument should be a string literal"),
    };
    let buffer = BufferedReader::new(File::open(&path));
    let mut reader = xml::EventReader::new(buffer);

    let mut schema = Schema::new();

    for event in reader.events() {
        match event {
            events::Error(error) => error!(cx, sp, (format!("XML parse error: {}", error)).as_slice()),
            event => match schema.dispatch(event) {
                Ok(()) => (),
                Err(err) => error!(cx, sp, (format!("XML schema error: {}", err)).as_slice()),
            }
        }
    }

    return box MacItem(schema.get_module(cx));
}

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc::plugin::Registry) {
    reg.register_macro("import_protocol", import_protocol);
}
