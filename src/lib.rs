#![feature(phase, globs)]

#[phase(plugin)]
extern crate proto_generator;

import_protocol!("/usr/share/wayland/wayland.xml")

