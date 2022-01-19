use std::io::{Read, Seek};
use crate::webasm::ast::{CodeSection, Module};
use crate::webasm::lazy::{Lazy, Readable};

pub fn hydrate_module(module: &Module, src: &mut (impl Read + Seek)) {
    hydrate_section(&module.type_section, src);
    hydrate_section(&module.func_section, src);
    hydrate_section(&module.export_section, src);
    hydrate_section(&module.code_section, src);
    module.other_sections.iter().for_each(|section| { section.1.get(src); });

    if let Some(code) = &module.code_section {
        hydrate_code_section(code.get(src), src);
    }
}

fn hydrate_section<T: Readable>(dst: &Option<Lazy<T>>, src: &mut (impl Read + Seek)) {
    if let Some(section) = &dst {
        section.get(src);
    }
}

fn hydrate_code_section(section: &CodeSection, src: &mut (impl Read + Seek)) {
    section.0.iter().for_each(|code| {
        code.expr.get(src);
    });
}