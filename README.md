# debug-stub-derive

[![debug-stub-derive on crates.io][cratesio-image]][cratesio]
[![debug-stub-derive on docs.rs][docsrs-image]][docsrs]

[cratesio-image]: https://img.shields.io/crates/v/debug_stub_derive.svg
[cratesio]: https://crates.io/crates/debug_stub_derive
[docsrs-image]: https://docs.rs/debug_stub_derive/badge.svg?version=0.3.0
[docsrs]: https://docs.rs/debug_stub_derive/0.3.0/

A drop-in replacement for `#[derive(Debug)]`
that supports replacement values for members which do not implement 
[`fmt::Debug`](https://doc.rust-lang.org/std/fmt/trait.Debug.html).

## Usage

Add this to your `Cargo.toml`:

```toml
[dependencies]
debug_stub_derive = "0.3.0"
```

and this to your crate root:

```rust
#[macro_use]
extern crate debug_stub_derive;
```

## License

Licensed under either of
 * Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)
at your option.


### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you shall be dual licensed as above, without any
additional terms or conditions.

