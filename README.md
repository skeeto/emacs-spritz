# Spritz in Emacs

Spritz is a new stream cipher by Ron Rivest and Jacob Schuldt. It's
basically a redesign of RC4 using modern cryptographics tools and
knowledge. This implementation is based directly on the whitepaper,
implementing the the exact set of low-level primitives.

* http://people.csail.mit.edu/rivest/pubs/RS14.pdf

Spritz is a "sponge function" and, as such, has an API analgous to a
sponge. It absorbs bytes and is squeezed to emit bytes. This is the
sponge API:

* `spritz-create`      -- initializes a new state
* `spritz-copy`        -- copy an existing state
* `spritz-absorb`      -- absorb bytes into the state
* `spritz-absorb-stop` -- cleanly separates different inputs
* `spritz-drip`        -- output a single byte
* `spritz-squeeze`     -- output multiple bytes (convenience)

These methods are used to implement all sorts of cryptographic
functions: hash, MAC, PRNG, encryption, and decryption. Some of these
are provided as a slightly highter level API:

* `spritz-hash`        -- hashes a string or a buffer
* `spritz-random`      -- random number generator (like `cl-random`)
* `spritz-random-iv`   -- generate a secure initialization vector (IV)
* `spritz-random-uuid` -- generate a version 4 UUID

The "random" functions will, by default, use an internal random state
that is seeded at package load time by a dozen different sources of
entropy. This ensures the output of the PRNG functions is secure.
Additional entropy, such as bytes from /dev/random, can be absorbed
into this state by passing it to `spritz-random-stir'. Accidentally
absorbing non-random data is harmless.

Finally, two user interface commands are defined. These use Spritz as
both a stream cipher and key derivation function.

* `spritz-encrypt-buffer`
* `spritz-decrypt-buffer`

Test vectors can be found in the unit tests.
